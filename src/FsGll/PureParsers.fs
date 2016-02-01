module FsGll.PureParsers

open System
open System.IO
open System.Collections
open System.Collections.Generic
open FSharpx.Prelude
open System.Threading
open FsGll.InputStream

//let trace (s: Lazy<string>) = printfn "%s" s.Value
let trace _ = ()

//
//let mutable foundInDone = 0
//let mutable notFoundInDone = 0
//let mutable savedToPopped = 0
//let mutable foundInPopped = 0
//let mutable notFoundInPopped = 0
//let mutable savedToDone = 0
//
//let mutable foundInSaved = 0
//let mutable savedToSaved = 0
//
//let mutable queuePushed = 0
//
//let mutable trampolinesCreated = 0

// dirty hack to overcome Set and Map problem
type TUid = int
type Unique() =
    static let mutable (au : TUid) = 0
    let uid : TUid = Interlocked.Increment(&au)
    member this.Uid = uid
    interface IComparable with 
        override this.CompareTo that = 
            if this.Equals(that) then 0
            else 
                match that with 
                | :? Unique as that -> 
                    let h1, h2 = this.GetHashCode(), that.GetHashCode()
                    if h1 <> h2 
                    then h1 - h2 
                    else uid - that.Uid //else (if uid = that.Uid then 0 else failwith msgLibraryError) // TODO uid - that.Uid 
                | _ -> invalidArg "that" msgLibraryError

    override this.Equals that = match that with :? Unique as that -> uid = that.Uid | _ -> false
    override this.GetHashCode () = uid.GetHashCode()

[<AbstractClass>]
type GParserResult<'a> (tail: InputStream<'a>) = 
    inherit Unique()
    abstract member Succeeded : bool
    member this.Failed = not this.Succeeded
    member this.Tail = tail
    
type GSuccess<'a, 'r when 'r: equality> (value: 'r, tail: InputStream<'a>) = 
    inherit GParserResult<'a>(tail)
    let myhash = lazy (hash value + hash tail)
    override this.Succeeded = true
    member this.Value = value
    override this.ToString() = sprintf "SUCC(%A|%A)" value tail
    override this.Equals that = 
        match that with 
        | :? GSuccess<'a, 'r> as that -> value = that.Value && tail = that.Tail
        | _ -> false
    override this.GetHashCode() = myhash.Value

type GFailure<'a> (msg: string, tail: InputStream<'a>) = 
    inherit GParserResult<'a> (tail)
    let myhash = lazy (hash msg + hash tail)
    override this.Succeeded = false
    member this.Message = msg
    override this.Equals that = 
        match that with 
        | :? GFailure<'a> as that -> msg = that.Message && tail = that.Tail
        | _ -> false
    override this.GetHashCode() = myhash.Value
    override this.ToString() = sprintf "FAIL(m:%s|%A)" msg tail

let success<'a, 'r when 'r: equality> value tail = new GSuccess<'a, 'r>(value, tail) :> GParserResult<_>
let failure<'a> msg tail = new GFailure<'a>(msg, tail) :> GParserResult<'a>

let parserResult<'a, 'r when 'r: equality> (r: GParserResult<'a>) = 
    match r with
    | :? GSuccess<'a, 'r> as succ -> Success (succ.Value)
    | :? GFailure<'a> as fail -> Failure (fail.Message, fail.Tail)
    | _ -> failwith msgLibraryError

open FSharpx.State

let inline modify<'s> (f: 's -> 's) = getState >>= (putState << f)

type PState<'a> = State<unit, Trampoline<'a> >

and Cont() = 
    static member New<'a> (fn: GParserResult<'a> -> PState<'a>) = new Continuation<'a>(fn)
    static member WithSucc<'a, 'r when 'r : equality> (sf: GSuccess<'a, 'r> -> GParserResult<'a>) (f: Continuation<'a>) = 
        Cont.New(function :? GSuccess<'a, 'r> as s1 -> sf s1 |> f.F           
                        | :? GFailure<'a> as fail -> fail :> GParserResult<'a> |> f.F
                        | _ -> failwith msgLibraryError)

and Continuation<'a>(f: GParserResult<'a> -> PState<'a>) = 
    inherit Unique()
    member this.F : (GParserResult<'a> -> PState<'a>)  = f
    static member New fn = new Continuation<'a>(fn)
    override this.Equals that = 
        let tobj = (that :?> Continuation<'a>).F
        (f :> Object).Equals(tobj)
    override this.GetHashCode() = base.GetHashCode()
    
and RSet<'a> = Set<GParserResult<'a> >
and SSet<'a> = Set<GParserResult<'a> >
and FSet<'a> = Set<Continuation<'a> >
and HOMap<'k,'v when 'k: comparison> = Map<'k,'v>

and [<AbstractClass>] GParser<'a> () = 
    inherit Unique()
    abstract member Chain : InputStream<'a> -> Continuation<'a> -> PState<'a>

and [<AbstractClass>] Parser<'a, 'r when 'r : equality> () = 
    inherit GParser<'a> ()
    abstract member Apply : InputStream<'a> -> GParserResult<'a> list

and [<AbstractClass>] NonTerminalParser<'a, 'r when 'r : equality> () = 
    inherit Parser<'a, 'r> ()
    override this.Apply (inp: InputStream<'a>) = 
        { 
            Queue = []; Done = Map.empty; Popped = Map.empty; DisRes = Map.empty
            Backlinks = Map.empty; Saved = Map.empty; Stopped = false
            Results = Set.empty; Postrace = []
        }
        |> 
        state {
            do! this.Chain(inp) (Continuation<_>.New(fun res ->
                let r = if res.Succeeded && not res.Tail.IsEmpty 
                        then failure "UnexpectedTrailingChars" (res.Tail) else res
                modify <| fun t -> { t with Results = t.Results.Add(r) }
            ))
            do! T.Run ()
            let! t = getState
            let successes, failures = t.Results |> Seq.toList |> List.partition (fun r -> r.Succeeded) 
            return (if List.isEmpty successes then failures else successes)
        } 
        |> fst
        

and [<AbstractClass>] TerminalParser<'a, 'r when 'r : equality> () = 
    inherit Parser<'a, 'r> ()

    abstract member Parse : InputStream<'a> -> GParserResult<'a>

    override this.Chain (inp) (f) = this.Parse inp |> f.F
    override this.Apply (inp: InputStream<'a>) = 
            match this.Parse(inp) with 
            | :? GSuccess<'a, 'r> as succ1 -> [succ1]
            | x -> [x]

and DisjunctiveParser<'a, 'r when 'r : equality>(left: Parser<'a, 'r>, right: Parser<'a, 'r>) as disj =
    inherit NonTerminalParser<'a, 'r>()
    
    let gather = lazy (disj.GatherImpl(new HashSet<_>() ) )
    
    member this.GatherImpl(seen: HashSet<DisjunctiveParser<'a, 'r> >) = 
        seen.Add(this) |> ignore
        let recprocess (p: Parser<'a, 'r>) = 
            match p with
            | :? DisjunctiveParser<'a, 'r> as d -> 
                if not <| seen.Contains d then d.GatherImpl(seen) else []
            | p -> [p]

        recprocess(left) @ recprocess(right)
    
    override this.Chain(inp) (f) = state {
        let invoc = (new Unique()).Uid
        do! modify (fun t -> { t with DisRes = t.DisRes.Add(invoc, Set.empty) })
        for p in gather.Value do 
            return! T.Add(p, inp) (Cont.New(fun res -> state {
                let! t = getState
                let results = Map.find invoc t.DisRes
                if not (results.Contains res) then 
                    do! f.F res
                    do! modify (fun t -> { t with DisRes = t.DisRes.Add(invoc, t.DisRes.[invoc].Add res) }) 
            })) 
        }

and Trampoline<'a> = { 
    // R
    Queue: (GParser<'a> * InputStream<'a>) list

    // U_j
    Done: Map<InputStream<'a>, Set<GParser<'a> > >

    // P
    Popped: Map<InputStream<'a>, HOMap<GParser<'a>, SSet<'a> > >

    // GSS back edges
    Backlinks: Map<InputStream<'a>, HOMap<GParser<'a>, FSet<'a> > >

    // prevents divergence in cyclic GSS traversal
    Saved: HOMap<GParserResult<'a>, FSet<'a> >

    Stopped: bool
    Results: SSet<'a>

    Postrace: string list
    DisRes: Map<TUid, SSet<'a> >
} 
and T () =

    static member Run<'a> () : PState<'a> = state { 
        let! t = getState
        if not (List.isEmpty t.Queue || t.Stopped) then
            do! T.Step ()
            return! T.Run ()
    }
         
    static member Stop<'a> () : PState<'a> = modify (fun t -> { t with Stopped = true })

    static member Step<'a> () : PState<'a> = state { 
        let! t1 = getState
        let p, s = t1.Queue.Head
        do! modify (fun t -> { t with Queue = List.tail t.Queue })
        trace <| lazy (sprintf "Removed: %A" (p, s))
        do! p.Chain (s) (Cont.New(fun res -> state {
            let! t = getState
            let popped =
                let parsers = match t.Popped.TryFind(s) with | Some ps -> ps | _ -> Map.empty
                let rset = match parsers.TryFind(p) with | Some rs -> Set.add res rs 
                                                         | _ -> (if res.Succeeded then Set.singleton res else Set.empty)
                t.Popped.Add(s, parsers.Add(p, rset))

            do! modify (fun t -> { t with Popped = popped })
            if res.Succeeded then trace(lazy (sprintf "Saved (to popped): %A *=> %A\n" (p, s) res))
            let! t = getState
            let links = t.Backlinks.[s].[p] |> Seq.toList
            // TODO OK  ???
            match t.Saved.TryFind(res) with
            | Some _ ->
                for f in links do
                    let! t = getState
                    let set = t.Saved.[res]              // TODO CAN BE A BUG
                    if not (set.Contains f) then
                        do! modify (fun t -> { t with Saved = t.Saved.Add(res, set.Add f) })
                        do! res |> f.F
            | _ -> 
                do! modify (fun t -> { t with Saved = t.Saved.Add (res, Set.empty) } )
                for f in links do
                    let! t = getState
                    let set = t.Saved.[res]
                    do! modify (fun t -> { t with Saved = t.Saved.Add(res, set.Add f) }) 
                    do! res |> f.F
            }
        ))
        }

    static member Add<'a> (p: GParser<'a>, s: InputStream<'a>) (f: Continuation<'a>) : PState<'a> = state {
        let tuple = (p, s)
        let! t = getState
        let backlinks = 
            let parsers = match t.Backlinks.TryFind(s) with | Some ps -> ps | _ -> Map.empty
            let fset = match parsers.TryFind(p) with | Some fs -> Set.add f fs | _ -> Set.singleton f
            t.Backlinks.Add(s, parsers.Add(p, fset))
        do! modify <| fun t -> { t with Backlinks = backlinks }
        let! t = getState
        match t.Popped.TryFind(s) with
        | Some parsers when parsers.ContainsKey p ->
            for res in parsers.[p] do
                trace(lazy(sprintf "Revisited: %A *=> %A\n" tuple res))
                do! f.F (res) // if we've already done that, use the result
        | _ ->
            let parsers = match t.Done.TryFind(s) with | Some ps -> ps | _ -> Set.empty
            if not (parsers.Contains p) then 
                do! modify (fun t -> { t with Queue = tuple :: t.Queue; Done = t.Done.Add(s, parsers.Add p) })
                trace(lazy(sprintf "Added: %A\n" tuple))
        }

#nowarn "1189"

let inline private ntp<'a, 'rret when 'rret : equality> chain = 
    { new NonTerminalParser<'a, 'rret>() with 
      override nt.Chain s f = chain s f
    } :> Parser<'a, 'rret>

let inline preturn<'a, 'r when 'r: equality> (a: 'r) : Parser<'a, 'r> = 
    { new TerminalParser<'a, 'r>() 
      with override this.Parse inp = success<'a, 'r> a inp } :> Parser<'a, 'r>

let inline perror<'a, 'r when 'r: equality> (msg: string) : Parser<'a, 'r> = 
    { new TerminalParser<'a, 'r>() 
      with override this.Parse inp = failure<'a> msg inp } :> Parser<'a, 'r>

let inline satisfy<'a when 'a: equality> (pred : 'a -> bool) = 
    { new TerminalParser<'a, 'a>() with 
      override this.Parse (s: InputStream<'a>) =
        if s.IsEmpty then failure<'a> "UnexpectedEOF" s
        else
            let h = s.Head
            if h |> pred then success<'a, 'a> h (s.Drop)
            else failure<'a> "Unexpected token" s }

let (>>==) (p: Parser<'a, 'r>) (fn: 'r -> Parser<'a, 'r2>) : Parser<'a, 'r2> = 
    ntp <| fun s f -> T.Add(p, s) <| Cont.New (function
                | :? GSuccess<'a, 'r> as s1 -> fn(s1.Value).Chain(s1.Tail) (f)
                | fail -> fail |> f.F)

let (>>=)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: Parser<'a, 'r>) (fn: 'r -> Parser<'a, 'r2>) : Parser<'a, 'r2> = 
    { new NonTerminalParser<'a, 'r2>() with 
      override nt.Chain(inp: InputStream<'a>) (cont) =
            T.Add(p, inp) <| Cont.New (fun res ->
                match res with 
                | :? GSuccess<'a, 'r> as s1 -> fn(s1.Value).Chain(s1.Tail) (cont)
                | fail -> fail |> cont.F )  
    } :> Parser<'a, 'r2>

let many1<'a, 'r when 'r : equality> (p: Parser<'a, 'r>) = 
    { new NonTerminalParser<'a, 'r list>() with 
        override q.Chain s f = 
            T.Add(p, s) (Cont.New (fun r -> state {
                match r with
                | :? GSuccess<'a, 'r> as s1 -> 
                    do! success<'a, 'r list> [s1.Value] s1.Tail |> f.F
                    do! T.Add(q, s1.Tail) (Cont.WithSucc (fun s2 -> success<'a, 'r list> (s1.Value :: s2.Value) s2.Tail) f)
                | fail -> do! fail |> f.F } ) ) 
            
    } :> Parser<'a, 'r list>

let many<'a, 'r when 'r : equality> (p: Parser<'a, 'r>) = 
    { new NonTerminalParser<'a, 'r list>() with 
        override nt.Chain s f = state {
            do! success<'a, 'r list> [] s |> f.F
            do! T.Add(many1 p, s) f }
    } :> Parser<'a, 'r list>

let opt<'a, 'r when 'r : equality> (p: Parser<'a, 'r>) = 
    { new NonTerminalParser<'a, 'r option>() with 
        override nt.Chain(s) (f) : PState<'a> = state {
            do! (success<'a, 'r option> None s) |> f.F
            do! T.Add(p, s) (Cont.WithSucc (fun r -> success (Some r.Value) r.Tail) f) }
    } :> Parser<'a, 'r option>

let epsilon<'a> = preturn ()

let (<|>)<'a, 'r when 'r : equality> (a: Parser<'a, 'r>) (b: Parser<'a, 'r>) = 
    new DisjunctiveParser<'a, 'r>(a, b) :> Parser<'a, 'r>

let (<|>%) p x = p <|> preturn x

let (<?>) p msg = p <|> perror msg

let (>>%) this x = this >>= fun _ -> preturn x

//let (.>>)<'a, 'r1, 'r2 when 'r1: equality and 'r2: equality> (p: Parser<'a, 'r1>) (q: Parser<'a, 'r2>) = 
//    p >>= fun (x: 'r1) -> q >>% x
let (.>>)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: Parser<'a, 'r>) (q: Parser<'a, 'r2>) : Parser<'a, 'r> = 
    { new NonTerminalParser<'a, 'r>() with 
      override nt.Chain s f = 
            T.Add(p, s) <| Cont.New(function
                 | :? GSuccess<'a, 'r> as s1 -> 
                    //q.Chain(s1.Tail) <| Cont.WithSucc<'a, 'r2> (fun s2 -> success<'a, 'r> s1.Value s2.Tail) f
                    q.Chain(s1.Tail) <| Cont.New(function 
                                        | :? GSuccess<'a, 'r2> as s2 -> success<'a, 'r> s1.Value s2.Tail |> f.F
                                        | fail -> fail |> f.F)
                 | fail -> fail |> f.F )
    } :> Parser<'a, 'r>

//let (>>.) this that = this >>= fun _ -> that
let (>>.)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: Parser<'a, 'r>) (q: Parser<'a, 'r2>) : Parser<'a, 'r2> = 
    { new NonTerminalParser<'a, 'r2>() with 
      override nt.Chain s f = 
          T.Add(p, s) <| Cont.New(fun r -> if r.Succeeded then q.Chain(r.Tail) f else r |> f.F)
    } :> Parser<'a, 'r2>

let (.>>.) p1 p2 = 
    p1 >>= fun a ->
    p2 >>= fun b -> preturn (a, b)

let between popen pclose (p: Parser<'a, 'r>) : Parser<'a, 'r> = 
    popen >>. p .>> pclose

//let (|>>) p fn = p >>= (preturn << fn)
let (|>>)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: Parser<'a, 'r>) fn = 
    match p with 
    | :? TerminalParser<'a, 'r> as p -> 
        { new TerminalParser<'a, 'r2>() with 
          override this.Parse(s) =
              match p.Parse(s) with
              | :? GSuccess<'a, 'r> as r -> success<'a, 'r2> (fn r.Value) r.Tail
              | fail -> fail
        } :> Parser<'a, 'r2>
    | _ -> 
        { new NonTerminalParser<'a, 'r2>() with 
          override nt.Chain s f = T.Add(p, s) (Cont.WithSucc (fun r -> success<'a, 'r2> (fn r.Value) r.Tail) f)
        } :> Parser<'a, 'r2>

let pipe2 p1 p2 fn = 
    p1 >>= fun a ->
    p2 >>= fun b -> preturn (fn a b)

let pipe3 p1 p2 p3 fn = 
    p1 >>= fun a ->
    p2 >>= fun b ->
    p3 >>= fun c -> preturn (fn a b c)

let pipe4 p1 p2 p3 p4 fn = 
    p1 >>= fun a ->
    p2 >>= fun b ->
    p3 >>= fun c -> 
    p4 >>= fun d -> preturn (fn a b c d)

let sepBy (p: Parser<'a, 'r>) sep : Parser<'a, 'r list> = 
    pipe2 p (many (sep >>. p)) (fun hd tl -> hd :: tl) <|>% []

let notFollowedBy (p: TerminalParser<'a, 'r>) : Parser<'a, unit> = 
    { new NonTerminalParser<'a, unit> () with
      override nt.Chain s f =
          f.F <| if (p.Parse s).Failed then success<'a, unit> () s
                 else failure "SyntaxError (notFollowedBy)" s 
    } :> Parser<'a, unit>
    
let private dummyParser<'a, 'r when 'r : equality> = 
   { new TerminalParser<'a, 'r>() with override this.Parse s = failwith msgDummyUsed } :> Parser<'a, 'r>

let createParserForwardedToRef<'a, 'r when 'r : equality>(name: string) = 
    let r = ref dummyParser<'a, 'r>
    { new NonTerminalParser<'a, 'r>() with 
      override this.Chain s f = (!r).Chain s f } :> Parser<'a, 'r>, r

let runParser (p: Parser<'a, 'r>) (s: 'a seq) : ParserResult<'a, 'r> list = 
    let res = p.Apply(new ArrayInputStream<'a>(Seq.toArray s, 0))
    if res.IsEmpty then failwith msgLibraryError
    res |> List.map parserResult<'a, 'r>
