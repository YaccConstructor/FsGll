module FsGll.PureParsers

open System
open System.IO
open System.Collections
open System.Collections.Generic
open FSharpx.Prelude
open System.Threading


//let trace s = printfn "%s" s
let trace _ = ()

let mutable foundInDone = 0
let mutable notFoundInDone = 0
let mutable savedToPopped = 0
let mutable foundInPopped = 0
let mutable notFoundInPopped = 0
let mutable savedToDone = 0

let mutable foundInSaved = 0
let mutable savedToSaved = 0

let mutable queuePushed = 0

let mutable trampolinesCreated = 0

type InputStream<'a> (underlying: 'a seq, ind: int) = 
    member this.Ind = ind
    member this.Head = Seq.head underlying
    member this.IsEmpty = Seq.isEmpty underlying
    member this.Drop x = new InputStream<_>(Seq.skip x underlying, ind + x)
    override this.ToString() = underlying |> Seq.map (fun x -> x.ToString()) |> Seq.fold (+) "" 

    // It looks like F# Collections.Set<_> uses only IComparable interface and 
    // forgets about check objects equality with .Equals, so dirty hacks is necessary
    override this.Equals that = 
        match that with
        | :? InputStream<'a> as s -> ind = s.Ind
        | _ -> false
    override this.GetHashCode() = ind.GetHashCode()
    interface IComparable with
        override this.CompareTo that =
            match that with 
            | :? InputStream<'a> as that -> this.Ind - that.Ind
            | _ -> invalidArg "that" "Can not compare"

// dirty hack to overcome Set problem
type Unique() =
    static let mutable (au : int) = 0
    let uid : int = Interlocked.Increment(&au)
    member this.Uid = uid

    // Some dirty code to store Unique in Set
    interface IComparable with 
        override this.CompareTo that = 
            if this.Equals(that) then 0
            else 
                match that with 
                | :? Unique as that -> 
                    let h1, h2 = this.GetHashCode(), that.GetHashCode()
                    if h1 <> h2 then h1 - h2 else uid - that.Uid 
                | _ -> invalidArg "that" "Can not compare"
    override this.Equals _ = false 
    override this.GetHashCode() = uid.GetHashCode()

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

type ParserResult<'r> = 
   | Success of value: 'r
   | Failure of msg: string

let parserResult<'a, 'r when 'r: equality> (r: GParserResult<'a>) = 
    match r with
    | :? GSuccess<'a, 'r> as succ -> Success (succ.Value)
    | :? GFailure<'a> as fail -> Failure (fail.Message)
    | _ -> failwith "impossible"


let mutable private gParserAutoincrement = 0
let mutable private disjunctiveParserChainInvokations = 0

type Cont() = 
    static member New<'a> fn = new Continuation<'a>(fn)
    static member WithSucc<'a, 'r when 'r : equality> sf (f: Continuation<'a>) = 
        Cont.New(fun (t, r) ->
            match r with
            | :? GSuccess<'a, 'r> as s1 -> (t, sf s1) |> f.F
            | fail -> (t, fail) |> f.F)

and Continuation<'a>(f: Trampoline<'a> * GParserResult<'a> -> Trampoline<'a>) = 
    inherit Unique()
    member this.F = f
    static member New fn = new Continuation<'a>(fn)
    override this.Equals that = 
        //Object.ReferenceEquals (f, (that :?> Continuation<'a>).F) |> ignore
        //let refeq = Object.ReferenceEquals (f, (that :?> Continuation<'a>).F)
        let tobj = (that :?> Continuation<'a>).F
        let eqeq = (f :> Object).Equals(tobj)
        eqeq
    override this.GetHashCode() = base.GetHashCode()
    
and RSet<'a> = Set<GParserResult<'a> >
and SSet<'a> = Set<GParserResult<'a> >
and FSet<'a> = Set<Continuation<'a> >
and HOMap<'k,'v when 'k: comparison> = Map<'k,'v>

and [<AbstractClass>] GParser<'a> () = 
    inherit Unique()
    abstract member Chain : Trampoline<'a> * InputStream<'a> -> Continuation<'a> -> Trampoline<'a>

and [<AbstractClass>] Parser<'a, 'r when 'r : equality> () = 
    inherit GParser<'a> ()
    abstract member Apply : InputStream<'a> -> GParserResult<'a> list

and [<AbstractClass>] NonTerminalParser<'a, 'r when 'r : equality> () = 
    inherit Parser<'a, 'r> ()
    override this.Apply (inp: InputStream<'a>) = 
        let t = { 
            Queue = []; Done = Map.empty; Popped = Map.empty; DisjResults = Map.empty
            Backlinks = Map.empty; Saved = Map.empty; Stopped = false
            Successes = Set.empty; Failures = Set.empty; Postrace = []
        }

        let t = this.Chain(t, inp) (Continuation<_>.New(fun (t, res) ->
            if res.Succeeded && not res.Tail.IsEmpty 
            then failure "UnexpectedTrailingChars" (res.Tail) else res
            |> t.AddResult
        ))

        let t = t.Run()

        (if t.Successes.Count = 0 then t.Failures else t.Successes) |> Seq.toList

and [<AbstractClass>] TerminalParser<'a, 'r when 'r : equality> () = 
    inherit Parser<'a, 'r> ()

    abstract member Parse : InputStream<'a> -> GParserResult<'a>

    override this.Apply (inp: InputStream<'a>) = 
            match this.Parse(inp) with 
            | :? GSuccess<'a, 'r> as succ1 -> [succ1]
            | x -> [x]
    
    override this.Chain (t, inp) (f) = (t, this.Parse inp) |> f.F

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
    
    override this.Chain(t, inp) (f) = 
        let lst = gather.Value
        let invocation = (new Unique()).Uid
        lst 
        |> List.fold (fun t p -> 
            t.Add(p, inp) (Cont.New(fun (t, res) ->
                let results = Map.find invocation t.DisjResults
                if results.Contains (res) then t
                else { f.F (t, res) with 
                        DisjResults = t.DisjResults.Add(invocation, results.Add(res)) }
            ))
        ) { t with DisjResults = t.DisjResults.Add(invocation, Set.empty) }
    
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
    Successes: SSet<'a>
    Failures: SSet<'a>
    Postrace: string list
    DisjResults: Map<int, SSet<'a> >
} with
    member t.AddResult(r: GParserResult<'a>) : Trampoline<'a> = 
        if r.Succeeded 
        then { t with Successes = t.Successes.Add(r) }
        else { t with Failures = t.Failures.Add(r) }

    member t.Run() : Trampoline<'a> = 
        if List.isEmpty t.Queue || t.Stopped then t
        else t.Step().Run()
         
    member t.Stop() : Trampoline<'a> = { t with Stopped = true }

    member t.Step() : Trampoline<'a> = 
        let p, s = t.Queue.Head
        let t = { t with Queue = List.tail t.Queue }
        p.Chain (t, s) (Cont.New(fun (t, res) ->
            let popped =
                if not <| res.Succeeded then t.Popped
                else
                    //savedToPopped <- savedToPopped + 1 
                    let parsers = match t.Popped.TryFind(s) with | Some ps -> ps | _ -> Map.empty
                    let rset = match parsers.TryFind(p) with | Some rs -> rs | _ -> Set.empty
                    t.Popped.Add(s, parsers.Add(p, rset.Add(res)))

            let t = { t with Popped = popped }
            match t.Saved.TryFind(res) with
            | Some _ ->
                let links = t.Backlinks.[s].[p]
                links |> Seq.fold (fun t f ->
                    let set = t.Saved.[res]              // CAN BE A BUG
                    if (set.Contains(f)) then t
                    else ({ t with Saved = t.Saved.Add(res, set.Add f) }, res) |> f.F
                ) t
            | _ -> 
                let set = Set.empty
                let t = { t with Saved = t.Saved.Add (res, set) }
                t.Backlinks.[s].[p] |> Seq.fold (fun t f ->
                    ({ t with Saved = t.Saved.Add(res, set.Add f) }, res) |> f.F
                ) t
        ))

    member t.Add (p: GParser<'a>, s: InputStream<'a>) (f: Continuation<'a>) : Trampoline<'a> = 
        let tuple = (p, s)

        let backlinks = 
            let parsers = match t.Backlinks.TryFind(s) with | Some ps -> ps | _ -> Map.empty
            let fset = match parsers.TryFind(p) with | Some fs -> fs | _ -> Set.empty
            t.Backlinks.Add(s, parsers.Add(p, fset.Add(f)))
        let t = { t with Backlinks = backlinks }
        match t.Popped.TryFind(s) with
        | Some parsers when parsers.ContainsKey p ->
            parsers.[p] |> Seq.fold (fun t res -> f.F (t, res)) t // if we've already done that, use the result
        | _ ->
            let parsers = match t.Done.TryFind(s) with | Some ps -> ps | _ -> Set.empty
            if parsers.Contains p then t
            else { t with Queue = tuple :: t.Queue; Done = t.Done.Add(s, parsers.Add p) }

#nowarn "1189"

let preturn<'a, 'r when 'r: equality> (a: 'r) : Parser<'a, 'r> = 
    { new TerminalParser<'a, 'r>() 
      with override this.Parse inp = success<'a, 'r> a inp } :> Parser<'a, 'r>

let perror<'a, 'r when 'r: equality> (msg: string) : Parser<'a, 'r> = 
    { new TerminalParser<'a, 'r>() 
      with override this.Parse inp = failure<'a> msg inp } :> Parser<'a, 'r>

let satisfy<'a when 'a: equality> (pred : 'a -> bool) = 
    { new TerminalParser<'a, 'a>() with 
      override this.Parse (s: InputStream<'a>) =
        if s.IsEmpty then failure<'a> "UnexpectedEOF" s
        else
            let h = s.Head
            if h |> pred then success<'a, 'a> h (s.Drop 1)
            else failure<'a> "Unexpected token" s }

let (>>=)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: Parser<'a, 'r>) (fn: 'r -> Parser<'a, 'r2>) : Parser<'a, 'r2> = 
    { new NonTerminalParser<'a, 'r2>() with 
      override nt.Chain(t, inp) (cont) = 
            t.Add(p, inp) (Cont.New (fun (t, res) -> 
                match res with 
                | :? GSuccess<'a, 'r> as succ1 -> fn(succ1.Value).Chain(t, succ1.Tail) (cont)
                | fail -> (t, fail) |> cont.F))
    } :> Parser<'a, 'r2>

let many1<'a, 'r when 'r : equality> (this: Parser<'a, 'r>) = 
    { new NonTerminalParser<'a, 'r list>() with 
        override nt.Chain(t, inp) (f) = 
            t.Add(this, inp) (Cont.New (fun (t, r) -> 
                match r with
                | :? GSuccess<'a, 'r> as s1 -> 
                    let t = (t, success<'a, 'r list> [s1.Value] s1.Tail) |> f.F
                    t.Add(nt, s1.Tail) (Cont.WithSucc (fun s2 -> success<'a, 'r list> (s1.Value :: s2.Value) s2.Tail) f)
                | fail -> (t, fail) |> f.F
            ))
    } :> Parser<'a, 'r list>

let many<'a, 'r when 'r : equality> (this: Parser<'a, 'r>) = 
    { new NonTerminalParser<'a, 'r list>() with 
        override nt.Chain (t, s) f =
            let t = (t, success<'a, 'r list> [] s) |> f.F
            t.Add(many1 this, s) f
    } :> Parser<'a, 'r list>

let opt<'a, 'r when 'r : equality> (this: Parser<'a, 'r>) = 
    { new NonTerminalParser<'a, 'r option>() with 
        override nt.Chain(t, s) (f) = 
            let t = (t, success<'a, 'r option> None s) |> f.F
            t.Add(this, s) (Cont.WithSucc (fun r -> success (Some r.Value) r.Tail) f)
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
      override nt.Chain(t, inp) (f) = 
            t.Add(p, inp) (Cont.New(fun (t, r) ->
                 match r with 
                 | :? GSuccess<'a, 'r> as s1 -> 
                     q.Chain(t, s1.Tail) (Cont.WithSucc (fun s -> success<'a, 'r> s1.Value s.Tail) f)
                 | fail -> (t, fail) |> f.F))
    } :> Parser<'a, 'r>

//let (>>.) this that = this >>= fun _ -> that
let (>>.)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: Parser<'a, 'r>) (q: Parser<'a, 'r2>) : Parser<'a, 'r2> = 
    { new NonTerminalParser<'a, 'r2>() with 
      override nt.Chain (t, s) f = 
          t.Add(p, s) (Cont.New(fun (t, r) -> if r.Succeeded then q.Chain(t, r.Tail) (f) else (t, r) |> f.F))
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
          override nt.Chain (t, s) f = 
              t.Add(p, s) (Cont.WithSucc (fun r -> success<'a, 'r2> (fn r.Value) r.Tail) f)
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
      override this.Chain(t, inp) (f) =
          f.F <| if (p.Parse inp).Failed then (t, success<'a, unit> () inp) 
                 else (t, failure "SyntaxError (notFollowedBy)" inp) 
    } :> Parser<'a, unit>
    
let private dummyParser<'a, 'r when 'r : equality> = 
   { new TerminalParser<'a, 'r>() with override this.Parse s = failwith "DummyParser" } :> Parser<'a, 'r>

let createParserForwardedToRef<'a, 'r when 'r : equality>(name:string) = 
    let r = ref dummyParser<'a, 'r>
    { new NonTerminalParser<'a, 'r>() with 
      override this.Chain(t, inp) (f) = (!r).Chain(t, inp) (f) } :> Parser<'a, 'r>, r
