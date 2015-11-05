module PureParsers

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



[<AbstractClass>]
type GParserResult<'a> (tail: InputStream<'a>) = 
    abstract member Succeeded : bool
    member this.Failed = not this.Succeeded
    member this.Tail = tail

    // Some dirty code to store results in Set
    override this.Equals that = true
    override this.GetHashCode() = 0
    interface IComparable with override this.CompareTo that = 0
    
type GSuccess<'a, 'r when 'r: equality> (value: 'r, tail: InputStream<'a>) = 
    inherit GParserResult<'a>(tail)
    let myhash = lazy (hash value + hash tail)
    override this.Succeeded = true
    member this.Data = (value, tail)
    member this.Value = value
    override this.ToString() = sprintf "SUCC(%A|%A)" value tail
    override this.Equals that = 
        match that with 
        | :? GSuccess<'a, 'r> as that -> value = that.Value && tail = that.Tail
        | _ -> false
    override this.GetHashCode() = myhash.Value
    interface IComparable with
        override this.CompareTo (that) = this.GetHashCode() - that.GetHashCode()

type GFailure<'a> (msg: string, tail: InputStream<'a>) = 
    inherit GParserResult<'a> (tail)
    override this.Succeeded = false
    member this.Message = msg
    override this.Equals that = 
        match that with 
        | :? GFailure<'a> as that -> msg = that.Message && tail = that.Tail
        | _ -> false
    override this.GetHashCode() = hash msg + hash tail
    override this.ToString() = sprintf "FAIL(m:%s|%A)" msg tail
    interface IComparable with
        override this.CompareTo (that) = this.GetHashCode() - that.GetHashCode()

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

type Continuation<'a>(f: Trampoline<'a> * GParserResult<'a> -> Trampoline<'a>) = 
    member this.Apply = f
    static member New fn = new Continuation<'a>(fn)
    with interface IComparable with override this.CompareTo that = 0
    
and RSet<'a> = Set<GParserResult<'a> >
and SSet<'a> = Set<GParserResult<'a> >
and FSet<'a> = Set<Continuation<'a> >
and HOMap<'k,'v when 'k: comparison> = Map<'k,'v>

and [<AbstractClass>] GParser<'a> () as p = 
    let uid = Interlocked.Increment(&gParserAutoincrement)
    abstract member Chain : Trampoline<'a> * InputStream<'a> -> Continuation<'a> -> Trampoline<'a>
    member this.Uid = uid
    interface IComparable with
        override this.CompareTo that =
            match that with 
            | :? GParser<'a> as that -> this.Uid - that.Uid
            | _ -> invalidArg "that" "Can not compare"

and [<AbstractClass>] Parser<'a, 'r when 'r : equality> () = 
    inherit GParser<'a> ()
    abstract member Apply : InputStream<'a> -> GParserResult<'a> list

and [<AbstractClass>] NonTerminalParser<'a, 'r when 'r : equality> () = 
    inherit Parser<'a, 'r> ()
    override this.Apply (inp: InputStream<'a>) = 
        let t = { 
            queue = []; _done = Map.empty; 
            popped = Map.empty; backlinks = Map.empty;
            saved = Map.empty; stopped = false; 
            successes = Set.empty; failures = Set.empty; postrace = []
            disjResults = Map.empty
        }

        let t = this.Chain(t, inp) (Continuation<_>.New(fun (t, res) ->
            if res.Succeeded && not res.Tail.IsEmpty 
            then failure "UnexpectedTrailingChars" (res.Tail) else res
            |> t.AddResult
        ))

        let t = t.Run()

        (if t.successes.Count = 0 then t.failures else t.successes) |> Seq.toList

and [<AbstractClass>] TerminalParser<'a, 'r when 'r : equality> () = 
    inherit Parser<'a, 'r> ()

    abstract member Parse : InputStream<'a> -> GParserResult<'a>

    override this.Apply (inp: InputStream<'a>) = 
            match this.Parse(inp) with 
            | :? GSuccess<'a, 'r> as succ1 -> [succ1]
            | x -> [x]
    
    override this.Chain (t, inp) (f) = (t, this.Parse inp) |> f.Apply

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
    //static member val ChainInvokations = 0 with get, set
    override this.Chain(t, inp) (f) = 
        let lst = gather.Value
        let invocation = Interlocked.Increment(&disjunctiveParserChainInvokations)
        lst 
        |> List.fold (fun t p -> 
            t.Add(p, inp) (Continuation<_>.New(fun (t, res) ->
                let results = Map.find invocation t.disjResults
                if results.Contains (res) then t
                else { f.Apply (t, res) with 
                        disjResults = t.disjResults.Add(invocation, results.Add(res)) }
            ))
        ) { t with disjResults = t.disjResults.Add(invocation, Set.empty) }
    //interface IComparable with
        //member x.CompareTo y = compare (x.GetHashCode()) (y.GetHashCode())

and Trampoline<'a> = { 
    // R
    queue: (GParser<'a> * InputStream<'a>) list

    // U_j
    _done: Map<InputStream<'a>, Set<GParser<'a> > >

    // P
    popped: Map<InputStream<'a>, HOMap<GParser<'a>, SSet<'a> > >

    // GSS back edges
    backlinks: Map<InputStream<'a>, HOMap<GParser<'a>, FSet<'a> > >

    // prevents divergence in cyclic GSS traversal
    saved: HOMap<GParserResult<'a>, FSet<'a> >

    stopped: bool
    successes: SSet<'a>
    failures: SSet<'a>
    postrace: string list
    disjResults: Map<int, SSet<'a> >
} with
    member t.AddResult(r: GParserResult<'a>) : Trampoline<'a> = 
        if r.Succeeded 
        then { t with successes = t.successes.Add(r) }
        else { t with failures = t.failures.Add(r) }
    member t.Run() : Trampoline<'a> = 
        if (not <| List.isEmpty t.queue) && not t.stopped 
        then t.Step().Run()
        else t
    member t.Stop() : Trampoline<'a> = { t with stopped = true }
    member t.Step() : Trampoline<'a> = 
        let p, s = t.queue.Head
        let t = { t with queue = List.tail t.queue }
        p.Chain (t, s) (Continuation<_>.New (fun (t, res) ->
            let popped =
                if not <| res.Succeeded then t.popped
                else
                    //savedToPopped <- savedToPopped + 1 
                    let parsers = match t.popped.TryFind(s) with | Some ps -> ps | _ -> Map.empty
                    let rset = match parsers.TryFind(p) with | Some rs -> rs | _ -> Set.empty
                    t.popped.Add(s, parsers.Add(p, rset.Add(res)))

            let t = { t with popped = popped }
            match t.saved.TryFind(res) with
            | Some _ ->
                let links = t.backlinks.[s].[p]
                links |> Seq.fold (fun t f ->
                    let set = t.saved.[res]
                    if (set.Contains(f)) then t
                    else ({ t with saved = t.saved.Add(res, set.Add f) }, res) |> f.Apply
                ) t
            | _ -> 
                let set = Set.empty
                let t = { t with saved = t.saved.Add (res, set) }
                t.backlinks.[s].[p] |> Seq.fold (fun t f ->
                    ({ t with saved = t.saved.Add(res, set.Add f) }, res) |> f.Apply
                ) t
        ))

    member t.Add (p: GParser<'a>, s: InputStream<'a>) (f: Continuation<'a>) : Trampoline<'a> = 
        let tuple = (p, s)

        let backlinks = 
            let parsers = match t.backlinks.TryFind(s) with | Some ps -> ps | _ -> Map.empty
            let fset = match parsers.TryFind(p) with | Some fs -> fs | _ -> Set.empty
            t.backlinks.Add(s, parsers.Add(p, fset.Add(f)))
        let t = { t with backlinks = backlinks }
        match t.popped.TryFind(s) with
        | Some parsers when parsers.ContainsKey p ->
            parsers.[p] |> Seq.fold (fun t res ->    // if we've already done that, use the result
                f.Apply (t, res)
            ) t
        | _ ->
            let parsers = match t._done.TryFind(s) with | Some ps -> ps | _ -> Set.empty
            { t with queue = tuple :: t.queue; _done = t._done.Add(s, parsers.Add p) }

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
            t.Add(p, inp) (Continuation<_>.New (fun (t, res) -> 
                match res with 
                | :? GSuccess<'a, 'r> as succ1 -> fn(succ1.Value).Chain(t, succ1.Tail) (cont)
                | fail -> (t, fail) |> cont.Apply))
    } :> Parser<'a, 'r2>
//
//let many1<'a, 'r when 'r : equality> (this: Parser<'a, 'r>) = 
//    { new NonTerminalParser<'a, 'r list>() with 
//        override nt.Chain(t, inp) (f) = 
//            t.Add(this, inp) (Continuation<_>.New (function
//                | :? GSuccess<'a, 'r> as succ1 -> 
//                    success<'a, 'r list> [succ1.Value] succ1.Tail |> f
//                    t.Add(nt, succ1.Tail) (function
//                        | :? GSuccess<'a, 'r list> as succ2 -> 
//                            success (succ1.Value :: succ2.Value) succ2.Tail |> f
//                        | fail -> f fail
//                    )
//                | fail -> f fail
//            ))
//    } :> Parser<'a, 'r list>
//
//let many<'a, 'r when 'r : equality> (this: Parser<'a, 'r>) = 
//    { new NonTerminalParser<'a, 'r list>() with 
//        override nt.Chain(t, inp) (f) = 
//            success<'a, 'r list> [] inp |> f
//            t.Add(many1 this, inp) (f)
//    } :> Parser<'a, 'r list>
//
//let opt<'a, 'r when 'r : equality> (this: Parser<'a, 'r>) = 
//    { new NonTerminalParser<'a, 'r option>() with 
//        override nt.Chain(t, inp) (f) = 
//            success<'a, 'r option> None inp |> f
//            t.Add(this, inp) (function
//                | :? GSuccess<'a, 'r> as succ1 -> success (Some succ1.Value) succ1.Tail |> f
//                | fail -> f fail)
//    } :> Parser<'a, 'r option>

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
      override nt.Chain(t, inp) (cont) = 
            t.Add(p, inp) 
                (Continuation<_>.New(fun (t, r) ->
                 match r with 
                 | :? GSuccess<'a, 'r> as s1 -> 
                     q.Chain(t, s1.Tail) 
                         (Continuation<_>.New(fun (t, r2) -> 
                            (t, (if r2.Succeeded then success<'a, 'r> s1.Value r2.Tail else r2)) |> cont.Apply))
                 | f -> (t, f) |> cont.Apply))
    } :> Parser<'a, 'r>

//let (>>.) this that = this >>= fun _ -> that
let (>>.)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: Parser<'a, 'r>) (q: Parser<'a, 'r2>) : Parser<'a, 'r2> = 
    { new NonTerminalParser<'a, 'r2>() with 
      override nt.Chain(t, inp) (cont) = 
          t.Add(p, inp) (Continuation<_>.New(fun (t, r) -> 
            if r.Succeeded then q.Chain(t, r.Tail) (cont) else (t, r) |> cont.Apply))
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
          override this.Parse(inp) =
              match p.Parse(inp) with
              | :? GSuccess<'a, 'r> as s1 -> success<'a, 'r2> (fn s1.Value) s1.Tail
              | fail -> fail
        } :> Parser<'a, 'r2>
    | _ -> 
        { new NonTerminalParser<'a, 'r2>() with 
          override nt.Chain(t, inp) (cont) = 
              t.Add(p, inp) (Continuation<_>.New(fun (t, r) ->
                match r with
                | :? GSuccess<'a, 'r> as s1 -> (t, success<'a, 'r2> (fn s1.Value) s1.Tail) |> cont.Apply
                | fail -> (t, fail) |> cont.Apply))
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

//let sepBy (p: Parser<'a, 'r>) sep : Parser<'a, 'r list> = 
//    pipe2 p (many (sep >>. p)) (fun hd tl -> hd :: tl) <|>% []
//
//let notFollowedBy (p: TerminalParser<'a, 'r>) : Parser<'a, unit> = 
//    { new NonTerminalParser<'a, unit> () with
//      override this.Chain(t, inp) (f) =
//          let res = p.Parse(inp)
//          if res.Failed then success<'a, unit> () inp |> f
//          else failure "SyntaxError (notFollowedBy)" inp |> f
//    } :> Parser<'a, unit>
    
let private dummyParser<'a, 'r when 'r : equality> = 
   { new TerminalParser<'a, 'r>()
     with override this.Parse (inp: InputStream<'a>) = failwith "Used dummyParser" } :> Parser<'a, 'r>

let createParserForwardedToRef<'a, 'r when 'r : equality>(name:string) = 
    let r = ref dummyParser<'a, 'r>
    { new NonTerminalParser<'a, 'r>() with 
      override this.Chain(t, inp) (f) = (!r).Chain(t, inp) (f) } :> Parser<'a, 'r>, r
