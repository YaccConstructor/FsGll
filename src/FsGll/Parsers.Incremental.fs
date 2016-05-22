module FsGll.Parsers.Incremental

open System
open System.Collections.Generic

open FsGll.Mutable.PriorityQueue

type StreamIndex = int

[<AbstractClass>]
[<AllowNullLiteral>]
type InputStream<'a> () = 
    abstract member Inside : StreamIndex -> bool
    abstract member Item : StreamIndex -> 'a
    abstract member Extend : InputStream<'a> -> InputStream<'a>
    abstract member End : StreamIndex
    //override this.ToString() = underlying |> Seq.map (fun x -> x.ToString()) |> Seq.fold (+) "" 

type ArrayInputStream<'a> (arr: 'a []) = 
    inherit InputStream<'a>()
    member this.UnderlyingArray = arr
    override this.Inside i = i < arr.Length
    override this.Item i = arr.[i]
    override this.ToString() = sprintf "%A" <| arr
    override this.Extend(s: InputStream<'a>) = 
        match s with 
        | :? ArrayInputStream<'a> as s -> new ArrayInputStream<'a>([arr; s.UnderlyingArray] |> Array.concat) :> InputStream<'a>
        | _ -> failwith "different input streams not allowed"
    override this.End = arr.Length

//let trace (s: Lazy<string>) = printfn "%s" s.Value
let trace _ = ()

[<AbstractClass>]
type GParserResult<'a> (tail: StreamIndex) = 
    abstract member Succeeded : bool
    member this.Failed = not this.Succeeded
    member this.Tail = tail
        
type GSuccess<'a, 'r when 'r: equality> (value: 'r, tail: StreamIndex) = 
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

type GFailure<'a> (msg: string, tail: StreamIndex) = 
    inherit GParserResult<'a> (tail)
    override this.Succeeded = false
    member this.Message = msg
    override this.Equals that = 
        match that with 
        | :? GFailure<'a> as that -> msg = that.Message && tail = that.Tail
        | _ -> false
    override this.GetHashCode() = hash msg + hash tail
    override this.ToString() = sprintf "FAIL(m:%s|%A)" msg tail

let success<'a, 'r when 'r: equality> value tail = new GSuccess<'a, 'r>(value, tail) :> GParserResult<_>
let failure<'a> msg tail = new GFailure<'a>(msg, tail) :> GParserResult<'a>

//let parserResult<'a, 'r when 'r: equality> (r: GParserResult<'a>) = 
//    match r with
//    | :? GSuccess<'a, 'r> as succ -> Success (succ.Value)
//    | :? GFailure<'a> as fail -> Failure (fail.Message, fail.Tail)
//    | _ -> failwith "impossible"


type Continuation<'a> = GParserResult<'a> -> unit
type RSet<'a> = HashSet<GParserResult<'a> >
type SSet<'a> = HashSet<GParserResult<'a> >
type FSet<'a> = HashSet<Continuation<'a> >

type HOMap<'k,'v> = Dictionary<'k,'v>

type GPartial<'a, 'r when 'r: equality> (parser: Parser<'a, 'r>) =
    inherit GParserResult<'a>(-1)
    member this.Parser = parser
    override this.Succeeded : bool = failwith "not implemented"

and [<AbstractClass>] GParser<'a> () = 
    abstract member Chain : Trampoline<'a> * StreamIndex -> Continuation<'a> -> unit
    abstract member Consuming : bool
    default this.Consuming = false

and [<AbstractClass>] Parser<'a, 'r when 'r : equality> () = 
    inherit GParser<'a> ()
    abstract member Apply : InputStream<'a> -> GParserResult<'a> list

and [<AbstractClass>] NonTerminalParser<'a, 'r when 'r : equality> () = 
    inherit Parser<'a, 'r> ()
    
    override this.Apply (inp: InputStream<'a>) = 
        let t = Trampoline<'a>.New(inp)

        let (successes, failures) = new RSet<'a>(), new RSet<'a>()
        
        this.Chain(t, 0) (fun res ->
            if res.Succeeded && not (t.Stream.Inside(res.Tail + 1)) then 
                successes.Add(res) |> ignore
            elif res.Succeeded then failure "UnexpectedTrailingChars" (res.Tail) |> failures.Add |> ignore
            else res |> failures.Add |> ignore
        )
        t.Run()
        if t.PastEnd.Count > 0 then 
            let part = new GPartial<'a, 'r>(new PartialParser<'a, 'r>(t, successes, failures)) :> GParserResult<'a>
            part :: (successes |> Seq.toList)
        else (if successes.Count = 0 then failures else successes) |> Seq.toList

// Implemented with shallow copying trampoline
and PartialParser<'a, 'r when 'r : equality> (oldT: Trampoline<'a>, successes: RSet<'a>, failures: RSet<'a>) = 
    inherit Parser<'a, 'r> ()
    let stream: InputStream<'a> = oldT.Stream
    let _pastEnd = new HashSet<_>(oldT.PastEnd |> Seq.map id)
    override this.Apply(inp: InputStream<'a>) = 
        let ind = stream.End
        
        successes.Clear()
        failures.Clear()

        let t = oldT.ContinuedAt(inp)

        t.Stream <- stream.Extend(inp)
        for (p, f) in _pastEnd do
            //p.Chain(t, ind) f
            t.Add (p, ind) f
        t.Run()
        if t.PastEnd.Count > 0 then 
            let part = new GPartial<'a, 'r>(new PartialParser<'a, 'r>(t, successes, failures)) :> GParserResult<'a>
            part :: (successes |> Seq.toList)
        else (if successes.Count = 0 then failures else successes) |> Seq.toList
    override this.Chain(_, _) _ = failwith "Not implemented for PartialParser"

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
        let results = new HashSet<GParserResult<'a> > ()
        let lst = gather.Value
        lst |> List.iter (fun p -> 
            t.Add(p, inp) (fun res ->
                if not <| results.Contains (res) then 
                    f res
                    (res) |> results.Add |> ignore
            )
        )
    //interface IComparable with
        //member x.CompareTo y = compare (x.GetHashCode()) (y.GetHashCode())

and Trampoline<'a>(initStream: InputStream<'a>, 
                   _done: ResizeArray<HashSet<GParser<'a> > >,
                   popped: ResizeArray<HOMap<GParser<'a>, SSet<'a> > >,
                   backlinks: ResizeArray<HOMap<GParser<'a>, FSet<'a> > >,
                   saved: HOMap<GParserResult<'a>, FSet<'a> >) as tram =
    let mutable stream = initStream
    // R
    let _queue = new PriorityQueue<GParser<'a> * StreamIndex >(fun (_, i1) (_, i2) -> i1 - i2) 
    let _pastEnd = new HashSet<GParser<'a> * Continuation<'a> >()

    let posTrace = new ResizeArray<string>()

    let remove() = 
        //let ind = _queue.Count - 1
        //let tup = _queue.[ind]
        let tup = _queue.Pop()
        //_queue.RemoveAt ind |> ignore
        trace <| lazy (sprintf "Removed: %A" tup)
        tup

    let step() =
        let p, s = remove()
        //posTrace.Add(sprintf "%d %A" s p)
        let cont = fun (res: GParserResult<'a>) ->
            while popped.Count <= s do popped.Add(null)
            if popped.[s] <> null then
                let parsers = popped.[s]
                if not <| parsers.ContainsKey(p) then
                    parsers.Add(p, new SSet<'a>() )
            else
                let nmap = new HOMap<GParser<'a>, SSet<'a> >() in nmap.Add(p, new SSet<'a>() )
                popped.[s] <- nmap

            if res.Succeeded then
                popped.[s].[p].Add(res) |> ignore
                //savedToPopped <- savedToPopped + 1
                trace(lazy (sprintf "Saved (to popped): %A *=> %A\n" (p, s) res))

            match saved.TryGetValue(res) with
            | true, set ->
                backlinks.[s].[p] |> Seq.toArray |> Array.iter (fun f ->
                    if not (set.Contains(f)) then
                        set.Add f |> ignore
                        f res
                )
            | _, _ -> 
                let set = new HashSet<GParserResult<'a> -> unit>()
                saved.Add (res, set)
                backlinks.[s].[p] |> Seq.toArray |> Array.iter (fun f ->
                    set.Add f |> ignore
                    f res 
                )
//        if not (stream.Inside(s)) && p.Consuming then 
//            _pastEnd.Add(p, s, cont) |> ignore
//        else 
        p.Chain (tram, s) cont
        

    static member New<'a>(s) = 
        new Trampoline<'a>(s,  
                       _done = new ResizeArray<HashSet<GParser<'a> > >(), // U_j
                       popped = new ResizeArray<HOMap<GParser<'a>, SSet<'a> > >(), // P
                       backlinks = new ResizeArray<HOMap<GParser<'a>, FSet<'a> > >(), // GSS back edges
                       saved = new HOMap<GParserResult<'a>, FSet<'a> >() // prevents divergence in cyclic GSS traversal
                       )
    
    member this.Stream with get () : InputStream<'a> = stream and set (value : InputStream<'a>) = stream <- value
    member this.Saved  = saved
    member this.Popped = popped
    member this.Done   = _done
    member this.Backlinks = backlinks
    member this.Queue = _queue
    member this.PosTrace = posTrace
    member this.PastEnd : HashSet<GParser<'a> * Continuation<'a> > = _pastEnd
    
    member this.ContinuedAt (newTail: InputStream<'a>) : Trampoline<'a> = 
        new Trampoline<'a>(stream.Extend(newTail), 
                           new ResizeArray<_>(_done), 
                           new ResizeArray<_>(popped), 
                           new ResizeArray<_>(backlinks), 
                           new Dictionary<_,_>(saved |> Seq.map (fun v -> (v.Key, v.Value)) |> dict))

    // L_0
    member this.Run () : unit =
        while (_queue.Count > 0) do
            step()
            
    member this.Add (p: GParser<'a>, s: StreamIndex) (f: GParserResult<'a> -> unit) : unit = 
        if not (stream.Inside(s)) && p.Consuming then 
            _pastEnd.Add(p, f) |> ignore
        else
            let tuple = (p, s)

            while backlinks.Count <= s do backlinks.Add(null)
            if backlinks.[s] <> null then
                let parsers = backlinks.[s]
                if not <| parsers.ContainsKey(p) then 
                    parsers.Add(p, new FSet<'a>() )
            else
                let nmap = new HOMap<GParser<'a>, FSet<'a> >() in nmap.Add(p, new FSet<'a>())
                backlinks.[s] <- nmap

            backlinks.[s].[p].Add(f) |> ignore

            if s < popped.Count && popped.[s] <> null && popped.[s].ContainsKey p then
                let parsers = popped.[s]
                //foundInPopped <- foundInPopped + 1
                parsers.[p] |> Seq.toArray |> Seq.iter(fun res ->            // if we've already done that, use the result
                    trace(lazy(sprintf "Revisited: %A *=> %A\n" tuple res))
                    f res )
            
            else
    //            if not (_done.ContainsKey s) then _done.Add(s, new HashSet<GParser<'a> >())
    //            if not (_done.[s].Contains p) then 
    //                _queue.Add(tuple)
    //                _done.[s].Add(p) |> ignore

                let addTuple(parsers: HashSet<GParser<'a> >) = 
                    _queue.Add(tuple)
                    parsers.Add(p) |> ignore
                    trace(lazy(sprintf "Added: %A\n" tuple))

                while _done.Count <= s do _done.Add(null)
                if _done.[s] <> null then
                    let parsers = _done.[s]
                    if not <| parsers.Contains (p) then 
                        addTuple(parsers) 
                else
                    let parsers = new HashSet<GParser<'a> >()
                    _done.[s] <- parsers
                    addTuple(parsers)

#nowarn "1189"

let inline private withSucc<'a, 'r when 'r : equality> (sf: _ -> GParserResult<'a>) (f: Continuation<'a>) = 
    (fun (r: GParserResult<'a>) ->
        match r with
        | :? GSuccess<'a, 'r> as s1 -> sf s1 |> f
        | fail -> fail |> f)

let preturn<'a, 'r when 'r: equality> (a: 'r) : Parser<'a, 'r> = 
    { new NonTerminalParser<'a, 'r>() 
      with override this.Chain(t, ind) (cont) = success<'a, 'r> a ind |> cont } :> Parser<'a, 'r>

let perror<'a, 'r when 'r: equality> (msg: string) : Parser<'a, 'r> = 
    { new NonTerminalParser<'a, 'r>() 
      with override this.Chain(t, ind) (cont) = failure<'a> msg ind |> cont } :> Parser<'a, 'r>

let satisfy<'a when 'a: equality> (pred : 'a -> bool) = 
    { new NonTerminalParser<'a, 'a>() with 
      override this.Consuming = true
      override nt.Chain(t, ind) (cont) = 
        //if t.Stream.Inside(ind) then 
            let s = t.Stream
            let h = s.[ind]
            if h |> pred then success<'a, 'a> h (ind + 1)
            else failure<'a> "Unexpected token" ind
            |> cont 
        //else t.PastEnd.Add(nt :> GParser<'a>, ind, cont) |> ignore
    } :> Parser<'a, 'a>

let (>>=)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: Parser<'a, 'r>) (fn: 'r -> Parser<'a, 'r2>) : Parser<'a, 'r2> = 
    { new NonTerminalParser<'a, 'r2>() with 
      override nt.Chain(t, inp) (cont) = 
            t.Add(p, inp) (function 
                | :? GSuccess<'a, 'r> as succ1 -> fn(succ1.Value).Chain(t, succ1.Tail) (cont)
                | fail -> cont fail)
    } :> Parser<'a, 'r2>   

let many1<'a, 'r when 'r : equality> (this: Parser<'a, 'r>) = 
    { new NonTerminalParser<'a, 'r list>() with 
        override nt.Chain(t, inp) (f) = 
            t.Add(this, inp) (function
                | :? GSuccess<'a, 'r> as succ1 -> 
                    success<'a, 'r list> [succ1.Value] succ1.Tail |> f
                    t.Add(nt, succ1.Tail) (function
                        | :? GSuccess<'a, 'r list> as succ2 -> 
                            success (succ1.Value :: succ2.Value) succ2.Tail |> f
                        | fail -> f fail
                    )
                | fail -> f fail
            )
    } :> Parser<'a, 'r list>

let many<'a, 'r when 'r : equality> (this: Parser<'a, 'r>) = 
    { new NonTerminalParser<'a, 'r list>() with 
        override nt.Chain(t, inp) (f) = 
            success<'a, 'r list> [] inp |> f
            t.Add(many1 this, inp) (f)
    } :> Parser<'a, 'r list>

let opt<'a, 'r when 'r : equality> (this: Parser<'a, 'r>) = 
    { new NonTerminalParser<'a, 'r option>() with 
        override nt.Chain(t, inp) (f) = 
            success<'a, 'r option> None inp |> f
            t.Add(this, inp) (function
                | :? GSuccess<'a, 'r> as succ1 -> success (Some succ1.Value) succ1.Tail |> f
                | fail -> f fail)
    } :> Parser<'a, 'r option>

let epsilon<'a> = preturn ()

let (<|>)<'a, 'r when 'r : equality> (a: Parser<'a, 'r>) (b: Parser<'a, 'r>) = 
    new DisjunctiveParser<'a, 'r>(a, b) :> Parser<'a, 'r>

let (|>>) p fn = p >>= (preturn << fn)

let (>>.) p1 p2 = 
    p1 >>= fun _ ->
    p2 >>= fun b -> preturn (b)

let (.>>) p1 p2 = 
    p1 >>= fun a ->
    p2 >>= fun _ -> preturn (a)

let (.>>.) p1 p2 = 
    p1 >>= fun a ->
    p2 >>= fun b -> preturn (a, b)

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

let private dummyParser<'a, 'r when 'r : equality> = 
   { new NonTerminalParser<'a, 'r>() with override this.Chain(t, ind) (cont) = failwith "Dummy used" } :> Parser<'a, 'r>

let createParserForwardedToRef<'a, 'r when 'r : equality>(name:string) = 
    let r = ref dummyParser<'a, 'r>
    { new NonTerminalParser<'a, 'r>() with 
      override this.Chain(t, inp) (f) = (!r).Chain(t, inp) (f) } :> Parser<'a, 'r>, r

let runParser (p: Parser<'a, 'r>) (s: 'a seq) : GParserResult<'a> list = 
    let res = p.Apply(new ArrayInputStream<'a>(Seq.toArray s))
    if res.IsEmpty then failwith "Lib err"
    res
