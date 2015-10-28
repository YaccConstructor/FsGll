module AbstractParsers

open System
open System.IO
open System.Collections
open System.Collections.Generic
open FSharpx.Prelude


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

//[<CustomEquality; CustomComparison>]
type CharStream (s: String, ?ind) = 
    let mutable ind = defaultArg ind 0
    
    member this.Ind = ind
    member this.IsEmpty = ind >= s.Length
    member this.Head() = s.[ind]
    member this.Drop x = new CharStream(s, ind + x)
    //member this.Take x = if ind + x > s.Length then "" else s.Substring(ind, x)
    member this.Take x = s.Substring(ind, min (s.Length - ind) x)
    override this.Equals (that) = 
        match that with 
        | :? CharStream as that -> this.Ind = that.Ind
        | _                     -> false

    override this.GetHashCode() = hash (ind)
    override this.ToString() = "CharStream(ind:" + string ind + "|" + s.Substring(ind) + ")"

type InputStream<'a> (underlying: 'a seq) = 
    member this.Head = Seq.head underlying
    member this.IsEmpty = Seq.isEmpty underlying
    member this.Drop x = new InputStream<_>(Seq.skip x underlying)

[<AbstractClass>]
type GParserResult<'a> (tail: InputStream<'a>) = 
    abstract member Succeeded : bool
    member this.Failed = not this.Succeeded
    member this.Tail = tail
    //override this.ToString() = sprintf "GPR(%A)" this.Tail
    
type GSuccess<'a, 'r when 'r: equality> (value: 'r, tail: InputStream<'a>) = 
    inherit GParserResult<'a>(tail)
    override this.Succeeded = true
    member this.Data = (value, tail)
    member this.Value = value
    override this.ToString() = sprintf "SUCC(%A)" this.Data
    override this.Equals that = 
        match that with 
        | :? GSuccess<'a, 'r> as that -> value = that.Value && tail = that.Tail
        | _ -> false
    override this.GetHashCode() = hash value + hash tail

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

//type Failure (msg: string, tail: CharStream) = 
//    inherit GParseResult (tail)
//    override this.IsSucc = false
//    member this.Data = (msg, tail)
//    override this.Equals that = 
//        match that with 
//        | :? Failure as that -> this.Data = that.Data
//        | _ -> false
//    override this.GetHashCode() = hash (this.Data)

//type ParserResult () = class end
//type Success<'a> (value: 'a, tail: CharStream) = class inherit ParserResult () end
//type Failure (msg: string) = class inherit ParserResult () end

type RSet<'a> = HashSet<GParserResult<'a> >
type SSet<'a> = HashSet<GParserResult<'a> >
type FSet<'a> = HashSet<GParserResult<'a> -> unit>

type HOMap<'k,'v> = Dictionary<'k,'v>

type [<AbstractClass>] GParser<'a> () = 
    abstract member Chain : Trampoline<'a> * InputStream<'a> -> (GParserResult<'a> -> unit) -> unit

and [<AbstractClass>] Parser<'a, 'r when 'r : equality> () = 
    inherit GParser<'a> ()
    abstract member Apply : InputStream<'a> -> GParserResult<'a> list
    abstract member MapWithTail<'r2 when 'r2 : equality> : ( (InputStream<'a> * 'r) -> 'r2) -> Parser<'a, 'r2>
    abstract member Seq<'r2  when 'r2 : equality> : Parser<'a, 'r2> -> Parser<'a, 'r * 'r2>

and [<AbstractClass>] NonTerminalParser<'a, 'r when 'r : equality> () = 
    inherit Parser<'a, 'r> ()
    override this.Apply (inp: InputStream<'a>) = 
        let t = new Trampoline<'a>()

        let successes = new HashSet<GParserResult<'a> > ()
        let failures = new HashSet<GParserResult<'a> > ()

        this.Chain(t, inp) (fun res ->
            if res.Succeeded && res.Tail.IsEmpty then successes.Add(res) |> ignore
            elif res.Succeeded then failure "UnexpectedTrailingChars" (res.Tail) |> failures.Add |> ignore
            else res |> failures.Add |> ignore
        )

        t.Run()

        (if successes.Count = 0 then failures else successes) |> Seq.toList

    override this.Seq<'r2 when 'r2 : equality> (that: Parser<'a, 'r2>) = 
        new SequentialParser<'a, 'r, 'r2>(this, that) :> Parser<'a, 'r * 'r2>

    override this.MapWithTail<'r2 when 'r2 : equality> (f : ( (InputStream<'a> * 'r) -> 'r2)) = 
        { new NonTerminalParser<'a, 'r2> () with
            override nt.Chain(t, inp)(f2) = 
                this.Chain(t, inp) (function
                    | :? GSuccess<'a, 'r> as succ -> success (f (inp, succ.Value)) succ.Tail |> f2
                    | fail -> fail |> f2
                )
        } :> Parser<'a, 'r2>

and SequentialParser<'a, 'r, 'r2 when 'r : equality and 'r2 : equality> (left: Parser<'a, 'r>, right: Parser<'a, 'r2>) = 
    inherit NonTerminalParser<'a, 'r * 'r2>()
    member this.Left = left
    member this.Right = right
    override this.Chain (t, inp) (f: GParserResult<'a> -> unit) = //<'r * 'r2>
        left.Chain(t, inp) (function
            | :? GSuccess<'a, 'r> as succ1 ->
                right.Chain(t, succ1.Tail) (fun res -> 
                    match res with
                    | :? GSuccess<'a, 'r2> as succ2 -> (success<'a, _> (succ1.Value, succ2.Value) succ2.Tail |> f)
                    | fail -> fail |> f
                )
            | fail -> fail |> f
        )

    override this.Equals that = 
        match that with
        | :? SequentialParser<'a, 'r, 'r2> as that -> left = that.Left && right = that.Right
        | _ -> false 
     
    override this.GetHashCode() = hash left + hash right

and [<AbstractClass>] TerminalParser<'a, 'r when 'r : equality> () = 
    inherit Parser<'a, 'r> ()

    abstract member Parse : InputStream<'a> -> GParserResult<'a>

    override this.Apply (inp: InputStream<'a>) = 
            match this.Parse(inp) with 
            | :? GSuccess<'a, 'r> as succ1 -> [succ1]
            | x -> [x]
    
    override this.Chain (t, inp) (f) = inp |> this.Parse |> f

    override this.MapWithTail<'r2 when 'r2 : equality> (f : ( (InputStream<'a> * 'r) -> 'r2)) = 
        { new TerminalParser<'a, 'r2> () with
            override nt.Parse(inp) = 
                match this.Parse(inp) with
                | :? GSuccess<'a, 'r> as succ1 -> success(f (succ1.Tail, succ1.Value)) succ1.Tail
                | fail -> fail
        } :> Parser<'a, 'r2>

    override this.Seq<'r2 when 'r2 : equality> (other: Parser<'a, 'r2>) = 
        match other with 
        | :? TerminalParser<'a, 'r2> as other -> 
            { new TerminalParser<'a, 'r * 'r2> () with
                member nt.Parse(inp: InputStream<'a>) = 
                    match this.Parse inp with
                    | :? GSuccess<'a, 'r> as succ1 -> //(res1, tail) -> 
                        match other.Parse succ1.Tail with 
                        | :? GSuccess<'a, 'r2> as succ2 -> 
                            success<'a, 'r * 'r2> (succ1.Value, succ2.Value) succ2.Tail
                        | fail -> fail
                    | fail -> fail
            } :> Parser<'a, 'r * 'r2>
        | other -> new SequentialParser<'a, 'r, 'r2>(this, other) :> Parser<'a, 'r * 'r2>

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

//and LiteralParser(c: char) = CharParser ()
//    inherit TerminalParser<char>()B
//    override this.Parse (inp: CharStream) = 
//        if inp.IsEmpty 
//        then Failure "UnexpectedEOF"
//        else if inp.Head() = c then Success (c, inp.Drop 1) else Failure("Unexpected character")

//and PredicateParser<'a when 'a : equality>(pred: 'a -> bool) = 
//    inherit TerminalParser<'a, 'a>()
//    override this.Parse (s: InputStream<'a>) : GParserResult<'a> = failure<'a> "aaa" s
//        if s.IsEmpty then failure<'a> "UnexpectedEOF" s
//        else
//            let h = s.Head
//            if h |> pred then success<_, _> h (s.Drop 1)
//            else failure<'a> (sprintf "Unexpected token '%A'" h) s

and Trampoline<'a>() as tram =
    // R
    let _queue = new ResizeArray<GParser<'a> * InputStream<'a>>()

    // U_j
    let _done = new Dictionary<InputStream<'a>, HashSet<GParser<'a> > >()

    // P
    let popped = new Dictionary<InputStream<'a>, HOMap<GParser<'a>, SSet<'a> > >()

    // GSS back edges
    let backlinks = new Dictionary<InputStream<'a>, HOMap<GParser<'a>, FSet<'a> > >()

    // prevents divergence in cyclic GSS traversal
    let saved = new HOMap<GParserResult<'a>, FSet<'a> >()

    let hasNext() = _queue.Count > 0

    let remove() = 
        let ind = _queue.Count - 1
        let tup = _queue.[ind]
        _queue.RemoveAt ind |> ignore
        //trace "removed"
        tup

    let step() =
        let p, s = remove()
        p.Chain (tram, s) <| fun res ->
            match popped.TryGetValue(s) with
            | true, parsers-> 
                if not <| parsers.ContainsKey(p) then
                    popped.[s].Add(p, new SSet<'a>() )
            | _, _ ->
                popped.Add(s, new HOMap<GParser<'a>, SSet<'a> >() )
                popped.[s].Add(p, new SSet<'a>() )

            if res.Succeeded then
                popped.[s].[p].Add(res) |> ignore
                savedToPopped <- savedToPopped + 1
                //trace(sprintf "Saved (to popped): %A *=> %A\n" (p, s) res)

            match saved.TryGetValue(res) with
            | true, set ->
                backlinks.[s].[p] |> Seq.toArray |> Array.iter (fun f ->
                    if (not <| set.Contains(f)) then
                        set.Add f |> ignore
                        savedToSaved <- savedToSaved + 1
                        f res
                    else 
                        foundInSaved <- foundInSaved + 1
                )
            | _, _ -> 
                let set = new HashSet<GParserResult<'a> -> unit>()
                saved.Add (res, set)
                backlinks.[s].[p] |> Seq.toArray |> Seq.iter (fun f ->
                    set.Add f |> ignore
                    savedToSaved <- savedToSaved + 1
                    f res 
                )

    member this.Saved  = saved
    member this.Popped = popped
    member this.Done   = _done
    member this.Backlinks = backlinks
    // L_0
    member this.Run () : unit =
        while (hasNext()) do
            step()
            
    member this.Add (p: GParser<'a>, s: InputStream<'a>) (f: GParserResult<'a> -> unit) : unit = 
        let tuple = (p, s)
//        
//        match backlinks.TryGetValue(s) with
//        | true, parsers-> 
//            if not <| parsers.ContainsKey(p) then 
//                backlinks.[s].Add(p, new HashSet<(GParserResult<'a> -> unit)>() )
//        | _, _ ->
//            backlinks.Add(s, new HOMap<GParser<'a>, FSet<'a> >() )
//            backlinks.[s].Add(p, new FSet<'a>() )
//
//        backlinks.[s].[p].Add(f) |> ignore
//
//        match popped.TryGetValue(s) with
//        | true, parsers when parsers.ContainsKey p ->
//            //foundInPopped <- foundInPopped + 1
//            parsers.[p] |> Seq.iter(fun res ->            // if we've already done that, use the result
//                //trace(sprintf "Revisited: %A *=> %A\n" tuple res)
//                f res
//            )
//        | _ ->
//            notFoundInPopped <- notFoundInPopped + 1
//            let addTuple(parsers: HashSet<GParser<'a> >) = 
//                //notFoundInDone <- notFoundInDone + 1
//                _queue.Add(tuple)
//                parsers.Add(p) |> ignore
//                //trace("Added: ")
//                //queuePushed <- queuePushed + 1
//
//            match _done.TryGetValue(s) with
//            | true, parsers ->
//                if not <| parsers.Contains (p) then 
//                    addTuple(parsers) 
//            | _, _ ->
//                let parsers = new HashSet<GParser<'a> >()
//                _done.Add(s, parsers)
//                //savedToDone <- savedToDone + 1
//                addTuple(parsers)
        ()

#nowarn "1189"

let preturn<'a, 'r when 'r: equality> (a: 'r) = 
    { new TerminalParser<'a, 'r>() with override this.Parse inp = success<'a, 'r> a inp } :> Parser<'a, 'r>

let predicate<'a when 'a: equality> (pred : 'a -> bool) = 
    { new TerminalParser<'a, 'a>() with 
      override this.Parse (s: InputStream<'a>) =
        if s.IsEmpty then failure<'a> "UnexpectedEOF" s
        else
            let h = s.Head
            if h |> pred then success<'a, 'a> h (s.Drop 1)
            else failure<'a> (sprintf "Unexpected token '%A'" h) s }

let (>>=)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> p (fn:'r -> Parser<'a, 'r2>) = 
    { new NonTerminalParser<'a, 'r option>() with 
      override nt.Chain(t, inp) (cont) = 
            t.Add(p, inp) (fun res -> 
                match res with 
                | :? GSuccess<'a, 'r> as succ1 -> 
                    fn(succ1.Value).Chain(t, succ1.Tail) (fun res -> 
                        match res with
                        | :? GSuccess<'a, 'r2> as succ2 -> 
                            success<'a, _> succ2.Value succ2.Tail |> cont
                        | fail -> fail |> cont
                    )
                | fail -> cont fail
            )
    } :> Parser<'a, _>


//let (|>>) p f = p >>= 

//let (.^)<'a, 'r, 'rr when 'r : equality and 'rr : equality> (p: Parser<'a, 'r>) (f: 'r -> 'rr) =
//    p.MapWithTail (fun (_, r) -> f r)

//let (-~-)<'r, 'r2 when 'r : equality and 'r2 : equality> (a: Parser<'r>) (b: Parser<'r2>) = new SequentialParser<_, _>(a, b) :> Parser<_>

let (<|>)<'a, 'r when 'r : equality> (a: Parser<'a, 'r>) (b: Parser<'a, 'r>) = 
    new DisjunctiveParser<'a, 'r>(a, b) :> Parser<'a, 'r>

let (>>%) this x = this >>= fun _ -> preturn x
let (.>>) this that = this >>= fun _ -> that
let (>>.) this that = that >>= fun x -> that >>% x

//let many1<'a, 'r when 'r : equality> (this: Parser<'a, 'r>) = 
//    { new NonTerminalParser<'a, 'r list>() with 
//        override nt.Chain(t, inp) (f) = 
//            t.Add(this, inp) (fun res ->
//                match res with
//                | :? GSuccess<'r> as succ1 -> 
//                    success [succ1.Value] succ1.Tail |> f
//                    t.Add(nt, succ1.Tail) (fun res -> 
//                        match res with 
//                        | :? GSuccess<'r list> as succ2 -> success (succ1.Value :: succ2.Value) succ2.Tail |> f
//                        | fail -> f fail
//                    )
//                | fail -> f fail
//            )
//    } :> Parser<'a, _>

//let opt<'a, 'r when 'r : equality> (this: Parser<'a, 'r>) = 
//    { new NonTerminalParser<'a, 'r option>() with 
//        override nt.Chain(t, inp) (f) = 
//            success<'a, 'r option> None inp |> f
//            t.Add(this, inp) (fun res -> 
//                match res with 
//                | :? GSuccess<'r> as succ1 -> success (Some succ1.Value) succ1.Tail |> f
//                | fail -> f fail
//            )
//    } :> Parser<'a, _>

//let many<'a, 'r when 'r : equality> (this: Parser<'a, 'r>) : Parser<'a, 'r list> = (this |> many1 |> opt) .^ (function Some x -> x | _ -> [])

let epsilon<'a> = preturn ()

let private dummyParser<'a, 'r when 'r : equality> = 
   { new TerminalParser<'a, 'r>()
     with override this.Parse (inp: InputStream<'a>) = failwith "Used dummyParser" } :> Parser<'a, 'r>

let createParserForwardedToRef<'a, 'r when 'r : equality>(name:string) = 
    let r = ref dummyParser<'a, 'r>
    { new NonTerminalParser<'a, 'r>() with 
        override this.Chain(t, inp) (f) =
            //printfn "%A" name 
            (!r).Chain(t, inp) (f) } :> Parser<'a, 'r>, r
