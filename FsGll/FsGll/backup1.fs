open System
open System.Collections
open System.Collections.Generic

let trace s = printfn "%s" s

//[<CustomEquality; CustomComparison>]
type CharStream (s: String, ?ind) = 
    let mutable ind = defaultArg ind 0
    
    member this.Ind = ind
    member this.IsEmpty = ind >= s.Length
    member this.Head() = s.[ind]
    member this.Drop x = new CharStream(s, ind + x)
    override this.Equals (that) = 
        match that with 
        | :? CharStream as that -> this.Ind = that.Ind
        | _                     -> false

    override this.GetHashCode() = hash (ind)

let handleWhitespace(s: CharStream) = s

let processTail(tail: CharStream) = 
    let newTail = handleWhitespace tail
    if not <| newTail.IsEmpty then Some(newTail) else None

type GParserResult = interface end

type ParserResult<'a> = 
    | Success of value: 'a * tail: CharStream
    | Failure of msg: string
    interface GParserResult

//type ParserResult () = class end
//type Success<'a> (value: 'a, tail: CharStream) = class inherit ParserResult () end
//type Failure (msg: string) = class inherit ParserResult () end

type RSet = HashSet<GParserResult >
type SSet = HashSet<GParserResult >
type FSet = HashSet<GParserResult -> unit>

type HOMap<'k,'v> = Dictionary<'k,'v>


[<AbstractClass>] type GParser () = class end
[<AbstractClass>]
type Parser<'r> () = 
    inherit GParser ()
    abstract member Apply : CharStream -> ParserResult<'r> list
    abstract member Chain : Trampoline * CharStream -> (ParserResult<'r> -> unit) -> unit
    abstract member Seq<'r2> : Parser<'r2> -> Parser<'r * 'r2>
    abstract member MapWithTail<'r2> : ( (CharStream * 'r) -> 'r2) -> Parser<'r2>
    //default this.Seq<'r2> (that: Parser<'r2>) = new SequentialParser<'r, 'r2>(this, that) :> Parser<'r * 'r2>


and [<AbstractClass>] NonTerminalParser<'r> () = 
    inherit Parser<'r> ()
    override this.Apply (inp: CharStream) = 
        let t = new Trampoline()
        let successes = new HashSet<ParserResult<'r> > ()
        let failures = new HashSet<ParserResult<'r> > ()

        this.Chain(t, inp) (function
            | Success (res, tail) as succ when tail.IsEmpty ->  
                successes.Add(succ) |> ignore
            | Success (_, rail) -> 
                Failure("UnexpectedTrailingChars") |> failures.Add |> ignore
            | Failure _ as f -> f |> failures.Add |> ignore
        )

        t.Run()
        (if successes.Count = 0 then failures else successes) |> Seq.toList

    override this.MapWithTail<'r2> (f) = 
        { new NonTerminalParser<'r2> () with
            member nt.Chain(t, inp)(f2) = 
                this.Chain(t, inp) (function
                    | Success(res, tail) -> Success(f (inp, res), tail) |> f2
                    | Failure x -> x |> Failure |> f2
                )
        } :> Parser<'r2>

    override this.Seq<'r2> (that: Parser<'r2>) = new SequentialParser<'r, 'r2>(this, that) :> Parser<'r * 'r2>

and SequentialParser<'r, 'r2> (left: Parser<'r>, right: Parser<'r2>) = 
    inherit NonTerminalParser<'r * 'r2>()
    override this.Chain (t, inp) (f: ParserResult<'r * 'r2> -> unit) = 
        left.Chain(t, inp) (function
            | Success (res1, tail) ->
                right.Chain(t, tail) (function
                    | Success (res2, tail) -> ((res1, res2), tail) |> Success |> f
                    | Failure x -> Failure x |> f
                )
            | Failure x -> Failure x |> f
        )

and [<AbstractClass>] TerminalParser<'r> () = 
    inherit Parser<'r> ()

    abstract member Parse : CharStream -> ParserResult<'r>

    override this.Apply (inp: CharStream) = 
            match this.Parse(handleWhitespace inp) with 
            | Success (res, tail) -> 
                match processTail tail with 
                | Some tail -> [Success(res, tail)]
                | None      -> [Failure("UnexpectedTrailingChars")]
            | x -> [x]
    
    override this.Chain (t, inp) (f) =
        inp |> handleWhitespace |> this.Parse |> f

    override this.MapWithTail<'r2> (f) = 
        { new TerminalParser<'r2> () with
            member nt.Parse(inp) = 
                let newTail = handleWhitespace(inp)
                match this.Parse(newTail) with
                | Success(res, tail) -> Success(f (newTail, res), tail) 
                | Failure x -> Failure x
        } :> Parser<'r2>

    override this.Seq<'r2> (other: Parser<'r2>) = 
        match other with 
        | :? TerminalParser<'r2> as other -> 
            { new TerminalParser<'r * 'r2> () with
                member nt.Parse(inp: CharStream) = 
                    match this.Parse(handleWhitespace(inp)) with
                    | Success (res1, tail) -> 
                        match other.Parse(handleWhitespace(tail)) with 
                        | Success(res2, tail) -> ((res1, res2), tail) |> Success
                        | Failure x -> Failure x
                    | Failure x -> Failure x
            } :> Parser<'r * 'r2>
        | other -> new SequentialParser<'r, 'r2>(this, other) :> Parser<'r * 'r2>

and DisjunctiveParser<'r>(left: Parser<'r>, right: Parser<'r>) as disj =
    inherit NonTerminalParser<'r>()
    
    let gather = lazy (disj.GatherImpl(new HashSet<_>() ) )

    member this.GatherImpl(seen: HashSet<DisjunctiveParser<'r> >) = 
        seen.Add(this) |> ignore
        let recprocess (p: Parser<'r>) = 
            match p with
            | :? DisjunctiveParser<'r> as d -> 
                if not <| seen.Contains d then d.GatherImpl(seen) else []
            | p -> [p]

        recprocess(left) @ recprocess(right)
    override this.Chain(t, inp) (f) = 
        let results = new HashSet<ParserResult<'r> > ()
        gather.Value |> List.iter (fun p -> 
            t.Add(p, inp) (fun res -> 
                if not <| results.Contains (res :> obj :?> ParserResult<'r>) then 
                    f (res :> obj :?> ParserResult<'r>)
                    (res :> obj :?> ParserResult<'r>) |> results.Add |> ignore
            )
            
        )
    //interface IComparable with
        //member x.CompareTo y = compare (x.GetHashCode()) (y.GetHashCode())


and LiteralParser(c: char) =
    inherit TerminalParser<char>()
    override this.Parse (inp: CharStream) = 
        if inp.IsEmpty 
        then Failure "UnexpectedEOF"
        else if inp.Head() = c then Success (c, inp.Drop 1) else Failure("Unexpected character")

and Trampoline () as tram =
    // R
    let _queue = new ResizeArray<Parser<obj> * CharStream>()

    // U_j
    let _done = new Dictionary<CharStream, HashSet<Parser<obj> > >()

    // P
    let popped = new Dictionary<CharStream, Dictionary<GParser, SSet > >()

    // GSS back edges
    let backlinks = new Dictionary<CharStream, HOMap<GParser, FSet > >()

    // prevents divergence in cyclic GSS traversal
    let saved = new HOMap<GParserResult, FSet >()

    let hasNext() = _queue.Count > 0

    let remove() = 
        let tup = _queue.[_queue.Count - 1]
        trace "removed"
        tup

    let step() =
        let p, s = remove()
        p.Chain (tram, s) <| fun res ->
            match popped.TryGetValue(s) with
            | true, parsers-> 
                if not <| parsers.ContainsKey(p :> GParser) then
                    popped.[s].Add(p, new SSet() )
            | _, _ ->
                popped.Add(s, new HOMap<GParser, SSet >() )
                popped.[s].Add(p, new HashSet<GParserResult>() )

            match res with
            | Success _ as succ->
                popped.[s].[p].Add(succ) |> ignore
                trace(sprintf "Saved: %A *=> %A\n" (p, s) succ)
            | _ -> ()

            match saved.TryGetValue(res) with
            | true, set ->  
                backlinks.[s].[p] |> Seq.iter (fun f ->    
                    if (not <| set.Contains(f)) then
                        set.Add f |> ignore
                        f res
                )
            | _, _ -> 
                let set = new HashSet<GParserResult -> unit>()
                saved.Add (res, set)
                backlinks.[s].[p] |> Seq.iter (fun f ->    
                    set.Add f |> ignore
                    f res 
                )

    // L_0
    member this.Run() =
        while (hasNext()) do
            step()
            
    member this.Add (p: GParser, s: CharStream) (f: ParserResult<obj> -> unit) = 
        //let p = p :> obj :?> Parser<obj>
        let tuple = (p, s)

        match backlinks.TryGetValue(s) with
            | true, parsers-> 
                if not <| parsers.ContainsKey(p) then
                    backlinks.[s].Add(p, new FSet() )
            | _, _ ->
                backlinks.Add(s, new HOMap<GParser, FSet >() )
                backlinks.[s].Add(p, new FSet() )

        backlinks.[s].[p].Add(f :> obj :?> (GParserResult -> unit)) |> ignore

        match popped.TryGetValue(s) with
        | true, parsers when parsers.ContainsKey p ->
            parsers.[p] |> Seq.iter(fun res ->            // if we've already done that, use the result
                trace(sprintf "Saved: %A *=> %A\n" tuple res)
                f (res :?> ParserResult<obj>)
            )
        | _ ->
            let addTuple(parsers: HashSet<Parser<obj> >) = 
                _queue.Add(tuple)
                parsers.Add(p) |> ignore
                trace("Added: ")

            match _done.TryGetValue(s) with
            | true, parsers ->
                if not <| parsers.Contains (p) then addTuple(parsers)
            | _, _ ->
                let parsers = new HashSet<Parser<obj>>()
                _done.Add(s, parsers)
                addTuple(parsers)
        ()

let (<^>)<'r, 'rr> (p: Parser<'r>) (f: 'r -> 'rr) = p.MapWithTail (fun (_, r) -> f r)
let (^^)<'r, 'r2, 'rr> (p: Parser<'r * 'r2>) (f: 'r -> 'r2 -> 'rr) = p.MapWithTail (fun (_, (r, r2) ) -> f r r2)
let (-~-)<'r, 'r2> (a: Parser<'r>) (b: Parser<'r2>) = new SequentialParser<_, _>(a, b)
let (<|>)<'r> (a: Parser<'r>) (b: Parser<'r>) = new DisjunctiveParser<_>(a, b)
let lit c = new LiteralParser(c)

[<EntryPoint>]
let main argv = 
    let num = lit '0' <|> lit '1'
    let num2 = (num -~- num)
    let expr = (num -~- num ^^ (fun a b -> string a + string b)) <|> (num <^> string)

    let a = expr.Apply(new CharStream("010"))

    0
