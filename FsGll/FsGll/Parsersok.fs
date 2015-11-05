module Parsersok

open System
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

let mutable trampolinesCreated = 0

let mutable foundInSaved = 0
let mutable savedToSaved = 0

//[<CustomEquality; CustomComparison>]
[<AllowNullLiteral>]
type CharStream (s: String, ?ind) = 
    let mutable ind = defaultArg ind 0
    
    member this.Ind = ind
    member this.IsEmpty = ind >= s.Length
    member this.Head() = s.[ind]
    member this.Drop x = new CharStream(s, ind + x)
    member this.Take x = if ind + x > s.Length then "" else s.Substring(ind, x)
    override this.Equals (that) = 
        match that with 
        | :? CharStream as that -> this.Ind = that.Ind
        | _                     -> false

    override this.GetHashCode() = hash (ind)
    override this.ToString() = "CharStream(" + string ind + "|" + s.Substring(ind) + ")"

let handleWhitespace(s: CharStream) = s

let processTail(tail: CharStream) = 
    let newTail = handleWhitespace tail
    if not <| newTail.IsEmpty then Some(newTail) else None

[<Interface>]
type IParserResult = 
    abstract member IsSucc : bool 
    abstract member FailComparison : string * CharStream

[<CustomEquality; NoComparison>]
type ParserResult<'r when 'r : equality> = 
    | Success of value: 'r * tail: CharStream
    | Failure of msg: string * tail: CharStream
    override this.Equals that = 
        match this with
        | Success (v, t) -> 
            match that with 
            | :? ParserResult<'r> as thatr -> 
                match thatr with 
                | Success (v2, t2) -> v = v2 && t = t2 
                | _ -> false
            | _ -> false
        | Failure (s, t) ->
            match that with 
            | :? IParserResult as that when not that.IsSucc -> (s, t) = that.FailComparison 
            | _ -> false

    override this.GetHashCode() = 
        match this with 
        | Success (v, t) -> hash v + hash t 
        | Failure (m, t) -> hash m + hash t

    interface IParserResult with
        member x.IsSucc = match x with Success _ -> true | _ -> false
        member x.FailComparison = match x with Failure (m, t) -> (m, t) | _ -> (null, null)

//type ParserResult () = class end
//type Success<'a> (value: 'a, tail: CharStream) = class inherit ParserResult () end
//type Failure (msg: string) = class inherit ParserResult () end

type RSet = HashSet<IParserResult >
type SSet = HashSet<IParserResult >
type FSet = HashSet<IParserResult -> unit>

type HOMap<'k,'v> = Dictionary<'k,'v>

type [<AbstractClass>] GParser () = 
    abstract member ChainG : Trampoline * CharStream -> (IParserResult -> unit) -> unit

and [<AbstractClass>] Parser<'r when 'r : equality> () = 
    inherit GParser ()
    abstract member Apply : CharStream -> ParserResult<'r> list
    abstract member Chain : Trampoline * CharStream -> (ParserResult<'r> -> unit) -> unit
    abstract member Seq<'r2  when 'r2 : equality> : Parser<'r2> -> Parser<'r * 'r2>
    abstract member MapWithTail<'r2 when 'r2 : equality> : ( (CharStream * 'r) -> 'r2) -> Parser<'r2>
    override this.ChainG (t, inp) (f) = this.Chain (t, inp) f
    //default this.Seq<'r2> (that: Parser<'r2>) = new SequentialParser<'r, 'r2>(this, that) :> Parser<'r * 'r2>

and [<AbstractClass>] NonTerminalParser<'r when 'r : equality> () = 
    inherit Parser<'r> ()
    override this.Apply (inp: CharStream) = 
        let t = new Trampoline()
        trampolinesCreated <- trampolinesCreated + 1

        let successes = new HashSet<ParserResult<'r> > ()
        let failures = new HashSet<ParserResult<'r> > ()

        this.Chain(t, inp) (function
            | Success (res, tail) as succ when tail.IsEmpty ->
                successes.Add(succ) |> ignore
            | Success (_, tail) -> 
                Failure("UnexpectedTrailingChars", tail) |> failures.Add |> ignore
            | Failure _ as f -> f |> failures.Add |> ignore
        )

        t.Run()
//        let sv = t.Saved
//        sv 
//        |> Seq.filter (fun (x: KeyValuePair<GParserResult, FSet>) -> x.Key.IsSucc |> not) 
//        |> Seq.iter(fun x -> printfn "%A" (x.Key))
////        ks |> Seq.iter (function 
////            | Failure (_,_) -> ()
////            | _ -> ())
        (if successes.Count = 0 then failures else successes) |> Seq.toList

    override this.MapWithTail<'r2 when 'r2 : equality> (f) = 
        { new NonTerminalParser<'r2> () with
            override nt.Chain(t, inp)(f2) = 
                this.Chain(t, inp) (function
                    | Success (res, tail) -> Success(f (inp, res), tail) |> f2
                    | Failure (x,y) -> (x,y) |> Failure |> f2
                )
        } :> Parser<'r2>

    override this.Seq<'r2 when 'r2 : equality> (that: Parser<'r2>) = new SequentialParser<'r, 'r2>(this, that) :> Parser<'r * 'r2>

and SequentialParser<'r, 'r2 when 'r : equality and 'r2 : equality> (left: Parser<'r>, right: Parser<'r2>) = 
    inherit NonTerminalParser<'r * 'r2>()
    member this.Left = left
    member this.Right = right
    override this.Chain (t, inp) (f: ParserResult<'r * 'r2> -> unit) = 
        left.Chain(t, inp) (function
            | Success (res1, tail) ->
                right.Chain(t, tail) (function
                    | Success (res2, tail) -> ((res1, res2), tail) |> Success |> f
                    | Failure (x,y) -> (x,y) |> Failure |> f
                )
            | Failure (x,y) -> (x,y) |> Failure |> f
        )
    override this.Equals that = 
        match that with
        | :? SequentialParser<'r, 'r2> as that -> left = that.Left && right = that.Right
        | _ -> false 
     
    override this.GetHashCode() = hash left + hash right

and [<AbstractClass>] TerminalParser<'r when 'r : equality> () = 
    inherit Parser<'r> ()

    abstract member Parse : CharStream -> ParserResult<'r>

    override this.Apply (inp: CharStream) = 
            match this.Parse(handleWhitespace inp) with 
            | Success (res, tail) -> 
                match processTail tail with 
                | Some tail -> [Success(res, tail)]
                | None      -> [Failure("UnexpectedTrailingChars", tail)]
            | x -> [x]
    
    override this.Chain (t, inp) (f) =
        inp |> handleWhitespace |> this.Parse |> f

    override this.MapWithTail<'r2 when 'r2 : equality> (f) = 
        { new TerminalParser<'r2> () with
            override nt.Parse(inp) = 
                let newTail = handleWhitespace(inp)
                match this.Parse(newTail) with
                | Success(res, tail) -> Success(f (newTail, res), tail) 
                | Failure (x,y) -> Failure (x,y)
        } :> Parser<'r2>

    override this.Seq<'r2 when 'r2 : equality> (other: Parser<'r2>) = 
        match other with 
        | :? TerminalParser<'r2> as other -> 
            { new TerminalParser<'r * 'r2> () with
                member nt.Parse(inp: CharStream) = 
                    match this.Parse(handleWhitespace(inp)) with
                    | Success (res1, tail) -> 
                        match other.Parse(handleWhitespace(tail)) with 
                        | Success(res2, tail) -> ((res1, res2), tail) |> Success
                        | Failure (x,y) -> Failure (x,y)
                    | Failure (x,y) -> Failure (x,y)
            } :> Parser<'r * 'r2>
        | other -> new SequentialParser<'r, 'r2>(this, other) :> Parser<'r * 'r2>

and DisjunctiveParser<'r when 'r : equality>(left: Parser<'r>, right: Parser<'r>) as disj =
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
                let res = res :?> ParserResult<'r> 
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

and StringParser(s: string) = 
    inherit TerminalParser<string>() 
    member this.Str = s
    override this.Parse inp = 
        let trunc = inp.Take(s.Length)
        if trunc.Length < s.Length then Failure ("UnexpectedEOF", inp)
        else
            if trunc = s then Success (s, inp.Drop (s.Length)) 
            else Failure("Expected '" + s + "', got '" + trunc + "'.", inp)
    override this.Equals that = 
        match that with
        | :? StringParser as that -> s = that.Str
        | _ -> false
    override this.GetHashCode () = hash s

and CharParser(pred: char -> bool) = 
    inherit TerminalParser<char>()
    override this.Parse inp = 
        if inp.IsEmpty then Failure ("UnexpectedEOF", inp)
        else
            let h = inp.Head() 
            if h |> pred then Success (h, inp.Drop 1) 
            else Failure("Unexpected character '" + string h + "'", inp)

and EpsilonParser() = 
    inherit TerminalParser<unit>()
        override this.Parse inp = Success((), inp)

and Trampoline () as tram =
    // R
    let _queue = new ResizeArray<GParser * CharStream>()

    // U_j
    let _done = new Dictionary<CharStream, HashSet<GParser> >()

    // P
    let popped = new Dictionary<CharStream, HOMap<GParser, SSet > >()

    // GSS back edges
    let backlinks = new Dictionary<CharStream, HOMap<GParser, FSet > >()

    // prevents divergence in cyclic GSS traversal
    let saved = new HOMap<IParserResult, FSet >()

    let hasNext() = _queue.Count > 0

    let remove() = 
        let ind = _queue.Count - 1
        let tup = _queue.[ind]
        _queue.RemoveAt ind |> ignore
        trace "removed"
        tup

    let step() =
        let p, s = remove()
        p.ChainG (tram, s) <| fun res ->
            match popped.TryGetValue(s) with
            | true, parsers-> 
                if not <| parsers.ContainsKey(p) then
                    popped.[s].Add(p, new SSet() )
            | _, _ ->
                popped.Add(s, new HOMap<GParser, SSet >() )
                popped.[s].Add(p, new SSet() )

            if res.IsSucc then            
                popped.[s].[p].Add(res) |> ignore
                savedToPopped <- savedToPopped + 1
                trace(sprintf "Saved (to popped): %A *=> %A\n" (p, s) res)

            match saved.TryGetValue(res) with
            | true, set ->  
                foundInSaved <- foundInSaved + 1
                backlinks.[s].[p] |> Seq.toArray |> Array.iter (fun f ->    
                    if (not <| set.Contains(f)) then
                        set.Add f |> ignore
                        f res
                )
            | _, _ -> 
                let set = new HashSet<IParserResult -> unit>()
                saved.Add (res, set)
                savedToSaved <- savedToSaved + 1
                backlinks.[s].[p] |> Seq.toArray |> Seq.iter (fun f ->    
                    set.Add f |> ignore
                    f res 
                )

    // L_0
    member this.Run() =
        while (hasNext()) do
            step()
            
    member this.Saved = saved

    member this.Add (p: GParser, s: CharStream) (f: IParserResult -> unit) = 
        let tuple = (p, s)
        
        match backlinks.TryGetValue(s) with
        | true, parsers-> if not <| parsers.ContainsKey(p) then backlinks.[s].Add(p, new FSet() )
        | _, _ ->
            backlinks.Add(s, new HOMap<GParser, FSet >() )
            backlinks.[s].Add(p, new FSet() )

        backlinks.[s].[p].Add(f) |> ignore

        match popped.TryGetValue(s) with
        | true, parsers when parsers.ContainsKey p ->
            foundInPopped <- foundInPopped + 1
            parsers.[p] |> Seq.iter(fun res ->            // if we've already done that, use the result    
                trace(sprintf "Saved: %A *=> %A\n" tuple res)
                f res
            )
        | _ ->
            notFoundInPopped <- notFoundInPopped + 1
            let addTuple(parsers: HashSet<GParser>) = 
                _queue.Add(tuple)
                parsers.Add(p) |> ignore
                trace("Added: ")

            match _done.TryGetValue(s) with
            | true, parsers ->
                if not <| parsers.Contains (p) then 
                    addTuple(parsers) 
                    notFoundInDone <- notFoundInDone + 1
                else foundInDone <- foundInDone + 1
            | _, _ ->
                let parsers = new HashSet<GParser>()
                _done.Add(s, parsers)
                savedToDone <- savedToDone + 1
                addTuple(parsers)
        ()

#nowarn "1189"

let (.^)<'r, 'rr when 'r : equality and 'rr : equality> (p: Parser<'r>) (f: 'r -> 'rr) =
    p.MapWithTail (fun (_, r) -> f r)

let (^^)<'r, 'r2, 'rr when 'r : equality and 'r2 : equality and 'rr : equality> (p: Parser<'r * 'r2>) (f: 'r -> 'r2 -> 'rr) =
    p.MapWithTail (fun (_, (r, r2) ) -> f r r2)

let (^^^)<'r, 'r2, 'r3, 'rr when 'r : equality and 'r2 : equality and 'r3 : equality and 'rr : equality> 
    (p: Parser<('r * 'r2) * 'r3>) (f: 'r -> 'r2 -> 'r3 -> 'rr) = 
        p.MapWithTail (fun (_, ((r, r2), r3) ) -> f r r2 r3)

let (-~-)<'r, 'r2 when 'r : equality and 'r2 : equality> (a: Parser<'r>) (b: Parser<'r2>) = new SequentialParser<_, _>(a, b) :> Parser<_>

let (<|>)<'r when 'r : equality> (a: Parser<'r>) (b: Parser<'r>) = new DisjunctiveParser<'r>(a, b) :> Parser<'r>

let matchc (p) = new CharParser(p)

let str s = new StringParser(s)

let (.>>) this that = this -~- that ^^ (fun r _ -> r)

let (>>.) this that = this -~- that ^^ (fun _ r -> r)

let many1<'r when 'r : equality> (this: Parser<'r>) = 
    { new NonTerminalParser<'r list>() with 
        override nt.Chain(t, inp) (f) = 
            t.Add(this, inp) (fun res -> 
                match (res :> obj :?> ParserResult<_>) with 
                | Success(res1, tail) -> 
                    ([res1], tail) |> Success |> f
                    t.Add(nt, tail) (fun res-> 
                        match (res :> obj :?> ParserResult<_>) with 
                        | Success(res2, tail) -> (res1 :: res2, tail) |> Success |> f
                        | Failure (x,y) -> (x,y) |> Failure |> f
                    )
                | Failure (x,y) -> (x,y) |> Failure |> f
            )
    } :> Parser<_>

let opt<'r when 'r : equality> (this: Parser<'r>) = 
    { new NonTerminalParser<'r option>() with 
        override nt.Chain(t, inp) (f) = 
            (None, inp) |> Success |> f
            t.Add(this, inp) (fun res -> 
                match (res :> obj :?> ParserResult<_>) with 
                | Success(res, tail) -> f(Success(Some(res), tail))
                | Failure (x,y) -> (x,y) |> Failure |> f
            )
    } :> Parser<_>

let many<'r when 'r : equality> (this: Parser<'r>) : Parser<'r list> = (this |> many1 |> opt) .^ (function Some x -> x | _ -> [])

let epsilon = new EpsilonParser()

let createParserForwardedToRef<'r when 'r : equality>(_name: string) = 
    let dummyParser = { new TerminalParser<'r>() with override this.Parse (inp: CharStream) = failwith "unimplemented" }
    let r = ref (dummyParser :> Parser<'r>)
    { new NonTerminalParser<'r>() with 
        override this.Chain(t, inp) (f) =
            //printfn "%A" _name 
            (!r).Chain(t, inp) (function Success _ as s -> f(s) | x -> f(x) ) } :> Parser<'r>, r


let pdigit = new CharParser(Char.IsDigit)
let pwhitespace = (new CharParser(Char.IsWhiteSpace) |> many1) .^ konst ()
let pletter = new CharParser(Char.IsLetter)