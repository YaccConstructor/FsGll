module Parsers

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

let handleWhitespace(s: CharStream) = s

let processTail(tail: CharStream) = 
    let newTail = handleWhitespace tail
    if not <| newTail.IsEmpty then Some(newTail) else None

[<AbstractClass>]
type GParserResult (tail: CharStream) = 
    abstract member Succeeded : bool
    member this.Failed = not this.Succeeded
    member this.Tail = tail
    //override this.ToString() = sprintf "GPR(%A)" this.Tail
    
type GSuccess<'r when 'r: equality> (value: 'r, tail: CharStream) = 
    inherit GParserResult(tail)
    override this.Succeeded = true
    member this.Data = (value, tail)
    member this.Value = value
    override this.ToString() = sprintf "SUCC(%A)" this.Data
    override this.Equals that = 
        match that with 
        | :? GSuccess<'r> as that -> value = that.Value && tail = that.Tail
        | _ -> false
    override this.GetHashCode() = hash value + hash tail

type GFailure (msg: string, tail: CharStream) = 
    inherit GParserResult (tail)
    override this.Succeeded = false
    member this.Message = msg
    override this.Equals that = 
        match that with 
        | :? GFailure as that -> msg = that.Message && tail = that.Tail
        | _ -> false
    override this.GetHashCode() = hash msg + hash tail
    override this.ToString() = sprintf "FAIL(m:%s|%A)" msg tail

let success<'r when 'r: equality> value tail = new GSuccess<'r>(value, tail) :> GParserResult
let failure msg tail = new GFailure(msg, tail) :> GParserResult

type ParserResult<'r> = 
   | Success of value: 'r * tail: CharStream
   | Failure of msg: string * tail: CharStream

let parserResult<'r when 'r: equality> (r: GParserResult) = 
    match r with
    | :? GSuccess<'r> as succ -> Success (succ.Value, succ.Tail)
    | :? GFailure as fail -> Failure (fail.Message, fail.Tail)
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

type RSet = HashSet<GParserResult >
type SSet = HashSet<GParserResult >
type FSet = HashSet<GParserResult -> unit>

type HOMap<'k,'v> = Dictionary<'k,'v>

type [<AbstractClass>] GParser () = 
    abstract member Chain : Trampoline * CharStream -> (GParserResult -> unit) -> unit

and [<AbstractClass>] Parser<'r when 'r : equality> () = 
    inherit GParser ()
    abstract member Apply : CharStream -> GParserResult list
    abstract member MapWithTail<'r2 when 'r2 : equality> : ( (CharStream * 'r) -> 'r2) -> Parser<'r2>
    abstract member Seq<'r2  when 'r2 : equality> : Parser<'r2> -> Parser<'r * 'r2>

and [<AbstractClass>] NonTerminalParser<'r when 'r : equality> () = 
    inherit Parser<'r> ()
    override this.Apply (inp: CharStream) = 
        let t = new Trampoline()
        trampolinesCreated <- trampolinesCreated + 1

        let successes = new HashSet<GParserResult> ()
        let failures = new HashSet<GParserResult> ()

        this.Chain(t, inp) (fun res ->
            if res.Succeeded && res.Tail.IsEmpty then successes.Add(res) |> ignore
            elif res.Succeeded then failure "UnexpectedTrailingChars" (res.Tail) |> failures.Add |> ignore
            else res |> failures.Add |> ignore
        )

        t.Run()
//        let sv = t.Saved
//        printfn "SAVED CNT: %d\n" (Seq.length sv)
//        printfn "BACKLINKS CNT: %d\n" (Seq.length t.Backlinks)
//        printfn "DONE CNT: %d %A\n" (Seq.length t.Done) 
//            (t.Done |> Seq.map (fun (m: KeyValuePair<CharStream,HashSet<GParser> >) -> m.Value.Count) |> Seq.toArray)
//        printfn "POPPED CNT: %d\n" (Seq.length t.Popped)
//
//        let sv2 = 
//            sv
//            //|> Seq.filter (fun (x: KeyValuePair<GParserResult, FSet>) -> x.Key.Succeeded |> not) 
//            |> Seq.map(fun (x: KeyValuePair<GParserResult, FSet>) -> sprintf "%A\n" x.Key)
//            |> Seq.sort
//            |> Seq.toArray
//        printfn "EQUAL: %A\n" (Array.length sv2 = (sv2 |> Seq.distinct |> Seq.length))
//        sv2 |> (curry (File.WriteAllLines) "dump.txt")
//        [sprintf "%A" t.Backlinks] |> Seq.toArray |> (curry (File.WriteAllLines) "dump.txt")
        (if successes.Count = 0 then failures else successes) |> Seq.toList

    override this.Seq<'r2 when 'r2 : equality> (that: Parser<'r2>) = new SequentialParser<'r, 'r2>(this, that) :> Parser<'r * 'r2>

    override this.MapWithTail<'r2 when 'r2 : equality> (f : ( (CharStream * 'r) -> 'r2)) = 
        { new NonTerminalParser<'r2> () with
            override nt.Chain(t, inp)(f2) = 
                this.Chain(t, inp) (function
                    | :? GSuccess<'r> as succ -> success (f (inp, succ.Value)) succ.Tail |> f2
                    | fail -> fail |> f2
                )
        } :> Parser<'r2>

and SequentialParser<'r, 'r2 when 'r : equality and 'r2 : equality> (left: Parser<'r>, right: Parser<'r2>) = 
    inherit NonTerminalParser<'r * 'r2>()
    member this.Left = left
    member this.Right = right
    override this.Chain (t, inp) (f: GParserResult -> unit) = //<'r * 'r2>
        left.Chain(t, inp) (function
            | :? GSuccess<'r> as succ1 ->
                right.Chain(t, succ1.Tail) (fun res -> 
                    match res with
                    | :? GSuccess<'r2> as succ2 -> (success (succ1.Value, succ2.Value) succ2.Tail |> f)
                    | fail -> fail |> f
                )
            | fail -> fail |> f
        )

    override this.Equals that = 
        match that with
        | :? SequentialParser<'r, 'r2> as that -> left = that.Left && right = that.Right
        | _ -> false 
     
    override this.GetHashCode() = hash left + hash right

and [<AbstractClass>] TerminalParser<'r when 'r : equality> () = 
    inherit Parser<'r> ()

    abstract member Parse : CharStream -> GParserResult

    override this.Apply (inp: CharStream) = 
            match this.Parse(handleWhitespace inp) with 
            | :? GSuccess<'r> as succ1 ->
                match processTail succ1.Tail with 
                | Some tail -> [GSuccess(succ1.Value, tail)]
                | None      -> [GFailure("UnexpectedTrailingChars", succ1.Tail)]
            | x -> [x]
    
    override this.Chain (t, inp) (f) =
        inp |> handleWhitespace |> this.Parse |> f

    override this.MapWithTail<'r2 when 'r2 : equality> (f : ( (CharStream * 'r) -> 'r2)) = 
        { new TerminalParser<'r2> () with
            override nt.Parse(inp) = 
                let newTail = handleWhitespace(inp)
                match this.Parse(newTail) with
                | :? GSuccess<'r> as succ1 -> success(f (newTail, succ1.Value)) succ1.Tail
                | fail -> fail
        } :> Parser<'r2>

    override this.Seq<'r2 when 'r2 : equality> (other: Parser<'r2>) = 
        match other with 
        | :? TerminalParser<'r2> as other -> 
            { new TerminalParser<'r * 'r2> () with
                member nt.Parse(inp: CharStream) = 
                    match this.Parse(handleWhitespace(inp)) with
                    | :? GSuccess<'r> as succ1 -> //(res1, tail) -> 
                        match other.Parse(handleWhitespace(succ1.Tail)) with 
                        | :? GSuccess<'r2> as succ2 -> success (succ1.Value, succ2.Value) succ2.Tail
                        | fail -> fail
                    | fail -> fail
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
        let results = new HashSet<GParserResult > ()
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

and StringParser(s: string) = 
    inherit TerminalParser<string>() 
    member this.Str = s
    override this.Parse inp = 
        let trunc = inp.Take(s.Length)
        if trunc.Length < s.Length then failure "UnexpectedEOF" inp
        else
            if trunc = s then success s (inp.Drop (s.Length))
            else failure ("Expected '" + s + "', got '" + trunc + "'.") inp
    override this.Equals that = 
        match that with
        | :? StringParser as that -> s = that.Str
        | _ -> false
    override this.GetHashCode () = hash s

and CharParser(pred: char -> bool) = 
    inherit TerminalParser<char>()
    override this.Parse inp = 
        if inp.IsEmpty then failure "UnexpectedEOF" inp
        else
            let h = inp.Head() 
            if h |> pred then success h (inp.Drop 1) 
            else failure ("Unexpected character '" + string h + "'") inp

and EpsilonParser() = 
    inherit TerminalParser<unit>()
        override this.Parse inp = success () inp

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
    let saved = new HOMap<GParserResult, FSet >()

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
                    popped.[s].Add(p, new SSet() )
            | _, _ ->
                popped.Add(s, new HOMap<GParser, SSet >() )
                popped.[s].Add(p, new SSet() )

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
                let set = new HashSet<GParserResult -> unit>()
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
    member this.Run() =
        while (hasNext()) do
            step()
            
    member this.Add (p: GParser, s: CharStream) (f: GParserResult -> unit) = 
        let tuple = (p, s)
        
        match backlinks.TryGetValue(s) with
        | true, parsers-> if not <| parsers.ContainsKey(p) then backlinks.[s].Add(p, new FSet() )
        | _, _ ->
            backlinks.Add(s, new HOMap<GParser, FSet >() )
            backlinks.[s].Add(p, new FSet() )

        backlinks.[s].[p].Add(f) |> ignore

        match popped.TryGetValue(s) with
        | true, parsers when parsers.ContainsKey p ->
            //foundInPopped <- foundInPopped + 1
            parsers.[p] |> Seq.iter(fun res ->            // if we've already done that, use the result
                //trace(sprintf "Revisited: %A *=> %A\n" tuple res)
                f res
            )
        | _ ->
            notFoundInPopped <- notFoundInPopped + 1
            let addTuple(parsers: HashSet<GParser>) = 
                //notFoundInDone <- notFoundInDone + 1
                _queue.Add(tuple)
                parsers.Add(p) |> ignore
                //trace("Added: ")
                //queuePushed <- queuePushed + 1

            match _done.TryGetValue(s) with
            | true, parsers ->
                if not <| parsers.Contains (p) then 
                    addTuple(parsers) 
            | _, _ ->
                let parsers = new HashSet<GParser>()
                _done.Add(s, parsers)
                //savedToDone <- savedToDone + 1
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
                match res with
                | :? GSuccess<'r> as succ1 -> 
                    success [succ1.Value] succ1.Tail |> f
                    t.Add(nt, succ1.Tail) (fun res -> 
                        match res with 
                        | :? GSuccess<'r list> as succ2 -> success (succ1.Value :: succ2.Value) succ2.Tail |> f
                        | fail -> f fail
                    )
                | fail -> f fail
            )
    } :> Parser<_>

let opt<'r when 'r : equality> (this: Parser<'r>) = 
    { new NonTerminalParser<'r option>() with 
        override nt.Chain(t, inp) (f) = 
            success<'r option> None inp |> f
            t.Add(this, inp) (fun res -> 
                match res with 
                | :? GSuccess<'r> as succ1 -> success (Some succ1.Value) succ1.Tail |> f
                | fail -> f fail
            )
    } :> Parser<_>

let many<'r when 'r : equality> (this: Parser<'r>) : Parser<'r list> = (this |> many1 |> opt) .^ (function Some x -> x | _ -> [])

let epsilon = new EpsilonParser()

let private dummyParser<'r when 'r : equality> = 
   { new TerminalParser<'r>()
     with override this.Parse (inp: CharStream) = failwith "unimplemented" } :> Parser<'r>

let createParserForwardedToRef<'r when 'r : equality>(name:string) = 
    let r = ref dummyParser<'r>
    { new NonTerminalParser<'r>() with 
        override this.Chain(t, inp) (f) =
            //printfn "%A" name 
            (!r).Chain(t, inp) (f) } :> Parser<'r>, r


let pdigit = new CharParser(Char.IsDigit)
let pwhitespace = (new CharParser(Char.IsWhiteSpace) |> many1) .^ konst ()
let pletter = new CharParser(Char.IsLetter)

let apply<'r when 'r : equality> (p: Parser<'r>) (s: CharStream) = p.Apply(s) |> List.map(parserResult<'r>)