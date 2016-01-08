module FsGll.AbstractParsers

open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharpx.Prelude
open FsGll.InputStream

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
    override this.ToString() = sprintf "SUCC(%A|%A)" value tail
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

let parserResult<'a, 'r when 'r: equality> (r: GParserResult<'a>) = 
    match r with
    | :? GSuccess<'a, 'r> as succ -> Success (succ.Value)
    | :? GFailure<'a> as fail -> Failure (fail.Message, fail.Tail)
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

type Continuation<'a> = GParserResult<'a> -> unit
type RSet<'a> = HashSet<GParserResult<'a> >
type SSet<'a> = HashSet<GParserResult<'a> >
type FSet<'a> = HashSet<Continuation<'a> >

type HOMap<'k,'v> = Dictionary<'k,'v>

type [<AbstractClass>] GParser<'a> () = 
    abstract member Chain : Trampoline<'a> * InputStream<'a> -> Continuation<'a> -> unit

and [<AbstractClass>] Parser<'a, 'r when 'r : equality> () = 
    inherit GParser<'a> ()
    abstract member Apply : InputStream<'a> -> GParserResult<'a> list

and [<AbstractClass>] NonTerminalParser<'a, 'r when 'r : equality> () = 
    inherit Parser<'a, 'r> ()
    override this.Apply (inp: InputStream<'a>) = 
        let t = new Trampoline<'a>()

        let successes = new HashSet<GParserResult<'a> > ()
        let failures = new HashSet<GParserResult<'a> > ()

        this.Chain(t, inp) (fun res ->
            if res.Succeeded && res.Tail.IsEmpty then 
                //if anySuccess then t.Stop()
                successes.Add(res) |> ignore
            elif res.Succeeded then failure "UnexpectedTrailingChars" (res.Tail) |> failures.Add |> ignore
            else res |> failures.Add |> ignore
        )

        t.Run()

        (if successes.Count = 0 then failures else successes) |> Seq.toList

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
    
    override this.Chain (_, inp) (f) = inp |> this.Parse |> f

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

and Trampoline<'a>() as tram =
    // R
    let postrace = new ResizeArray<string>()
    let _queue = new ResizeArray<GParser<'a> * InputStream<'a>>()

    // U_j
    let _done = new Dictionary<InputStream<'a>, HashSet<GParser<'a> > >()

    // P
    let popped = new Dictionary<InputStream<'a>, HOMap<GParser<'a>, SSet<'a> > >()

    // GSS back edges
    let backlinks = new Dictionary<InputStream<'a>, HOMap<GParser<'a>, FSet<'a> > >()

    // prevents divergence in cyclic GSS traversal
    let saved = new HOMap<GParserResult<'a>, FSet<'a> >()

    let mutable stopped = false
    
    let hasNext() = _queue.Count > 0

    let remove() = 
        let ind = _queue.Count - 1
        let tup = _queue.[ind]
        _queue.RemoveAt ind |> ignore
        trace "removed"
        tup

    let step() =
        let p, s = remove()
        //postrace.Add(sprintf "%d %A" s.Ind p)
        p.Chain (tram, s) <| fun res ->
            match popped.TryGetValue(s) with
            | true, parsers-> 
                if not <| parsers.ContainsKey(p) then
                    parsers.Add(p, new SSet<'a>() )
            | _, _ ->
                let nmap = new HOMap<GParser<'a>, SSet<'a> >() in nmap.Add(p, new SSet<'a>() )
                popped.Add(s, nmap)

            if res.Succeeded then
                popped.[s].[p].Add(res) |> ignore
                //savedToPopped <- savedToPopped + 1
                trace(lazy (sprintf "Saved (to popped): %A *=> %A\n" (p, s) res))

            match saved.TryGetValue(res) with
            | true, set ->
                backlinks.[s].[p] |> Seq.toArray |> Array.iter (fun f ->
                    if not (set.Contains(f)) then
                        set.Add f |> ignore
                        //savedToSaved <- savedToSaved + 1
                        f res
                    else 
                        foundInSaved <- foundInSaved + 1
                )
            | _, _ -> 
                let set = new HashSet<GParserResult<'a> -> unit>()
                saved.Add (res, set)
                backlinks.[s].[p] |> Seq.toArray |> Array.iter (fun f ->
                    set.Add f |> ignore
                    //savedToSaved <- savedToSaved + 1
                    f res 
                )

    member this.Saved  = saved
    member this.Popped = popped
    member this.Done   = _done
    member this.Backlinks = backlinks
    member this.Postrace = postrace

    member this.Stop() : unit = stopped <- true

    // L_0
    member this.Run () : unit =
        while (hasNext() && not stopped) do
            step()
            
    member this.Add (p: GParser<'a>, s: InputStream<'a>) (f: GParserResult<'a> -> unit) : unit = 
        let tuple = (p, s)
        match backlinks.TryGetValue(s) with
        | true, parsers-> 
            if not <| parsers.ContainsKey(p) then 
                parsers.Add(p, new FSet<'a>() )
        | _, _ ->
            let nmap = new HOMap<GParser<'a>, FSet<'a> >() in nmap.Add(p, new FSet<'a>())
            backlinks.Add(s, nmap)

        backlinks.[s].[p].Add(f) |> ignore

        match popped.TryGetValue(s) with
        | true, parsers when parsers.ContainsKey p ->
            //foundInPopped <- foundInPopped + 1
            parsers.[p] |> Seq.toArray |> Seq.iter(fun res ->            // if we've already done that, use the result
                trace(lazy(sprintf "Revisited: %A *=> %A\n" tuple res))
                f res )
            
        | _ ->
//            if not (_done.ContainsKey s) then _done.Add(s, new HashSet<GParser<'a> >())
//            if not (_done.[s].Contains p) then 
//                _queue.Add(tuple)
//                _done.[s].Add(p) |> ignore

            notFoundInPopped <- notFoundInPopped + 1
            let addTuple(parsers: HashSet<GParser<'a> >) = 
                //notFoundInDone <- notFoundInDone + 1
                _queue.Add(tuple)
                parsers.Add(p) |> ignore
                trace(lazy(sprintf "Added: %A *=> %A\n" tuple))
                //queuePushed <- queuePushed + 1

            match _done.TryGetValue(s) with
            | true, parsers ->
                if not <| parsers.Contains (p) then 
                    addTuple(parsers) 
            | _, _ ->
                let parsers = new HashSet<GParser<'a> >()
                _done.Add(s, parsers)
                //savedToDone <- savedToDone + 1
                addTuple(parsers)
        ()

#nowarn "1189"

let inline private withSucc<'a, 'r when 'r : equality> (sf: _ -> GParserResult<'a>) (f: Continuation<'a>) = 
    (fun (r: GParserResult<'a>) ->
        match r with
        | :? GSuccess<'a, 'r> as s1 -> sf s1 |> f
        | fail -> fail |> f)

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
            if h |> pred then success<'a, 'a> h (s.Drop)
            else failure<'a> "Unexpected token" s }

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

let (<|>%) p x = p <|> preturn x

let (<?>) p msg = p <|> perror msg

let (>>%) this x = this >>= fun _ -> preturn x

//let (.>>)<'a, 'r1, 'r2 when 'r1: equality and 'r2: equality> (p: Parser<'a, 'r1>) (q: Parser<'a, 'r2>) = 
//    p >>= fun (x: 'r1) -> q >>% x
let (.>>)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: Parser<'a, 'r>) (q: Parser<'a, 'r2>) : Parser<'a, 'r> = 
    { new NonTerminalParser<'a, 'r>() with 
      override nt.Chain(t, inp) (cont) = 
            t.Add(p, inp) 
                (function
                 | :? GSuccess<'a, 'r> as s1 -> 
                     q.Chain(t, s1.Tail) 
                         (fun r2 -> cont (if r2.Succeeded then success<'a, 'r> s1.Value r2.Tail else r2))
                 | f -> cont f)
    } :> Parser<'a, 'r>

//let (>>.) this that = this >>= fun _ -> that
let (>>.)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: Parser<'a, 'r>) (q: Parser<'a, 'r2>) : Parser<'a, 'r2> = 
    let nonTermPar () = { new NonTerminalParser<'a, 'r2>() with 
                          override nt.Chain(t, s) (f) = 
                              t.Add(p, s) (fun r -> if r.Succeeded then q.Chain(t, r.Tail) f else f r)
                        } :> Parser<'a, 'r2>
    match p with 
    | :? TerminalParser<'a,'r> as p -> 
        match q with 
        | :? TerminalParser<'a,'r2> as q ->
            { new TerminalParser<'a, 'r2>() with
              override x.Parse s = 
                  match p.Parse(s) with
                  | :? GSuccess<'a, 'r> as r -> q.Parse(r.Tail)
                  | fail -> fail
            } :> Parser<'a, 'r2>
        | _ -> nonTermPar ()
    | _ -> nonTermPar ()

    

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
          override nt.Chain(t, s) (f) = t.Add(p, s) (withSucc (fun r -> success<'a, 'r2> (fn r.Value) r.Tail) f)
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

let sepBy1 (p: Parser<'a, 'r>) sep : Parser<'a, 'r list> = 
    pipe2 p (many (sep >>. p)) (fun hd tl -> hd :: tl) 

let sepBy (p: Parser<'a, 'r>) sep : Parser<'a, 'r list> = sepBy1 p sep <|>% []

let notFollowedBy (p: TerminalParser<'a, 'r>) : Parser<'a, unit> = 
    { new NonTerminalParser<'a, unit> () with
      override this.Chain(t, s) (f) =
          f <| if p.Parse(s).Failed then success<'a, unit> () s
               else failure "SyntaxError (notFollowedBy)" s
    } :> Parser<'a, unit>
    
let pstr (pat: string) = 
    { new TerminalParser<char, string>() with 
      override this.Parse s = 
          let s = s :?> StringInputStream 
          if s.SMatch(pat) then success<char, string> pat (s.DropN pat.Length)
          else failure "Unexpected" s
    } :> Parser<char, string>

let preg(pat: string) =
    let regex = new Regex("^(" + pat + ")")
    { new TerminalParser<char, string>() with 
      override this.Parse s = 
          let s = s :?> StringInputStream 
          match s.RMatch(regex) with
          | true, capt -> success<char, string> pat (s.DropN capt.Length)
          | _, _ ->  failure "Unexpected" s
    } :> Parser<char, string>

let private dummyParser<'a, 'r when 'r : equality> = 
   { new TerminalParser<'a, 'r>() with override this.Parse s = failwith msgDummyUsed } :> Parser<'a, 'r>

let createParserForwardedToRef<'a, 'r when 'r : equality>(name:string) = 
    let r = ref dummyParser<'a, 'r>
    { new NonTerminalParser<'a, 'r>() with 
      override this.Chain(t, inp) (f) = (!r).Chain(t, inp) (f) } :> Parser<'a, 'r>, r

let runParser (p: Parser<'a, 'r>) (s: 'a seq) : ParserResult<'a, 'r> list = 
    let res = p.Apply(new ArrayInputStream<'a>(Seq.toArray s, 0))
    if res.IsEmpty then failwith msgLibraryError
    res |> List.map parserResult<'a, 'r>
