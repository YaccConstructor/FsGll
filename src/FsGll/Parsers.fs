module FsGll.Parsers

open System
open System.Threading
open FsGll.InputStream

let msgLibraryError = "Error in FsGll library."
let msgDummyUsed = "Parser created with createParserForwardedToRef is not defined"

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
    | _ -> failwith msgLibraryError

[<Interface>]
type IParser<'a, 'r when 'r : equality> = 
    abstract member Chain<'t, 'cont, 'cres> : ('t * InputStream<'a>) -> 'cont -> 'cres
    abstract member Bind<'r2 when 'r2 : equality> : ('r -> IParser<'a, 'r2>) -> IParser<'a, 'r2>
    abstract member Or : IParser<'a, 'r> -> IParser<'a, 'r>
    abstract member Many1 : IParser<'a, 'r list>
    abstract member Many : IParser<'a, 'r list>
    abstract member First<'r2 when 'r2: equality> : IParser<'a, 'r2> -> IParser<'a, 'r>
    abstract member Second<'r2 when 'r2: equality> : IParser<'a, 'r2> -> IParser<'a, 'r2>
    abstract member Map<'r2 when 'r2: equality> : ('r -> 'r2) -> IParser<'a, 'r2>
    abstract member Return<'r2 when 'r2: equality> : 'r2 -> IParser<'a, 'r2>

#nowarn "1189"
let (>>=)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: IParser<'a, 'r>) (fn: 'r -> IParser<'a, 'r2>) = p.Bind(fn)
let many<'a, 'r when 'r : equality> (p: IParser<'a, 'r>) = p.Many
let many1<'a, 'r when 'r : equality> (p: IParser<'a, 'r>) = p.Many1
let (>>.)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: IParser<'a, 'r>) (q: IParser<'a, 'r2>) = p.Second(q)
let (.>>)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: IParser<'a, 'r>) (q: IParser<'a, 'r2>) = p.First(q)
let (|>>)<'a, 'r, 'r2 when 'r: equality and 'r2 : equality> (p: IParser<'a, 'r>) (fn: 'r -> 'r2) = p.Map(fn) 

let (.>>.) p1 p2 = 
    p1 >>= fun a ->
    p2 >>= fun b -> p1.Return (a, b)

let pipe2 p1 p2 fn = 
    p1 >>= fun a ->
    p2 >>= fun b -> p1.Return (fn a b)

let pipe3 p1 p2 p3 fn = 
    p1 >>= fun a ->
    p2 >>= fun b ->
    p3 >>= fun c -> p1.Return (fn a b c)

let pipe4 p1 p2 p3 p4 fn = 
    p1 >>= fun a ->
    p2 >>= fun b ->
    p3 >>= fun c -> 
    p4 >>= fun d -> p1.Return (fn a b c d)

let between popen pclose (p: IParser<'a, 'r>) : IParser<'a, 'r> = 
    popen >>. p .>> pclose

let (<|>) (a: IParser<_,_>) (b: IParser<_,_>) = a.Or(b) :> IParser<'a, 'r>
 
let (<|>%) p x = p <|> p.Return x

let (<?>) p msg = p <|> perror msg

let (>>%) this x = this >>= fun _ -> this.Return x :> IParser<_,_>

let sepBy (p: IParser<'a, 'r>) sep : IParser<'a, 'r list> = 
    pipe2 p ((sep >>. p).Many) (fun hd tl -> hd :: tl) <|>% []
