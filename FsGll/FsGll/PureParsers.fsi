module PureParsers

[<AbstractClass>]
type GParserResult<'a> (tail: InputStream<'a>) = 
    abstract member Succeeded : bool
    member this.Failed = not this.Succeeded
    member this.Tail = tail
    //override this.ToString() = sprintf "GPR(%A)" this.Tail
    abstract member GCompare : GParserResult<'a> -> int
    interface IComparable 
    
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
        override this.CompareTo (that) =
            this.GetHashCode() - that.GetHashCode()

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
