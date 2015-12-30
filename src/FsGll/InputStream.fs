module FsGll.InputStream

open System
open System.Text.RegularExpressions

[<AbstractClass>]
type InputStream<'a> (ind: int) = 
    abstract member Head : 'a
    abstract member IsEmpty: bool
    abstract member Drop : InputStream<'a>
    member this.Ind = ind
    //override this.ToString() = underlying |> Seq.map (fun x -> x.ToString()) |> Seq.fold (+) "" 

    // It looks like F# Collections.Set<_> uses only IComparable interface and 
    // forgets about check objects equality with .Equals, so dirty hacks is necessary
    override this.Equals that = ind = (that :?> InputStream<'a>).Ind
    override this.GetHashCode() = ind.GetHashCode()
    interface IComparable with
        override this.CompareTo that = this.Ind - (that :?> InputStream<'a>).Ind
            
type ArrayInputStream<'a> (arr: 'a [], ind: int) = 
    inherit InputStream<'a>(ind)
    override this.Head = arr.[ind]
    override this.IsEmpty = ind >= arr.Length
    override this.Drop = new ArrayInputStream<'a>(arr, ind + 1) :> InputStream<'a>
    override this.ToString() = sprintf "%A" <| Array.sub arr ind (Array.length arr - ind)

type StringInputStream (s: string, ind: int) = 
    inherit InputStream<char>(ind)
    override this.Head = s.[ind]
    override this.IsEmpty = ind >= s.Length
    override this.Drop = new StringInputStream(s, ind + 1) :> InputStream<char>
    override this.ToString() = sprintf "%A" <| s.Substring(ind)
    member this.DropN n = new StringInputStream(s, ind + n) :> InputStream<char>
    member this.SMatch (pat: string) = if s.Length - ind >= pat.Length then s.Substring(ind, pat.Length) = pat else false
    member this.RMatch (reg: Regex) = 
        let m = reg.Match(s.Substring(ind))
        if m.Success then (true, m.Value) else (false, "")

let fromArray a = new ArrayInputStream<_>(a, 0)

type ParserResult<'a, 'r> = 
   | Success of value: 'r
   | Failure of msg: string * tail: InputStream<'a>

let msgLibraryError = "Error in FsGll library."
let msgDummyUsed = "Parser created with createParserForwardedToRef is not defined"
