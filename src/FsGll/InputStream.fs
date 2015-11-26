module FsGll.InputStream

open System

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

let fromArray a = new ArrayInputStream<_>(a, 0)