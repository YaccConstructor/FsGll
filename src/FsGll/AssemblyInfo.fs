namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsGll")>]
[<assembly: AssemblyProductAttribute("FsGll")>]
[<assembly: AssemblyDescriptionAttribute("An parser combinator library based on the GLL algorithm for F#")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
