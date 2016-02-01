namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsGll")>]
[<assembly: AssemblyProductAttribute("FsGll")>]
[<assembly: AssemblyDescriptionAttribute("FsGll")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
