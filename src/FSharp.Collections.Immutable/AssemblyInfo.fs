namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharp.Collections.Immutable")>]
[<assembly: AssemblyProductAttribute("FSharp.Collections.Immutable")>]
[<assembly: AssemblyDescriptionAttribute("F# bindings for System.Collections.Immutable.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
