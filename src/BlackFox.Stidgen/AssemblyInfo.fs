namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("BlackFox.Stidgen")>]
[<assembly: AssemblyProductAttribute("stidgen")>]
[<assembly: AssemblyDescriptionAttribute("Strongly Typed ID type Generator")>]
[<assembly: AssemblyVersionAttribute("0.5.2")>]
[<assembly: AssemblyFileVersionAttribute("0.5.2")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.5.2"
