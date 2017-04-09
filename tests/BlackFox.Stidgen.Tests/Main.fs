module Main

open Expecto
open BlackFox.Stidgen.Description
open BlackFox.Stidgen.CsharpCodeTesting

[<EntryPoint>]
let main args =
    let config = {defaultConfig with parallel = false}
    runTestsInAssembly config args
