module Main

open Expecto

#nowarn "46"

[<EntryPoint>]
let main args =
    let config = {defaultConfig with parallel = false}
    runTestsInAssembly config args
