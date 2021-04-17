module Main

open Expecto

#nowarn "46"

[<EntryPoint>]
let main args =
    let writeResults = TestResults.writeNUnitSummary "TestResults.xml"
    let config = { defaultConfig with runInParallel = false }
    let config = config.appendSummaryHandler writeResults
    runTestsInAssembly config args
