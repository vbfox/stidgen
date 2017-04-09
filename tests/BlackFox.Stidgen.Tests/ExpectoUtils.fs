[<AutoOpen>]
module private ExpectoUtils

open Expecto

let testWithParamsN name cases namef testf =
    let tests = [
        for case in cases do
            let name = namef case
            let test = fun () -> testf case
            yield testCase name test
    ]
    testList name tests

let testWithParams name cases testf =
    testWithParamsN name cases (sprintf "%A") testf
