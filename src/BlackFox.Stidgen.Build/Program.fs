module BlackFox.Stidgen.Build.Program

open BlackFox.TypedTaskDefinitionHelper
open Fake.Core
open Fake.BuildServer

let setupFakeContext (argv: string list) =
    let argvTweaked =
        match argv with
        | [ singleArg ] when not (singleArg.StartsWith("-")) ->
            [ "--target"; singleArg ]
        | _ -> argv
    let execContext = Context.FakeExecutionContext.Create false "build.fsx" argvTweaked
    Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)

let setupBuildServers () =
    BuildServer.install [
        AppVeyor.Installer
    ]

[<EntryPoint>]
let main argv =
    setupFakeContext (List.ofArray argv)
    setupBuildServers ()
    let defaultTask = Tasks.createAndGetDefault ()
    RunTaskOrDefault (EmptyTask "Default" [ defaultTask ])
    0
