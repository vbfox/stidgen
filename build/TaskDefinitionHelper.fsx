#r "../packages/FAKE/tools/FakeLib.dll"

namespace BlackFox

/// Allow to define FAKE tasks with a syntax similar to Gulp tasks
[<AutoOpen>]
module TaskDefinitionHelper =
    open Fake
    open System
    open System.Text.RegularExpressions

    type TaskInfo = {
        name: string
        dependencies: TaskInfo list
        isSoft: bool
    }
    with
        member this.Always
            with get() = { this with isSoft = false }
        member this.IfNeeded
            with get() = { this with isSoft = true }

    let mutable private tasks : TaskInfo list = []

    let inline private registerTask meta body =
        Target meta.name body
        tasks <- meta::tasks

    type TaskBuilder(meta: TaskInfo) =
        member __.TryFinally(f, compensation) =
            try
                f()
            finally
                compensation()
        member __.TryWith(f, catchHandler) =
            try
                f()
            with e -> catchHandler e
        member __.Using(disposable: #IDisposable, f) =
            try
                f disposable
            finally
                match disposable with
                | null -> ()
                | disp -> disp.Dispose()
        member __.For(sequence, f) =
            for i in sequence do f i
        member __.Combine(f1, f2) = f2(); f1
        member __.Zero() = ()
        member __.Delay f = f
        member __.Run f =
            registerTask meta f
            meta

    /// Define a task with it's dependencies
    let Task name dependencies body =
        let taskInfo = { name = name; dependencies = dependencies; isSoft = false }
        registerTask taskInfo body
        taskInfo

    /// Define a task with it's dependencies
    let inline task name dependencies =
        let taskInfo = { name = name; dependencies = dependencies; isSoft = false }
        TaskBuilder(taskInfo)

    /// Define a task without any body, only dependencies
    let inline EmptyTask name dependencies =
        let taskInfo = { name = name; dependencies = dependencies; isSoft = false }
        registerTask taskInfo (fun () -> ())
        taskInfo

    /// Send all the defined inter task dependencies to FAKE
    let ApplyTasksDependencies () =
         for taskMetadata in tasks do
            for dependency in taskMetadata.dependencies do
                if dependency.isSoft then
                    dependency.name ?=> taskMetadata.name |> ignore
                else
                    dependency.name ==> taskMetadata.name |> ignore

         tasks <- []

    /// Run the task specified on the command line if there was one or the
    /// default one otherwise.
    let RunTaskOrDefault (taskInfo: TaskInfo) =
        ApplyTasksDependencies ()
        RunTargetOrDefault taskInfo.name
