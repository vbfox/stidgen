module BlackFox.TraceListener

#r @"../packages/FAKE/tools/FakeLib.dll"

open System
open Fake

/// Implements a TraceListener for System.Console.
/// ## Parameters
///  - `importantMessagesToStdErr` - Defines whether to trace important messages to StdErr.
///  - `colorMap` - A function which maps TracePriorities to ConsoleColors.
type MyConsoleTraceListener(importantMessagesToStdErr, colorMap) =
    let mutable useColor = true
    let protectColor f =
        try
            if useColor then f()
        with
        | :? System.ArgumentNullException ->
            useColor <- false

    let writeText toStdErr color newLine text =
        let curColor = Console.ForegroundColor
        try
            protectColor (fun () -> if curColor <> color then Console.ForegroundColor <- color)
            let printer =
                match toStdErr, newLine with
                | true, true -> eprintfn
                | true, false -> eprintf
                | false, true -> printfn
                | false, false -> printf
            printer "%s" text
        finally
          protectColor (fun () -> if curColor <> color then Console.ForegroundColor <- curColor)

    interface ITraceListener with
        /// Writes the given message to the Console.
        member this.Write msg =
            let color = colorMap msg
            match msg with
            | StartMessage -> ()
            | OpenTag _ -> ()
            | CloseTag _ -> ()
            | ImportantMessage text | ErrorMessage text ->
                writeText importantMessagesToStdErr color true text
            | LogMessage(text, newLine) | TraceMessage(text, newLine) ->
                writeText false color newLine text
            | FinishedMessage -> ()

let traceListener = MyConsoleTraceListener(importantMessagesToStdErr, colorMap)

Fake.TraceListener.listeners.Clear()
Fake.TraceListener.listeners.Add(traceListener)
