module BlackFox.Stidgen.Control

type Result<'Success, 'Failure> =
    | Success of 'Success
    | Failure of 'Failure

[<Sealed>]
type ResultBuilder () =
    static let zero = Success ()
    member inline __.Return value : Result<'Success, 'Failure> = Success value
 
    member inline __.ReturnFrom (m : Result<'Success, 'Failure>) = m
    member __.Zero () : Result<unit, 'Failure> = zero
    member inline __.Delay (generator : unit -> Result<'Success, 'Failure>) : Result<'Success, 'Failure> = generator ()
    
    member inline __.Combine (r1, r2) : Result<'Success, 'Failure> =
        match r1 with
        | Failure error -> Failure error
        | Success () -> r2

    member inline __.Bind (value, binder : 'T -> Result<'U, 'Failure>) : Result<'U, 'Failure> =
        match value with
        | Success x -> binder x
        | Failure f -> Failure f

    member inline __.TryWith (body, handler) =
        try 
            body ()
        with 
            e -> handler e
 
    member inline __.TryFinally (body, compensation) =
        try 
            body ()
        finally 
            compensation () 
 
    member this.Using (resource : ('T :> System.IDisposable), body : _ -> Result<_,_>)
        : Result<'U, 'Error> =

        try
            body resource
        finally
            match resource with 
                | null -> () 
                | _ -> resource.Dispose()

    member this.While (guard, body : Result<unit, 'Failure>) : Result<_,_> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            zero

    member this.For (sequence : seq<_>, body : 'T -> Result<unit, 'Failure>) =
        let enumerator = sequence.GetEnumerator ()
        try
            this.While (
                enumerator.MoveNext,
                body enumerator.Current)
        finally
            match enumerator with 
                | null -> () 
                | _ -> enumerator.Dispose()

[<AutoOpen>]
module WorkflowBuilders =
    [<CompiledName("Result")>]
    let result = new ResultBuilder ()