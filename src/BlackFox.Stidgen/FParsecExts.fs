module BlackFox.Stidgen.FParsecExts

open FParsec

[<Sealed>]
type Inline =
    /// This is a specialized implementation of Inline.SepBy
    /// The difference with the original is that it accept to end by the separator
    /// but if it does it backtrack to before the separator.
    [<NoDynamicInvocation>]
    static member inline SepByBacktrackEnd(stateFromFirstElement,
                                            foldState,
                                            resultFromState,
                                            elementParser: Parser<_,_>,
                                            separatorParser: Parser<_,_>) : Parser<_,'u> =
        fun stream ->
            let mutable stateTag = stream.StateTag
            let mutable reply = elementParser stream
            if reply.Status = Ok then
                let mutable xs    = stateFromFirstElement reply.Result
                let mutable error = reply.Error
                stateTag <- stream.StateTag
                let mutable stateBeforeSep = CharStreamState(stream)
                let mutable sepReply = separatorParser stream
                let mutable sepStateTag = stream.StateTag
                while sepReply.Status = Ok && (reply <- elementParser stream; reply.Status = Ok) do
                    xs <- foldState xs sepReply.Result reply.Result
                    if sepStateTag <> stream.StateTag then
                        error <- reply.Error
                    elif stateTag <> sepStateTag then
                        error <- mergeErrors sepReply.Error reply.Error
                    else
                        failwith "Infinite loop"
                    
                    stateTag <- stream.StateTag
                    stateBeforeSep <- CharStreamState(stream)
                    sepReply <- separatorParser stream
                    sepStateTag <- stream.StateTag
                if sepReply.Status = Error && stateTag = sepStateTag then
                    Reply(Ok, resultFromState xs, mergeErrors error sepReply.Error)
                else
                    if reply.Status <> Ok then
                        error <- if sepStateTag <> stream.StateTag then
                                    reply.Error
                                    else
                                    let error2 = mergeErrors sepReply.Error reply.Error
                                    if stateTag <> sepStateTag then error2
                                    else mergeErrors error error2
                        if sepStateTag = stream.StateTag && sepReply.Status = Ok then
                            stream.BacktrackTo(&stateBeforeSep)
                            Reply(Ok, resultFromState xs, error)
                        else
                            Reply(reply.Status, error)
                    else
                        let error = if stateTag <> sepStateTag then sepReply.Error
                                    else mergeErrors error sepReply.Error
                        Reply(sepReply.Status, error)
            else
                Reply(reply.Status, reply.Error)


let sepBy1BacktrackEnd p sep = Inline.SepByBacktrackEnd((fun x -> [x]), (fun xs _ x -> x::xs), List.rev,       p, sep)
