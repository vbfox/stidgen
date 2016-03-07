#r @"../packages/FAKE/tools/FakeLib.dll"

namespace BlackFox

module AppveyorEx =
    open Fake
    open System.IO
    
    let private sendToAppVeyor args = 
        ExecProcess (fun info -> 
            info.FileName <- "appveyor"
            info.Arguments <- args) (System.TimeSpan.MaxValue)
        |> ignore
        
    /// Set environment variable
    let SetVariable name value =
        sendToAppVeyor <| sprintf "SetVariable -Name \"%s\" -Value \"%s\"" name value

    type ArtifactType = Auto | WebDeployPackage

    type PushArtifactParams =
        {
            Path: string
            FileName: string
            DeploymentName: string
            Type: ArtifactType option
        }
        
    let defaultPushArtifactParams =
        {
            Path = ""
            FileName = ""
            DeploymentName = ""
            Type = None
        }

    let private appendArgIfNotNullOrEmpty value name builder =
        if (isNotNullOrEmpty value) then
            appendWithoutQuotes (sprintf "-%s \"%s\"" name value) builder
        else
            builder

    let PushArtifactEx (setParams : PushArtifactParams -> PushArtifactParams) =
        if buildServer = BuildServer.AppVeyor then
            let parameters = setParams defaultPushArtifactParams
            new System.Text.StringBuilder()
            |> append "PushArtifact"
            |> append parameters.Path
            |> appendArgIfNotNullOrEmpty parameters.FileName "FileName"
            |> appendArgIfNotNullOrEmpty parameters.DeploymentName "DeploymentName"
            |> appendIfSome parameters.Type (sprintf "-Type \"%A\"")
            |> toText
            |> sendToAppVeyor

    let inline PushArtifact path =
        PushArtifactEx (fun p ->
            { p with
                Path = path
                FileName = Path.GetFileName(path)
            })