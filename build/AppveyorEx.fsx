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
            /// The full local path to the artifact
            Path: string
            /// File name to display in the artifact tab
            FileName: string
            /// Deployment name
            DeploymentName: string
            /// Type of the artifact
            Type: ArtifactType
        }

    let defaultPushArtifactParams =
        {
            Path = ""
            FileName = ""
            DeploymentName = ""
            Type = Auto
        }

    let private appendArgIfNotNullOrEmpty value name builder =
        if (isNotNullOrEmpty value) then
            appendWithoutQuotes (sprintf "-%s \"%s\"" name value) builder
        else
            builder

    /// Push an artifact
    let PushArtifact (setParams : PushArtifactParams -> PushArtifactParams) =
        if buildServer = BuildServer.AppVeyor then
            let parameters = setParams defaultPushArtifactParams
            new System.Text.StringBuilder()
            |> append "PushArtifact"
            |> append parameters.Path
            |> appendArgIfNotNullOrEmpty parameters.FileName "FileName"
            |> appendArgIfNotNullOrEmpty parameters.DeploymentName "DeploymentName"
            |> appendArgIfNotNullOrEmpty (sprintf "%A" parameters.Type) "Type"
            |> toText
            |> sendToAppVeyor

    /// Push multiple artifacts
    let PushArtifacts paths =
        if buildServer = BuildServer.AppVeyor then
            for path in paths do
                PushArtifact (fun p -> { p with Path = path; FileName = Path.GetFileName(path) })