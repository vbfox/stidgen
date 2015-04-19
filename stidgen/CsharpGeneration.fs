module BlackFox.Stidgen.CsharpGeneration

open System
open System.IO
open System.Text
open BlackFox.Stidgen.Description
open BlackFox.Stidgen.FluentRoslyn
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Formatting
open Microsoft.CodeAnalysis.Simplification

type private ParsedInfo =
    {
        NamespaceProvided : bool
        UnderlyingTypeSyntax : TypeSyntax
        GeneratedTypeSyntax : TypeSyntax
    }

let private (|?>) x (c, f) = if c then f x else x
let private (|??>) x (c, f, g) = if c then f x else g x

let private visibilityToKeyword = function
    | Public -> SyntaxKind.PublicKeyword
    | Private -> SyntaxKind.PrivateKeyword
    | Protected -> SyntaxKind.ProtectedKeyword

let private makeValueProperty info idType =
    SyntaxFactory.PropertyDeclaration(info.UnderlyingTypeSyntax, idType.ValueProperty)
        .AddAccessorListAccessors(
            SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                |> withSemicolon,
            SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration)
                |> addModifiers [|SyntaxKind.PrivateKeyword|]
                |> withSemicolon
            )
        |> addModifiers [|SyntaxKind.PublicKeyword|]

let private firstCharToLower (x:string) = 
    if x.Length = 0 then
        x
    else
        let first = System.Char.ToLowerInvariant(x.[0])
        first.ToString() + x.Substring(1)

let private typenameof<'t> = SyntaxFactory.ParseTypeName(typedefof<'t>.FullName)

let private makeCtor info idType =
    let argName = firstCharToLower idType.ValueProperty

    let checkForNull = 
        SyntaxFactory.IfStatement(
            SyntaxFactory.BinaryExpression(
                SyntaxKind.EqualsExpression,
                SyntaxFactory.IdentifierName(argName),
                SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression)
            ),
            SyntaxFactory.Block(
                SyntaxFactory.ThrowStatement(
                    SyntaxFactory.ObjectCreationExpression(typenameof<ArgumentNullException>)
                    |> addArgument (stringLiteral argName)
                )
            )
        )

    let assignProperty =
        setThisMember idType.ValueProperty (SyntaxFactory.IdentifierName(argName))

    SyntaxFactory.ConstructorDeclaration(idType.Name)
    |> addModifiers [|SyntaxKind.PublicKeyword|]
    |> addParameter argName info.UnderlyingTypeSyntax
    |?> (not idType.AllowNull, addBodyStatement checkForNull)
    |> addBodyStatement assignProperty

let private makeToString info idType =
    let returnToString =
        SyntaxFactory.ReturnStatement(
            SyntaxFactory.InvocationExpression(
                thisMemberAccess idType.ValueProperty |> memberAccess "ToString"
            )
        )

    let returnIfNull =
        SyntaxFactory.IfStatement(
            SyntaxFactory.BinaryExpression(
                SyntaxKind.EqualsExpression,
                thisMemberAccess idType.ValueProperty,
                nullLiteral),
            SyntaxFactory.Block()
                |> addStatement (SyntaxFactory.ReturnStatement(stringLiteral ""))
        )

    SyntaxFactory.MethodDeclaration(stringTypeSyntax, "ToString")
    |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.OverrideKeyword|]
    |?> (idType.AllowNull, addBodyStatement returnIfNull)
    |> addBodyStatement returnToString

let private addCast fromType toType cast expressionMaker generatedClass =
    let paramName = "x"
    let makeCast cast' = 
        SyntaxFactory.ConversionOperatorDeclaration(SyntaxFactory.Token(cast'), toType)
            |> addModifiers [|SyntaxKind.PublicKeyword;SyntaxKind.StaticKeyword|]
            |> addParameter paramName fromType
            |> addBodyStatement (SyntaxFactory.ReturnStatement(expressionMaker paramName)) 

    let addCast' cast' = generatedClass |> addMember (makeCast cast')

    match cast with
    | None -> generatedClass
    | Implicit -> addCast' SyntaxKind.ImplicitKeyword
    | Explicit -> addCast' SyntaxKind.ExplicitKeyword

let private makeClass idType info = 
    let visibility = visibilityToKeyword idType.Visibility

    SyntaxFactory.ClassDeclaration(idType.Name)
        |> addModifiers [|visibility; SyntaxKind.PartialKeyword|]
        |> addMember (idType |> makeValueProperty info)
        |> addMember (idType |> makeCtor info)
        |> addMember (idType |> makeToString info)
        |> addCast info.UnderlyingTypeSyntax info.GeneratedTypeSyntax idType.CastFromUnderlying
            (fun n -> objectCreation info.GeneratedTypeSyntax [|SyntaxFactory.IdentifierName(n)|])
        |> addCast info.GeneratedTypeSyntax info.UnderlyingTypeSyntax idType.CastToUnderlying
            (fun n -> simpleMemberAccess n idType.ValueProperty)

let makeRootNode idType = 
    let namespaceProvided = not (String.IsNullOrEmpty(idType.Namespace))
    let generatedFullName = if namespaceProvided then idType.Namespace + "." + idType.Name else idType.Name
    let info =
        {
            NamespaceProvided = namespaceProvided
            UnderlyingTypeSyntax = SyntaxFactory.ParseTypeName(idType.Type.FullName)
            GeneratedTypeSyntax  = SyntaxFactory.ParseTypeName(generatedFullName)
        }

    let generatedClass = makeClass idType info

    let rootMember =
        if String.IsNullOrEmpty(idType.Namespace) then
            generatedClass :> MemberDeclarationSyntax
        else
            SyntaxFactory.NamespaceDeclaration(SyntaxFactory.IdentifierName(idType.Namespace))
                .AddMembers(generatedClass) :> MemberDeclarationSyntax

    SyntaxFactory.CompilationUnit()
        .AddUsings("System")
        .AddMembers(rootMember)

let private makeDocument (rootNode:SyntaxNode) =
    let workspace = new AdhocWorkspace()
    let project = workspace.AddProject("MyProject", LanguageNames.CSharp)

    let mscorlib = PortableExecutableReference.CreateFromAssembly(typedefof<obj>.Assembly)
    let project = project.AddMetadataReference(mscorlib)
    workspace.TryApplyChanges(project.Solution) |> ignore

    project.AddDocument("GeneratedId.cs", rootNode)

let private simplifyDocumentAsync (doc:Document) = 
    async {
        let! root = doc.GetSyntaxRootAsync() |> Async.AwaitTask
        let newRoot = root.WithAdditionalAnnotations(Simplifier.Annotation)
        let newDoc = doc.WithSyntaxRoot(newRoot)

        return! Simplifier.ReduceAsync(newDoc) |> Async.AwaitTask
    }

let private formatDocumentAsync (doc:Document) =
    async {
        let! root = doc.GetSyntaxRootAsync() |> Async.AwaitTask
        let newRoot = root.WithAdditionalAnnotations(Formatter.Annotation)
        let newDoc = doc.WithSyntaxRoot(newRoot)

        return! Formatter.FormatAsync(newDoc) |> Async.AwaitTask
    }

let private (|!>) a f = async.Bind(a, f)

let private rootNodeToStringAsync node =
    async {
        let document = makeDocument node
        let! formatted = document |> simplifyDocumentAsync |!> formatDocumentAsync
        let! finalNode = formatted.GetSyntaxRootAsync() |> Async.AwaitTask

        return finalNode.GetText().ToString()
    }

let idTypeToStringAsync idType =
    idType
        |> makeRootNode
        |> rootNodeToStringAsync

let idTypeToString idType =
    idType |> idTypeToStringAsync |> Async.RunSynchronously