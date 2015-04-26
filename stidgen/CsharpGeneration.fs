module BlackFox.Stidgen.CsharpGeneration

open System
open System.IO
open System.Text
open BlackFox.Stidgen.Description
open BlackFox.Stidgen.FluentRoslyn
open BlackFox.Stidgen.FluentRoslyn.Operators
open BlackFox.Stidgen.FluentRoslyn.WellKnownMethods
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Formatting
open Microsoft.CodeAnalysis.Simplification

type private ParsedInfo =
    {
        IdType : IdType
        AllowNull : bool
        NamespaceProvided : bool
        UnderlyingTypeSyntax : TypeSyntax
        GeneratedTypeSyntax : TypeSyntax
        ValueMemberAccess : MemberAccessExpressionSyntax
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

let private makeCtor info idType =
    let argName = firstCharToLower idType.ValueProperty

    let checkForNull =
        if'
            (equals (identifier argName) (Literal.Null))
            (block [|throw (objectCreation typenameof<ArgumentNullException> [|Literal.String argName|]) |])

    let assignProperty =
        setThisMember idType.ValueProperty (SyntaxFactory.IdentifierName(argName))

    SyntaxFactory.ConstructorDeclaration(idType.Name)
    |> addModifiers [|SyntaxKind.PublicKeyword|]
    |> addParameter argName info.UnderlyingTypeSyntax
    |?> (not info.AllowNull, addBodyStatement checkForNull)
    |> addBodyStatement assignProperty

let private returnCallVoidMethodOnValue name info =
    ret
        (invocation
            (info.ValueMemberAccess |> memberAccess name)
            Array.empty)

let private makeIfValueNull fillBlock info =
    if'
        (equals (info.ValueMemberAccess) Literal.Null)
        (fillBlock emptyBlock)

let private makeToString info idType =
    let isString = idType.Type = typedefof<string>
    let returnToString =
        if isString then
            (ret (thisMemberAccess idType.ValueProperty))
        else
            info |> returnCallVoidMethodOnValue "ToString"

    let returnIfNull = info |> makeIfValueNull (fun block ->
        block |> addStatement (ret Literal.EmptyString)
        )

    SyntaxFactory.MethodDeclaration(TypeSyntax.String, "ToString")
    |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.OverrideKeyword|]
    |?> ((info.AllowNull && not isString), addBodyStatement returnIfNull)
    |> addBodyStatement returnToString

let private makeGetHashCode info idType =
    let returnGetHashCode = ret (getHashCode info.ValueMemberAccess)

    let returnIfNull = info |> makeIfValueNull (fun block ->
        block |> addStatement (ret Literal.Zero)
        )

    SyntaxFactory.MethodDeclaration(TypeSyntax.Int, "GetHashCode")
    |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.OverrideKeyword|]
    |?> (info.AllowNull, addBodyStatement returnIfNull)
    |> addBodyStatement returnGetHashCode

let private makeEquals info (idType : IdType) =
    let returnIfNull = info |> makeIfValueNull (fun block ->
        block |> addStatement (ret (Literal.Int 0))
        )

    let parameterName = "obj"
    let parameter = identifier parameterName

    let notIs typeSyntax = not' (is typeSyntax parameter)
    let incorrectTypeCondition = 
        if idType.EqualsUnderlying then
            and' (notIs info.GeneratedTypeSyntax) (notIs info.UnderlyingTypeSyntax) :> ExpressionSyntax
        else
            notIs info.GeneratedTypeSyntax :> ExpressionSyntax
    let returnFalseForIncorrectType = if' incorrectTypeCondition (block [|ret Literal.False|])

    let castParameterToType t = cast t parameter
    
    let returnEqualsValue expr = ret (objectEquals (identifier idType.ValueProperty) expr)

    let returnArgCastToIdValueEqualsValue =
        returnEqualsValue ((castParameterToType info.GeneratedTypeSyntax) |> memberAccess idType.ValueProperty)

    let returnArgCastToUnderlyingEqualsValue = 
        returnEqualsValue (castParameterToType info.UnderlyingTypeSyntax)

    let ifIsUnderlyingReturnEquals =
        if'
            (is info.UnderlyingTypeSyntax parameter)
            (block [|returnArgCastToUnderlyingEqualsValue|])

    SyntaxFactory.MethodDeclaration(TypeSyntax.Bool, "Equals")
    |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.OverrideKeyword|]
    |> addParameter parameterName TypeSyntax.Object
    |> addBodyStatement returnFalseForIncorrectType
    |?> (idType.EqualsUnderlying, addBodyStatement ifIsUnderlyingReturnEquals)
    |> addBodyStatement returnArgCastToIdValueEqualsValue

let private addCast fromType toType cast expressionMaker generatedClass =
    let parameterName = "x"
    let makeCast cast' = 
        SyntaxFactory.ConversionOperatorDeclaration(SyntaxFactory.Token(cast'), toType)
            |> addModifiers [|SyntaxKind.PublicKeyword;SyntaxKind.StaticKeyword|]
            |> addParameter parameterName fromType
            |> addBodyStatement (SyntaxFactory.ReturnStatement(expressionMaker parameterName)) 

    let addCast' cast' = generatedClass |> addMember (makeCast cast')

    match cast with
    | None -> generatedClass
    | Implicit -> addCast' SyntaxKind.ImplicitKeyword
    | Explicit -> addCast' SyntaxKind.ExplicitKeyword

let private makeClass idType info = 
    let visibility = visibilityToKeyword idType.Visibility

    let addMember' builder (decl : ClassDeclarationSyntax) =
        decl |> addMember (idType |> builder info)

    SyntaxFactory.ClassDeclaration(idType.Name)
        |> addModifiers [|visibility; SyntaxKind.PartialKeyword|]
        |> addMember' makeValueProperty
        |> addMember' makeCtor
        |> addMember' makeToString
        |> addMember' makeGetHashCode
        |> addMember' makeEquals
        |> addCast info.UnderlyingTypeSyntax info.GeneratedTypeSyntax idType.CastFromUnderlying
            (fun n -> objectCreation info.GeneratedTypeSyntax [|SyntaxFactory.IdentifierName(n)|])
        |> addCast info.GeneratedTypeSyntax info.UnderlyingTypeSyntax idType.CastToUnderlying
            (fun n -> simpleMemberAccess n idType.ValueProperty)

let makeRootNode idType = 
    let namespaceProvided = not (String.IsNullOrEmpty(idType.Namespace))

    let info =
        {
            IdType = idType
            AllowNull = idType.AllowNull && idType.Type.IsClass
            NamespaceProvided = namespaceProvided
            UnderlyingTypeSyntax = SyntaxFactory.ParseTypeName(idType.Type.FullName)
            GeneratedTypeSyntax  = SyntaxFactory.ParseTypeName(idType.Name)
            ValueMemberAccess = thisMemberAccess idType.ValueProperty
        }

    let generatedClass = makeClass idType info

    let rootMember =
        if String.IsNullOrEmpty(idType.Namespace) then
            generatedClass :> MemberDeclarationSyntax
        else
            SyntaxFactory.NamespaceDeclaration(SyntaxFactory.IdentifierName(idType.Namespace))
                .AddMembers(generatedClass) :> MemberDeclarationSyntax

    emptyFile
        |> addUsings [|"System"|]
        |> addMember rootMember

let private makeDocument (rootNode:SyntaxNode) =
    let workspace = new AdhocWorkspace()
    let project = workspace.AddProject("MyProject", LanguageNames.CSharp)

    let mscorlib = PortableExecutableReference.CreateFromAssembly(typedefof<obj>.Assembly)
    let project = project.AddMetadataReference(mscorlib)
    workspace.TryApplyChanges(project.Solution) |> ignore

    project.AddDocument("GeneratedId.cs", rootNode)

let private simplifyDocumentAsync (doc:Document) = 
    async {
        let! root = !! doc.GetSyntaxRootAsync()
        let newRoot = root.WithAdditionalAnnotations(Simplifier.Annotation)
        let newDoc = doc.WithSyntaxRoot(newRoot)

        return! !! Simplifier.ReduceAsync(newDoc)
    }

let private formatDocumentAsync (doc:Document) =
    async {
        let! root = !! doc.GetSyntaxRootAsync()
        let newRoot = root.WithAdditionalAnnotations(Formatter.Annotation)
        let newDoc = doc.WithSyntaxRoot(newRoot)

        return! !! Formatter.FormatAsync(newDoc)
    }

let private rootNodeToStringAsync node =
    async {
        let document = makeDocument node
        let! formatted = document |> simplifyDocumentAsync |!> formatDocumentAsync
        let! finalNode = !! formatted.GetSyntaxRootAsync()

        return finalNode.GetText().ToString()
    }

let idTypeToStringAsync idType =
    idType
        |> makeRootNode
        |> rootNodeToStringAsync

let idTypeToString idType =
    idType |> idTypeToStringAsync |> Async.RunSynchronously