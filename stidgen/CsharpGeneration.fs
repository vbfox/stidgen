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
        Id : IdType
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

let private makeValueProperty info =
    SyntaxFactory.PropertyDeclaration(info.UnderlyingTypeSyntax, info.Id.ValueProperty)
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

let private makeCtor info =
    let argName = firstCharToLower info.Id.ValueProperty

    let checkForNull =
        if'
            (equals (identifier argName) (Literal.Null))
            (block [|throw (objectCreation typesyntaxof<ArgumentNullException> [|Literal.String argName|]) |])

    let assignProperty =
        setThisMember info.Id.ValueProperty (SyntaxFactory.IdentifierName(argName))

    SyntaxFactory.ConstructorDeclaration(info.Id.Name)
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

let private makeToString info =
    let isString = info.Id.Type = typedefof<string>
    let returnToString =
        if isString then
            (ret (thisMemberAccess info.Id.ValueProperty))
        else
            info |> returnCallVoidMethodOnValue "ToString"

    let returnIfNull = info |> makeIfValueNull (fun block ->
        block |> addStatement (ret Literal.EmptyString)
        )

    SyntaxFactory.MethodDeclaration(TypeSyntax.String, "ToString")
    |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.OverrideKeyword|]
    |?> ((info.AllowNull && not isString), addBodyStatement returnIfNull)
    |> addBodyStatement returnToString

module private Equality =
    let private returnEqualsValue info expr = ret (objectEquals (identifier info.Id.ValueProperty) expr)
    let makeGetHashCode info =
        let returnGetHashCode = ret (getHashCode info.ValueMemberAccess)

        let returnIfNull = info |> makeIfValueNull (fun block ->
            block |> addStatement (ret Literal.Zero)
            )

        SyntaxFactory.MethodDeclaration(TypeSyntax.Int, "GetHashCode")
        |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.OverrideKeyword|]
        |?> (info.AllowNull, addBodyStatement returnIfNull)
        |> addBodyStatement returnGetHashCode

    let makeEquals info =
        let returnIfNull = info |> makeIfValueNull (fun block ->
            block |> addStatement (ret (Literal.Int 0))
            )

        let parameterName = "other"
        let parameter = identifier parameterName

        let notIs typeSyntax = not' (is typeSyntax parameter)
        let incorrectTypeCondition = 
            if info.Id.EqualsUnderlying then
                and' (notIs info.GeneratedTypeSyntax) (notIs info.UnderlyingTypeSyntax) :> ExpressionSyntax
            else
                notIs info.GeneratedTypeSyntax :> ExpressionSyntax
        let returnFalseForIncorrectType = if' incorrectTypeCondition (block [|ret Literal.False|])

        let castParameterToType t = cast t parameter
    
        let returnArgCastToIdValueEqualsValue =
            returnEqualsValue info ((castParameterToType info.GeneratedTypeSyntax) |> memberAccess info.Id.ValueProperty)

        let returnArgCastToUnderlyingEqualsValue = 
            returnEqualsValue info (castParameterToType info.UnderlyingTypeSyntax)

        let ifIsUnderlyingReturnEquals =
            if'
                (is info.UnderlyingTypeSyntax parameter)
                (block [|returnArgCastToUnderlyingEqualsValue|])

        SyntaxFactory.MethodDeclaration(TypeSyntax.Bool, "Equals")
        |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.OverrideKeyword|]
        |> addParameter parameterName TypeSyntax.Object
        |> addBodyStatement returnFalseForIncorrectType
        |?> (info.Id.EqualsUnderlying, addBodyStatement ifIsUnderlyingReturnEquals)
        |> addBodyStatement returnArgCastToIdValueEqualsValue

    let makeEqualsGenerated info =
        let parameterName = "other"
        let body = returnEqualsValue info (identifier parameterName |> memberAccess info.Id.ValueProperty)

        SyntaxFactory.MethodDeclaration(TypeSyntax.Bool, "Equals")
        |> addModifiers [|SyntaxKind.PublicKeyword|]
        |> addParameter parameterName info.GeneratedTypeSyntax
        |> addBodyStatement body
        
    let makeEqualsUnderlying info =
        let parameterName = "other"
        let body = returnEqualsValue info (identifier parameterName)

        SyntaxFactory.MethodDeclaration(TypeSyntax.Bool, "Equals")
        |> addModifiers [|SyntaxKind.PublicKeyword|]
        |> addParameter parameterName info.UnderlyingTypeSyntax
        |> addBodyStatement body

    let iequatableOf t =
        let iEquatable = typedefof<IEquatable<_>>
        let ns = TypeSyntax.MakeQualified(iEquatable.Namespace.Split('.'))

        TypeSyntax.MakeGeneric iEquatable.Name [|t|]

module private Casts =
    let addCast fromType toType cast expressionMaker generatedClass =
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

    let addCastToUnderlyingType info generatedClass = 
        generatedClass
        |> addCast info.GeneratedTypeSyntax info.UnderlyingTypeSyntax info.Id.CastToUnderlying
            (fun n -> simpleMemberAccess n info.Id.ValueProperty)

    let addCastFromUnderlyingType info generatedClass = 
        generatedClass
        |> addCast info.UnderlyingTypeSyntax info.GeneratedTypeSyntax info.Id.CastFromUnderlying
            (fun n -> objectCreation info.GeneratedTypeSyntax [|SyntaxFactory.IdentifierName(n)|])

let private makeClass idType info = 
    let visibility = visibilityToKeyword idType.Visibility

    let addMember' builder (decl : ClassDeclarationSyntax) =
        decl |> addMember (builder info)

    SyntaxFactory.ClassDeclaration(idType.Name)
        |> addBaseTypes [| Equality.iequatableOf info.GeneratedTypeSyntax |]
        |?> (info.Id.EqualsUnderlying, addBaseTypes [| Equality.iequatableOf info.UnderlyingTypeSyntax |])
        |> addModifiers [|visibility; SyntaxKind.PartialKeyword|]
        |> addMember' makeValueProperty
        |> addMember' makeCtor
        |> addMember' makeToString
        |> addMember' Equality.makeGetHashCode
        |> addMember' Equality.makeEquals
        |> addMember' Equality.makeEqualsGenerated
        |?> (info.Id.EqualsUnderlying, addMember' Equality.makeEqualsUnderlying)
        |> Casts.addCastFromUnderlyingType info
        |> Casts.addCastToUnderlyingType info

let makeRootNode idType = 
    let namespaceProvided = not (String.IsNullOrEmpty(idType.Namespace))

    let info =
        {
            Id = idType
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