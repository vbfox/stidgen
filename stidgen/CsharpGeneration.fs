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
        ThisValueMemberAccess : MemberAccessExpressionSyntax
        ValueAccess : ExpressionSyntax -> ExpressionSyntax
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
            (info.ThisValueMemberAccess |> memberAccess name)
            Array.empty)

let private makeIfValueNull fillBlock info =
    if'
        (equals (info.ThisValueMemberAccess) Literal.Null)
        (fillBlock emptyBlock)

let private makeToString info =
    let isString = info.Id.UnderlyingType = typedefof<string>
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
    open System.Reflection

    let makeGetHashCode info =
        let returnGetHashCode = ret (getHashCode info.ThisValueMemberAccess)

        let returnIfNull = info |> makeIfValueNull (fun block ->
            block |> addStatement (ret Literal.Zero)
            )

        SyntaxFactory.MethodDeclaration(TypeSyntax.Int, "GetHashCode")
        |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.OverrideKeyword|]
        |?> (info.AllowNull, addBodyStatement returnIfNull)
        |> addBodyStatement returnGetHashCode

    /// Call the most adapted underlying equals method between underlying-typed expressions.
    let private underlyingEquals info exprA exprB eq =
        let opMethod =
            info.Id.UnderlyingType.GetMethod(
                (if eq then "op_Equality" else "op_Inequality"),
                BindingFlags.Static ||| BindingFlags.Public,
                null,
                CallingConventions.Any,
                [|info.Id.UnderlyingType;info.Id.UnderlyingType|],
                null)
        
        if opMethod <> null then
            // Prefer the operator as for CLR implementations it's an obvious optimization for native types and strings
            let op = if eq then equals else notEquals
            op exprA exprB :> ExpressionSyntax
        else
            // Otherwise Object.Equals is a safe choice
            objectEquals exprA exprB :> ExpressionSyntax

    let private thisValueEquals info expr =
        underlyingEquals info info.ThisValueMemberAccess expr

    let makeStaticEquals info =
        let parameterA = identifier "a"
        let parameterB = identifier "b"

        let value x = x :> ExpressionSyntax |> info.ValueAccess
        let body = ret (underlyingEquals info (value parameterA) (value parameterB) true)

        SyntaxFactory.MethodDeclaration(TypeSyntax.Bool, "Equals")
        |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.StaticKeyword|]
        |> addParameter "a" info.GeneratedTypeSyntax
        |> addParameter "b" info.GeneratedTypeSyntax
        |> addBodyStatement body

    let makeEquals info =
        let returnIfNull = info |> makeIfValueNull (fun block ->
            block |> addStatement (ret (Literal.Int 0))
            )

        let parameterName = "other"
        let parameter = identifier parameterName :> ExpressionSyntax

        let notIs typeSyntax = not' (is typeSyntax parameter)
        let incorrectTypeCondition = 
            if info.Id.EqualsUnderlying then
                and' (notIs info.GeneratedTypeSyntax) (notIs info.UnderlyingTypeSyntax) :> ExpressionSyntax
            else
                notIs info.GeneratedTypeSyntax :> ExpressionSyntax
        let returnFalseForIncorrectType = if' incorrectTypeCondition (block [|ret Literal.False|])

        let returnArgCastToUnderlyingEqualsValue = 
            ret (thisValueEquals info (parameter |> cast info.UnderlyingTypeSyntax) true)

        let ifIsUnderlyingReturnEquals =
            if'
                (is info.UnderlyingTypeSyntax parameter)
                (block [|returnArgCastToUnderlyingEqualsValue|])

        let returnArgCastToIdValueEqualsValue =
            ret (
                thisValueEquals
                    info
                    (parameter |> cast info.GeneratedTypeSyntax :> ExpressionSyntax |> info.ValueAccess)
                    true
                )

        SyntaxFactory.MethodDeclaration(TypeSyntax.Bool, "Equals")
        |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.OverrideKeyword|]
        |> addParameter parameterName TypeSyntax.Object
        |> addBodyStatement returnFalseForIncorrectType
        |?> (info.Id.EqualsUnderlying, addBodyStatement ifIsUnderlyingReturnEquals)
        |> addBodyStatement returnArgCastToIdValueEqualsValue

    let makeEqualsGenerated info =
        let parameterName = "other"
        let parameter = identifier parameterName :> ExpressionSyntax
        let body = ret (thisValueEquals info (info.ValueAccess parameter) true)

        SyntaxFactory.MethodDeclaration(TypeSyntax.Bool, "Equals")
        |> addModifiers [|SyntaxKind.PublicKeyword|]
        |> addParameter parameterName info.GeneratedTypeSyntax
        |> addBodyStatement body
        
    let makeEqualsUnderlying info =
        let parameterName = "other"
        let parameter = identifier parameterName :> ExpressionSyntax
        let body = ret (thisValueEquals info parameter true)

        SyntaxFactory.MethodDeclaration(TypeSyntax.Bool, "Equals")
        |> addModifiers [|SyntaxKind.PublicKeyword|]
        |> addParameter parameterName info.UnderlyingTypeSyntax
        |> addBodyStatement body

    let private iEquatable = typedefof<IEquatable<_>>
    let private iEquatableNamespace = NameSyntax.MakeQualified(iEquatable.Namespace.Split('.'))
    let iequatableOf t =
        SyntaxFactory.QualifiedName(iEquatableNamespace, NameSyntax.MakeGeneric iEquatable.Name [|t|])

    let makeOperator info eq leftArgType rightArgType =
        let left = identifier "left"
        let right = identifier "right"

        let value x = x :> ExpressionSyntax |> info.ValueAccess
        let body = ret (underlyingEquals info (value left) (value right) eq)

        let operatorToken = if eq then SyntaxKind.EqualsEqualsToken else SyntaxKind.ExclamationEqualsToken
        SyntaxFactory.OperatorDeclaration(TypeSyntax.Bool, SyntaxFactory.Token(operatorToken))
        |> addModifiers [|SyntaxKind.PublicKeyword;SyntaxKind.StaticKeyword|]
        |> addParameter "left" leftArgType
        |> addParameter "right" rightArgType
        |> addBodyStatement body

    let addOperators info (classDeclaration:StructDeclarationSyntax) =
        classDeclaration
        |> addMember (makeOperator info true info.GeneratedTypeSyntax info.GeneratedTypeSyntax)
        |> addMember (makeOperator info false info.GeneratedTypeSyntax info.GeneratedTypeSyntax)

module private Casts =
    let addCast fromType toType cast expressionMaker generatedClass =
        let parameterName = "x"
        let makeCast cast' = 
            SyntaxFactory.ConversionOperatorDeclaration(SyntaxFactory.Token(cast'), toType)
                |> addModifiers [|SyntaxKind.PublicKeyword;SyntaxKind.StaticKeyword|]
                |> addParameter parameterName fromType
                |> addBodyStatement (ret (expressionMaker parameterName)) 

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

module private Convertible =
    open System.Reflection
    
    let private iconvertible = typeof<IConvertible>
    let private iconvertibleName = namesyntaxof<IConvertible>

    let makeMember (m : MethodInfo) info =
        // Initial declaration
        let returnType = NameSyntax.FromType m.ReturnType
        let declaration =
            SyntaxFactory.MethodDeclaration(returnType, m.Name)
                .WithExplicitInterfaceSpecifier(SyntaxFactory.ExplicitInterfaceSpecifier(iconvertibleName))

        // Add parameters
        let parameters = m.GetParameters() |> Array.map (fun p -> (p.Name, NameSyntax.FromType p.ParameterType))
        let declaration = parameters |> Seq.fold (fun decl (name, type') -> decl |> addParameter name type') declaration

        // Add body
        let parametersForCall = parameters |> Array.map (fun (name, _) -> identifier name :> ExpressionSyntax)
        let body = 
            ret
                (invocation
                    (info.ThisValueMemberAccess |> cast iconvertibleName |> memberAccess m.Name)
                    parametersForCall)
        declaration |> addBodyStatement body

    let private addIConvertibleMethods info (classDeclaration : StructDeclarationSyntax) =
        iconvertible.GetMethods()
            |> Array.fold (fun decl m -> decl |> addMember (makeMember m info)) classDeclaration

    let addIConvertibleMembers info (classDeclaration : StructDeclarationSyntax) =
        if iconvertible.IsAssignableFrom(info.Id.UnderlyingType) then
            classDeclaration
            |> addIConvertibleMethods info
            |> addBaseTypes [| iconvertibleName |]
        else
            classDeclaration

let private makeClass idType info = 
    let visibility = visibilityToKeyword idType.Visibility

    let addMember' builder (decl : StructDeclarationSyntax) =
        decl |> addMember (builder info)

    (struct' idType.Name)
        |> addBaseTypes [| Equality.iequatableOf info.GeneratedTypeSyntax |]
        |?> (info.Id.EqualsUnderlying, addBaseTypes [| Equality.iequatableOf info.UnderlyingTypeSyntax |])
        |> addModifiers [|visibility; SyntaxKind.PartialKeyword|]
        |> addMember' makeValueProperty
        |> addMember' makeCtor
        |> addMember' makeToString
        |> addMember' Equality.makeGetHashCode
        |> addMember' Equality.makeEquals
        |> addMember' Equality.makeEqualsGenerated
        |> addMember' Equality.makeStaticEquals
        |> Equality.addOperators info
        |?> (info.Id.EqualsUnderlying, addMember' Equality.makeEqualsUnderlying)
        |> Casts.addCastFromUnderlyingType info
        |> Casts.addCastToUnderlyingType info
        |> Convertible.addIConvertibleMembers info

let makeRootNode idType = 
    let namespaceProvided = not (String.IsNullOrEmpty(idType.Namespace))

    let info =
        {
            Id = idType
            AllowNull = idType.AllowNull && idType.UnderlyingType.IsClass
            NamespaceProvided = namespaceProvided
            UnderlyingTypeSyntax = SyntaxFactory.ParseTypeName(idType.UnderlyingType.FullName)
            GeneratedTypeSyntax  = SyntaxFactory.ParseTypeName(idType.Name)
            ThisValueMemberAccess = thisMemberAccess idType.ValueProperty
            ValueAccess = (fun expr -> expr |> dottedMemberAccess [idType.ValueProperty])
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