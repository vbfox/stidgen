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
        PropertyName : string
        FieldName : string
    }

let private value info x = x :> ExpressionSyntax |> info.ValueAccess

let private (|?>) x (c, f) = if c then f x else x
let private (|??>) x (c, f, g) = if c then f x else g x

let private visibilityToKeyword = function
    | Public -> SyntaxKind.PublicKeyword
    | Internal -> SyntaxKind.InternalKeyword

let private makeValueField info =
    field info.UnderlyingTypeSyntax info.FieldName
        |> addModifiers [|SyntaxKind.PrivateKeyword; SyntaxKind.ReadOnlyKeyword|]

let private makeValueProperty info =
    let body = block [| ret info.ThisValueMemberAccess |]
    SyntaxFactory.PropertyDeclaration(info.UnderlyingTypeSyntax, info.PropertyName)
        |> addModifiers [|SyntaxKind.PublicKeyword|]
        |> addGetter body
        
/// Intern an expression of underlying type if needed && possible
let private internIfNeeded arg info =
    let underlyingIsString = typeof<string> = info.Id.UnderlyingType
    if info.Id.InternString && underlyingIsString
    then
        let intern = WellKnownMethods.stringIntern arg
        if info.AllowNull then
            // String.Intern throw if it's argument is null so we must handle that
            cond (equals arg Literal.Null) Literal.Null intern :> ExpressionSyntax
        else
            intern :> ExpressionSyntax
    else
        arg :> ExpressionSyntax

let private makeCtor info =
    let argName = info.FieldName
    let arg = identifier argName
    let checkForNull = throwIfArgumentNull argName
    let assignProperty = setThisMember info.FieldName (internIfNeeded arg info)

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
            (ret info.ThisValueMemberAccess)
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

        let value = value info

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

        let value = value info
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
            (fun n -> value info (identifier n))

    let addCastFromUnderlyingType info generatedClass = 
        generatedClass
        |> addCast info.UnderlyingTypeSyntax info.GeneratedTypeSyntax info.Id.CastFromUnderlying
            (fun n -> objectCreation info.GeneratedTypeSyntax [|identifier n|])

module private Convertible =
    open System.Reflection
    
    let private iconvertible = typeof<IConvertible>
    let private iconvertibleName = namesyntaxof<IConvertible>

    let private makeNullCheck (m : MethodInfo) info =
        match m.Name with
        | "ToType" -> 
            // ToType equivalent on System.Convert is named ChangeType
            let typeVariable = (identifier (m.GetParameters().[0].Name))
            if'
                (equals info.ThisValueMemberAccess Literal.Null)
                (block
                    [|
                        ret (invocation (dottedMemberAccess' ["System"; "Convert"; "ChangeType"]) [|Literal.Null; typeVariable|])
                    |])
        | "ToChar" -> 
            // To char always throw for null, so no need to call the static version
            let argName = m.GetParameters().[0].Name
            throwIfArgumentNull argName
        | _ ->
            // For other ToXXX methods call the static equivalent on System.Convert
            if'
                (equals info.ThisValueMemberAccess Literal.Null)
                (block
                    [|
                        ret (invocation (dottedMemberAccess' ["System"; "Convert"; m.Name]) [|Literal.Null|])
                    |])


    let private makeMember (m : MethodInfo) info =
        // Initial declaration
        let returnType = NameSyntax.FromType m.ReturnType
        let declaration =
            SyntaxFactory.MethodDeclaration(returnType, m.Name)
                .WithExplicitInterfaceSpecifier(SyntaxFactory.ExplicitInterfaceSpecifier(iconvertibleName))

        // Add parameters
        let parameters = m.GetParameters() |> Array.map (fun p -> (p.Name, NameSyntax.FromType p.ParameterType))
        let declaration = parameters |> Seq.fold (fun decl (name, type') -> decl |> addParameter name type') declaration

        // Body
        let bodyCheck = makeNullCheck m info
        let parametersForCall = parameters |> Array.map (fun (name, _) -> identifier name :> ExpressionSyntax)
        let bodyRet = 
            ret
                (invocation
                    (info.ThisValueMemberAccess |> cast iconvertibleName |> memberAccess m.Name)
                    parametersForCall)

        // Add body
        declaration |> addBodyStatements [| bodyCheck; bodyRet|]

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

module GeneratedCodeAttribute = 
    let private nameSyntax = namesyntaxof<System.CodeDom.Compiler.GeneratedCodeAttribute>

    let inline private addToMember typeMember =
        let toolName = Literal.String "BlackFox.Stidgen"
        let toolVersion = Literal.String AssemblyVersionInformation.Version
        let attribute = makeAttribute nameSyntax [toolName; toolVersion]
        typeMember |> addAttribute attribute

    /// Add the 'GeneratedCodeAttribute' to all members of the type
    let addToAllMembers (typeSyntax : StructDeclarationSyntax) =
        let members = typeSyntax.Members |> Seq.map (fun m -> 
            match m with
            | :? PropertyDeclarationSyntax as property -> property |> addToMember :> MemberDeclarationSyntax
            | :? FieldDeclarationSyntax as field -> field |> addToMember :> MemberDeclarationSyntax
            | :? MethodDeclarationSyntax as method' -> method' |> addToMember :> MemberDeclarationSyntax
            | :? OperatorDeclarationSyntax as operator -> operator |> addToMember :> MemberDeclarationSyntax
            | :? ConversionOperatorDeclarationSyntax as cast -> cast |> addToMember :> MemberDeclarationSyntax
            | :? ConstructorDeclarationSyntax as ctor -> ctor |> addToMember :> MemberDeclarationSyntax
            | _ -> m
            )
        typeSyntax.WithMembers( members |> toSyntaxList )

let private makeClass idType info = 
    let visibility = visibilityToKeyword idType.Visibility

    let addMember' builder (decl : StructDeclarationSyntax) =
        decl |> addMember (builder info)

    (struct' idType.Name)
        |> addBaseTypes [| Equality.iequatableOf info.GeneratedTypeSyntax |]
        |?> (info.Id.EqualsUnderlying, addBaseTypes [| Equality.iequatableOf info.UnderlyingTypeSyntax |])
        |> addModifiers [|visibility; SyntaxKind.PartialKeyword|]
        |> addMember' makeValueField
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
        |> GeneratedCodeAttribute.addToAllMembers

let private makeInfo idType =
    let namespaceProvided = not (String.IsNullOrEmpty(idType.Namespace))

    let propertyName = FirstChar.toUpper idType.ValueProperty
    let fieldName = FirstChar.toLower idType.ValueProperty

    {
        Id = idType
        AllowNull = idType.AllowNull && idType.UnderlyingType.IsClass
        NamespaceProvided = namespaceProvided
        UnderlyingTypeSyntax = SyntaxFactory.ParseTypeName(idType.UnderlyingType.FullName)
        GeneratedTypeSyntax  = SyntaxFactory.ParseTypeName(idType.Name)
        ThisValueMemberAccess = thisMemberAccess fieldName
        ValueAccess = (fun expr -> expr |> dottedMemberAccess [fieldName])
        PropertyName = propertyName
        FieldName = fieldName
    }


let makeRootNode idType = 
    let info = makeInfo idType
    let generatedClass = makeClass idType info

    let rootMember =
        if String.IsNullOrEmpty(idType.Namespace) then
            generatedClass :> MemberDeclarationSyntax
        else
            SyntaxFactory.NamespaceDeclaration(SyntaxFactory.IdentifierName(idType.Namespace))
                .AddMembers(generatedClass) :> MemberDeclarationSyntax

    emptyFile
        |> addUsings [ "System" ]
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