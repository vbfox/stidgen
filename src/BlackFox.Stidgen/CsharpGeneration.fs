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
        CanBeNull : bool
        AllowNull : bool
        NamespaceProvided : bool
        UnderlyingTypeSyntax : TypeSyntax
        GeneratedTypeSyntax : TypeSyntax
        ThisValueMemberAccess : MemberAccessExpressionSyntax
        ValueAccess : ExpressionSyntax -> ExpressionSyntax
        PropertyName : string
        FieldName : string
        CheckMethodName : string
    }

/// Get the value using the private field from an instance of the Id type
let private value info x = x :> ExpressionSyntax |> info.ValueAccess

let private (|?>) x (c, f) = if c then f x else x
let private (|??>) x (c, f, g) = if c then f x else g x

let private visibilityToKeyword = function
    | Public -> SyntaxKind.PublicKeyword
    | Internal -> SyntaxKind.InternalKeyword

module private ProtobufNet =
    let ifEnabled info arg f = if info.Id.ProtobufnetSerializable then f arg else arg

    let addProtoMember info field =
        ifEnabled info field
            <| addAttribute (makeAttribute (identifier "ProtoMember") [Literal.Int 1])

    let addProtoContract info (generatedType:StructDeclarationSyntax) =
        ifEnabled info generatedType
            <| addAttribute (makeAttribute (identifier "ProtoContract") [])

    let addUsing infos file =
        let enabledForOne = infos |> Seq.exists (fun i -> i.Id.ProtobufnetSerializable)
        if enabledForOne then
            file |> addUsings ["ProtoBuf"]
        else
            file

let private makeValueField info =
    field info.UnderlyingTypeSyntax info.FieldName
        |> addModifiers [|SyntaxKind.PrivateKeyword; SyntaxKind.ReadOnlyKeyword|]
        |> ProtobufNet.addProtoMember info

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

let private makeCheckValuePartial info =
    SyntaxFactory.MethodDeclaration(TypeSyntax.Void, info.CheckMethodName)
    |> addModifiers [|SyntaxKind.PartialKeyword|]
    |> addParameter info.FieldName info.UnderlyingTypeSyntax
    |> withSemicolon

let private makeCtor info =
    let argName = info.FieldName
    let arg = identifier argName
    let checkForNull = throwIfArgumentNull argName
    let assignProperty = setThisMember info.FieldName (internIfNeeded arg info)

    SyntaxFactory.ConstructorDeclaration(info.Id.Name)
    |> addModifiers [|SyntaxKind.PublicKeyword|]
    |> addParameter argName info.UnderlyingTypeSyntax
    |?> (not info.AllowNull && info.CanBeNull, addBodyStatement checkForNull)
    |> addBodyStatement (invocationStatement (thisMemberAccess info.CheckMethodName) [arg])
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
    |?> ((info.CanBeNull && not isString), addBodyStatement returnIfNull)
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
        |?> (info.CanBeNull, addBodyStatement returnIfNull)
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
    let addCast fromType toType cast expressionMaker generatedType =
        let parameterName = "x"
        let makeCast cast' = 
            SyntaxFactory.ConversionOperatorDeclaration(SyntaxFactory.Token(cast'), toType)
                |> addModifiers [ SyntaxKind.PublicKeyword; SyntaxKind.StaticKeyword ]
                |> addParameter parameterName fromType
                |> addBodyStatement (ret (expressionMaker parameterName)) 

        let addCast' cast' = generatedType |> addMember (makeCast cast')

        match cast with
        | None -> generatedType
        | Implicit -> addCast' SyntaxKind.ImplicitKeyword
        | Explicit -> addCast' SyntaxKind.ExplicitKeyword
        
    /// IdType -> UnderlyingType
    let addToUnderlyingType info generatedType = 
        generatedType
        |> addCast info.GeneratedTypeSyntax info.UnderlyingTypeSyntax info.Id.CastToUnderlying
            (fun n -> value info (identifier n))
    
    /// UnderlyingType -> IdType
    let addFromUnderlyingType info generatedType = 
        generatedType
        |> addCast
            info.UnderlyingTypeSyntax
            info.GeneratedTypeSyntax
            info.Id.CastFromUnderlying
            (fun n -> objectCreation info.GeneratedTypeSyntax [|identifier n|])

    /// Get a nullable version of the underlying type if it can't be null.
    /// Or the underlying type itself otherwise.
    let underlyingNullableIfNeeded info =
        if info.CanBeNull then
            info.UnderlyingTypeSyntax
        else
            nullable info.UnderlyingTypeSyntax :> TypeSyntax

    /// IdType? -> UnderlyingType?
    let addToUnderlyingTypeNullable info generatedType =
        let toType = underlyingNullableIfNeeded info
        let maker (argName:string) =
            let argId = identifier argName
            let hasValue = argId |> dottedMemberAccess ["HasValue"]
            let underlyingValue = argId |> dottedMemberAccess ["Value"; info.FieldName]

            let condition =
                if info.CanBeNull then
                    or' (not' hasValue) (equals underlyingValue Literal.Null)
                else
                    parenthesis (not' hasValue)

            cond condition (cast toType Literal.Null) underlyingValue

        generatedType
        |> addCast
            (nullable info.GeneratedTypeSyntax)
            toType
            info.Id.CastToUnderlying
            maker

    /// UnderlyingType? -> IdType?
    let addFromUnderlyingTypeNullable info generatedType = 
        let toType = nullable info.GeneratedTypeSyntax
        let maker (argName:string) =
            let argId = identifier argName
            let hasValue = argId |> dottedMemberAccess ["HasValue"]
            let underlyingValue =
                if info.CanBeNull then argId :> ExpressionSyntax else argId |> dottedMemberAccess ["Value"]
            let newId = objectCreation info.GeneratedTypeSyntax [underlyingValue]
            let condition = if info.CanBeNull then equals argId Literal.Null else parenthesis (not' hasValue)

            cond condition (cast toType Literal.Null) newId

        generatedType
        |> addCast
            (underlyingNullableIfNeeded info)
            toType
            info.Id.CastFromUnderlying
            maker

    /// Add all casts to the type
    let addAll info generatedType =
        generatedType
            |> addToUnderlyingType info
            |> addFromUnderlyingType info
            |> addToUnderlyingTypeNullable info
            |> addFromUnderlyingTypeNullable info

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
        declaration
            |?> (info.CanBeNull, addBodyStatement bodyCheck)
            |> addBodyStatement bodyRet

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
    // We can't provide the full name and rely on the simplifier as it doesn't handle this case (As of roslyn 1.0.0-rc2)
    let private nameSyntax = identifier "GeneratedCode"

    let inline private addToMember typeMember =
        let toolName = Literal.String "BlackFox.Stidgen"
        let toolVersion = Literal.String AssemblyVersionInformation.Version
        let attribute = makeAttribute nameSyntax [toolName; toolVersion]
        typeMember |> addAttribute attribute

    let private isPartial (method' : MethodDeclarationSyntax) =
        method'.Modifiers.Any(SyntaxKind.PartialKeyword)

    /// Add the 'GeneratedCodeAttribute' to all members of the type
    let addToAllMembers (typeSyntax : StructDeclarationSyntax) =
        let members = typeSyntax.Members |> Seq.map (fun m -> 
            match m with
            | :? PropertyDeclarationSyntax as property -> property |> addToMember :> MemberDeclarationSyntax
            | :? FieldDeclarationSyntax as field -> field |> addToMember :> MemberDeclarationSyntax
            | :? MethodDeclarationSyntax as method' ->
                let method' = if isPartial method' then method' else method' |> addToMember
                method' :> MemberDeclarationSyntax
            | :? OperatorDeclarationSyntax as operator -> operator |> addToMember :> MemberDeclarationSyntax
            | :? ConversionOperatorDeclarationSyntax as cast -> cast |> addToMember :> MemberDeclarationSyntax
            | :? ConstructorDeclarationSyntax as ctor -> ctor |> addToMember :> MemberDeclarationSyntax
            | _ -> m
            )
        typeSyntax.WithMembers( members |> toSyntaxList )

let private makeClass info = 
    let visibility = visibilityToKeyword info.Id.Visibility

    let addMember' builder (decl : StructDeclarationSyntax) =
        decl |> addMember (builder info)

    (struct' info.Id.Name)
        |> addBaseTypes [| Equality.iequatableOf info.GeneratedTypeSyntax |]
        |?> (info.Id.EqualsUnderlying, addBaseTypes [| Equality.iequatableOf info.UnderlyingTypeSyntax |])
        |> ProtobufNet.addProtoContract info
        |> addModifiers [|visibility; SyntaxKind.PartialKeyword|]
        |> addMember' makeValueField
        |> addMember' makeValueProperty
        |> addMember' makeCtor
        |> addMember' makeCheckValuePartial
        |> addMember' makeToString
        |> addMember' Equality.makeGetHashCode
        |> addMember' Equality.makeEquals
        |> addMember' Equality.makeEqualsGenerated
        |> addMember' Equality.makeStaticEquals
        |> Equality.addOperators info
        |?> (info.Id.EqualsUnderlying, addMember' Equality.makeEqualsUnderlying)
        |> Casts.addAll info
        |> Convertible.addIConvertibleMembers info
        |> GeneratedCodeAttribute.addToAllMembers

let private makeInfo idType =
    let namespaceProvided = not (String.IsNullOrEmpty(idType.Namespace))

    let propertyName = FirstChar.toUpper idType.ValueProperty
    let fieldName = FirstChar.toLower idType.ValueProperty

    {
        Id = idType
        CanBeNull = idType.UnderlyingType.IsClass || idType.UnderlyingType.IsInterface
        AllowNull = idType.AllowNull
        NamespaceProvided = namespaceProvided
        UnderlyingTypeSyntax = SyntaxFactory.ParseTypeName(idType.UnderlyingType.FullName)
        GeneratedTypeSyntax  = SyntaxFactory.ParseTypeName(idType.Name)
        ThisValueMemberAccess = thisMemberAccess fieldName
        ValueAccess = (fun expr -> expr |> dottedMemberAccess [fieldName])
        PropertyName = propertyName
        FieldName = fieldName
        CheckMethodName = "Check" + propertyName
    }

let private topOfFileComments = @"----------------------
 <auto-generated>
     Generated by stidgen
 </auto-generated>
----------------------"

let private makeFileLevelNodes (idTypes : ParsedInfo list) =
    idTypes
    |> Seq.groupBy (fun i -> i.Id.Namespace)
    |> Seq.sortBy (fun (ns, _) -> ns)
    |> Seq.collect(fun (ns, nsTypes) ->
        let classNodes = nsTypes |> Seq.map(fun info ->
                info |> makeClass :> MemberDeclarationSyntax
            )

        if String.IsNullOrEmpty(ns) then
            classNodes
        else
            let nsNode =
                SyntaxFactory.NamespaceDeclaration(SyntaxFactory.IdentifierName(ns))
                |> addMembers classNodes
                :> MemberDeclarationSyntax

            [nsNode] :> MemberDeclarationSyntax seq
    )

let makeRootNode (idTypes : IdType list) =
    let infos = idTypes |> List.map makeInfo
    let fileLevelNodes = infos |> makeFileLevelNodes

    emptyFile
        |> addUsings [ "System"; "System.CodeDom.Compiler" ]
        |> ProtobufNet.addUsing infos
        |> addMembers fileLevelNodes
        |> addTriviaBefore (makeSingleLineComments topOfFileComments)

module DocumentGeneration =

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

    let makeClean node =
        async {
            return! node
                |> makeDocument
                |> simplifyDocumentAsync
                |!> formatDocumentAsync
        }

let private rootNodeToStringAsync node =
    async {
        let! document = DocumentGeneration.makeClean node
        let! finalNode = !! document.GetSyntaxRootAsync()

        return finalNode.GetText().ToString()
    }

let idTypesToStringAsync idType =
    idType
        |> makeRootNode
        |> rootNodeToStringAsync

let idTypesToString idType =
    idType |> idTypesToStringAsync |> Async.RunSynchronously