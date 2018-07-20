module BlackFox.Stidgen.CsharpGeneration

open System
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
        /// Can the underlying type be null (class or interface)
        CanBeNull : bool
        /// Is the input value allowed to be null (Underlyng can be null and it is allowed)
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

let private typeCanBeNull (t: Type) = t.IsClass || t.IsInterface

let private visibilityToKeyword = function
    | Public -> SyntaxKind.PublicKeyword
    | Internal -> SyntaxKind.InternalKeyword

module private SerializationAttributes =
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

    module private DataContract =
        let ifEnabled info arg f = if info.Id.DataContractSerializable then f arg else arg

        let addDataMember info field =
            ifEnabled info field (fun field ->
                let orderArgument = attributeArgument "Order" (Literal.Int 1)
                let attribute = makeAttribute' (identifier "DataMember") [orderArgument]
                addAttribute attribute field)

        let addDataContract info (generatedType:StructDeclarationSyntax) =
            ifEnabled info generatedType
                <| addAttribute (makeAttribute (identifier "DataContract") [])

        let addUsing infos file =
            let enabledForOne = infos |> Seq.exists (fun i -> i.Id.DataContractSerializable)
            if enabledForOne then
                file |> addUsings ["System.Runtime.Serialization"]
            else
                file

    let addUsing infos file =
        file
        |> ProtobufNet.addUsing infos
        |> DataContract.addUsing infos

    let addToGeneratedType info (generatedType:StructDeclarationSyntax) =
        generatedType
        |> ProtobufNet.addProtoContract info
        |> DataContract.addDataContract info

    let addToField info field =
        field
        |> ProtobufNet.addProtoMember info
        |> DataContract.addDataMember info

let private makeValueField info =
    // Not marked as readonly for the potential perf gain
    // See https://codeblog.jonskeet.uk/2014/07/16/micro-optimization-the-surprising-inefficiency-of-readonly-fields/
    field info.UnderlyingTypeSyntax info.FieldName
        |> addModifiers [| SyntaxKind.PrivateKeyword |]
        |> SerializationAttributes.addToField info

let private makeValueProperty info =
    let body = block [| ret info.ThisValueMemberAccess |]
    SyntaxFactory.PropertyDeclaration(info.UnderlyingTypeSyntax, info.PropertyName)
        |> addModifiers [| SyntaxKind.PublicKeyword |]
        |> addGetter body

/// Is interning enabled (Config + underlying = string)
let inline private internEnabled info =
    info.Id.InternString && (typeof<string> = info.Id.UnderlyingType)

/// Intern an expression of underlying type if needed && possible
let private internIfNeeded arg info =
    if internEnabled info
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
    |> addBodyStatement assignProperty
    |> addBodyStatement (invocationStatement (thisMemberAccess info.CheckMethodName) [info.ThisValueMemberAccess])

let private returnCallVoidMethodOnValue name info =
    ret
        (invocation
            (info.ThisValueMemberAccess |> memberAccess name)
            [||])

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

    let getHashCodeDoc = parseDocumentationComment @"/// <summary>Serves as the default hash function.</summary>
/// <returns>A hash code for the current object.</returns>"

    let objectEqualsDoc = parseDocumentationComment @"/// <summary>
/// Determines whether the specified object is equal to the current object.
/// <param name=""other"">The object to compare with the current object.</param>
/// </summary>
/// <returns>
/// true if the specified object is equal to the current object;
/// otherwise, false.
/// </returns>"

    module private MemberAccess =
        let objectReferenceEquals = dottedMemberAccess' ["object"; "ReferenceEquals"]
        let debugAssert = dottedMemberAccess' ["Debug"; "Assert"]
        let stringIsInterned = dottedMemberAccess' ["string"; "IsInterned"]
        let runtimeHelpersGetHashCode = dottedMemberAccess' ["RuntimeHelpers"; "GetHashCode"]

    let private makeGetHashCode info =
        let returnGetHashCode =
            if internEnabled info then
                // We can use the very fast GetHashCode that uses the reference address
                ret (invocation MemberAccess.runtimeHelpersGetHashCode [|info.ThisValueMemberAccess|])
            else
                // Need to call the real one
                ret (getHashCode info.ThisValueMemberAccess)

        let returnIfNull = info |> makeIfValueNull (fun block ->
            block |> addStatement (ret Literal.Zero)
            )

        SyntaxFactory.MethodDeclaration(TypeSyntax.Int, "GetHashCode")
        |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.OverrideKeyword|]
        |?> (info.CanBeNull && not (internEnabled info), addBodyStatement returnIfNull)
        |> addBodyStatement returnGetHashCode
        |> addTriviaBefore [getHashCodeDoc]

    /// All of the following types have '!=' and '==' implemented with another instance of the same type
    /// but there is no op_Equality or op_Inequality method present as they exists directly at the IL level.
    let predefinedEqualityOperators =
        [
            typeof<SByte>; typeof<Int16>; typeof<Int32>; typeof<Int64>; typeof<Byte>;
            typeof<UInt16>; typeof<UInt32>; typeof<UInt64>; typeof<Single>;
            typeof<Double>; typeof<Boolean>; typeof<Char>
        ]

    let private assertValueIsInterned expr =
        invocation MemberAccess.debugAssert
            [|
                or'
                    (equals expr Literal.Null)
                    (equals
                        (invocation MemberAccess.stringIsInterned [| expr |])
                        Literal.Null)
                Literal.String "Value should always be interned if interning is enabled"
            |]
            |> statement

    let inline private assertValueIsInternedIfNeeded info expr method =
        (|?>) method (internEnabled info, addBodyStatement (assertValueIsInterned expr))

    /// Call the most adapted underlying equals method between underlying-typed expressions.
    let private underlyingEquals info exprA exprB eq enableInterning =
        if enableInterning && internEnabled info then
            // Both should be interned strings, so we can use obj.ReferenceEquals
            let refEquals = invocation MemberAccess.objectReferenceEquals [|exprA; exprB|]
            if eq then
                refEquals :> ExpressionSyntax
            else
                not' refEquals :> ExpressionSyntax
        else
            let isPredefined = predefinedEqualityOperators |> List.exists (fun t -> info.Id.UnderlyingType = t)

            let operatorMethod =
                info.Id.UnderlyingType.GetMethod(
                    (if eq then "op_Equality" else "op_Inequality"),
                    BindingFlags.Static ||| BindingFlags.Public,
                    null,
                    CallingConventions.Any,
                    [|info.Id.UnderlyingType;info.Id.UnderlyingType|],
                    null)

            if isPredefined || (not (isNull operatorMethod)) then
                // Prefer the operator as for CLR implementations it's an obvious optimization for native types and strings
                let op = if eq then equals else notEquals
                op exprA exprB :> ExpressionSyntax
            else
                // Otherwise Object.Equals is a safe choice
                let equalsMethod = objectEquals exprA exprB
                if eq then
                    equalsMethod :> ExpressionSyntax
                else
                    not' equalsMethod :> ExpressionSyntax

    let private thisValueEquals info expr eq enableInterning =
        underlyingEquals info info.ThisValueMemberAccess expr eq enableInterning

    let private makeStaticEquals info =
        let parameterA = identifier "a"
        let parameterB = identifier "b"

        let value = value info

        let body = ret (underlyingEquals info (value parameterA) (value parameterB) true true)

        SyntaxFactory.MethodDeclaration(TypeSyntax.Bool, "Equals")
        |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.StaticKeyword|]
        |> addParameter "a" info.GeneratedTypeSyntax
        |> addParameter "b" info.GeneratedTypeSyntax
        |> assertValueIsInternedIfNeeded info (value parameterA)
        |> assertValueIsInternedIfNeeded info (value parameterB)
        |> addBodyStatement body

    let private makeEquals info =
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
            // Interning optimization can't be enabled (We can receive any string)
            ret (thisValueEquals info (parameter |> cast info.UnderlyingTypeSyntax) true false)

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
                    true
                )

        SyntaxFactory.MethodDeclaration(TypeSyntax.Bool, "Equals")
        |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.OverrideKeyword|]
        |> addParameter parameterName TypeSyntax.Object
        |> assertValueIsInternedIfNeeded info info.ThisValueMemberAccess
        |> addBodyStatement returnFalseForIncorrectType
        |?> (info.Id.EqualsUnderlying, addBodyStatement ifIsUnderlyingReturnEquals)
        |> addBodyStatement returnArgCastToIdValueEqualsValue
        |> addTriviaBefore [objectEqualsDoc]

    let private makeEqualsGenerated info =
        let parameterName = "other"
        let parameter = identifier parameterName :> ExpressionSyntax
        let body = ret (thisValueEquals info (info.ValueAccess parameter) true true)

        SyntaxFactory.MethodDeclaration(TypeSyntax.Bool, "Equals")
        |> addModifiers [|SyntaxKind.PublicKeyword|]
        |> addParameter parameterName info.GeneratedTypeSyntax
        |> assertValueIsInternedIfNeeded info info.ThisValueMemberAccess
        |> assertValueIsInternedIfNeeded info (info.ValueAccess parameter)
        |> addBodyStatement body

    let private makeEqualsUnderlying info =
        let parameterName = "other"
        let parameter = identifier parameterName :> ExpressionSyntax
        let body = ret (thisValueEquals info parameter true false)

        SyntaxFactory.MethodDeclaration(TypeSyntax.Bool, "Equals")
        |> addModifiers [|SyntaxKind.PublicKeyword|]
        |> addParameter parameterName info.UnderlyingTypeSyntax
        |> addBodyStatement body

    let private iEquatable = typedefof<IEquatable<_>>
    let private iEquatableNamespace = NameSyntax.MakeQualified(iEquatable.Namespace.Split('.'))
    let private iequatableOf t =
        SyntaxFactory.QualifiedName(iEquatableNamespace, NameSyntax.MakeGeneric iEquatable.Name [|t|])

    let private makeOperator info eq leftArgType rightArgType leftValueExtractor rightValueExtractor enableInterning =
        let left = identifier "left"
        let right = identifier "right"

        let leftValue = leftValueExtractor info left
        let rightValue = rightValueExtractor info right
        let body = ret (underlyingEquals info leftValue rightValue eq enableInterning)

        let operatorToken = if eq then SyntaxKind.EqualsEqualsToken else SyntaxKind.ExclamationEqualsToken
        SyntaxFactory.OperatorDeclaration(TypeSyntax.Bool, SyntaxFactory.Token(operatorToken))
        |> addModifiers [|SyntaxKind.PublicKeyword;SyntaxKind.StaticKeyword|]
        |> addParameter "left" leftArgType
        |> addParameter "right" rightArgType
        |?> (enableInterning, assertValueIsInternedIfNeeded info leftValue)
        |?> (enableInterning, assertValueIsInternedIfNeeded info rightValue)
        |> addBodyStatement body

    let private addOperators info (classDeclaration:StructDeclarationSyntax) =
        let direct _ expr = expr
        let generated = info.GeneratedTypeSyntax
        let underlying = info.UnderlyingTypeSyntax

        classDeclaration
        |> addMember (makeOperator info true generated generated value value true)
        |> addMember (makeOperator info false generated generated value value true)
        |?> (info.Id.EqualsUnderlying, addMember (makeOperator info true generated underlying value direct false))
        |?> (info.Id.EqualsUnderlying, addMember (makeOperator info false generated underlying value direct false))

    let addEqualityMembers info (decl:StructDeclarationSyntax) =
        decl
        |> addBaseTypes [| iequatableOf info.GeneratedTypeSyntax |]
        |?> (info.Id.EqualsUnderlying, addBaseTypes [| iequatableOf info.UnderlyingTypeSyntax |])
        |> addMember (makeGetHashCode info)
        |> addMember (makeEquals info)
        |> addMember (makeEqualsGenerated info)
        |> addMember (makeStaticEquals info)
        |> addOperators info
        |?> (info.Id.EqualsUnderlying, addMember (makeEqualsUnderlying info))

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

    let isNullable (t:Type) =
        t.IsGenericType
            && t.GetGenericTypeDefinition() = typeof<Nullable<int>>.GetGenericTypeDefinition()

    /// Add all casts to the type
    let addAll info generatedType =
        let underlyingIsNullable = isNullable info.Id.UnderlyingType

        generatedType
            |> addToUnderlyingType info
            |> addFromUnderlyingType info
            |?> (not underlyingIsNullable, addToUnderlyingTypeNullable info)
            |?> (not underlyingIsNullable, addFromUnderlyingTypeNullable info)

/// Helpers to lift an interface implementation from the base type to the generated one
module private InterfaceLift =
    open System.Reflection

    type GetNullCheckType = MethodInfo -> ParsedInfo -> StatementSyntax

    type InterfaceLiftParams =
        {
            Explicit: bool
        }

    type InterfaceLift =
        {
            LiftedType : Type
            GetNullCheck : GetNullCheckType
            Info : ParsedInfo
            Params : InterfaceLiftParams
        }

    let makeLift<'i> getNullCheck params' info =
        {
            LiftedType = typeof<'i>
            GetNullCheck = getNullCheck
            Params = params'
            Info = info
        }

    let inline type' (lift : InterfaceLift) = lift.LiftedType
    let inline name (lift : InterfaceLift) = NameSyntax.FromType(type' lift)

    let private makeMember (m : MethodInfo) lift =
        // Initial declaration
        let returnType = NameSyntax.FromType m.ReturnType
        let declaration = SyntaxFactory.MethodDeclaration(returnType, m.Name)
        let declaration =
            if lift.Params.Explicit then
                declaration.WithExplicitInterfaceSpecifier(SyntaxFactory.ExplicitInterfaceSpecifier(name lift))
            else
                declaration |> addModifiers [SyntaxKind.PublicKeyword]

        // Add parameters
        let parameters = m.GetParameters() |> Array.map (fun p -> (p.Name, NameSyntax.FromType p.ParameterType))
        let declaration = parameters |> Seq.fold (fun decl (name, type') -> decl |> addParameter name type') declaration

        // Body
        let parametersForCall = parameters |> Array.map (fun (name, _) -> identifier name :> ExpressionSyntax)
        let bodyRet =
            ret
                (invocation
                    (lift.Info.ThisValueMemberAccess |> cast (name lift) |> memberAccess m.Name)
                    parametersForCall)

        // Add body
        declaration
            |?> (lift.Info.CanBeNull, addBodyStatement (lift.GetNullCheck m lift.Info))
            |> addBodyStatement bodyRet

    let addAllMethods lift (classDeclaration : StructDeclarationSyntax) =
        (type' lift).GetMethods()
            |> Array.fold (fun decl m -> decl |> addMember (makeMember m lift)) classDeclaration

    let isPossible lift = (type' lift).IsAssignableFrom(lift.Info.Id.UnderlyingType)

    let addLiftedMethods lift (decl : StructDeclarationSyntax) =
        if isPossible lift then
            decl
            |> addAllMethods lift
            |> addBaseTypes [| (name lift) |]
        else
            decl

/// Implement IConvertible by calling System.Convert methods on the underlying type
module private Convertible =
    open System.Reflection
    open InterfaceLift

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
            :> StatementSyntax
        | "ToChar" ->
            // To char always throw for null, so no need to call the static version
            let argName = m.GetParameters().[0].Name
            throwIfArgumentNull argName
            :> StatementSyntax
        | _ ->
            // For other ToXXX methods call the static equivalent on System.Convert
            if'
                (equals info.ThisValueMemberAccess Literal.Null)
                (block
                    [|
                        ret (invocation (dottedMemberAccess' ["System"; "Convert"; m.Name]) [|Literal.Null|])
                    |])
            :> StatementSyntax

    let addIConvertibleMembers info =
        let lift = makeLift<IConvertible> makeNullCheck { Explicit=true } info
        addLiftedMethods lift

module private NonGenericComparable =
    open InterfaceLift

    let docComment = parseDocumentationComment @"/// <summary>
/// Compares the current instance with another object of the same type and
/// returns an integer that indicates whether the current instance precedes,
/// follows, or occurs in the same position in the sort order as the other
/// object.
/// </summary>
/// <param name=""other"">An object to compare with this instance.</param>
/// <returns>
/// A value that indicates the relative order of the objects being compared.
/// The return value has these meanings: Value Meaning Less than zero This
/// instance precedes <paramref name=""other"" /> in the sort order.  Zero This
/// instance occurs in the same position in the sort order as
/// <paramref name=""other"" />. Greater than zero This instance follows
/// <paramref name=""other"" /> in the sort order.
/// </returns>"

    let private makeNullCheck _ (info:ParsedInfo) =
        if'
            (equals info.ThisValueMemberAccess Literal.Null)
            (ifelse
                (equals (identifier "obj") Literal.Null)
                (ret Literal.Zero)
                (ret (Literal.Int -1)) // invert of x.CompareTo(null)=1
            )
        :> StatementSyntax

    let private iComparable = typedefof<IComparable>

    /// Implement IComparable of the underlying on the Id type
    let private addLiftedComparable (info: ParsedInfo) decl =
        info.Id.UnderlyingType.GetInterfaces()
        |> Array.tryFind(fun interf -> interf = iComparable)
        |> Option.map(fun interf ->
            let lift = {
                LiftedType = interf
                GetNullCheck = makeNullCheck
                Params = { Explicit = true }
                Info = info }

            addLiftedMethods lift decl
        )
        |> Option.defaultValue decl

    let addComparable info decl =
        decl
        |?> (info.Id.EqualsUnderlying, addLiftedComparable info)

module private GenericComparable =
    open InterfaceLift
    open System.Reflection

    let private docComment = NonGenericComparable.docComment

    let private makeNullCheck (m: MethodInfo) (info:ParsedInfo) =
        let equatableType = m.DeclaringType.GenericTypeArguments.[0]
        if typeCanBeNull equatableType then
            if'
                (equals info.ThisValueMemberAccess Literal.Null)
                (ifelse
                    (equals (identifier "other") Literal.Null)
                    (ret Literal.Zero)
                    (ret (Literal.Int -1)) // invert of x.CompareTo(null)=1
                )
            :> StatementSyntax
        else
            if'
                (equals info.ThisValueMemberAccess Literal.Null)
                (ret (Literal.Int -1))
            :> StatementSyntax

    let private iComparable = typedefof<IComparable<_>>
    let private iComparableNamespace = NameSyntax.MakeQualified(iComparable.Namespace.Split('.'))
    let private iComparableOf t =
        SyntaxFactory.QualifiedName(iComparableNamespace, NameSyntax.MakeGeneric iComparable.Name [|t|])

    /// Implement all IComparable<'t> of the underlying on the Id type
    let private addLiftedComparable (info: ParsedInfo) decl =
        info.Id.UnderlyingType.GetInterfaces()
        |> Array.filter(fun interf -> interf.IsGenericType && interf.GetGenericTypeDefinition() = iComparable)
        |> Array.fold(fun currentDecl interf ->
            let lift = {
                LiftedType = interf
                GetNullCheck = makeNullCheck
                Params = { Explicit = false }
                Info = info }

            currentDecl |> addLiftedMethods lift
        ) decl

    let private isSelfComparable info =
        let selfEquatableType = iComparable.MakeGenericType(info.Id.UnderlyingType)
        info.Id.UnderlyingType.GetInterfaces() |> Array.exists (fun interf -> interf = selfEquatableType)

    /// Implement IComparable<Id>
    let private addSelfComparable (info: ParsedInfo) decl =
        let comparableUnderlying = iComparable.MakeGenericType(info.Id.UnderlyingType)
        let otherField = identifier "other" |> memberAccess info.FieldName
        let nullCheck =
            if'
                (equals info.ThisValueMemberAccess Literal.Null)
                (ifelse
                    (equals otherField Literal.Null)
                    (ret Literal.Zero)
                    (ret (Literal.Int -1))
                )
            :> StatementSyntax
        let bodyRet =
            ret
                (invocation
                    (info.ThisValueMemberAccess |> cast (NameSyntax.FromType comparableUnderlying) |> memberAccess "CompareTo")
                    [ otherField ])

        let compareToMethod =
            SyntaxFactory.MethodDeclaration(namesyntaxof<int>, "CompareTo")
            |> addModifiers [SyntaxKind.PublicKeyword]
            |> addParameter "other" info.GeneratedTypeSyntax
            |?> (info.CanBeNull, addBodyStatement nullCheck)
            |> addBodyStatement bodyRet
            |> addTriviaBefore [docComment]

        decl
            |> addMember compareToMethod
            |> addBaseTypes [ iComparableOf info.GeneratedTypeSyntax ]

    let addComparable info decl =
        decl
        |?> (info.Id.EqualsUnderlying, addLiftedComparable info)
        |?> (isSelfComparable info, addSelfComparable info)

module private Formattable =
    open System.Globalization
    open InterfaceLift

    let private makeNullCheck _ (info:ParsedInfo) =
        if'
            (equals info.ThisValueMemberAccess Literal.Null)
            (ret Literal.EmptyString)
        :> StatementSyntax

    let addToStringWithFormat lift (decl : StructDeclarationSyntax) =
        let body =
            ret (
                invocation
                    (lift.Info.ThisValueMemberAccess |> memberAccess "ToString")
                    [
                        identifier "format"
                        (namesyntaxof<CultureInfo> |> memberAccess "CurrentCulture")
                    ]
                )

        let method' =
            SyntaxFactory.MethodDeclaration(NameSyntax.String, "ToString")
            |> addModifiers [SyntaxKind.PublicKeyword]
            |> addParameter "format" NameSyntax.String
            |?> (lift.Info.CanBeNull, addBodyStatement (makeNullCheck () lift.Info))
            |> addBodyStatement body

        decl |> addMember method'

    let addFormattable info decl =
        let lift = makeLift<IFormattable> makeNullCheck { Explicit=false } info
        decl
        |> addLiftedMethods lift
        |?> (isPossible lift, addToStringWithFormat lift)

/// Transform all 'Parse' and 'TryParse' static methods from the underlying type to the ID type.
/// Also generate a 'TryParse' returning a Nullable<T>
module private ParseMethods =
    open System.Reflection
    open FromReflection

    let private isParseMethod (result:Type) (m:MethodInfo) =
        let parameters = m.GetParameters()
        m.Name = "Parse"
            && m.IsStatic
            && parameters.Length >= 1
            && parameters.[0].ParameterType = typeof<string>
            && m.ReturnType = result

    let makeIdType x info = objectCreation info.GeneratedTypeSyntax [x]

    let private makeParseMethod (info:ParsedInfo) (parseMethod:MethodInfo) =
        let parse = callStaticMethod' parseMethod (getArgumentsForCall parseMethod)
        let body = ret (makeIdType parse info)

        SyntaxFactory.MethodDeclaration(info.GeneratedTypeSyntax, "Parse")
        |> addModifiers [SyntaxKind.PublicKeyword; SyntaxKind.StaticKeyword]
        |> addParameters' (getParametersForDeclaration parseMethod)
        |> withBody [body]

    let private isTryParseMethod (result:Type) (m:MethodInfo) =
        let parameters = m.GetParameters()
        let lastParamId = parameters.Length - 1
        m.Name = "TryParse"
            && m.IsStatic
            && parameters.Length >= 2
            && parameters.[0].ParameterType = typeof<string>
            && parameters.[lastParamId].ParameterType.HasElementType // &result
            && parameters.[lastParamId].ParameterType.GetElementType() = result
            && parameters.[lastParamId].IsOut
            && m.ReturnType = typeof<bool>

    let private callTryParse (m:MethodInfo) identifierName =
        let rawArgs = getArgumentsForCall m |> List.ofSeq |> List.rev
        let outArg = rawArgs.Head.WithExpression(identifier identifierName)
        let finalArgs = (outArg :: rawArgs.Tail) |> List.rev
        callStaticMethod' m finalArgs

    let private makeNullableTryParseMethod (info:ParsedInfo) (tryParseMethod:MethodInfo) =
        let resultType = nullable info.GeneratedTypeSyntax
        let body : StatementSyntax list =
            [
                variable info.UnderlyingTypeSyntax "parsed"
                initializedVariable TypeSyntax.Bool "isValid" (callTryParse tryParseMethod "parsed");
                ret (
                    cond
                        (identifier "isValid")
                        (makeIdType (identifier "parsed") info)
                        (cast resultType Literal.Null)
                )
            ]

        let parameterCount = tryParseMethod.GetParameters().Length - 1
        let parameters = getParametersForDeclaration tryParseMethod |> Seq.take(parameterCount)

        SyntaxFactory.MethodDeclaration(resultType, "TryParse")
        |> addModifiers [SyntaxKind.PublicKeyword; SyntaxKind.StaticKeyword]
        |> addParameters' parameters
        |> withBody body

    let private makeStandardTryParseMethod (info:ParsedInfo) (tryParseMethod:MethodInfo) =
        let resultArgName = (tryParseMethod.GetParameters() |> Seq.last).Name
        let body : StatementSyntax list =
            [
                variable info.UnderlyingTypeSyntax "parsed"
                initializedVariable TypeSyntax.Bool "isValid" (callTryParse tryParseMethod "parsed")
                set (identifier resultArgName) (
                    cond
                        (identifier "isValid")
                        (makeIdType (identifier "parsed") info)
                        (default' info.GeneratedTypeSyntax)
                )
                ret (identifier "isValid")
            ]

        let parameters = getParametersForDeclaration tryParseMethod |> List.ofSeq |> List.rev
        let outArg = parameters.Head.WithType(info.GeneratedTypeSyntax)
        let parameters = (outArg :: parameters.Tail) |> List.rev

        SyntaxFactory.MethodDeclaration(typesyntaxof<bool>, "TryParse")
        |> addModifiers [SyntaxKind.PublicKeyword; SyntaxKind.StaticKeyword]
        |> addParameters' parameters
        |> withBody body

    let private getSomeMethods filter (info:ParsedInfo) =
        info.Id.UnderlyingType.GetMethods() |> Seq.filter (filter info.Id.UnderlyingType)

    let addParseMethods info (decl : StructDeclarationSyntax) =
        let make filter makeMethod = getSomeMethods filter info |> Seq.map(makeMethod info)
        let members =
             [
                make isParseMethod makeParseMethod
                make isTryParseMethod makeStandardTryParseMethod
                make isTryParseMethod makeNullableTryParseMethod
             ]

        let members = members |> Seq.collect id |> Seq.map (fun m -> m :> MemberDeclarationSyntax)

        decl |> addMembers members

module private DebuggerDisplayAttribute =
    let private nameSyntax = identifier "DebuggerDisplay"

    let private mkAttribute info =
        let text = sprintf "{%s,nq}" info.FieldName
        makeAttribute nameSyntax [Literal.String text]

    let add info (decl : StructDeclarationSyntax) =
        let attr = mkAttribute info
        decl |> addAttribute attr

/// Add the [GeneratedCodeAttribute] for each generated member
module GeneratedCodeAttribute =
    // We can't provide the full name and rely on the simplifier as it doesn't handle this case (As of roslyn 1.0.0-rc2)
    let private nameSyntax = identifier "GeneratedCode"

    let private toolName = Literal.String "BlackFox.Stidgen"
    let private toolVersion = Literal.String AssemblyVersionInformation.AssemblyVersion
    let private attribute = makeAttribute nameSyntax [toolName; toolVersion]

    let inline private addToMember (typeMember: #SyntaxNode) =
        let leadingTrivia = typeMember.GetLeadingTrivia()
        typeMember.WithoutLeadingTrivia()
        |> addAttribute attribute
        |> addTriviaBefore leadingTrivia

    let private isPartial (method' : MethodDeclarationSyntax) =
        method'.Modifiers.Any(SyntaxKind.PartialKeyword)

    let private addToMember' (m : MemberDeclarationSyntax) =
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

    /// Add the 'GeneratedCodeAttribute' to all members of the type
    let addToAllMembers (typeSyntax : StructDeclarationSyntax) =
        let members = typeSyntax.Members |> Seq.map addToMember'
        typeSyntax.WithMembers( members |> toSyntaxList )

let private makeClass info =
    let visibility = visibilityToKeyword info.Id.Visibility

    let addMember' builder (decl : StructDeclarationSyntax) =
        decl |> addMember (builder info)

    (struct' info.Id.Name)
        |> SerializationAttributes.addToGeneratedType info
        |> addModifiers [|visibility; SyntaxKind.PartialKeyword|]
        |> addMember' makeValueField
        |> addMember' makeValueProperty
        |> addMember' makeCtor
        |> addMember' makeCheckValuePartial
        |> addMember' makeToString
        |> Equality.addEqualityMembers info
        |> GenericComparable.addComparable info
        |> NonGenericComparable.addComparable info
        |> Casts.addAll info
        |> ParseMethods.addParseMethods info
        |> Convertible.addIConvertibleMembers info
        |> Formattable.addFormattable info
        |> GeneratedCodeAttribute.addToAllMembers
        |> DebuggerDisplayAttribute.add info

let private makeInfo idType =
    let namespaceProvided = not (String.IsNullOrEmpty(idType.Namespace))

    let propertyName = FirstChar.toUpper idType.ValueProperty
    let fieldName = FirstChar.toLower idType.ValueProperty

    {
        Id = idType
        CanBeNull = typeCanBeNull idType.UnderlyingType
        AllowNull = idType.AllowNull
        NamespaceProvided = namespaceProvided
        UnderlyingTypeSyntax = NameSyntax.FromType(idType.UnderlyingType)
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

let private addNamespaceTypes (ns, nsTypes) =
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

let private makeFileLevelNodes (idTypes : ParsedInfo list) =
    idTypes
    |> Seq.groupBy (fun i -> i.Id.Namespace)
    |> Seq.sortBy (fun (ns, _) -> ns)
    |> Seq.collect addNamespaceTypes

let makeRootNode (idTypes : IdType seq) =
    let infos = idTypes |> List.ofSeq |> List.map makeInfo
    let fileLevelNodes = infos |> makeFileLevelNodes

    emptyFile
        |> addUsings [
            "System"
            "System.CodeDom.Compiler" (*GeneratedCodeAttribute*)
            "System.Diagnostics" (* DebuggerDisplayAttribute *)
            "System.Runtime.CompilerServices" (* RuntimeHelpers *)
        ]
        |> SerializationAttributes.addUsing infos
        |> addMembers fileLevelNodes
        |> addTriviaBefore (makeSingleLineComments topOfFileComments)

module DocumentGeneration =
    open System.Reflection
    open Microsoft.CodeAnalysis.Host.Mef

    let private createWorkspace () =
        // Our own assembly is added to the list as when IL-merged it's where all the
        // roslyn classes will be.
        let assemblies =
            MefHostServices.DefaultAssemblies
            |> Seq.append [ Assembly.GetExecutingAssembly() ]

        let hostServices = MefHostServices.Create(assemblies)
        new AdhocWorkspace(hostServices)

    let private makeDocument (assemblies: Assembly seq) (rootNode:SyntaxNode) =
        let workspace = createWorkspace ()
        let project = workspace.AddProject("MyProject", LanguageNames.CSharp)

        let project =
            assemblies
            |> Seq.toList
            |> List.append [ typedefof<obj>.Assembly ]
            |> List.map (fun a -> a.Location)
            |> List.distinct
            |> List.map (fun f -> PortableExecutableReference.CreateFromFile(f))
            |> List.fold (fun (p:Project) ref -> p.AddMetadataReference(ref)) project

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

    let makeClean assemblies node =
        async {
            return! node
                |> makeDocument assemblies
                |> simplifyDocumentAsync
                |!> formatDocumentAsync
        }

let private rootNodeToStringAsync assemblies node =
    async {
        let! document = DocumentGeneration.makeClean assemblies node
        let! finalNode = !! document.GetSyntaxRootAsync()

        return finalNode.GetText().ToString()
    }

/// Transform a list of ID Types that should go into a file into the text
/// content of this file
let idTypesToStringAsync idTypes =
    let assemblies = idTypes |> Seq.map (fun t -> t.UnderlyingType.Assembly)
    idTypes
        |> makeRootNode
        |> rootNodeToStringAsync assemblies

/// Transform a list of ID Types that should go into a file into the text
/// content of this file
let idTypesToString idTypes =
    idTypes
        |> idTypesToStringAsync
        |> Async.RunSynchronously
