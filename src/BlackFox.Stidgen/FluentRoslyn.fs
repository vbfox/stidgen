﻿module BlackFox.Stidgen.FluentRoslyn

open System.Linq
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

module Operators =
    /// A version of the pipe operators for async workflows
    let (|!>) a f = async.Bind(a, f)

    /// Unary operator to convert C# Task<'t> to F# Async<'t>
    let (!!) t = Async.AwaitTask t

type TypeSyntax with
    /// void
    static member Void = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.VoidKeyword))
    /// object
    static member Object = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.ObjectKeyword))
    /// string
    static member String = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.StringKeyword))
    /// int
    static member Int = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.IntKeyword))
    /// bool
    static member Bool = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.BoolKeyword))

module Literal =
    let inline private literalKeyword x = SyntaxFactory.LiteralExpression(x)
    let inline private ofType t x = SyntaxFactory.LiteralExpression(t, x)
    /// null
    let Null = literalKeyword SyntaxKind.NullLiteralExpression
    /// true
    let True = literalKeyword SyntaxKind.TrueLiteralExpression
    /// false
    let False = literalKeyword SyntaxKind.FalseLiteralExpression
    /// true or false
    let Bool (b:bool) = if b then True else False
    /// "s"
    let inline String (s:string) = SyntaxFactory.Literal(s) |> ofType SyntaxKind.StringLiteralExpression
    /// ""
    let EmptyString = String ""
    /// i
    let inline Int (i:int) = SyntaxFactory.Literal(i) |> ofType SyntaxKind.NumericLiteralExpression
    /// 0
    let Zero = Int 0

let toSyntaxList (source : 't seq) = SyntaxFactory.List<'t>(source)
let toSeparatedList (source : 't seq) = SyntaxFactory.SeparatedList<'t>(source)
let identifier (identifierName : string) = SyntaxFactory.IdentifierName(identifierName)

/// T?
let nullable typeSyntax = SyntaxFactory.NullableType(typeSyntax)

/// T?
let nullable' identifierName = nullable (identifier identifierName)

let addUsings (usings : string seq) (compilationUnit : CompilationUnitSyntax) =
    let directives =
        usings
        |> Seq.map (fun name -> SyntaxFactory.UsingDirective(identifier name))
        |> Seq.toArray

    compilationUnit.AddUsings(directives)

type StructDeclarationSyntax with
    member this.addModifiers(syntaxKinds) =
        let tokens = syntaxKinds |> Seq.map SyntaxFactory.Token |> Seq.toArray
        this.AddModifiers(tokens)

    member this.addAttributeList(attributes:AttributeSyntax seq) =
        let attributeList = SyntaxFactory.AttributeList(attributes |> toSeparatedList)
        this.AddAttributeLists([|attributeList|])

    member this.addAttribute(attribute) =
        this.addAttributeList [attribute]

    member this.addTriviaBefore(trivia) =
        let newTrivia = Seq.concat [trivia; this.GetLeadingTrivia() :> SyntaxTrivia seq] |> Seq.toArray
        this.WithLeadingTrivia(newTrivia)

    member this.addMembers members =
        this.AddMembers(members |> Seq.toArray)

    member this.addMember member'=
        this.AddMembers([|member'|])

    member this.addBaseTypes (types : TypeSyntax seq) =
        let baseTypes = types |> Seq.map (fun t -> SyntaxFactory.SimpleBaseType(t) :> BaseTypeSyntax) |> Seq.toArray
        this.AddBaseListTypes(baseTypes)

type ClassDeclarationSyntax with
    member this.addModifiers(syntaxKinds) =
        let tokens = syntaxKinds |> Seq.map SyntaxFactory.Token |> Seq.toArray
        this.AddModifiers(tokens)

    member this.addAttributeList(attributes:AttributeSyntax seq) =
        let attributeList = SyntaxFactory.AttributeList(attributes |> toSeparatedList)
        this.AddAttributeLists([|attributeList|])

    member this.addAttribute(attribute) =
        this.addAttributeList [attribute]

    member this.addTriviaBefore(trivia) =
        let newTrivia = Seq.concat [trivia; this.GetLeadingTrivia() :> SyntaxTrivia seq] |> Seq.toArray
        this.WithLeadingTrivia(newTrivia)

    member this.addMembers members =
        this.AddMembers(members |> Seq.toArray)

    member this.addMember member'=
        this.AddMembers([|member'|])

    member this.addBaseTypes (types : TypeSyntax seq) =
        let baseTypes = types |> Seq.map (fun t -> SyntaxFactory.SimpleBaseType(t) :> BaseTypeSyntax) |> Seq.toArray
        this.AddBaseListTypes(baseTypes)

type FieldDeclarationSyntax with
    member this.addModifiers(syntaxKinds) =
        let tokens = syntaxKinds |> Seq.map SyntaxFactory.Token |> Seq.toArray
        this.AddModifiers(tokens)

    member this.addAttributeList(attributes:AttributeSyntax seq) =
        let attributeList = SyntaxFactory.AttributeList(attributes |> toSeparatedList)
        this.AddAttributeLists([|attributeList|])

    member this.addAttribute(attribute) =
        this.addAttributeList [attribute]

    member this.addTriviaBefore(trivia) =
        let newTrivia = Seq.concat [trivia; this.GetLeadingTrivia() :> SyntaxTrivia seq] |> Seq.toArray
        this.WithLeadingTrivia(newTrivia)

type PropertyDeclarationSyntax with
    member this.addModifiers(syntaxKinds) =
        let tokens = syntaxKinds |> Seq.map SyntaxFactory.Token |> Seq.toArray
        this.AddModifiers(tokens)

    member this.addAttributeList(attributes:AttributeSyntax seq) =
        let attributeList = SyntaxFactory.AttributeList(attributes |> toSeparatedList)
        this.AddAttributeLists([|attributeList|])

    member this.addAttribute(attribute) =
        this.addAttributeList [attribute]

    member this.addTriviaBefore(trivia) =
        let newTrivia = Seq.concat [trivia; this.GetLeadingTrivia() :> SyntaxTrivia seq] |> Seq.toArray
        this.WithLeadingTrivia(newTrivia)

type ParameterSyntax with
    member this.addModifiers(syntaxKinds) =
        let tokens = syntaxKinds |> Seq.map SyntaxFactory.Token |> Seq.toArray
        this.AddModifiers(tokens)

    member this.addAttributeList(attributes:AttributeSyntax seq) =
        let attributeList = SyntaxFactory.AttributeList(attributes |> toSeparatedList)
        this.AddAttributeLists([|attributeList|])

    member this.addAttribute(attribute) =
        this.addAttributeList [attribute]

    member this.addTriviaBefore(trivia) =
        let newTrivia = Seq.concat [trivia; this.GetLeadingTrivia() :> SyntaxTrivia seq] |> Seq.toArray
        this.WithLeadingTrivia(newTrivia)

type MethodDeclarationSyntax with
    member this.addModifiers(syntaxKinds) =
        let tokens = syntaxKinds |> Seq.map SyntaxFactory.Token |> Seq.toArray
        this.AddModifiers(tokens)

    member this.addAttributeList(attributes:AttributeSyntax seq) =
        let attributeList = SyntaxFactory.AttributeList(attributes |> toSeparatedList)
        this.AddAttributeLists([|attributeList|])

    member this.addAttribute(attribute) =
        this.addAttributeList [attribute]

    member this.addTriviaBefore(trivia) =
        let newTrivia = Seq.concat [trivia; this.GetLeadingTrivia() :> SyntaxTrivia seq] |> Seq.toArray
        this.WithLeadingTrivia(newTrivia)

    member this.withSemicolon =
        let token = SyntaxFactory.Token(SyntaxKind.SemicolonToken)
        this.WithSemicolonToken(token)

    member this.addParameters' parameters =
        this.AddParameterListParameters(parameters |> Seq.toArray)

    member this.addParameter' name parameterType modifiers =
        let parameter =
            SyntaxFactory.Parameter(SyntaxFactory.Identifier(name))
                .WithType(parameterType)
                .addModifiers(modifiers)

        this.addParameters' [parameter]

    member this.addParameter name parameterType = this.addParameter' name parameterType []
    member this.addOutParameter name parameterType = this.addParameter' name parameterType [SyntaxKind.OutKeyword]
    member this.addRefParameter name parameterType = this.addParameter' name parameterType [SyntaxKind.RefKeyword]

    member this.addBodyStatement statement =
        this.AddBodyStatements([|statement|])

    member this.withBody (statements: StatementSyntax seq) =
        let block = SyntaxFactory.Block(SyntaxFactory.List<StatementSyntax>(statements))
        this.WithBody(block)

type OperatorDeclarationSyntax with
    member this.addModifiers(syntaxKinds) =
        let tokens = syntaxKinds |> Seq.map SyntaxFactory.Token |> Seq.toArray
        this.AddModifiers(tokens)

    member this.addAttributeList(attributes:AttributeSyntax seq) =
        let attributeList = SyntaxFactory.AttributeList(attributes |> toSeparatedList)
        this.AddAttributeLists([|attributeList|])

    member this.addAttribute(attribute) =
        this.addAttributeList [attribute]

    member this.addTriviaBefore(trivia) =
        let newTrivia = Seq.concat [trivia; this.GetLeadingTrivia() :> SyntaxTrivia seq] |> Seq.toArray
        this.WithLeadingTrivia(newTrivia)

    member this.withSemicolon =
        let token = SyntaxFactory.Token(SyntaxKind.SemicolonToken)
        this.WithSemicolonToken(token)

    member this.addParameters' parameters =
        this.AddParameterListParameters(parameters |> Seq.toArray)

    member this.addParameter' name parameterType modifiers =
        let parameter =
            SyntaxFactory.Parameter(SyntaxFactory.Identifier(name))
                .WithType(parameterType)
                .addModifiers(modifiers)

        this.addParameters' [parameter]

    member this.addParameter name parameterType = this.addParameter' name parameterType []
    member this.addOutParameter name parameterType = this.addParameter' name parameterType [SyntaxKind.OutKeyword]
    member this.addRefParameter name parameterType = this.addParameter' name parameterType [SyntaxKind.RefKeyword]

    member this.addBodyStatement statement =
        this.AddBodyStatements([|statement|])

    member this.withBody (statements: StatementSyntax seq) =
        let block = SyntaxFactory.Block(SyntaxFactory.List<StatementSyntax>(statements))
        this.WithBody(block)

type ConversionOperatorDeclarationSyntax with
    member this.addModifiers(syntaxKinds) =
        let tokens = syntaxKinds |> Seq.map SyntaxFactory.Token |> Seq.toArray
        this.AddModifiers(tokens)

    member this.addAttributeList(attributes:AttributeSyntax seq) =
        let attributeList = SyntaxFactory.AttributeList(attributes |> toSeparatedList)
        this.AddAttributeLists([|attributeList|])

    member this.addAttribute(attribute) =
        this.addAttributeList [attribute]

    member this.addTriviaBefore(trivia) =
        let newTrivia = Seq.concat [trivia; this.GetLeadingTrivia() :> SyntaxTrivia seq] |> Seq.toArray
        this.WithLeadingTrivia(newTrivia)

    member this.withSemicolon =
        let token = SyntaxFactory.Token(SyntaxKind.SemicolonToken)
        this.WithSemicolonToken(token)

    member this.addParameters' parameters =
        this.AddParameterListParameters(parameters |> Seq.toArray)

    member this.addParameter' name parameterType modifiers =
        let parameter =
            SyntaxFactory.Parameter(SyntaxFactory.Identifier(name))
                .WithType(parameterType)
                .addModifiers(modifiers)

        this.addParameters' [parameter]

    member this.addParameter name parameterType = this.addParameter' name parameterType []
    member this.addOutParameter name parameterType = this.addParameter' name parameterType [SyntaxKind.OutKeyword]
    member this.addRefParameter name parameterType = this.addParameter' name parameterType [SyntaxKind.RefKeyword]

    member this.addBodyStatement statement =
        this.AddBodyStatements([|statement|])

    member this.withBody (statements: StatementSyntax seq) =
        let block = SyntaxFactory.Block(SyntaxFactory.List<StatementSyntax>(statements))
        this.WithBody(block)

type ConstructorDeclarationSyntax with
    member this.addModifiers(syntaxKinds) =
        let tokens = syntaxKinds |> Seq.map SyntaxFactory.Token |> Seq.toArray
        this.AddModifiers(tokens)

    member this.addAttributeList(attributes:AttributeSyntax seq) =
        let attributeList = SyntaxFactory.AttributeList(attributes |> toSeparatedList)
        this.AddAttributeLists([|attributeList|])

    member this.addAttribute(attribute) =
        this.addAttributeList [attribute]

    member this.addTriviaBefore(trivia) =
        let newTrivia = Seq.concat [trivia; this.GetLeadingTrivia() :> SyntaxTrivia seq] |> Seq.toArray
        this.WithLeadingTrivia(newTrivia)

    member this.withSemicolon() =
        let token = SyntaxFactory.Token(SyntaxKind.SemicolonToken)
        this.WithSemicolonToken(token)

    member this.addParameters'(parameters) =
        this.AddParameterListParameters(parameters |> Seq.toArray)

    member this.addParameter'(name, parameterType, modifiers) =
        let parameter =
            SyntaxFactory.Parameter(SyntaxFactory.Identifier(name))
                .WithType(parameterType)
                .addModifiers(modifiers)

        this.addParameters' [parameter]

    member this.addParameter(name, parameterType) = this.addParameter'(name, parameterType, [])
    member this.addOutParameter(name, parameterType) = this.addParameter'(name, parameterType, [SyntaxKind.OutKeyword])
    member this.addRefParameter(name, parameterType) = this.addParameter'(name, parameterType, [SyntaxKind.RefKeyword])

    member this.addBodyStatement statement =
        this.AddBodyStatements([|statement|])

    member this.withBody (statements: StatementSyntax seq) =
        let block = SyntaxFactory.Block(SyntaxFactory.List<StatementSyntax>(statements))
        this.WithBody(block)

let inline addStatement statement (input:^T) =
    (^T : (member AddStatements : StatementSyntax array -> ^T) (input, [|statement|]))

let makeAttribute' name (args:AttributeArgumentSyntax list) = 
    // Special casing the empty list as null generate an attribute without empty parenthesis after it
    let finalArgs = if args.IsEmpty then null else SyntaxFactory.AttributeArgumentList(args |> toSeparatedList)
    SyntaxFactory.Attribute(name, finalArgs)

let attributeArgument (name : string) value =
    SyntaxFactory.AttributeArgument(value).WithNameEquals(SyntaxFactory.NameEquals(name))

let makeAttribute name args =
    let mappedArgs = args |> List.map (fun a -> SyntaxFactory.AttributeArgument(a))
    makeAttribute' name mappedArgs

let makeSingleLineComments (s:string) = 
    let lines = System.Text.RegularExpressions.Regex.Split(s, "\r\n|\r|\n")
    lines |> Array.map (fun l -> SyntaxFactory.Comment(sprintf "//%s\r\n" l))
    
let addTriviaBefore (trivia : SyntaxTrivia seq) (node : #SyntaxNode) =
    let newTrivia = Seq.concat [trivia; node.GetLeadingTrivia() :> SyntaxTrivia seq] |> Seq.toArray
    node.WithLeadingTrivia(newTrivia)

/// get { body }
let addGetter body (property:PropertyDeclarationSyntax) =
    property.AddAccessorListAccessors(SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration, body))

/// set { body }
let addSetter body (property:PropertyDeclarationSyntax) =
    property.AddAccessorListAccessors(SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration, body))

/// onExpr.name
let memberAccess (name : string) (onExpr : ExpressionSyntax) =
    SyntaxFactory.MemberAccessExpression(
        SyntaxKind.SimpleMemberAccessExpression,
        onExpr,
        (identifier name)
    )

/// [|a;b;c|] expr -> expr.a.b.c
let dottedMemberAccess (identifiers:string list) (expr: ExpressionSyntax) =
    let rec memberAccessRec (remaining:string list) = 
        match remaining with
        | [] -> expr
        | one :: rest -> memberAccessRec rest |> memberAccess one :> ExpressionSyntax

    memberAccessRec (List.rev identifiers)

/// [|a;b;c|] -> a.b.c
let dottedMemberAccess' identifiers =
    match identifiers with
    | [] -> failwith "No identifiers provided"
    | first::rest -> dottedMemberAccess rest (identifier first)

/// id.member
let simpleMemberAccess (id:string) (``member``:string) =
    (identifier id) |> memberAccess  ``member``

/// this
let this = SyntaxFactory.ThisExpression()

/// this.member
let thisMemberAccess (memberName:string) = this |> memberAccess memberName

/// left = right;
let set left right = 
    SyntaxFactory.ExpressionStatement(
        SyntaxFactory.AssignmentExpression(
            SyntaxKind.SimpleAssignmentExpression,
            left,
            right
        )
    )

/// this.memberName = value;
let setThisMember (memberName:string) value = set (thisMemberAccess memberName) value

/// new createdType(argumentExpressions)
let objectCreation createdType argumentExpressions =
    let args = argumentExpressions |> Seq.map (fun a -> SyntaxFactory.Argument(a))
    let argList = SyntaxFactory.ArgumentList(args |> toSeparatedList)

    SyntaxFactory.ObjectCreationExpression(createdType)
        .WithArgumentList(argList)

let statement (expr:#ExpressionSyntax) = SyntaxFactory.ExpressionStatement(expr)

let arg x = SyntaxFactory.Argument(x)
let outArg x = SyntaxFactory.Argument(null, SyntaxFactory.Token(SyntaxKind.OutKeyword), x)
let refArg x = SyntaxFactory.Argument(null, SyntaxFactory.Token(SyntaxKind.RefKeyword), x)

/// expression(args)
let invocation' (expression : ExpressionSyntax) (args : ArgumentSyntax seq) =
    let argList = SyntaxFactory.ArgumentList(args |> toSeparatedList)
    SyntaxFactory.InvocationExpression(expression).WithArgumentList(argList)

/// expression(argumentExpressions)
let invocation (expression : ExpressionSyntax) (argumentExpressions : ExpressionSyntax seq) =
    let args = argumentExpressions |> Seq.map (fun a -> SyntaxFactory.Argument(a))
    invocation' expression args

/// expression(argumentExpressions);
let invocationStatement (expression : ExpressionSyntax) (argumentExpressions : ExpressionSyntax seq) =
    invocation expression argumentExpressions |> statement

let private variable' ``type`` (name:string) (value: ExpressionSyntax option) =
    let declarator = SyntaxFactory.VariableDeclarator(name)
    let declarator =
        match value with
        | Some(value) -> declarator.WithInitializer(SyntaxFactory.EqualsValueClause(value))
        | None -> declarator
    let declarators = SyntaxFactory.SingletonSeparatedList<VariableDeclaratorSyntax>(declarator)
    SyntaxFactory.VariableDeclaration(``type``, declarators)

/// Type name = value;
let initializedVariable ``type`` name value =
    SyntaxFactory.LocalDeclarationStatement(variable' ``type`` name (Some(value)))

/// Type name;
let variable ``type`` name =
    SyntaxFactory.LocalDeclarationStatement(variable' ``type`` name None)

/// Type name = value;
let field ``type`` name = SyntaxFactory.FieldDeclaration(variable' ``type`` name None)

/// Type name = value;
let initializedField ``type`` name value =
    SyntaxFactory.FieldDeclaration(variable' ``type`` name (Some(value)))

/// return expression;
let ret expression = SyntaxFactory.ReturnStatement(expression)

/// (expression)
let parenthesis expression = SyntaxFactory.ParenthesizedExpression(expression)

/// ((toType) expression)
let cast toType expression = parenthesis (SyntaxFactory.CastExpression(toType, expression))

/// (expression is checkedType)
let is checkedType expression = parenthesis(SyntaxFactory.BinaryExpression(SyntaxKind.IsExpression, expression, checkedType))

/// (left == right)
let equals left right = parenthesis (SyntaxFactory.BinaryExpression(SyntaxKind.EqualsExpression, left, right))

/// (left != right)
let notEquals left right = parenthesis (SyntaxFactory.BinaryExpression(SyntaxKind.NotEqualsExpression, left, right))

/// (left || right)
let or' left right = parenthesis (SyntaxFactory.BinaryExpression(SyntaxKind.LogicalOrExpression, left, right))

/// (left && right)
let and' left right = parenthesis (SyntaxFactory.BinaryExpression(SyntaxKind.LogicalAndExpression, left, right))

/// (cond ? whenTrue : whenFalse)
let cond condition whenTrue whenFalse = parenthesis (SyntaxFactory.ConditionalExpression(condition, whenTrue, whenFalse))

/// !(expression)
let not' expression = SyntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, parenthesis expression)

/// if (condition) then then'
let if' condition then' = SyntaxFactory.IfStatement(condition, then')

/// if (condition) then then' else else'
let ifelse condition then' else' = SyntaxFactory.IfStatement(condition, then', SyntaxFactory.ElseClause(else'))

/// {}
let emptyBlock = SyntaxFactory.Block()

/// { statements }
let block (statements : StatementSyntax seq) = SyntaxFactory.Block(statements)

/// throw expression;
let throw expression = SyntaxFactory.ThrowStatement(expression)

/// throw exceptionType(args);
let throwException exceptionType args =
    throw (objectCreation exceptionType args)

let default' typeSyntax = SyntaxFactory.DefaultExpression(typeSyntax)

/// An empty file ("compilation unit")
let emptyFile = SyntaxFactory.CompilationUnit()

let class' (name:string) = SyntaxFactory.ClassDeclaration(name)
let struct' (name:string) = SyntaxFactory.StructDeclaration(name)

module WellKnownMethods =
    /// System.Object.Equals(objA, objB)
    let objectEquals objA objB =
        let method' = TypeSyntax.Object |> dottedMemberAccess ["Equals"]
        invocation method' [| objA; objB |]

    /// x.ToString()
    let toString x = invocation (memberAccess "ToString" x) [||]

    /// x.GetHashCode()
    let getHashCode x = invocation (memberAccess "GetHashCode" x) [||]

    /// System.String.Intern(s)
    let stringIntern s =
        let method' = TypeSyntax.String |> dottedMemberAccess ["Intern"]
        invocation method' [| s |]

type NameSyntax with
    static member private Global = SyntaxFactory.IdentifierName(SyntaxFactory.Token(SyntaxKind.GlobalKeyword))
    static member private PrefixWithGlobal name = SyntaxFactory.AliasQualifiedName(NameSyntax.Global, name)

    static member MakeQualified (parts : string seq) =
        parts |> Seq.fold
            (fun a b ->
                if isNull a then
                    NameSyntax.PrefixWithGlobal (SyntaxFactory.IdentifierName(b)) :> NameSyntax
                else
                    SyntaxFactory.QualifiedName(a, SyntaxFactory.IdentifierName(b)) :> NameSyntax
            )
            null

    static member MakeGeneric (name : string) (types : TypeSyntax seq) =
        let indexOfTilde = name.IndexOf('`')
        let name = if indexOfTilde > 0 then name.Substring(0, indexOfTilde) else name
        let typeList = SyntaxFactory.TypeArgumentList(types |> toSeparatedList)
        SyntaxFactory.GenericName(name).WithTypeArgumentList(typeList)

    static member FromType (t:System.Type) =
        let namespaceExpression = NameSyntax.MakeQualified (t.Namespace.Split('.'))
    
        let name =
            if t.IsGenericType then
                let types = t.GetGenericArguments() |> Array.map (fun t -> NameSyntax.FromType t :> TypeSyntax)
                NameSyntax.MakeGeneric t.Name types :> SimpleNameSyntax
            else
                SyntaxFactory.IdentifierName(t.Name) :> SimpleNameSyntax
    
        let fullName = SyntaxFactory.QualifiedName(namespaceExpression, name)

        fullName :> NameSyntax

/// typeof('t)
let namesyntaxof<'t> = NameSyntax.FromType(typeof<'t>)

/// typeof('t)
let typesyntaxof<'t> = NameSyntax.FromType(typeof<'t>) :> TypeSyntax

/// if (argName == null) { throw new ArgumentNullException("argName"); }
let throwIfArgumentNull argName =
    if'
        (equals (identifier argName) Literal.Null)
        (block [ throwException typesyntaxof<System.ArgumentNullException> [|Literal.String argName|] ])

module FromReflection =
    open System.Reflection

    /// Create an ExpressionSyntax representing an access to a static method
    let staticMethodAccess (m:MethodInfo) =
        let declaringType = NameSyntax.FromType(m.DeclaringType)
        declaringType |> dottedMemberAccess [m.Name]
    
    /// Invoke a static method
    let callStaticMethod' (m:MethodInfo) args =
        invocation' (staticMethodAccess m) args
        
    /// Invoke a static method
    let callStaticMethod (m:MethodInfo) args =
        invocation (staticMethodAccess m) args

    let getArgument (p:ParameterInfo) =
        let name = identifier p.Name
        match (p.IsOut, p.ParameterType.IsByRef) with
        | true, _ -> outArg name
        | false, true -> refArg name
        | false, false -> arg name

    let getArgumentsForCall (m:MethodInfo) =
        m.GetParameters() |> Seq.map getArgument

    let getModifiers (p:ParameterInfo) =
        match (p.IsOut, p.ParameterType.IsByRef) with
        | true, _ -> [SyntaxKind.OutKeyword]
        | false, true -> [SyntaxKind.RefKeyword]
        | false, false -> []

    let parameterInfoToParameter (p:ParameterInfo) = 
        SyntaxFactory.Parameter(SyntaxFactory.Identifier(p.Name))
            .WithType(NameSyntax.FromType(p.ParameterType))
            .addModifiers(getModifiers p)

    let getParametersForDeclaration (m:MethodInfo) = 
        m.GetParameters() |> Seq.map(parameterInfoToParameter)

let parseDocumentationComment (comment: string) =
    let text = comment + "\r\nvoid Stidgen() {}";
    let parsed =
        SyntaxFactory.ParseCompilationUnit(
            text,
            0,
            CSharpParseOptions(LanguageVersion.CSharp4, DocumentationMode.Parse, SourceCodeKind.Script))
        
    parsed.DescendantNodes(null, true).OfType<DocumentationCommentTriviaSyntax>().Single() |> SyntaxFactory.Trivia
