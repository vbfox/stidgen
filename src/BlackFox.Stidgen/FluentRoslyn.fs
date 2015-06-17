module BlackFox.Stidgen.FluentRoslyn

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

module Operators =
    /// A version of the pipe operators for async workflows
    let (|!>) a f = async.Bind(a, f)

    /// Unary operator to convert C# Task<'t> to F# Async<'t>
    let (!!) t = Async.AwaitTask t

type TypeSyntax with
    static member Void = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.VoidKeyword))
    static member Object = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.ObjectKeyword))
    static member String = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.StringKeyword))
    static member Int = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.IntKeyword))
    static member Bool = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.BoolKeyword))

module Literal = 
    let Null = SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression)
    let True = SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression)
    let False = SyntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression)
    let Bool (b:bool) = if b then True else False
   
    let String (s:string) =
        SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, SyntaxFactory.Literal(s))
    let EmptyString = String ""

    let Int (i:int) =
        SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(i))
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

let inline addBaseTypes (types : TypeSyntax seq) (input:^T) =
    let baseTypes = types |> Seq.map (fun t -> SyntaxFactory.SimpleBaseType(t) :> BaseTypeSyntax) |> Seq.toArray
    (^T : (member AddBaseListTypes : BaseTypeSyntax array -> ^T) (input, baseTypes))

let inline addModifiers syntaxKinds (input:^T) =
    let tokens = syntaxKinds |> Seq.map (fun k -> SyntaxFactory.Token(k)) |> Seq.toArray
    (^T : (member AddModifiers : SyntaxToken array -> ^T) (input, tokens))

let inline withSemicolon (input:^T) =
    let token = SyntaxFactory.Token(SyntaxKind.SemicolonToken)
    (^T : (member WithSemicolonToken : SyntaxToken -> ^T) (input, token))

let inline addParameter' name parameterType modifiers (input:^T) =
    let parameter =
        SyntaxFactory.Parameter(SyntaxFactory.Identifier(name)).WithType(parameterType)
        |> addModifiers modifiers
    (^T : (member AddParameterListParameters : ParameterSyntax array -> ^T) (input, [|parameter|]))

let inline addParameter name parameterType = addParameter' name parameterType []
let inline addOutParameter name parameterType = addParameter' name parameterType [SyntaxKind.OutKeyword]
let inline addRefParameter name parameterType = addParameter' name parameterType [SyntaxKind.RefKeyword]

let inline addArgument expression (input:^T) =
    let argument = SyntaxFactory.Argument(expression)
    (^T : (member AddArgumentListArguments : ArgumentSyntax array -> ^T) (input, [|argument|]))

let inline addBodyStatements statements (input:^T) =
    (^T : (member AddBodyStatements : StatementSyntax array -> ^T) (input, statements))

let inline addBodyStatement statement input =
    input |> addBodyStatements [|statement|] 

let inline addMembers members (input:^T) =
    (^T : (member AddMembers : MemberDeclarationSyntax array -> ^T) (input, members |> Seq.toArray))
   
let inline addMember member' (input:^T) =
    (^T : (member AddMembers : MemberDeclarationSyntax array -> ^T) (input, [|member'|]))

let inline addStatement statement (input:^T) =
    (^T : (member AddStatements : StatementSyntax array -> ^T) (input, [|statement|]))

let inline withBody (statements: StatementSyntax seq) (input:^T) =
    let block = SyntaxFactory.Block(SyntaxFactory.List<StatementSyntax>(statements))
    (^T : (member WithBody : BlockSyntax -> ^T) (input, block))

let inline addAttributeList (attributes:AttributeSyntax seq) (input:^T) =
    let attributeList = SyntaxFactory.AttributeList(attributes |> toSeparatedList)
    (^T : (member AddAttributeLists : AttributeListSyntax[] -> ^T) (input, [|attributeList|]))

let inline addAttribute attribute input =
    addAttributeList [attribute] input
    
let makeAttribute name args =
    let mappedArgs = args |> List.map (fun a -> SyntaxFactory.AttributeArgument(a))
    // Special casing the empty list as null generate an attribute without empty parenthesis after it
    let finalArgs = if mappedArgs.IsEmpty then null else SyntaxFactory.AttributeArgumentList(mappedArgs |> toSeparatedList)
    SyntaxFactory.Attribute(name, finalArgs)

let makeSingleLineComments (s:string) = 
    let lines = System.Text.RegularExpressions.Regex.Split(s, "\r\n|\r|\n")
    lines |> Array.map (fun l -> SyntaxFactory.Comment(sprintf "//%s\r\n" l))
    
let addTriviaBefore (trivia : SyntaxTrivia seq) (node : #SyntaxNode) =
    let newTrivia = Seq.concat [trivia; node.GetLeadingTrivia() :> SyntaxTrivia seq] |> Seq.toArray
    node.WithLeadingTrivia(newTrivia)

/// get;
let addEmptyGetter (property:PropertyDeclarationSyntax) =
    property.AddAccessorListAccessors(SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration) |> withSemicolon)

/// set;
let addEmptySetter (property:PropertyDeclarationSyntax) =
    property.AddAccessorListAccessors(SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration) |> withSemicolon)

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
    let toString x = invocation (memberAccess "ToString" x) Array.empty

    /// x.GetHashCode()
    let getHashCode x = invocation (memberAccess "GetHashCode" x) Array.empty

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
                if a = null then
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
        (equals (identifier argName) (Literal.Null))
        (block [ throwException typesyntaxof<System.ArgumentNullException> [|Literal.String argName|] ])

[<AutoOpen>]
module Reflection =
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