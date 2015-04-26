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

let addUsings (usings : string array) (compilationUnit : CompilationUnitSyntax) =
    let directives = usings |> Array.map (fun name ->
        SyntaxFactory.UsingDirective(SyntaxFactory.IdentifierName(name)))

    compilationUnit.AddUsings(directives)

let addBaseTypes (types : TypeSyntax array) (classDeclaration : ClassDeclarationSyntax) =
    let baseTypes = types |> Array.map (fun t -> SyntaxFactory.SimpleBaseType(t) :> BaseTypeSyntax)
    classDeclaration.AddBaseListTypes(baseTypes)

let inline addModifiers syntaxKinds (input:^T) =
    let tokens = syntaxKinds |> Array.map (fun k -> SyntaxFactory.Token(k))
    (^T : (member AddModifiers : SyntaxToken array -> ^T) (input, tokens))

let inline withSemicolon (input:^T) =
    let token = SyntaxFactory.Token(SyntaxKind.SemicolonToken)
    (^T : (member WithSemicolonToken : SyntaxToken -> ^T) (input, token))

let inline addParameter name parameterType (input:^T) =
    let parameter = SyntaxFactory.Parameter(SyntaxFactory.Identifier(name)).WithType(parameterType)
    (^T : (member AddParameterListParameters : ParameterSyntax array -> ^T) (input, [|parameter|]))

let inline addArgument expression (input:^T) =
    let argument = SyntaxFactory.Argument(expression)
    (^T : (member AddArgumentListArguments : ArgumentSyntax array -> ^T) (input, [|argument|]))

let inline addBodyStatement statement (input:^T) =
    (^T : (member AddBodyStatements : StatementSyntax array -> ^T) (input, [|statement|]))
   
let inline addMember member' (input:^T) =
    (^T : (member AddMembers : MemberDeclarationSyntax array -> ^T) (input, [|member'|]))

let inline addStatement statement (input:^T) =
    (^T : (member AddStatements : StatementSyntax array -> ^T) (input, [|statement|]))

let inline withBody (statements: StatementSyntax array) (input:^T) =
    let block = SyntaxFactory.Block(SyntaxFactory.List<StatementSyntax>(statements))
    (^T : (member WithBody : BlockSyntax -> ^T) (input, block))

/// this.memberName = value;
let setThisMember (memberName:string) value =
    SyntaxFactory.ExpressionStatement(
        SyntaxFactory.AssignmentExpression(
            SyntaxKind.SimpleAssignmentExpression,
            SyntaxFactory.MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                SyntaxFactory.ThisExpression(),
                SyntaxFactory.IdentifierName(memberName)
            ),
            value
        )
    )

let identifier (identifierName : string) = SyntaxFactory.IdentifierName(identifierName)

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

/// this.member
let thisMemberAccess (``member``:string) =
    SyntaxFactory.ThisExpression() |> memberAccess ``member``

/// new createdType(argumentExpressions)
let objectCreation createdType argumentExpressions =
    let args = argumentExpressions |> Array.map (fun a -> SyntaxFactory.Argument(a))
    let argList = SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList<ArgumentSyntax>(args))

    SyntaxFactory.ObjectCreationExpression(createdType)
        .WithArgumentList(argList)

/// expression(argumentExpressions);
let invocation (expression : ExpressionSyntax) (argumentExpressions : ExpressionSyntax array) =
    let args = argumentExpressions |> Array.map (fun a -> SyntaxFactory.Argument(a))
    let argList = SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList<ArgumentSyntax>(args))

    SyntaxFactory.InvocationExpression(expression)
        .WithArgumentList(argList)

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

/// (left || right)
let or' left right = parenthesis (SyntaxFactory.BinaryExpression(SyntaxKind.LogicalOrExpression, left, right))

/// (left && right)
let and' left right = parenthesis (SyntaxFactory.BinaryExpression(SyntaxKind.LogicalAndExpression, left, right))

/// !(expression)
let not' expression = SyntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, parenthesis expression)

/// if (condition) then then'
let if' condition then' = SyntaxFactory.IfStatement(condition, then')

/// if (condition) then then' else else'
let ifelse condition then' else' = SyntaxFactory.IfStatement(condition, then', SyntaxFactory.ElseClause(else'))

/// {}
let emptyBlock = SyntaxFactory.Block()

/// { statements }
let block (statements : StatementSyntax array) = SyntaxFactory.Block(statements)

/// throw expression;
let throw expression = SyntaxFactory.ThrowStatement(expression)

/// An empty file ("compilation unit")
let emptyFile = SyntaxFactory.CompilationUnit()

module WellKnownMethods =
    /// System.Object.Equals(objA, objB)
    let objectEquals objA objB = invocation (dottedMemberAccess' ["System"; "Object"; "Equals"]) [| objA; objB |]

    /// x.ToString()
    let toString x = invocation (memberAccess "ToString" x) Array.empty

    /// x.GetHashCode()
    let getHashCode x = invocation (memberAccess "GetHashCode" x) Array.empty

type TypeSyntax with
    static member private Global = SyntaxFactory.IdentifierName(SyntaxFactory.Token(SyntaxKind.GlobalKeyword))
    static member private PrefixWithGlobal name = SyntaxFactory.AliasQualifiedName(TypeSyntax.Global, name)

    static member MakeQualified (parts : string array) =
        parts |> Array.fold
            (fun a b ->
                if a = null then
                    TypeSyntax.PrefixWithGlobal (SyntaxFactory.IdentifierName(b)) :> NameSyntax
                else
                    SyntaxFactory.QualifiedName(a, SyntaxFactory.IdentifierName(b)) :> NameSyntax
            )
            null

    static member MakeGeneric (name : string) (types : TypeSyntax seq) =
        let indexOfTilde = name.IndexOf('`')
        let name = if indexOfTilde > 0 then name.Substring(0, indexOfTilde) else name
        let typeList = SyntaxFactory.TypeArgumentList(SyntaxFactory.SeparatedList<TypeSyntax>(types))
        SyntaxFactory.GenericName(name).WithTypeArgumentList(typeList)

    static member FromType (t:System.Type) =
        let namespaceExpression = TypeSyntax.MakeQualified (t.Namespace.Split('.'))
    
        let name =
            if t.IsGenericType then
                let types = t.GetGenericArguments() |> Array.map (fun t -> TypeSyntax.FromType t)
                TypeSyntax.MakeGeneric t.Name types :> SimpleNameSyntax
            else  
                SyntaxFactory.IdentifierName(t.Name) :> SimpleNameSyntax
    
        SyntaxFactory.QualifiedName(namespaceExpression, name) :> TypeSyntax

/// typeof('t)
let typesyntaxof<'t> = TypeSyntax.FromType(typeof<'t>)