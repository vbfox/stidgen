module BlackFox.Stidgen.FluentRoslyn

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

type CompilationUnitSyntax with
    member this.AddUsings([<System.ParamArrayAttribute>] items : string array) =
        let directives =
            items
            |> Array.map (fun name -> SyntaxFactory.UsingDirective(SyntaxFactory.IdentifierName(name)))

        this.AddUsings(directives)

let stringTypeSyntax = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.StringKeyword))

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

let nullLiteral = SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression)

let stringLiteral (s:string) =
    SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, SyntaxFactory.Literal(s))

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

let memberAccess (``member``:string) (expression:ExpressionSyntax) =
    SyntaxFactory.MemberAccessExpression(
        SyntaxKind.SimpleMemberAccessExpression,
        expression,
        SyntaxFactory.IdentifierName(``member``)
    )

let simpleMemberAccess (identifier:string) (``member``:string) =
    SyntaxFactory.IdentifierName(identifier)
    |> memberAccess ``member``

let thisMemberAccess (``member``:string) =
    SyntaxFactory.ThisExpression()
    |> memberAccess ``member``

let objectCreation createdType argumentExpressions =
    let args = argumentExpressions |> Array.map (fun a -> SyntaxFactory.Argument(a))
    let argList = SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList<ArgumentSyntax>(args))
    SyntaxFactory.ObjectCreationExpression(createdType).WithArgumentList(argList)