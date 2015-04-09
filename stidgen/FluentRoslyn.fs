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

let inline addModifiers syntaxKinds (input:^T) =
    let tokens = syntaxKinds |> Array.map (fun k -> SyntaxFactory.Token(k))
    (^T : (member AddModifiers : SyntaxToken array -> ^T) (input, tokens))

let inline withSemicolon (input:^T) =
    let token = SyntaxFactory.Token(SyntaxKind.SemicolonToken)
    (^T : (member WithSemicolonToken : SyntaxToken -> ^T) (input, token))