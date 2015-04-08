module BlackFox.Stidgen.FluentRoslyn

open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.CSharp

type CompilationUnitSyntax with
    member this.AddUsings([<System.ParamArrayAttribute>] items : string[]) =
        this.AddUsings(items |> Array.map (fun name -> SyntaxFactory.UsingDirective(SyntaxFactory.IdentifierName(name))))

type AccessorDeclarationSyntax with
    member this.WithSemicolonToken() =
        this.WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken))