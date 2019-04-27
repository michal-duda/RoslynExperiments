// Learn more about F# at http://fsharp.org

open System.Reflection
open System.IO
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis
open System

let createCompilation (path: string) = 
    let files = Directory.GetFiles(path, "*.cs", SearchOption.AllDirectories)

    let syntaxTrees = files |> Array.map (fun f -> CSharpSyntaxTree.ParseText(File.ReadAllText(f)).WithFilePath(f))
    let objType = typeof<System.Object>
    let mscorlibMetadata = MetadataReference.CreateFromFile(objType.GetTypeInfo().Assembly.Location);

    CSharpCompilation.Create("TestingCsProject",
        syntaxTrees,
        [|mscorlibMetadata|],
        new CSharpCompilationOptions(OutputKind.ConsoleApplication));

type AttributeRewriter(param1) =
   inherit CSharpSyntaxRewriter(param1)

   override this.VisitMethodDeclaration (node : Microsoft.CodeAnalysis.CSharp.Syntax.MethodDeclarationSyntax) : Microsoft.CodeAnalysis.SyntaxNode =

        let logging = node.AttributeLists |> Seq.tryFind (fun atr -> atr.Attributes |> Seq.exists(fun a -> a.Name.NormalizeWhitespace().ToFullString() = "Logging"))
        match logging with
            | Some(attribute) ->
                let paramsLog = String.Join(" ", node.ParameterList.Parameters |> Seq.map(fun p -> (sprintf "%s = {%s}" p.Identifier.Text p.Identifier.Text)))
                let newBody = SyntaxFactory.ParseStatement(sprintf "Trace.Log($\"%s\");" paramsLog)
                                .WithLeadingTrivia(SyntaxFactory.Whitespace("\n"))
                                .WithTrailingTrivia(SyntaxFactory.Whitespace("\n"))

                let syntaxList = SyntaxList(newBody).AddRange(node.Body.Statements)
                let body = SyntaxFactory.Block(syntaxList).WithTrailingTrivia(SyntaxFactory.Whitespace("\n"))
                let attrList = node.AttributeLists.Remove(attribute)
                base.VisitMethodDeclaration(node.WithAttributeLists(attrList).WithBody(body))

            | None -> base.VisitMethodDeclaration(node)

[<EntryPoint>]
let main argv =
    let compilation = createCompilation "..\..\..\..\TestingCsProject"
    
    for sourceTree in compilation.SyntaxTrees do

        let model = compilation.GetSemanticModel(sourceTree, true);
        // initialization of our rewriter class
        let rewriter = new AttributeRewriter(true);
    
        // analysis of the tree
        let newSource = rewriter.Visit(sourceTree.GetRoot());                
    
        if not(Directory.Exists(@"../new_src")) then
            Directory.CreateDirectory(@"../new_src") |> ignore
           
        File.WriteAllText(Path.Combine(@"../new_src", Path.GetFileName(sourceTree.FilePath)), newSource.ToFullString());
     
    0 // return an integer exit code