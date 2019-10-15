using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Read_Only_Analyzer_Demo
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class Read_Only_Analyzer_DemoAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "Read_Only_Analyzer_Demo";

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization
        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "CodeStyle";

        private static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            // TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            context.RegisterSymbolAction(AnalyzeSymbol, SymbolKind.Field);
        }

        private static void AnalyzeSymbol(SymbolAnalysisContext context)
        {
            // TODO: Replace the following code with your own analysis, generating Diagnostic objects for any issues you find
            var fieldSymbol = (IFieldSymbol)context.Symbol;

            if (fieldSymbol.IsReadOnly)
            {
                return;
            }

            var declarations = fieldSymbol.DeclaringSyntaxReferences;

            foreach (var declaration in declarations)
            {
                var syntaxTree = declaration.SyntaxTree;
                var variableDeclarator = (VariableDeclaratorSyntax)declaration.GetSyntax();
                var root = syntaxTree.GetRoot();

                var assignments = root.DescendantNodesAndSelf().OfType<AssignmentExpressionSyntax>();

                foreach (var assignment in assignments)
                {
                    if (assignment.Left is MemberAccessExpressionSyntax memberAccessSyntax)
                    {
                        if (memberAccessSyntax.Name.Identifier.ValueText == variableDeclarator.Identifier.ValueText)
                        {
                            // Check to see if the member access is in the constructor
                            var possibleConstructor = memberAccessSyntax.FirstAncestorOrSelf<SyntaxNode>(n => n is ConstructorDeclarationSyntax);

                            if (possibleConstructor == memberAccessSyntax)
                            {
                                // Not in constructor, field is modified, don't report diagnostic
                                return;
                            }
                        }
                    }
                }
            }

            var diagnostic = Diagnostic.Create(Rule, fieldSymbol.Locations[0], fieldSymbol.Name);
            context.ReportDiagnostic(diagnostic);
        }
    }
}
