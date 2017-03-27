using Irony.Parsing;

namespace csharp_simple_ebnf
{
    class EBNF : Grammar
    {
        public EBNF() : base(false)
        {
            // Terminals
            IdentifierTerminal TIdentifier = new IdentifierTerminal("identifier");
            StringLiteral TTerminal = new StringLiteral("terminal", "\'");
            KeyTerm TConcatenation = ToTerm(",", "concatenation");
            KeyTerm TAlternation = ToTerm("|", "alternation");

            // Non terminals
            NonTerminal TOperator = new NonTerminal("operator");
            NonTerminal TExpression = new NonTerminal("expression");
            NonTerminal TRule = new NonTerminal("rule");
            NonTerminal TBinary = new NonTerminal("binary");
            NonTerminal TStatement = new NonTerminal("statement");
            NonTerminal TDefinition = new NonTerminal("definition");
            NonTerminal TStatements = new NonTerminal("statements");

            // Rules definition
            TOperator.Rule = TConcatenation | TAlternation;
            TExpression.Rule = TTerminal | TRule | TBinary;
            TRule.Rule = ToTerm("<") + TIdentifier + ">" | TIdentifier;
            TBinary.Rule = TExpression + TOperator + TExpression;
            TStatement.Rule = TDefinition;
            TDefinition.Rule = TRule + "=" + TExpression + ";";
            TStatements.Rule = MakePlusRule(TStatements, TStatement);

            // Operators precedence and associativity
            RegisterOperators(1, TAlternation);
            RegisterOperators(2, TConcatenation);

            // Clean tree
            MarkPunctuation("<", ">", "=", ";");

            // Entry rule
            this.Root = TStatements;
        }
    }
}
