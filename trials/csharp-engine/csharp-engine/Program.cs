using System;
using csharp_engine.lr_engine;

namespace csharp_engine
{
    class Program
    {
        static void Main(string[] args)
        {
            // Input string
            string input = "2+3*(1+3)"; // = 14

            // Parsing
            LrParser parser = new LrParser(new LrTableImpl(), input);
            parser.parse();

            // Check success
            if (parser.config.action.type == LrAction.Type.Accept)
            {
                Console.WriteLine("success, AST:");
                Console.WriteLine(parser.ast);

                // Evaluate our AST
                LrEvaluate evaluate = new LrEvaluate(parser.ast);
                evaluate.NonTerm("statement", (node) => node.children[0]);
                evaluate.NonTerm("expression", (node) => {
                    if (node.children.Count == 1)
                        return node.children[0];
                    else
                    {
                        int v = int.Parse(node.children[0].value);
                        v += int.Parse(node.children[2].value);
                        return new LrAst.Node(LrAst.Node.Type.Term, v.ToString());
                    }
                });
                evaluate.NonTerm("term", (node) => {
                    if (node.children.Count == 1)
                        return node.children[0];
                    else
                    {
                        int v = int.Parse(node.children[0].value);
                        v *= int.Parse(node.children[2].value);
                        return new LrAst.Node(LrAst.Node.Type.Term, v.ToString());
                    }
                });
                evaluate.NonTerm("factor", (node) => node.children[node.children.Count == 1 ? 0 : 1]);
                evaluate.NonTerm("digit", (node) => node.children[0]);
                evaluate.Eval();

                // Print the resulting AST
                Console.WriteLine("result:");
                Console.WriteLine(parser.ast);
            }
            else
            {
                Console.WriteLine("parsing error at:");
                Console.WriteLine(parser.config.input);
            }

            // The End
            Console.WriteLine("\npress a key to quit...");
            Console.ReadKey(true);
        }
    }
}
