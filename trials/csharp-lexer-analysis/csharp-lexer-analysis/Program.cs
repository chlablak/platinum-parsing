using System;
using System.Diagnostics;

namespace csharp_lexer_analysis
{
    class Program
    {
        static void Main(string[] args)
        {
            // Test cases
            Tuple<int, int>[] tests =
            {
                Tuple.Create(10000, 100),
                Tuple.Create(1000, 1000),
                Tuple.Create(100, 10000),
                Tuple.Create(10, 100000),

                Tuple.Create(10000, 10000), // big
                Tuple.Create(10000, 1000000), // very big
                Tuple.Create(10000, 100000000), // very very big
                
                //Tuple.Create(3, 1000), // test
            };

            foreach(Tuple<int, int> test in tests)
            {
                Console.WriteLine("TEST WITH N = " + test.Item1 + " AND MAX = " + test.Item2);
                
                Console.Write("GENERATE DATA... ");
                Tuple<string, int> data = GenerateData(test.Item1, test.Item2);
                Console.WriteLine("OK");
                int offset = data.Item1.Length > 30 ? 30 : data.Item1.Length;
                Console.WriteLine("  PREVIEW: " + data.Item1.Substring(0, offset));
                Console.WriteLine("  EXPECTED RESULT: " + data.Item2);

                Console.Write("STARTING TEST WITHOUT LEXER... ");
                Stopwatch without = Stopwatch.StartNew();
                string result = TestWithoutLexer(data.Item1);
                without.Stop();
                if(result == data.Item2.ToString())
                    Console.WriteLine("OK");
                else
                {
                    Console.WriteLine("FAIL");
                    Console.WriteLine("  GOT: " + result);
                }

                Console.Write("STARTING TEST WITH LEXER... ");
                Stopwatch with = Stopwatch.StartNew();
                result = TestWithLexer(data.Item1);
                with.Stop();
                if (result == data.Item2.ToString())
                    Console.WriteLine("OK");
                else
                {
                    Console.WriteLine("FAIL");
                    Console.WriteLine("  GOT: " + result);
                }

                // Results
                Console.WriteLine("WITHOUT: " + without.ElapsedMilliseconds + " MS / WITH: " 
                    + with.ElapsedMilliseconds + " MS");
                Console.WriteLine("RATIO WITHOUT / WITH: " 
                    + ((double)without.ElapsedMilliseconds / with.ElapsedMilliseconds));
                Console.WriteLine();
            }

            Console.WriteLine("\npress any key to quit...");
            Console.ReadKey();
        }

        // generate an input expression and compute its result
        static Tuple<string, int> GenerateData(int n, int max)
        {
            Random rand = new Random();
            int result = 1;
            string input = "1";
            for(int i = 0; i < n; i++)
            {
                int current = rand.Next(1, max);
                double binop = rand.NextDouble();
                input += binop < 0.5 ? '-' : '+';
                input += current.ToString();
                result += binop < 0.5 ? -current : current;
            }
            return Tuple.Create(input, result);
        }

        static string TestWithoutLexer(string input)
        {
            engine_without.LrParser parser = new engine_without.LrParser(new TableWithout(), input);
            parser.Parse();
            engine_without.LrEvaluate evaluate = new engine_without.LrEvaluate(parser.ast);
            evaluate.NonTerm("digit without zero", (node) => node.children[0]);
            evaluate.NonTerm("digit", (node) => node.children[0]);
            evaluate.NonTerm("{<digit>}", (node) => {
                if (node.children.Count == 0)
                    return new engine_without.LrAst.Node(engine_without.LrAst.Node.Type.Term, "IGNORE");
                string concat = node.children[0].value;
                if (node.children[1].value != "IGNORE")
                    concat += node.children[1].value;
                return new engine_without.LrAst.Node(engine_without.LrAst.Node.Type.Term, concat);
            });
            evaluate.NonTerm("number", (node) => {
                string concat = node.children[0].value;
                if (node.children[1].value != "IGNORE")
                    concat += node.children[1].value;
                return new engine_without.LrAst.Node(engine_without.LrAst.Node.Type.Term, concat);
            });
            evaluate.NonTerm("binop", (node) => node.children[0]);
            evaluate.NonTerm("{<binop>,<number>}", (node) => {
                if(node.children.Count == 0)
                    return new engine_without.LrAst.Node(engine_without.LrAst.Node.Type.Term, "0");
                string a = node.children[0].value + node.children[1].value;
                string b = node.children[2].value;
                string c = "" + (Int32.Parse(a) + Int32.Parse(b));
                return new engine_without.LrAst.Node(engine_without.LrAst.Node.Type.Term, c);
            });
            evaluate.NonTerm("expr", (node) => {
                string a = node.children[0].value;
                string b = node.children[1].value;
                string c = "" + (Int32.Parse(a) + Int32.Parse(b));
                return new engine_without.LrAst.Node(engine_without.LrAst.Node.Type.Term, c);
            });
            evaluate.Eval();
            return parser.ast.root.children[0].value;
        }

        static string TestWithLexer(string input)
        {
            engine_with.DedicatedLexer lexer = new engine_with.DedicatedLexer(input);
            engine_with.LrParser parser = new engine_with.LrParser(new TableWith(), lexer.tokens);
            parser.Parse();
            engine_with.LrEvaluate evaluate = new engine_with.LrEvaluate(parser.ast);
            evaluate.NonTerm("number", (node) => node.children[0]);
            evaluate.NonTerm("binop", (node) => node.children[0]);
            evaluate.NonTerm("{<binop>,<number>}", (node) => {
                if (node.children.Count == 0)
                    return new engine_with.LrAst.Node(engine_with.LrAst.Node.Type.Term, "0");
                string a = node.children[0].value + node.children[1].value;
                string b = node.children[2].value;
                string c = "" + (Int32.Parse(a) + Int32.Parse(b));
                return new engine_with.LrAst.Node(engine_with.LrAst.Node.Type.Term, c);
            });
            evaluate.NonTerm("expr", (node) => {
                string a = node.children[0].value;
                string b = node.children[1].value;
                string c = "" + (Int32.Parse(a) + Int32.Parse(b));
                return new engine_with.LrAst.Node(engine_with.LrAst.Node.Type.Term, c);
            });
            evaluate.Eval();
            return parser.ast.root.children[0].value;
        }
    }
}
