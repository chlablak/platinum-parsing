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
            };

            foreach(Tuple<int, int> test in tests)
            {
                Console.WriteLine("TEST WITH N = " + test.Item1 + " AND MAX = " + test.Item2);
                
                Console.Write("GENERATE DATA... ");
                Tuple<string, int> data = GenerateData(test.Item1, test.Item2);
                Console.WriteLine("OK");
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
            int result = 0;
            string input = "0";
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
            /*engine_without.LrParser parser = new engine_without.LrParser(new TableWithout(), input);
            parser.Parse();
            engine_without.LrEvaluate evaluate = new engine_without.LrEvaluate(parser.ast);
            evaluate.Eval();
            return parser.ast.root.children[0].value;*/
            return "";
        }

        static string TestWithLexer(string input)
        {
            return "";
        }
    }
}
