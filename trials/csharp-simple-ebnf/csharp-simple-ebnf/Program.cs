using System;
using Irony.Parsing;
using System.Diagnostics;

namespace csharp_simple_ebnf
{
    class Program
    {
        static public ParseTreeNode getRoot(string sourceCode, Grammar grammar)
        {
            LanguageData language = new LanguageData(grammar);
            Parser parser = new Parser(language);
            ParseTree parseTree = parser.Parse(sourceCode);
            ParseTreeNode root = parseTree.Root;
            return root;
        }

        static public void dispTree(ParseTreeNode node, int level)
        {
            for (int i = 0; i < level; i++)
                Console.Write("  ");
            Console.WriteLine(node);
            foreach (ParseTreeNode child in node.ChildNodes)
                dispTree(child, level + 1);
        }

        static public string readFile(string path)
        {
            string[] lines = System.IO.File.ReadAllLines(@path);
            string text = "";
            foreach (string line in lines)
                text += line;
            return text;
        }

        static void Main(string[] args)
        {
            try
            {
                string text = readFile("../../../../grammar.ebnf");
                Console.WriteLine(text + "\n");

                EBNF g = new EBNF();
                dispTree(getRoot(text, g), 0);

                Stopwatch timer = new Stopwatch();
                timer.Start();
                for (int i = 0; i < 1000000; i++)
                {
                    getRoot(text, g);
                    if (i % 100000 == 0)
                        Console.WriteLine(i);
                }
                timer.Stop();
                Console.Write("\n" + (timer.ElapsedMilliseconds / 1000000.0) + "ms");
            }
            catch(Exception e)
            {
                Console.WriteLine(e);
            }
            
            Console.WriteLine("\npress enter to quit...");
            Console.ReadLine();
        }
    }
}
