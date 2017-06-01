using System;
using csharp_engine.lr_engine;

namespace csharp_engine
{
    class Program
    {
        static void Main(string[] args)
        {
            string input = "x*x+x";
            LrParser parser = new LrParser(new LrTableImpl(), input);
            parser.parse();
            Console.WriteLine(parser.ast);

            Console.WriteLine("\npress a key to quit...");
            Console.ReadKey(true);
        }
    }
}
