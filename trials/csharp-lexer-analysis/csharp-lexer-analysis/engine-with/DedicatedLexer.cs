using System.Collections.Generic;

namespace csharp_lexer_analysis.engine_with
{
    class DedicatedLexer
    {
        public List<Token> tokens { get; }
        private int current;

        public DedicatedLexer(string input)
        {
            tokens = new List<Token>();
            current = 0;

            // Lexing
            // ------
            string value = "";
            input += '+'; // add guard
            foreach(char c in input)
            {
                switch(c)
                {
                    // binop
                    case '-':
                    case '+':
                        tokens.Add(new Token("number", value));
                        tokens.Add(new Token("binop", new string(c, 1)));
                        value = "";
                        break;

                    // number 
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        value += c;
                        break;
                }
            }
            tokens.RemoveAt(tokens.Count - 1); // remove guard
        }

        public bool HasNext()
        {
            return current < tokens.Count;
        }

        public Token Next()
        {
            return tokens[current++];
        }

        public void Reset()
        {
            current = 0;
        }
    }
}
