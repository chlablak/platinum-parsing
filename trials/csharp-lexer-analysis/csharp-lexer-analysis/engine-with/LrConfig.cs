using System.Collections.Generic;

namespace csharp_lexer_analysis.engine_with
{
    /**
     * LR parser configuration
     */
    class LrConfig
    {
        public int count { get; set; }
        public Stack<int> stack { get; set; }
        public LrAction action { get; set; }
        public List<Token> input { get; set; }

        public LrConfig(int count, Stack<int> stack, LrAction action, List<Token> input)
        {
            this.count = count;
            this.stack = stack;
            this.action = action;
            this.input = input;
        }

        public override string ToString()
        {
            return "[" + count + "; " + action + "; " + input + "]";
        }
    }
}
