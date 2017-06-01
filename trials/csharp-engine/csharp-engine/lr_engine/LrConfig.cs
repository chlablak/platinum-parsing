using System.Collections.Generic;

namespace csharp_engine.lr_engine
{
    class LrConfig
    {
        public int count { get; set; }
        public Stack<int> stack { get; set; }
        public LrAction action { get; set; }
        public string input { get; set; }

        public LrConfig(int count, Stack<int> stack, LrAction action, string input)
        {
            this.count = count;
            this.stack = stack;
            this.action = action;
            this.input = input;
        }
    }
}
