using System.Collections.Generic;

namespace csharp_engine.lr_engine
{
    class LrParser
    {
        public LrTable table { get;  }
        public LrConfig config { get; }
        public LrAst ast { get; }

        public LrParser(LrTable table, string input)
        {
            this.table = table;
            LrAction action = this.table.action(0, input[0]);
            this.config = new LrConfig(0, new Stack<int>(), action, input);
            this.config.stack.Push(0);
            this.ast = new LrAst();
        }

        public bool hasNext()
        {
            LrAction.Type type = this.config.action.type;
            return type != LrAction.Type.Error && type != LrAction.Type.Accept;
        }

        public void next()
        {}

        public void parse()
        {
            while (hasNext())
                next();
        }
    }
}
