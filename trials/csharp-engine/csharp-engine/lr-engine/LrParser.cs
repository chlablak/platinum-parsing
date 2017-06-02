using System.Collections.Generic;

namespace csharp_engine.lr_engine
{
    /**
     * LR parser
     */
    class LrParser
    {
        /**
         * Properties
         */
        public LrTable table { get;  }
        public LrConfig config { get; }
        public LrAst ast { get; }

        /**
         * Construct a parser with a given table and input
         */
        public LrParser(LrTable table, string input)
        {
            this.table = table;
            LrAction action = table.Action(0, input.Length > 0 ? input[0] : table.Empty());
            this.config = new LrConfig(0, new Stack<int>(), action, input);
            this.config.stack.Push(0);
            this.ast = new LrAst();
        }

        /**
         * Check if there is an iteration left
         */
        public bool HasNext()
        {
            LrAction.Type type = config.action.type;
            return type != LrAction.Type.Error && type != LrAction.Type.Accept;
        }

        /**
         * Compute the Next iteration (config and AST will evolve)
         */
        public void Next()
        {
            switch (config.action.type)
            {
                case LrAction.Type.Shift:
                    config.count += 1;
                    config.stack.Push(config.action.value);
                    config.action = table.Action(config.action.value,
                        config.input.Length > 1 ? config.input[1] : table.Empty());
                    if (config.input.Length > 0)
                    {
                        ast.Shift(config.input[0]);
                        config.input = config.input.Substring(1);
                    }
                    break;

                case LrAction.Type.Reduce:
                    config.count += 1;
                    for (int i = 0; i < config.action.value; ++i)
                        config.stack.Pop();
                    ast.Reduce(config.action.name, config.action.value);
                    config.action = table.Action(config.stack.Peek(), config.action.name);
                    break;

                case LrAction.Type.Goto:
                    config.count += 1;
                    config.stack.Push(config.action.value);
                    config.action = table.Action(config.action.value,
                        config.input.Length > 0 ? config.input[0] : table.Empty());
                    break;

                // for Error and Accept -> do nothing
            }
        }

        /**
         * Compute all iterations and merge the resulting AST
         */
        public void Parse()
        {
            while (HasNext())
                Next();
            ast.Merge();
        }
    }
}
