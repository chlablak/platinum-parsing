using System;
using System.Collections.Generic;

namespace csharp_engine.lr_engine
{
    /**
     * Evaluate an AST
     */
    class LrEvaluate
    {
        /**
         * Contained AST
         */
        public LrAst ast { get; set; }

        // delegates
        private Dictionary<Tuple<string, LrAst.Node.Type>, Func<LrAst.Node, LrAst.Node>> delegates;

        /**
         * Constructor
         */
        public LrEvaluate(LrAst ast)
        {
            this.ast = ast;
            this.delegates = new Dictionary<Tuple<string, LrAst.Node.Type>, Func<LrAst.Node, LrAst.Node>>();
        }

        /**
         * Add a delegate for a non-terminal
         */
        public void NonTerm(string nonTerm, Func<LrAst.Node, LrAst.Node> del)
        {
            delegates.Add(Tuple.Create(nonTerm, LrAst.Node.Type.NonTerm), del);
        }

        /**
         * Add a delegate for a terminal
         */
        public void Term(string term, Func<LrAst.Node, LrAst.Node> del)
        {
            delegates.Add(Tuple.Create(term, LrAst.Node.Type.Term), del);
        }

        /**
         * Evaluate the AST with registered delegates
         */
        public void Eval()
        {
            ast.root = Eval(ast.root);
        }

        private LrAst.Node Eval(LrAst.Node node)
        {
            for(int i = 0; i < node.children.Count; ++i)
                node.children[i] = Eval(node.children[i]);
            Func<LrAst.Node, LrAst.Node> del;
            if (delegates.TryGetValue(Tuple.Create(node.value, node.type), out del))
                node = del.Invoke(node);
            return node;
        }
    }
}
