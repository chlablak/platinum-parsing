/**
 * LR table template for platinum-parsing
 * Author: Patrick Champion
 * Date: 2017.06.01
 */

using System;
using System.Collections.Generic;
using csharp_lexer_analysis.engine_with;

namespace csharp_lexer_analysis
{
    class TableWith : LrTable
    {
        private LrAction value, error;
        private Dictionary<Tuple<int, string>, LrAction> actions;
        private Dictionary<Tuple<int, string>, LrAction> gotos;

        public Token Empty()
        {
            return new Token(null);
        }

        /* ACTION table */
        public LrAction Action(int state, Token term)
        {
            return actions.TryGetValue(Tuple.Create(state, term.type), out value) ? value : error;
        }

        /* GOTO table */
        public LrAction Action(int state, string nonTerm)
        {
            return gotos.TryGetValue(Tuple.Create(state, nonTerm), out value) ? value : error;
        }

        public TableWith()
        {
            value = null;
            error = new LrAction(LrAction.Type.Error);
            string empty = Empty().type;
            actions = new Dictionary<Tuple<int, string>, LrAction>()
            {
                { Tuple.Create(0, "number"), new LrAction(LrAction.Type.Shift, 3) },
                { Tuple.Create(1, empty), new LrAction(LrAction.Type.Accept) },
                { Tuple.Create(2, "binop"), new LrAction(LrAction.Type.Shift, 4) },
                { Tuple.Create(2, empty), new LrAction(LrAction.Type.Reduce, 0, "{<binop>,<number>}") },
                { Tuple.Create(3, "binop"), new LrAction(LrAction.Type.Reduce, 1, "number") },
                { Tuple.Create(3, empty), new LrAction(LrAction.Type.Reduce, 1, "number") },
                { Tuple.Create(4, "number"), new LrAction(LrAction.Type.Reduce, 1, "binop") },
                { Tuple.Create(5, empty), new LrAction(LrAction.Type.Reduce, 2, "expr") },
                { Tuple.Create(6, "number"), new LrAction(LrAction.Type.Shift, 3) },
                { Tuple.Create(7, "binop"), new LrAction(LrAction.Type.Shift, 4) },
                { Tuple.Create(7, empty), new LrAction(LrAction.Type.Reduce, 0, "{<binop>,<number>}") },
                { Tuple.Create(8, empty), new LrAction(LrAction.Type.Reduce, 3, "{<binop>,<number>}") },
                
            };
            gotos = new Dictionary<Tuple<int, string>, LrAction>()
            {
                { Tuple.Create(0, "expr"), new LrAction(LrAction.Type.Goto, 1) },
                { Tuple.Create(0, "number"), new LrAction(LrAction.Type.Goto, 2) },
                { Tuple.Create(2, "binop"), new LrAction(LrAction.Type.Goto, 6) },
                { Tuple.Create(2, "{<binop>,<number>}"), new LrAction(LrAction.Type.Goto, 5) },
                { Tuple.Create(6, "number"), new LrAction(LrAction.Type.Goto, 7) },
                { Tuple.Create(7, "binop"), new LrAction(LrAction.Type.Goto, 6) },
                { Tuple.Create(7, "{<binop>,<number>}"), new LrAction(LrAction.Type.Goto, 8) },
                
            };
        }
    }
}
