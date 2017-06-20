/**
 * LR table template for platinum-parsing
 * Author: Patrick Champion
 * Date: 2017.06.01
 */

using System;
using System.Collections.Generic;
using /* INSERT LR ENGINE */;

namespace /* INSERT NAMESPACE */
{
    class /* INSERT CLASS NAME */ : LrTable
    {
        private LrAction value, error;
        private Dictionary<Tuple<int, char>, LrAction> actions;
        private Dictionary<Tuple<int, string>, LrAction> gotos;

        public char Empty()
        {
            return /* INSERT EMPTY SYMBOL */;
        }

        /* ACTION table */
        public LrAction Action(int state, char term)
        {
            return actions.TryGetValue(Tuple.Create(state, term), out value) ? value : error;
        }

        /* GOTO table */
        public LrAction Action(int state, string nonTerm)
        {
            return gotos.TryGetValue(Tuple.Create(state, nonTerm), out value) ? value : error;
        }

        public /* INSERT CLASS NAME */()
        {
            value = null;
            error = new LrAction(LrAction.Type.Error);
            actions = new Dictionary<Tuple<int, char>, LrAction>()
            {
                { Tuple.Create(0, '+'), new LrAction(LrAction.Type.Reduce, 0, "number") },
                { Tuple.Create(0, '-'), new LrAction(LrAction.Type.Reduce, 0, "number") },
                { Tuple.Create(0, Empty()), new LrAction(LrAction.Type.Reduce, 0, "number") },
                { Tuple.Create(1, Empty()), new LrAction(LrAction.Type.Accept) },
                { Tuple.Create(2, '+'), new LrAction(LrAction.Type.Shift, 3) },
                { Tuple.Create(2, '-'), new LrAction(LrAction.Type.Shift, 4) },
                { Tuple.Create(2, Empty()), new LrAction(LrAction.Type.Reduce, 0, "{<binop>,<number>}") },
                { Tuple.Create(3, Empty()), new LrAction(LrAction.Type.Reduce, 1, "binop") },
                { Tuple.Create(4, Empty()), new LrAction(LrAction.Type.Reduce, 1, "binop") },
                { Tuple.Create(5, Empty()), new LrAction(LrAction.Type.Reduce, 2, "expr") },
                { Tuple.Create(6, '+'), new LrAction(LrAction.Type.Reduce, 0, "number") },
                { Tuple.Create(6, '-'), new LrAction(LrAction.Type.Reduce, 0, "number") },
                { Tuple.Create(6, Empty()), new LrAction(LrAction.Type.Reduce, 0, "number") },
                { Tuple.Create(7, '+'), new LrAction(LrAction.Type.Shift, 3) },
                { Tuple.Create(7, '-'), new LrAction(LrAction.Type.Shift, 4) },
                { Tuple.Create(7, Empty()), new LrAction(LrAction.Type.Reduce, 0, "{<binop>,<number>}") },
                { Tuple.Create(8, Empty()), new LrAction(LrAction.Type.Reduce, 3, "{<binop>,<number>}") },
                
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
