/**
 * LR table template for platinum-parsing
 * Author: Patrick Champion
 * Date: 2017.06.01
 */

using System;
using System.Collections.Generic;
using csharp_engine.lr_engine;

namespace csharp_engine
{
    class LrTableImpl : LrTable
    {
        private LrAction value, error;
        private Dictionary<Tuple<int, char>, LrAction> actions;
        private Dictionary<Tuple<int, string>, LrAction> gotos;

        public char Empty()
        {
            return (char)0;
        }

        /* ACTION table */
        public LrAction action(int state, char term)
        {
            return actions.TryGetValue(Tuple.Create(state, term), out value) ? value : error;
        }

        /* GOTO table */
        public LrAction action(int state, string nonTerm)
        {
            return gotos.TryGetValue(Tuple.Create(state, nonTerm), out value) ? value : error;
        }

        public LrTableImpl()
        {
            value = null;
            error = new LrAction(LrAction.Type.Error);
            actions = new Dictionary<Tuple<int, char>, LrAction>()
            {
                { Tuple.Create(0, '('), new LrAction(LrAction.Type.Shift, 2) },
                { Tuple.Create(0, 'x'), new LrAction(LrAction.Type.Shift, 3) },
                { Tuple.Create(1, ')'), new LrAction(LrAction.Type.Reduce, 1, "E") },
                { Tuple.Create(1, '+'), new LrAction(LrAction.Type.Shift, 7) },
                { Tuple.Create(1, Empty()), new LrAction(LrAction.Type.Reduce, 1, "E") },
                { Tuple.Create(2, '('), new LrAction(LrAction.Type.Shift, 2) },
                { Tuple.Create(2, 'x'), new LrAction(LrAction.Type.Shift, 3) },
                { Tuple.Create(3, ')'), new LrAction(LrAction.Type.Reduce, 1, "F") },
                { Tuple.Create(3, '*'), new LrAction(LrAction.Type.Reduce, 1, "F") },
                { Tuple.Create(3, '+'), new LrAction(LrAction.Type.Reduce, 1, "F") },
                { Tuple.Create(3, Empty()), new LrAction(LrAction.Type.Reduce, 1, "F") },
                { Tuple.Create(4, Empty()), new LrAction(LrAction.Type.Reduce, 1, "S") },
                { Tuple.Create(5, ')'), new LrAction(LrAction.Type.Reduce, 1, "T") },
                { Tuple.Create(5, '*'), new LrAction(LrAction.Type.Shift, 9) },
                { Tuple.Create(5, '+'), new LrAction(LrAction.Type.Reduce, 1, "T") },
                { Tuple.Create(5, Empty()), new LrAction(LrAction.Type.Reduce, 1, "T") },
                { Tuple.Create(7, '('), new LrAction(LrAction.Type.Shift, 2) },
                { Tuple.Create(7, 'x'), new LrAction(LrAction.Type.Shift, 3) },
                { Tuple.Create(8, ')'), new LrAction(LrAction.Type.Shift, 11) },
                { Tuple.Create(9, '('), new LrAction(LrAction.Type.Shift, 2) },
                { Tuple.Create(9, 'x'), new LrAction(LrAction.Type.Shift, 3) },
                { Tuple.Create(10, ')'), new LrAction(LrAction.Type.Reduce, 3, "E") },
                { Tuple.Create(10, Empty()), new LrAction(LrAction.Type.Reduce, 3, "E") },
                { Tuple.Create(11, ')'), new LrAction(LrAction.Type.Reduce, 3, "F") },
                { Tuple.Create(11, '*'), new LrAction(LrAction.Type.Reduce, 3, "F") },
                { Tuple.Create(11, '+'), new LrAction(LrAction.Type.Reduce, 3, "F") },
                { Tuple.Create(11, Empty()), new LrAction(LrAction.Type.Reduce, 3, "F") },
                { Tuple.Create(12, ')'), new LrAction(LrAction.Type.Reduce, 3, "T") },
                { Tuple.Create(12, '+'), new LrAction(LrAction.Type.Reduce, 3, "T") },
                { Tuple.Create(12, Empty()), new LrAction(LrAction.Type.Reduce, 3, "T") },
                
            };
            gotos = new Dictionary<Tuple<int, string>, LrAction>()
            {
                { Tuple.Create(0, "E"), new LrAction(LrAction.Type.Goto, 4) },
                { Tuple.Create(0, "F"), new LrAction(LrAction.Type.Goto, 5) },
                { Tuple.Create(0, "S"), new LrAction(LrAction.Type.Goto, 6) },
                { Tuple.Create(0, "T"), new LrAction(LrAction.Type.Goto, 1) },
                { Tuple.Create(2, "E"), new LrAction(LrAction.Type.Goto, 8) },
                { Tuple.Create(2, "F"), new LrAction(LrAction.Type.Goto, 5) },
                { Tuple.Create(2, "T"), new LrAction(LrAction.Type.Goto, 1) },
                { Tuple.Create(7, "E"), new LrAction(LrAction.Type.Goto, 10) },
                { Tuple.Create(7, "F"), new LrAction(LrAction.Type.Goto, 5) },
                { Tuple.Create(7, "T"), new LrAction(LrAction.Type.Goto, 1) },
                { Tuple.Create(9, "F"), new LrAction(LrAction.Type.Goto, 5) },
                { Tuple.Create(9, "T"), new LrAction(LrAction.Type.Goto, 12) },
                
            };
        }
    }
}
