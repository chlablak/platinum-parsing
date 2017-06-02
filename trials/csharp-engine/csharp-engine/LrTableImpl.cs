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
    class LrTableImpl : LrTable
    {
        private LrAction value, error;
        private Dictionary<Tuple<int, char>, LrAction> actions;
        private Dictionary<Tuple<int, string>, LrAction> gotos;

        public char Empty()
        {
            return /* INSERT EMPTY SYMBOL */;
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
                { Tuple.Create(0, '0'), new LrAction(LrAction.Type.Shift, 5) },
                { Tuple.Create(0, '1'), new LrAction(LrAction.Type.Shift, 6) },
                { Tuple.Create(0, '2'), new LrAction(LrAction.Type.Shift, 7) },
                { Tuple.Create(0, '3'), new LrAction(LrAction.Type.Shift, 8) },
                { Tuple.Create(0, '4'), new LrAction(LrAction.Type.Shift, 9) },
                { Tuple.Create(0, '5'), new LrAction(LrAction.Type.Shift, 10) },
                { Tuple.Create(0, '6'), new LrAction(LrAction.Type.Shift, 11) },
                { Tuple.Create(0, '7'), new LrAction(LrAction.Type.Shift, 12) },
                { Tuple.Create(0, '8'), new LrAction(LrAction.Type.Shift, 13) },
                { Tuple.Create(0, '9'), new LrAction(LrAction.Type.Shift, 14) },
                { Tuple.Create(2, '+'), new LrAction(LrAction.Type.Shift, 15) },
                { Tuple.Create(2, Empty()), new LrAction(LrAction.Type.Reduce, 1, "expression") },
                { Tuple.Create(3, '*'), new LrAction(LrAction.Type.Shift, 16) },
                { Tuple.Create(3, '+'), new LrAction(LrAction.Type.Reduce, 1, "factor") },
                { Tuple.Create(3, Empty()), new LrAction(LrAction.Type.Reduce, 1, "factor") },
                { Tuple.Create(4, Empty()), new LrAction(LrAction.Type.Reduce, 1, "statement") },
                { Tuple.Create(5, '*'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(5, '+'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(5, Empty()), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(6, '*'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(6, '+'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(6, Empty()), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(7, '*'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(7, '+'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(7, Empty()), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(8, '*'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(8, '+'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(8, Empty()), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(9, '*'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(9, '+'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(9, Empty()), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(10, '*'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(10, '+'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(10, Empty()), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(11, '*'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(11, '+'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(11, Empty()), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(12, '*'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(12, '+'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(12, Empty()), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(13, '*'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(13, '+'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(13, Empty()), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(14, '*'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(14, '+'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(14, Empty()), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(15, '0'), new LrAction(LrAction.Type.Shift, 5) },
                { Tuple.Create(15, '1'), new LrAction(LrAction.Type.Shift, 6) },
                { Tuple.Create(15, '2'), new LrAction(LrAction.Type.Shift, 7) },
                { Tuple.Create(15, '3'), new LrAction(LrAction.Type.Shift, 8) },
                { Tuple.Create(15, '4'), new LrAction(LrAction.Type.Shift, 9) },
                { Tuple.Create(15, '5'), new LrAction(LrAction.Type.Shift, 10) },
                { Tuple.Create(15, '6'), new LrAction(LrAction.Type.Shift, 11) },
                { Tuple.Create(15, '7'), new LrAction(LrAction.Type.Shift, 12) },
                { Tuple.Create(15, '8'), new LrAction(LrAction.Type.Shift, 13) },
                { Tuple.Create(15, '9'), new LrAction(LrAction.Type.Shift, 14) },
                { Tuple.Create(16, '0'), new LrAction(LrAction.Type.Shift, 5) },
                { Tuple.Create(16, '1'), new LrAction(LrAction.Type.Shift, 6) },
                { Tuple.Create(16, '2'), new LrAction(LrAction.Type.Shift, 7) },
                { Tuple.Create(16, '3'), new LrAction(LrAction.Type.Shift, 8) },
                { Tuple.Create(16, '4'), new LrAction(LrAction.Type.Shift, 9) },
                { Tuple.Create(16, '5'), new LrAction(LrAction.Type.Shift, 10) },
                { Tuple.Create(16, '6'), new LrAction(LrAction.Type.Shift, 11) },
                { Tuple.Create(16, '7'), new LrAction(LrAction.Type.Shift, 12) },
                { Tuple.Create(16, '8'), new LrAction(LrAction.Type.Shift, 13) },
                { Tuple.Create(16, '9'), new LrAction(LrAction.Type.Shift, 14) },
                { Tuple.Create(17, Empty()), new LrAction(LrAction.Type.Reduce, 3, "expression") },
                { Tuple.Create(18, '+'), new LrAction(LrAction.Type.Reduce, 3, "factor") },
                { Tuple.Create(18, Empty()), new LrAction(LrAction.Type.Reduce, 3, "factor") },
                
            };
            gotos = new Dictionary<Tuple<int, string>, LrAction>()
            {
                { Tuple.Create(0, "expression"), new LrAction(LrAction.Type.Goto, 4) },
                { Tuple.Create(0, "factor"), new LrAction(LrAction.Type.Goto, 2) },
                { Tuple.Create(0, "statement"), new LrAction(LrAction.Type.Goto, 1) },
                { Tuple.Create(0, "term"), new LrAction(LrAction.Type.Goto, 3) },
                { Tuple.Create(15, "expression"), new LrAction(LrAction.Type.Goto, 17) },
                { Tuple.Create(15, "factor"), new LrAction(LrAction.Type.Goto, 2) },
                { Tuple.Create(15, "term"), new LrAction(LrAction.Type.Goto, 3) },
                { Tuple.Create(16, "factor"), new LrAction(LrAction.Type.Goto, 18) },
                { Tuple.Create(16, "term"), new LrAction(LrAction.Type.Goto, 3) },
                
            };
        }
    }
}
