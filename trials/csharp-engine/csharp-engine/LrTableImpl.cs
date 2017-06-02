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
        public LrAction Action(int state, char term)
        {
            return actions.TryGetValue(Tuple.Create(state, term), out value) ? value : error;
        }

        /* GOTO table */
        public LrAction Action(int state, string nonTerm)
        {
            return gotos.TryGetValue(Tuple.Create(state, nonTerm), out value) ? value : error;
        }

        public LrTableImpl()
        {
            value = null;
            error = new LrAction(LrAction.Type.Error);
            actions = new Dictionary<Tuple<int, char>, LrAction>()
            {
                { Tuple.Create(0, '('), new LrAction(LrAction.Type.Shift, 14) },
                { Tuple.Create(0, '0'), new LrAction(LrAction.Type.Shift, 2) },
                { Tuple.Create(0, '1'), new LrAction(LrAction.Type.Shift, 3) },
                { Tuple.Create(0, '2'), new LrAction(LrAction.Type.Shift, 4) },
                { Tuple.Create(0, '3'), new LrAction(LrAction.Type.Shift, 5) },
                { Tuple.Create(0, '4'), new LrAction(LrAction.Type.Shift, 6) },
                { Tuple.Create(0, '5'), new LrAction(LrAction.Type.Shift, 7) },
                { Tuple.Create(0, '6'), new LrAction(LrAction.Type.Shift, 8) },
                { Tuple.Create(0, '7'), new LrAction(LrAction.Type.Shift, 9) },
                { Tuple.Create(0, '8'), new LrAction(LrAction.Type.Shift, 10) },
                { Tuple.Create(0, '9'), new LrAction(LrAction.Type.Shift, 11) },
                { Tuple.Create(1, Empty()), new LrAction(LrAction.Type.Accept) },
                { Tuple.Create(2, ')'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(2, '*'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(2, '+'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(2, Empty()), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(3, ')'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(3, '*'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(3, '+'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(3, Empty()), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(4, ')'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(4, '*'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(4, '+'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(4, Empty()), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(5, ')'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(5, '*'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(5, '+'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(5, Empty()), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(6, ')'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(6, '*'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(6, '+'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(6, Empty()), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(7, ')'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(7, '*'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(7, '+'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(7, Empty()), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(8, ')'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(8, '*'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(8, '+'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(8, Empty()), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(9, ')'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(9, '*'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(9, '+'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(9, Empty()), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(10, ')'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(10, '*'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(10, '+'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(10, Empty()), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(11, ')'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(11, '*'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(11, '+'), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(11, Empty()), new LrAction(LrAction.Type.Reduce, 1, "digit") },
                { Tuple.Create(12, ')'), new LrAction(LrAction.Type.Reduce, 1, "expression") },
                { Tuple.Create(12, '+'), new LrAction(LrAction.Type.Shift, 17) },
                { Tuple.Create(12, Empty()), new LrAction(LrAction.Type.Reduce, 1, "expression") },
                { Tuple.Create(13, ')'), new LrAction(LrAction.Type.Reduce, 1, "factor") },
                { Tuple.Create(13, '*'), new LrAction(LrAction.Type.Reduce, 1, "factor") },
                { Tuple.Create(13, '+'), new LrAction(LrAction.Type.Reduce, 1, "factor") },
                { Tuple.Create(13, Empty()), new LrAction(LrAction.Type.Reduce, 1, "factor") },
                { Tuple.Create(14, '('), new LrAction(LrAction.Type.Shift, 14) },
                { Tuple.Create(14, '0'), new LrAction(LrAction.Type.Shift, 2) },
                { Tuple.Create(14, '1'), new LrAction(LrAction.Type.Shift, 3) },
                { Tuple.Create(14, '2'), new LrAction(LrAction.Type.Shift, 4) },
                { Tuple.Create(14, '3'), new LrAction(LrAction.Type.Shift, 5) },
                { Tuple.Create(14, '4'), new LrAction(LrAction.Type.Shift, 6) },
                { Tuple.Create(14, '5'), new LrAction(LrAction.Type.Shift, 7) },
                { Tuple.Create(14, '6'), new LrAction(LrAction.Type.Shift, 8) },
                { Tuple.Create(14, '7'), new LrAction(LrAction.Type.Shift, 9) },
                { Tuple.Create(14, '8'), new LrAction(LrAction.Type.Shift, 10) },
                { Tuple.Create(14, '9'), new LrAction(LrAction.Type.Shift, 11) },
                { Tuple.Create(15, Empty()), new LrAction(LrAction.Type.Reduce, 1, "statement") },
                { Tuple.Create(16, ')'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(16, '*'), new LrAction(LrAction.Type.Shift, 19) },
                { Tuple.Create(16, '+'), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(16, Empty()), new LrAction(LrAction.Type.Reduce, 1, "term") },
                { Tuple.Create(17, '('), new LrAction(LrAction.Type.Shift, 14) },
                { Tuple.Create(17, '0'), new LrAction(LrAction.Type.Shift, 2) },
                { Tuple.Create(17, '1'), new LrAction(LrAction.Type.Shift, 3) },
                { Tuple.Create(17, '2'), new LrAction(LrAction.Type.Shift, 4) },
                { Tuple.Create(17, '3'), new LrAction(LrAction.Type.Shift, 5) },
                { Tuple.Create(17, '4'), new LrAction(LrAction.Type.Shift, 6) },
                { Tuple.Create(17, '5'), new LrAction(LrAction.Type.Shift, 7) },
                { Tuple.Create(17, '6'), new LrAction(LrAction.Type.Shift, 8) },
                { Tuple.Create(17, '7'), new LrAction(LrAction.Type.Shift, 9) },
                { Tuple.Create(17, '8'), new LrAction(LrAction.Type.Shift, 10) },
                { Tuple.Create(17, '9'), new LrAction(LrAction.Type.Shift, 11) },
                { Tuple.Create(18, ')'), new LrAction(LrAction.Type.Shift, 21) },
                { Tuple.Create(19, '('), new LrAction(LrAction.Type.Shift, 14) },
                { Tuple.Create(19, '0'), new LrAction(LrAction.Type.Shift, 2) },
                { Tuple.Create(19, '1'), new LrAction(LrAction.Type.Shift, 3) },
                { Tuple.Create(19, '2'), new LrAction(LrAction.Type.Shift, 4) },
                { Tuple.Create(19, '3'), new LrAction(LrAction.Type.Shift, 5) },
                { Tuple.Create(19, '4'), new LrAction(LrAction.Type.Shift, 6) },
                { Tuple.Create(19, '5'), new LrAction(LrAction.Type.Shift, 7) },
                { Tuple.Create(19, '6'), new LrAction(LrAction.Type.Shift, 8) },
                { Tuple.Create(19, '7'), new LrAction(LrAction.Type.Shift, 9) },
                { Tuple.Create(19, '8'), new LrAction(LrAction.Type.Shift, 10) },
                { Tuple.Create(19, '9'), new LrAction(LrAction.Type.Shift, 11) },
                { Tuple.Create(20, ')'), new LrAction(LrAction.Type.Reduce, 3, "expression") },
                { Tuple.Create(20, Empty()), new LrAction(LrAction.Type.Reduce, 3, "expression") },
                { Tuple.Create(21, ')'), new LrAction(LrAction.Type.Reduce, 3, "factor") },
                { Tuple.Create(21, '*'), new LrAction(LrAction.Type.Reduce, 3, "factor") },
                { Tuple.Create(21, '+'), new LrAction(LrAction.Type.Reduce, 3, "factor") },
                { Tuple.Create(21, Empty()), new LrAction(LrAction.Type.Reduce, 3, "factor") },
                { Tuple.Create(22, ')'), new LrAction(LrAction.Type.Reduce, 3, "term") },
                { Tuple.Create(22, '+'), new LrAction(LrAction.Type.Reduce, 3, "term") },
                { Tuple.Create(22, Empty()), new LrAction(LrAction.Type.Reduce, 3, "term") },

            };
            gotos = new Dictionary<Tuple<int, string>, LrAction>()
            {
                { Tuple.Create(0, "digit"), new LrAction(LrAction.Type.Goto, 13) },
                { Tuple.Create(0, "expression"), new LrAction(LrAction.Type.Goto, 15) },
                { Tuple.Create(0, "factor"), new LrAction(LrAction.Type.Goto, 16) },
                { Tuple.Create(0, "statement"), new LrAction(LrAction.Type.Goto, 1) },
                { Tuple.Create(0, "term"), new LrAction(LrAction.Type.Goto, 12) },
                { Tuple.Create(14, "digit"), new LrAction(LrAction.Type.Goto, 13) },
                { Tuple.Create(14, "expression"), new LrAction(LrAction.Type.Goto, 18) },
                { Tuple.Create(14, "factor"), new LrAction(LrAction.Type.Goto, 16) },
                { Tuple.Create(14, "term"), new LrAction(LrAction.Type.Goto, 12) },
                { Tuple.Create(17, "digit"), new LrAction(LrAction.Type.Goto, 13) },
                { Tuple.Create(17, "expression"), new LrAction(LrAction.Type.Goto, 20) },
                { Tuple.Create(17, "factor"), new LrAction(LrAction.Type.Goto, 16) },
                { Tuple.Create(17, "term"), new LrAction(LrAction.Type.Goto, 12) },
                { Tuple.Create(19, "digit"), new LrAction(LrAction.Type.Goto, 13) },
                { Tuple.Create(19, "factor"), new LrAction(LrAction.Type.Goto, 16) },
                { Tuple.Create(19, "term"), new LrAction(LrAction.Type.Goto, 22) },

            };
        }
    }
}
