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
                $lr.table.rows:{row|$if(row.isTerm)$$if(row.term.isEmpty)$$if(row.action.isReduce)${ Tuple.Create($row.state.id$, Empty()), new LrAction(LrAction.Type.Reduce, $row.action.reduce.length$, "$row.action.reduce.name$") },
                $elseif(row.action.isShift)${ Tuple.Create($row.state.id$, Empty()), new LrAction(LrAction.Type.Shift, $row.action.shift$) },
                $endif$$else$$if(row.action.isReduce)${ Tuple.Create($row.state.id$, '$row.term.symbol$'), new LrAction(LrAction.Type.Reduce, $row.action.reduce.length$, "$row.action.reduce.name$") },
                $elseif(row.action.isShift)${ Tuple.Create($row.state.id$, '$row.term.symbol$'), new LrAction(LrAction.Type.Shift, $row.action.shift$) },
                $elseif(row.action.isAccept)${ Tuple.Create($row.state.id$, '$row.term.symbol$'), new LrAction(LrAction.Type.Accept) },
                $endif$$endif$$endif$}$
            };
            gotos = new Dictionary<Tuple<int, string>, LrAction>()
            {
                $lr.table.rows:{row|$if(!row.isTerm)$$if(row.action.isGoto)${ Tuple.Create($row.state.id$, "$row.nonTerm.name$"), new LrAction(LrAction.Type.Goto, $row.action.goto$) },
                $endif$$endif$}$
            };
        }
    }
}
