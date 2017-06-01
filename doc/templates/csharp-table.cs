/**
 * LR table template for platinum-parsing
 * Author: Patrick Champion
 * Date: 2017.06.01
 */

using System;
using System.Collections.Generic;
using PP.Engines.Lr;

namespace /* INSERT NAMESPACE */
{
  class LrTableImpl : LrTable
  {
    public static const char EMPTY = 0;
    public static const LrAction ERROR = new LrAction(LrAction.Type.Error);

    private Dictionary<Tuple<int, char>, LrAction> actions;
    private Dictionary<Tuple<int, string>, LrAction> gotos;

    /* ACTION table */
    public override LrAction action(int state, char term)
    {
      return actions.TryGetValue(Tuple.Create(state, term), ERROR);
    }

    /* GOTO table */
    public override LrAction action(int state, string nonTerm)
    {
      return gotos.TryGetValue(Tuple.Create(state, nonTerm), ERROR);
    }

    public LrTableImpl()
    {
      /* Generated from Template */
      /* ----------------------- */
      $lr.table.rows:{row|
        $if(row.isTerm)$
          $if(row.term.isEmpty)$
            $if(row.action.isReduce)$
              actions.Add(Tuple.Create($row.state.id$, EMPTY), new LrAction(LrAction.Type.Reduce, $row.action.reduce.length$, "$row.action.reduce.name$"));
            $elseif(row.action.isShift)$
              actions.Add(Tuple.Create($row.state.id$, EMPTY), new LrAction(LrAction.Type.Shift, $row.action.shift$));
            $endif$
          $else$
            $if(row.action.isReduce)$
              actions.Add(Tuple.Create($row.state.id$, '$row.term.symbol$'), new LrAction(LrAction.Type.Reduce, $row.action.reduce.length$, "$row.action.reduce.name$"));
            $elseif(row.action.isShift)$
              actions.Add(Tuple.Create($row.state.id$, '$row.term.symbol$'), new LrAction(LrAction.Type.Shift, $row.action.shift$));
            $elseif(row.action.isAccept)$
              actions.Add(Tuple.Create($row.state.id$, '$row.term.symbol$'), new LrAction(LrAction.Type.Accept));
            $endif$
          $endif$
        $else$
          $if(row.action.isGoto)$
            gotos.Add(Tuple.Create($row.state.id$, "$row.nonTerm.name$"), new LrAction(LrAction.Type.Goto, $row.action.goto$));
          $endif$
        $endif$
      }$
    }
  }
}
