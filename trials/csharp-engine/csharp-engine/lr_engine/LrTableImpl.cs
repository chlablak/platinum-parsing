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
      
                              gotos.Add(Tuple.Create(0, "E"), new LrAction(LrAction.Type.Goto, 4));
          
        
      
                              gotos.Add(Tuple.Create(0, "F"), new LrAction(LrAction.Type.Goto, 5));
          
        
      
                              gotos.Add(Tuple.Create(0, "S"), new LrAction(LrAction.Type.Goto, 6));
          
        
      
                              gotos.Add(Tuple.Create(0, "T"), new LrAction(LrAction.Type.Goto, 1));
          
        
      
                                            actions.Add(Tuple.Create(0, '('), new LrAction(LrAction.Type.Shift, 2));
            
          
        
      
                                            actions.Add(Tuple.Create(0, 'x'), new LrAction(LrAction.Type.Shift, 3));
            
          
        
      
                                            actions.Add(Tuple.Create(1, ')'), new LrAction(LrAction.Type.Reduce, 1, "E"));
            
          
        
      
                                            actions.Add(Tuple.Create(1, '+'), new LrAction(LrAction.Type.Shift, 7));
            
          
        
      
                                            actions.Add(Tuple.Create(1, EMPTY), new LrAction(LrAction.Type.Reduce, 1, "E"));
            
          
        
      
                              gotos.Add(Tuple.Create(2, "E"), new LrAction(LrAction.Type.Goto, 8));
          
        
      
                              gotos.Add(Tuple.Create(2, "F"), new LrAction(LrAction.Type.Goto, 5));
          
        
      
                              gotos.Add(Tuple.Create(2, "T"), new LrAction(LrAction.Type.Goto, 1));
          
        
      
                                            actions.Add(Tuple.Create(2, '('), new LrAction(LrAction.Type.Shift, 2));
            
          
        
      
                                            actions.Add(Tuple.Create(2, 'x'), new LrAction(LrAction.Type.Shift, 3));
            
          
        
      
                                            actions.Add(Tuple.Create(3, ')'), new LrAction(LrAction.Type.Reduce, 1, "F"));
            
          
        
      
                                            actions.Add(Tuple.Create(3, '*'), new LrAction(LrAction.Type.Reduce, 1, "F"));
            
          
        
      
                                            actions.Add(Tuple.Create(3, '+'), new LrAction(LrAction.Type.Reduce, 1, "F"));
            
          
        
      
                                            actions.Add(Tuple.Create(3, EMPTY), new LrAction(LrAction.Type.Reduce, 1, "F"));
            
          
        
      
                                            actions.Add(Tuple.Create(4, EMPTY), new LrAction(LrAction.Type.Reduce, 1, "S"));
            
          
        
      
                                            actions.Add(Tuple.Create(5, ')'), new LrAction(LrAction.Type.Reduce, 1, "T"));
            
          
        
      
                                            actions.Add(Tuple.Create(5, '*'), new LrAction(LrAction.Type.Shift, 9));
            
          
        
      
                                            actions.Add(Tuple.Create(5, '+'), new LrAction(LrAction.Type.Reduce, 1, "T"));
            
          
        
      
                                            actions.Add(Tuple.Create(5, EMPTY), new LrAction(LrAction.Type.Reduce, 1, "T"));
            
          
        
      
                              
          
        
      
                              gotos.Add(Tuple.Create(7, "E"), new LrAction(LrAction.Type.Goto, 10));
          
        
      
                              gotos.Add(Tuple.Create(7, "F"), new LrAction(LrAction.Type.Goto, 5));
          
        
      
                              gotos.Add(Tuple.Create(7, "T"), new LrAction(LrAction.Type.Goto, 1));
          
        
      
                                            actions.Add(Tuple.Create(7, '('), new LrAction(LrAction.Type.Shift, 2));
            
          
        
      
                                            actions.Add(Tuple.Create(7, 'x'), new LrAction(LrAction.Type.Shift, 3));
            
          
        
      
                                            actions.Add(Tuple.Create(8, ')'), new LrAction(LrAction.Type.Shift, 11));
            
          
        
      
                              gotos.Add(Tuple.Create(9, "F"), new LrAction(LrAction.Type.Goto, 5));
          
        
      
                              gotos.Add(Tuple.Create(9, "T"), new LrAction(LrAction.Type.Goto, 12));
          
        
      
                                            actions.Add(Tuple.Create(9, '('), new LrAction(LrAction.Type.Shift, 2));
            
          
        
      
                                            actions.Add(Tuple.Create(9, 'x'), new LrAction(LrAction.Type.Shift, 3));
            
          
        
      
                                            actions.Add(Tuple.Create(10, ')'), new LrAction(LrAction.Type.Reduce, 3, "E"));
            
          
        
      
                                            actions.Add(Tuple.Create(10, EMPTY), new LrAction(LrAction.Type.Reduce, 3, "E"));
            
          
        
      
                                            actions.Add(Tuple.Create(11, ')'), new LrAction(LrAction.Type.Reduce, 3, "F"));
            
          
        
      
                                            actions.Add(Tuple.Create(11, '*'), new LrAction(LrAction.Type.Reduce, 3, "F"));
            
          
        
      
                                            actions.Add(Tuple.Create(11, '+'), new LrAction(LrAction.Type.Reduce, 3, "F"));
            
          
        
      
                                            actions.Add(Tuple.Create(11, EMPTY), new LrAction(LrAction.Type.Reduce, 3, "F"));
            
          
        
      
                                            actions.Add(Tuple.Create(12, ')'), new LrAction(LrAction.Type.Reduce, 3, "T"));
            
          
        
      
                                            actions.Add(Tuple.Create(12, '+'), new LrAction(LrAction.Type.Reduce, 3, "T"));
            
          
        
      
                                            actions.Add(Tuple.Create(12, EMPTY), new LrAction(LrAction.Type.Reduce, 3, "T"));
            
          
        
      
    }
  }
}
