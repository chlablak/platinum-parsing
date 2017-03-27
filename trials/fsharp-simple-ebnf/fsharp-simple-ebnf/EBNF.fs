module EBNF

open FParsec

type TOperator = 
      TConcatenation 
    | TAlternation 
 
type TExpression = 
      TTerminal of string 
    | TRule of string 
    | TBinary of TExpression * TOperator * TExpression 
 
type TStatement = 
      TDefinition of TExpression * TExpression 
    | TStatements of TStatement list 
