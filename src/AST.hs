module AST where

import Types

data Expr
  = Value Value
  | Variable Symb
  | Assign Symb Expr
  | Call Symb [Expr]
  | Prim Symb [Expr]
  | If Expr Expr Expr
  | While Expr Expr
  | Print Expr
