module AST where

import Types

data Expr
  = Value Value
  | Variable Symb
  | Assign Symb Expr
  | Prim Symb [Expr]
  | Call Symb [Expr]
  | If Expr Expr Expr
  | While Expr Expr
  deriving (Show)
