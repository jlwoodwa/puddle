module Eval where

import SyntaxTree
import SyntaxList

evalV :: TreeExpr -> Val
evalV (Node op x y) = (evalO op) (evalV x) (evalV y)
evalV (Leaf (Num x)) = IntV x
evalV (Leaf (Var x)) = error "haven't yet implemented variables as values"

evalO :: Op -> Val -> Val -> Val -- Note: this function is clunky
evalO Add (IntV x) (IntV y) = IntV (x + y)
evalO Mult (IntV x) (IntV y) = IntV (x * y)

newtype Val = IntV Int deriving Show
