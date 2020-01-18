module Main where

import Data.HashMap.Strict

main = print $ evalE empty expression

expression = Math Eq (Math Add (Math Sub (Numb 2.3) (Numb (-2))) (Numb 300)) (Numb 304.3)

data Expr
  = Numb Double
  | Boolean Bool
  | Ident String
  | Math Op Expr Expr
  deriving (Show)

data Code
  = Assign String Expr
  | IfThen Expr Code Code
  | Print Code
  deriving (Show)

data Op
  = Add
  | Mult
  | Sub
  | Div
  | Eq
  | Leq
  | Nand
  deriving (Show)

type Env = HashMap String Val

data Val
  = NumbV Double
  | BooleanV Bool
  deriving (Show)

evalE :: Env -> Expr -> Val
evalE _ (Numb x) = NumbV x
evalE _ (Boolean x) = BooleanV x
evalE env (Ident id) = env ! id
evalE env (Math Add x y) = helperN env x y $ dopGen (+)
evalE env (Math Mult x y) = helperN env x y $ dopGen (*)
evalE env (Math Sub x y) = helperN env x y $ dopGen (-)
evalE env (Math Div x y) = helperN env x y $ dopGen (/)
evalE env (Math Eq x y) = helperB env x y $ copGen (==)
evalE env (Math Leq x y) = helperB env x y $ copGen (<=)
evalE env (Math Nand x y) = helperB env x y $ bopGen nand

helperN env x y op = NumbV $ evalE env x `op` evalE env y

helperB env x y op = BooleanV $ evalE env x `op` evalE env y

dopGen :: (Double -> Double -> Double) -> Val -> Val -> Double
dopGen op (NumbV x) (NumbV y) = x `op` y

bopGen :: (Bool -> Bool -> Bool) -> Val -> Val -> Bool
bopGen op (BooleanV x) (BooleanV y) = x `op` y

copGen :: (Double -> Double -> Bool) -> Val -> Val -> Bool
copGen op (NumbV x) (NumbV y) = x `op` y

nand :: Bool -> Bool -> Bool
nand x y =
  if x
    then not y
    else y
