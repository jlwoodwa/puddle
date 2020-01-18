module Main where

import Data.HashMap.Strict

main = print $ evalE empty expression

expression = Math Add (Math Sub (Numb 2.3) (Numb (-2))) (Numb 300)

data Code
  = Numb Double
  | Boolean Bool
  | Ident String
  | Math Op Code Code
  | IfThen Code Code Code
  | Assign String Code
  | Print Code
  deriving (Show)

data Op
  = Add
  | Mult
  | Sub
  | Div
  deriving (Show)

type Env = HashMap String Val

data Val
  = NumbV Double
  | BooleanV Bool
  deriving (Show)

opGen :: (Double -> Double -> Double) -> Val -> Val -> Double
opGen op (NumbV x) (NumbV y) = x `op` y

evalE :: Env -> Code -> Val
evalE _ (Numb x) = NumbV x
evalE _ (Boolean x) = BooleanV x
evalE env (Ident id) = env ! id
evalE env (Math Add x y) = helper env x y $ opGen (+)
evalE env (Math Mult x y) = helper env x y $ opGen (*)
evalE env (Math Sub x y) = helper env x y $ opGen (-)
evalE env (Math Div x y) = helper env x y $ opGen (/)
evalE _ _ = error "invalid expression"

helper env x y op = NumbV $ evalE env x `op` evalE env y
