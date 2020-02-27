module Eval where

import SyntaxList
import SyntaxTree

import Data.Map.Strict hiding (foldl, foldl', map)

type Env = Map String Val

evalSt :: TStatement -> Env -> IO Env
evalSt (TAssign v e) env = return $ insert v (evalV env e) env
evalSt (TPrint e) env = print (evalV env e) >> return env
evalSt (TRead v) env =
  putStrLn ("enter " ++ show v) >> (readLn :: IO Int) >>= \x ->
    return $ insert v (IntV x) env

evalC :: TCode -> Env -> IO Env
evalC code env = foldl (>>=) (return env) $ map evalSt code

evalV :: Env -> TreeExpr -> Val
evalV env (Node op x y) = evalO op (evalV env x) (evalV env y)
evalV _ (Leaf (Num x)) = IntV x
evalV env (Leaf (Var x)) = env ! x

evalO :: Op -> Val -> Val -> Val -- Note: this function is clunky
evalO Add (IntV x) (IntV y) = IntV $ x + y
evalO Mult (IntV x) (IntV y) = IntV $ x * y
evalO Leq (IntV x) (IntV y) = IntV . b2i $ x <= y

newtype Val =
  IntV Int
  deriving (Show, Read)

-- Utilities
b2i :: Bool -> Int
b2i True = 1
b2i False = 0
