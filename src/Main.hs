module Main where

import AST
import Evaluation
import SExprs
import Types

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Either.Combinators
import Data.Map.Strict
import Parser
import Text.Parsec

main :: IO ()
main =
  wrap (Env {_varTable = singleton "x" (Int 5), _funTable = empty}) $
  sexpr2expr $ fromRight' $ parse sexpr "" "(print x)"

--
--main' :: IO ()
--main' =
--  wrap (Env {_varTable = singleton "x" (Int 5), _funTable = empty}) $
--  Prim "print" [Prim "+" [Variable "x", Variable "x"]]
--
wrap :: Env -> Expr -> IO ()
wrap environ express =
  runExceptT (evalStateT (runPuddle (evaluate express)) environ) >>=
  either print print
