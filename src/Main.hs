module Main where

import AST
import Evaluation
import Types

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict

main :: IO ()
main =
  runExceptT
    (evalStateT
       (runPuddle (evaluate (Print (Prim "+" [Variable "x", Variable "x"]))))
       Env {_varTable = singleton "x" (Int 5), _funTable = empty}) >>=
  either print print
