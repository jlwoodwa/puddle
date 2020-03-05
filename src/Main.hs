module Main where

import AST
import Evaluation
import SExprs
import Types

import Control.Monad.Catch (catch)
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State.Strict
import Data.Either.Combinators
import Data.List
import Data.Map.Strict
import Parser
import System.IO
import Text.Parsec

main = void $ runExceptT (runStateT repl $ Env empty empty)

repl = do
  code' <- liftIO $ prompt "- "
  code <-
    if code' == ":{"
      then fmap (intercalate "\n") $
           unfoldWhileM (/= ":}") $ liftIO $ prompt "$ "
      else return code'
  catch (evaluate undefined) undefined

prompt x = putStr x >> hFlush stdout >> getLine
--main :: IO ()
--main =
--  wrap (Env {_varTable = singleton "x" (Int 5), _funTable = empty}) $
--  sexpr2expr $ fromRight' $ parse sexpr "" "(print x)"
--
--
--main' :: IO ()
--main' =
--  wrap (Env {_varTable = singleton "x" (Int 5), _funTable = empty}) $
--  Prim "print" [Prim "+" [Variable "x", Variable "x"]]
--
--wrap :: Env -> Expr -> IO ()
--wrap environ express =
--  runExceptT (evalStateT (runPuddle (evaluate express)) environ) >>=
--  either print print
