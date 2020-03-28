{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative.Tools
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State.Strict
import Data.Either.Combinators
import Data.Function ((&))
import Data.List
import Data.Map.Strict
import Data.Text (pack)
import Evaluation
import Parser
import SExprs
import System.IO
import Text.Parsec
import Types

main :: IO ()
main = void $ runExceptT $ runStateT (runPuddle repl) $ Env empty empty

repl :: Puddle void
repl = do
  code' <- liftIO $ prompt "- "
  code <-
    if code' == ":{"
      then intercalate "\n" <.> unfoldWhileM (/= ":}") . liftIO $ prompt "$ "
      else return code'
  liftIO . print =<< case parse sexpr "" (pack code) of
    Right expression -> evaluate $ sexpr2expr $ expression
    Left _ -> undefined
  repl

prompt :: String -> IO String
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
