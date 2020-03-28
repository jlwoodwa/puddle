{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative.Tools
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State.Strict
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
main = void $ runExceptT $ runStateT (runPuddle repl) $ Env [empty] empty

repl :: Puddle void
repl =
  liftIO (prompt "- ")
    >>= ( \case
            ":{" -> intercalate "\n" <.> unfoldWhileM (/= ":}") . liftIO $ prompt "$ "
            x -> return x
        )
    >>= ( \code -> case parse sexpr "" (pack code) of
            Right expression ->
              catchError
                (evaluate (sexpr2expr expression) >>= liftIO . print)
                (liftIO . print)
            Left x -> liftIO . print $ x
        )
      >> repl

prompt :: String -> IO String
prompt x = putStr x >> hFlush stdout >> getLine
