{-# LANGUAGE BlockArguments #-}

module Main where

import Eval
import Parser
import SyntaxList
import SyntaxTree

import Control.Exception
import Control.Monad.Loops
import Data.List
import Data.Map.Strict hiding (foldl)
import Data.Typeable
import System.IO

main = repl empty

repl env = do
  code <- prompt "- "
  -- support multi-line expressions
  code <-
    if code == ":{"
      then fmap (intercalate "\n") $ unfoldWhileM (/= ":}") $ prompt "$ "
      else return code
  catch
    (do newEnv <- evalSt (enforestSt $ parseSt code) env
        repl newEnv)
    (\e -> do
       let err = show (e :: ErrorCall)
       hPutStrLn stderr $ "Couldn't parse \"" ++ code ++ "\": \n" ++ err
       hPutStrLn stderr $ "Exception type " ++ show (typeOf e)
       repl env)

main' = evalC $ enforestC $ parseC "read x;\nprint x*2;"

-- utilities
prompt x = putStr x >> hFlush stdout >> getLine
