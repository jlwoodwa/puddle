{-# LANGUAGE BlockArguments #-}

module OldMain where

import Eval
import Parser
import SyntaxTree

import Control.Exception
import Control.Monad.Loops (unfoldWhileM)
import Data.List
import Data.Map.Strict hiding (foldl)
import Data.Typeable
import System.IO

main :: IO ()
main = repl empty

repl :: Env -> IO ()
repl env = do
  code' <- prompt "- "
  -- support multi-line expressions
  code <-
    if code' == ":{"
      then fmap (intercalate "\n") $ unfoldWhileM (/= ":}") $ prompt "$ "
      else return code'
  catch
    (do newEnv <- evalSt (enforestSt $ parseSt code) env
        repl newEnv)
    (\e -> do
       let err = show (e :: ErrorCall)
       hPutStrLn stderr $ "Couldn't parse \"" ++ code ++ "\": \n" ++ err
       hPutStrLn stderr $ "Exception type " ++ show (typeOf e)
       repl env)

main' :: Env -> IO Env
main' = evalC $ enforestC $ parseC "read x;\nprint x*2;"

-- utilities
prompt :: String -> IO String
prompt x = putStr x >> hFlush stdout >> getLine
