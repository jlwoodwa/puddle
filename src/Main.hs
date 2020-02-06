module Main where

import SyntaxList
import SyntaxTree
import Parser
import Eval

import Data.Map.Strict hiding (foldl)

main = repl empty

repl env = do
         code <- getLine
	 newEnv <- evalSt (enforestSt $ parseSt code) env
	 repl newEnv

main' = evalC $ enforestC $ parseC "read x;\nprint x*2;"
