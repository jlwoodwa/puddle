module Main where

import SyntaxList
import SyntaxTree
import Parser
import Eval

import Data.Map.Strict hiding (foldl)

--main' = foldl (>>=) (return empty)

main = evalC $ enforestC $ parseC "read x;\nprint x*2;"
