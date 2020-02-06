module Main where

import SyntaxList
import SyntaxTree
import Parser
import Eval

main = print $ evalV $ enforest $ parseV "2*3"
