module Main where

import SyntaxList
import SyntaxTree
import Parser

main = print $ enforest $ parseV "2+3"
