module Main where

import Control.Applicative hiding (some)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

main = parseTest double "-2"

double :: Parser String
double = perhap minus (:) <*> some digitChar

minus = optional $ char '-'

perhap parser op = maybe id op <$> parser
