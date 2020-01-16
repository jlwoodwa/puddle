module Main where

import Control.Applicative hiding (some)
import Data.Either.Combinators
import Data.Void
import Text.Parsec
import Text.Parsec.Char

type Parser = Parsec String ()

main = do
  let x = "2.0"
  print $ parse expr "" x

expr :: Parser String
expr = double <* eof

double :: Parser String
double = perhap minus id (:) <*> many1 digit <**> perhap decimal id (flip (++))

minus :: Parser Char
minus = char '-'

decimal :: Parser String
decimal = char '.' <:> many1 digit

-- Utility functions
(<:>) = liftA2 (:)

perhap parser def op = maybe def op <$> optionMaybe parser
