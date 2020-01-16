module Main where

import Control.Applicative hiding (optional, some)
import Data.Void
import Text.Parsec
import Text.Parsec.Char

type Parser = Parsec String ()

main = parseTest double "-2.0"

double :: Parser String
double = perhap minus id (:) <*> many1 digit <**> perhap decimal id (flip (++))

minus :: Parser (Maybe Char)
minus = optionMaybe $ char '-'

decimal :: Parser (Maybe String)
decimal = optionMaybe $ char '.' <:> many1 digit

-- Utility functions
(<:>) = liftA2 (:)

perhap parser def op = maybe def op <$> parser
