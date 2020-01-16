module Main where

import Control.Applicative hiding (some)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

main = parseTest double "-2.0"

double :: Parser String
double = perhap minus id (:) <*> some digitChar <**> perhap decimal id (flip (++))

minus = optional $ char '-'

decimal :: Parser (Maybe String)
decimal = optional $ char '.' <:> some digitChar

(<:>) = liftA2 (:)

perhap parser def op = maybe def op <$> parser
