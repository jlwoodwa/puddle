module Main where

import Control.Applicative hiding (some, (<|>))
import Data.Either.Combinators
import Data.Void
import Text.Parsec
import Text.Parsec.Char
import Data.Maybe

type Parser = Parsec String ()

data Math = Numb Double | Expr Math Op Math deriving (Show)
data Op = Add | Mult | Sub | Div deriving (Show)

toOp :: Char -> Op
toOp = fromJust . flip lookup [('+', Add), ('*', Mult), ('-', Sub), ('/', Div)]

main = do
  let x = "2.0+3"
  print $ parse (math <* eof) "" x

numb :: Parser Math
numb = Numb . read <$> double

op :: Parser Op
op = toOp <$> oneOf "+*-/"

expr :: Parser Math
expr = Expr <$> math <*> op <*> math

math :: Parser Math
math = try numb <|> expr

double :: Parser String
double = perhap minus id (:) <*> many1 digit <**> perhap decimal id (flip (++))

minus :: Parser Char
minus = char '-'

decimal :: Parser String
decimal = char '.' <:> many1 digit

-- Utility functions
(<:>) = liftA2 (:)

perhap parser def op = maybe def op <$> optionMaybe parser
