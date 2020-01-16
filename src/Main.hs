module Main where

import Control.Applicative hiding ((<|>), some)
import Data.Either.Combinators
import Data.Maybe
import Data.Void
import Text.Parsec
import Text.Parsec.Char
import Data.Function

import Debug.Trace

type Parser = Parsec String ()

data Math
  = Numb Double
  | Expr Math Op Math
  deriving (Show)

data Op
  = Add
  | Mult
  | Sub
  | Div
  deriving (Show)

toOp :: Char -> Op
toOp = fromJust . flip lookup [('+', Add), ('*', Mult), ('-', Sub), ('/', Div)] &* "converting to Op"

main = do
  let x = "2.0+3"
  print $ parse (math <* eof) "" x

numb :: Parser Math
numb = Numb . read <$> double &* "getting numb"

op :: Parser Op
op = toOp <$> oneOf "+*-/" &* "getting op"

expr :: Parser Math
expr = Expr <$> math <*> op <*> math &* "getting expr"

math :: Parser Math
math = try numb <|> expr &* "getting math"

double :: Parser String
double = perhap minus id (:) <*> many1 digit <**> perhap decimal id (flip (++)) &* "getting double"

minus :: Parser Char
minus = char '-' &* "getting minus"

decimal :: Parser String
decimal = char '.' <:> many1 digit &* "getting decimal"

infixl 0 &*
a &* b = a & trace b

-- Utility functions
(<:>) = liftA2 (:)

perhap parser def op = maybe def op <$> optionMaybe parser
