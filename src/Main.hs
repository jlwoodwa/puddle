module Main where

import Control.Applicative hiding ((<|>), some)
import Data.Maybe
import Data.Void
import Text.Parsec
import Text.Parsec.Char

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
toOp = fromJust . flip lookup [('+', Add), ('*', Mult), ('-', Sub), ('/', Div)]

main = do
  let x = "-2.0+3"
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
double = char '-' ?: many1 digit <? (char '.' <:> many1 digit)

-- Utility functions
(<:>) = liftA2 (:)

-- The "optionality" parser combinators
(?>) :: Semigroup a => Parser a -> Parser a -> Parser a
mx ?> my = try (mx <> my) <|> my

(?:) :: Parser a -> Parser [a] -> Parser [a]
mx ?: mys = try (mx <:> mys) <|> mys

(<?) :: Semigroup a => Parser a -> Parser a -> Parser a
mx <? my = try (mx <> my) <|> mx
