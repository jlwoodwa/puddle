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

-- I've figured out why this doesn't work.
--
-- `try numb` doesn't actually fail on an expression - it reads the first number
-- successfully. Only after that does the entire `math` parser fail on the
-- unexpected operator.
--
-- But I can't flip `numb` and `expr`, because that leads to infinite recursion
-- without end.
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
