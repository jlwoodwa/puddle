module Main where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Either.Extra
import Data.Maybe
import Data.Void
import Text.Parsec
import Text.Parsec.Char

main = do
  let x = "2.3+3.2" -- "if 2 = 3 then 1 else 0"
  print . evalC . handler $ parse (code <* eof) "" x

type Parser = Parsec String ()

-- code interpreter
data Code
  = Val Value
  | Decision Code Code Code
  deriving (Show)

data Value
  = Numerical Math
  | Boolean Bool
  deriving (Show)

evalC :: Code -> Value
evalC (Val x) = x
evalC (Decision (Val (Boolean b)) x y) =
  if b
    then evalC x
    else evalC y
evalC (Decision _ _ _) = error "invalid decision structure"

code :: Parser Code
code = Val . Numerical <$> math

-- math interpreter
evalM :: Math -> Double
evalM (Numb x) = x
evalM (Expr a Add b) = evalM a + evalM b
evalM (Expr a Mult b) = evalM a * evalM b
evalM (Expr a Sub b) = evalM a - evalM b
evalM (Expr a Div b) = evalM a / evalM b

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

numb :: Parser Math
numb = Numb . read <$> double

op :: Parser Op
op = toOp <$> oneOf "+*-/"

expr1 :: Parser Math
expr1 =
  wrapSpaces $
  try (Expr <$> expr2 <*> fmap toOp (oneOf "+-") <*> expr1) <|> expr2

expr2 :: Parser Math
expr2 =
  wrapSpaces $
  try (Expr <$> expr3 <*> fmap toOp (oneOf "/*") <*> expr2) <|> expr3

expr3 :: Parser Math
expr3 = wrapSpaces $ try (char '(' *> expr1 <* char ')') <|> numb

math :: Parser Math
math = expr1

double :: Parser String
double = char '-' ?: many1 digit <? (char '.' <:> many1 digit)

-- Utility functions
(<:>) = liftA2 (:)

wrapSpaces :: Parser a -> Parser a
wrapSpaces x = spaces *> x <* spaces

handler :: Either ParseError a -> a
handler (Right x) = x
handler (Left e) = error $ show e

-- The "optionality" parser combinators
(?>) :: Semigroup a => Parser a -> Parser a -> Parser a
mx ?> my = try (mx <> my) <|> my

(?:) :: Parser a -> Parser [a] -> Parser [a]
mx ?: mys = try (mx <:> mys) <|> mys

(<?) :: Semigroup a => Parser a -> Parser a -> Parser a
mx <? my = try (mx <> my) <|> mx
