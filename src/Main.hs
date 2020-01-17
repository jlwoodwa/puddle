module Main where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Either.Extra
import Data.Maybe
import Data.Void
import Text.Parsec
import Text.Parsec.Char

eval :: Math -> Double
eval (Numb x) = x
eval (Expr a Add b) = eval a + eval b
eval (Expr a Mult b) = eval a * eval b
eval (Expr a Sub b) = eval a - eval b
eval (Expr a Div b) = eval a / eval b

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
  forever $ do
    x <- getLine
    print . eval . handler $ parse (math <* eof) "" x

handler :: Either ParseError Math -> Math
handler (Right x) = x
handler (Left e) = error $ show e

numb :: Parser Math
numb = Numb . read <$> double

op :: Parser Op
op = toOp <$> oneOf "+*-/"

expr1 :: Parser Math
expr1 = wrapSpaces $ try (Expr <$> expr2 <*> fmap toOp (oneOf "+-") <*> expr1) <|> expr2

expr2 :: Parser Math
expr2 = wrapSpaces $ try (Expr <$> expr3 <*> fmap toOp (oneOf "/*") <*> expr2) <|> expr3

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

-- The "optionality" parser combinators
(?>) :: Semigroup a => Parser a -> Parser a -> Parser a
mx ?> my = try (mx <> my) <|> my

(?:) :: Parser a -> Parser [a] -> Parser [a]
mx ?: mys = try (mx <:> mys) <|> mys

(<?) :: Semigroup a => Parser a -> Parser a -> Parser a
mx <? my = try (mx <> my) <|> mx
