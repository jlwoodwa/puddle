module Parser where

import AST
import Primitives
import Types

import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

expr :: Parser Expr
expr = choice [value, variable, assign, prim, call, if', while']

value :: Parser Expr
value = Value <$> choice [bool, int]

bool :: Parser Value
bool = Bool . read <$> choice (map keyword ["True", "False"])

int :: Parser Value
int = Int . read <$> key (many1 digit)

variable :: Parser Expr
variable = Variable <$> symb

symb :: Parser Text
symb = T.pack <$> many1 letter

assign :: Parser Expr
assign =
  parens $ do
    try $ keyword "def"
    var <- parens symb
    spaces
    defn <- parens expr
    return $ Assign var defn

prim :: Parser Expr
prim =
  parens
    (try (justIfIn primTable . T.pack <$> tok) >>= \case
       Just f -> spaces >> Prim f <$> sepBy1 (parens expr) spaces
       Nothing -> unexpected "")

call :: Parser Expr
call = undefined

if' :: Parser Expr
if' = undefined

while' :: Parser Expr
while' = undefined

key :: Parser a -> Parser a
key x = try $ x <* notFollowedBy alphaNum

tok :: Parser String
tok = key $ many1 alphaNum

keyword :: String -> Parser String
keyword = key . string

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

justIfIn :: Ord a => M.Map a b -> a -> Maybe a
justIfIn table x = (guard $ M.member x table) >> Just x
