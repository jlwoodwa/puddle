module SExprs where

import Data.Char
import Data.Text (Text, pack)
import Text.Parsec
import Text.Parsec.Text

data SExpr
  = Slot Text
  | Many [SExpr]
  deriving (Show)

validChar :: Char -> Bool
validChar c = isAlphaNum c || elem c ("!#$%&|*+-/:<=>?@^_~" :: String)

ptoken :: Parser Text
ptoken = pack <$> many1 (satisfy validChar)

manyS :: Parser SExpr
manyS =
  char '(' *> (Many <$> sepBy1 sexpr (pack <$> many1 space)) <* char ')'

sexpr :: Parser SExpr
sexpr = manyS <|> (Slot <$> ptoken)
