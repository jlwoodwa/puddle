module Parser where

import SyntaxList

import Text.Parsec

type Parser = Parsec String ()

valpar :: Parser ValPar
valpar = try (char '(' *> (Par <$> linexp) <* char ')') <|> (Val . Num . read) <$> many1 digit <|> (Val . Var) <$> many1 alpha

op :: Parser Op
op = toOp <$> oneOf ops

linexp :: Parser LinearExpr
linexp = try (Cons <$> valpar <*> op <*> linexp) <|> Last <$> valpar

lcode :: Parser LCode
lcode = LAssign <$> many1 alpha <* char '=' <*> linexp <* char ';'

-- utilities

alpha :: Parser Char
alpha = foldr1 (<|>) $ map char $ enumFromTo 'a' 'z'

parseV :: String -> LinearExpr
parseV = handler . parse (linexp <* eof) ""

parseC :: String -> LCode
parseC = handler . parse (lcode <* eof) ""

handler :: Either ParseError a -> a
handler = either (error . show) $ id
