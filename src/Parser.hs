module Parser where

import SyntaxList

import Text.Parsec

type Parser = Parsec String ()

valpar :: Parser ValPar
valpar = try (char '(' *> (Par <$> linexp) <* char ')') <|> (Val . Num . read ) <$> many1 digit

op :: Parser Op
op = toOp <$> oneOf ops

linexp :: Parser LinearExpr
linexp = try (Cons <$> valpar <*> op <*> linexp) <|> Last <$> valpar

-- utilities

parseV :: String -> LinearExpr
parseV = handler . parse (linexp <* eof) ""

handler :: Either ParseError a -> a
handler = either (error . show) $ id

