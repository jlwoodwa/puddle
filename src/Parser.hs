module Parser where

import SyntaxList

import Text.Parsec

type Parser = Parsec String ()

valpar :: Parser ValPar
valpar =
  wrapSpaces $
  try (char '(' *> (Par <$> linexp) <* char ')') <|>
  Val . Num . read <$> many1 digit <|>
  Val . Var <$> many1 alpha

op :: Parser Op
op = wrapSpaces $ toOp <$> foldr1 (<|>) (map (try . string) ops)

linexp :: Parser LinearExpr
linexp = try (Cons <$> valpar <*> op <*> linexp) <|> Last <$> valpar

lstatement :: Parser LStatement
lstatement =
  try (LAssign <$> many1 alpha <* (spaces >> char '=') <*> linexp <* char ';') <|>
  try (LPrint <$> (string "print " >> spaces *> linexp <* char ';')) <|>
  (LRead <$> (string "read " >> spaces *> many1 alpha <* char ';'))

lcode :: Parser LCode
lcode = sepBy1 lstatement $ char '\n'

-- utilities
alpha :: Parser Char
alpha = foldr1 (<|>) $ map char $ enumFromTo 'a' 'z'

parseV :: String -> LinearExpr
parseV = handler . parse (linexp <* eof) ""

parseC :: String -> LCode
parseC = handler . parse (lcode <* eof) ""

parseSt :: String -> LStatement
parseSt = handler . parse lstatement ""

handler :: Either ParseError a -> a
handler = either (error . show) id

wrapSpaces :: Parser a -> Parser a
wrapSpaces = (<* spaces) . (spaces *>)
