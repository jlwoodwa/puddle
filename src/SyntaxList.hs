module SyntaxList where

data LStatement
  = LAssign String LinearExpr
  | LPrint LinearExpr
  | LRead String
  deriving (Show)

type LCode = [LStatement]

data LinearExpr
  = Cons ValPar Op LinearExpr
  | Last ValPar
  deriving (Show)

snoc :: (Op, ValPar) -> LinearExpr -> LinearExpr
snoc x (Cons vp o xs) = Cons vp o (snoc x xs)
snoc (o, vp2) (Last vp1) = Cons vp1 o (Last vp2)

data ValPar
  = Val Symb
  | Par LinearExpr
  deriving (Show)

data Op
  = Add
  | Mult
  | Leq
  deriving (Show)

toOp "+" = Add
toOp "*" = Mult
toOp "<=" = Leq

ops = ["+", "*", "<="]

binding :: Op -> Int
binding Add = 0
binding Mult = 1

maxOpBinding :: LinearExpr -> Int
maxOpBinding (Cons _ o1 (Cons _ o2 xs)) =
  max (binding o1) $ max (binding o2) (maxOpBinding xs)
maxOpBinding (Cons _ o _) = binding o

geqs :: Op -> LinearExpr -> Bool
geqs o (Last _) = True
geqs o x = binding o >= maxOpBinding x

data Symb
  = Num Int
  | Var String
  deriving (Show)

linearExample =
  Cons (Val $ Num 5) Mult $ Cons (Val $ Num 2) Add (Last $ Val $ Num 3)
