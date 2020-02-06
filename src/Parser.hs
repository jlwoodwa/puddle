module Parser where

data LinearExpr = Cons ValPar Op LinearExpr | Last ValPar deriving (Show)

snoc :: (Op, ValPar) -> LinearExpr -> LinearExpr
snoc x (Cons vp o xs) = Cons vp o (snoc x xs)
snoc (o, vp2) (Last vp1) = Cons vp1 o (Last vp2)

data ValPar = Val Symb | Par LinearExpr deriving Show

data Op = Add | Mult deriving Show

binding :: Op -> Int
binding Add = 0
binding Mult = 1

data Symb = Num Int | Var String deriving Show
