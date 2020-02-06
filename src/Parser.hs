module Parser where

data LinearExpr = Cons (ValPar, Op) LinearExpr | Last ValPar deriving Show
data ValPar = Val Symb | Par LinearExpr deriving Show

data Op = Add | Mult deriving Show
binding :: Op -> Int
binding Add = 0
binding Mult = 1

data Symb = Num Int | Var String deriving Show

data TreeExpr = Node Op TreeExpr TreeExpr | Leaf Symb deriving Show

enforest :: LinearExpr -> TreeExpr
enforest (Cons (v1, o) (Last v2)) = Node o (vp2tree v1) (vp2tree v2)

vp2tree :: ValPar -> TreeExpr
vp2tree (Val v) = Leaf v
vp2tree (Par x) = enforest x

linearExample = Cons (Val $ Num 2, Add) (Last $ Val $ Num 3)
