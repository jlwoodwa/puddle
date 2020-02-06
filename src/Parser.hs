module Parser where

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
  deriving (Show)

binding :: Op -> Int
binding Add = 0
binding Mult = 1

data Symb
  = Num Int
  | Var String
  deriving (Show)

data TreeExpr
  = Node Op TreeExpr TreeExpr
  | Leaf Symb
  deriving (Show)

enforest :: LinearExpr -> TreeExpr
enforest (Last (Val symb)) = Leaf symb
enforest (Last (Par e)) = enforest e
enforest (Cons vp1 o (Last vp2)) = Node o (vp2tree vp1) (vp2tree vp2)
enforest (Cons vp o x) = enforestWalk (Last vp) o x

enforestWalk :: LinearExpr -> Op -> LinearExpr -> TreeExpr
enforestWalk e1 o e2@(Last vp) = Node o (enforest e1) (enforest e2)
enforestWalk e1 o e2@(Cons vp o' e3) =
  if o `geqs` e1 && o `geqs` e2
    then Node o (enforest e1) (enforest e2)
    else enforestWalk (snoc (o, vp) e1) o' e3

vp2tree :: ValPar -> TreeExpr
vp2tree (Val v) = Leaf v
vp2tree (Par x) = enforest x

maxOpBinding :: LinearExpr -> Int
maxOpBinding (Cons _ o1 (Cons _ o2 xs)) =
  max (binding o1) $ max (binding o2) (maxOpBinding xs)
maxOpBinding (Cons _ o _) = binding o

geqs :: Op -> LinearExpr -> Bool
geqs o (Last _) = True
geqs o x = binding o >= maxOpBinding x

linearExample = Cons (Val $ Num 5) Mult $ Cons (Val $ Num 2) Add (Last $ Val $ Num 3)
