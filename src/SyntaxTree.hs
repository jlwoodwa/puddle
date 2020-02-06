module SyntaxTree where

import SyntaxList

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
