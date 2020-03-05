module Parser where

import AST
import Primitives
import SExprs
import Types

import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T

sexpr2expr :: SExpr -> Expr
sexpr2expr (Slot dat)
  | T.all isDigit dat = Value . Int . readT $ dat
  | dat == "True" = Value . Bool $ True
  | dat == "False" = Value . Bool $ False
  | T.all isAlpha dat = Variable dat
sexpr2expr (Many [Slot "def", Slot var, defn@(Many _)]) =
  Assign var (sexpr2expr defn)
sexpr2expr (Many [Slot "if", bln@(Many _), iftrue@(Many _), iffalse@(Many _)]) =
  If (sexpr2expr bln) (sexpr2expr iftrue) (sexpr2expr iffalse)
sexpr2expr (Many [Slot "while", cond@(Many _), loop@(Many _)]) =
  While (sexpr2expr cond) (sexpr2expr loop)
sexpr2expr (Many (Slot func:args))
  | M.member func primTable = Prim func (map sexpr2expr args)
  | otherwise = Call func (map sexpr2expr args)

readT :: Read a => T.Text -> a
readT = read . T.unpack
