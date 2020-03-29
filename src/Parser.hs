module Parser where

import AST
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import Primitives
import SExprs
import Types

sexpr2expr :: SExpr -> Expr
sexpr2expr = \case
  Slot dat
    | T.all isDigit dat -> Value . Int . readT $ dat
    | dat == "True" -> Value . Bool $ True
    | dat == "False" -> Value . Bool $ False
    | T.all isAlpha dat -> Variable dat
    | otherwise -> error "unrecognized form"
  Many [Slot "def", Slot var, defn]
    | T.all isAlpha var -> Assign var $ sexpr2expr defn
    | otherwise -> error "invalid variable name"
  Many (Slot "def" : _) -> error "unrecognized definition"
  Many [Slot "if", bln, iftrue, iffalse] -> If (sexpr2expr bln) (sexpr2expr iftrue) (sexpr2expr iffalse)
  Many (Slot "if" : _) -> error "unrecognized if statement"
  Many [Slot "while", cond, loop] -> While (sexpr2expr cond) (sexpr2expr loop)
  Many (Slot "while" : _) -> error "unrecognized while loop"
  Many [Slot "fun", Many (Slot f : params), body]
    | not $ all (\case Slot _ -> True; _ -> False) params -> error "can't bind to non-variable names"
    | not $ T.all isAlpha f -> error "invalid function name (declaration)"
    | not $ and $ map (T.all isAlpha . \case Slot x -> x) $ params -> error "invalid parameter name"
    | otherwise -> Fun f (map (\case Slot x -> x) params) (sexpr2expr body)
  Many (Slot "fun" : _) -> error "unrecognized function definition"
  Many (Slot func : args)
    | M.member func primTable -> Prim func $ map sexpr2expr args
    | not $ T.all isAlpha func -> error "invalid function name (application)"
    | otherwise -> Call func (map sexpr2expr args)

readT :: Read a => T.Text -> a
readT = read . T.unpack
