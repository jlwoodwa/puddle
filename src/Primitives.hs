module Primitives where

import Control.Monad.Except
import Data.Map.Strict
import Data.Text (Text)
import Types

primTable :: Map Symb Function
primTable =
  fromList
    [ ifunc "+" (+),
      ifunc "-" (-),
      ifunc "*" (*),
      ifunc "/" div,
      ifunc "mod" mod,
      bfunc "==" (==),
      bfunc "<" (<),
      bfunc ">" (>),
      bfunc "/=" (/=),
      bfunc ">=" (>=),
      bfunc "<=" (<=),
      ("print", _print)
    ]
  where
    ifunc :: Text -> (Int -> Int -> Int) -> (Symb, Function)
    ifunc s f = (s,) \case
      [Int x, Int y] -> return . Int $ f x y
      [_, _] -> throwError . TypeError $ s <> " requires its arguments to be integers"
      _ -> throwError . ArgError $ s <> " requires 2 arguments"
    bfunc :: Text -> (Int -> Int -> Bool) -> (Symb, Function)
    bfunc s f = (s,) \case
      [Int x, Int y] -> return . Bool $ f x y
      [_, _] -> throwError . TypeError $ s <> " requires its arguments to be integers"
      _ -> throwError . ArgError $ s <> " requires 2 arguments"

_print :: Function
_print = \case
  [x] -> liftIO $ print x >> return Unit
  _ -> throwError $ ArgError "print requires 1 argument"
