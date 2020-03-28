{-# LANGUAGE LambdaCase #-}

module Evaluation where

import AST
import Control.Lens
import Control.Monad.Except
import qualified Data.Map.Strict as M
import Primitives
import Types

evaluate :: Expr -> Puddle Value
evaluate (Value x) = return x
evaluate (Variable x) =
  use (varTable . at x) >>= maybe (throwError $ SymbolNotFound x) return
evaluate (Assign s e) =
  unit $ evaluate e >>= \v -> varTable %= M.insert s v
evaluate (Call f xs) =
  use (funTable . at f) >>= maybe (throwError $ SymbolNotFound f) (mapM evaluate xs >>=)
evaluate (Prim f xs) =
  maybe
    (throwError $ SomeException "Internal error: symbol recognized as primitive but not present in primTable")
    (mapM evaluate xs >>=)
    (primTable ^. at f)
evaluate (If b i e) = evaluate b >>= \case
  Bool True -> evaluate i
  Bool False -> evaluate e
  _ -> throwError $ TypeError "Can't use non-booleans as conditions."
evaluate (While b d) = evaluate b >>= \case
  Bool True -> evaluate d >> evaluate (While b d)
  Bool False -> return Unit
  _ -> throwError $ TypeError "Can't use non-booleans as conditions."

unit :: Monad m => m a -> m Value
unit = (>> return Unit)
