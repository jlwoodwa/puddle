{-# LANGUAGE LambdaCase #-}

module Evaluation where

import AST
import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Functor ((<&>))
import Data.List (foldl1')
import Data.Map.Merge.Strict
import qualified Data.Map.Strict as M
import Primitives
import Types

evaluate :: Expr -> Puddle Value
evaluate (Value x) = return x
evaluate (Variable s) =
  get
    <&> ( _varTable
            >>> (foldl1' $ merge preserveMissing preserveMissing $ zipWithMatched $ const const)
            >>> (M.!? s)
        )
      >>= maybe (throwError $ SymbolNotFound s) return
-- use (varTable . at s) >>= maybe (throwError $ SymbolNotFound s) return
evaluate (Assign s e) =
  unit $ evaluate e >>= \v -> varTable . ix 0 %= M.insert s v
evaluate (Fun s params e) = unit $
  funTable . ix 0 %= M.insert s \args -> do
    funTable %= (M.empty :)
    varTable %= (M.fromList (zip params args) :)
    v <- evaluate e
    varTable %= tail
    funTable %= tail
    return v
evaluate (Call f xs) =
  get
    <&> ( _funTable >>> (foldl1' $ merge preserveMissing preserveMissing $ zipWithMatched $ const const)
            >>> (M.!? f)
        )
      >>= maybe (throwError $ SymbolNotFound f) (mapM evaluate xs >>=)
--  use (funTable . at f) >>= maybe (throwError $ SymbolNotFound f) (mapM evaluate xs >>=)
evaluate (Prim f xs) =
  maybe
    ( error
        "Internal error: symbol recognized as primitive\
        \but not present in primTable"
    )
    (mapM evaluate xs >>=)
    (primTable ^. at f)
evaluate (If b t e) = evaluate b >>= \case
  Bool True -> evaluate t
  Bool False -> evaluate e
  _ -> throwError $ TypeError "Can't use non-booleans as conditions."
evaluate (While b d) = evaluate b >>= \case
  Bool True -> evaluate d >> evaluate (While b d)
  Bool False -> return Unit
  _ -> throwError $ TypeError "Can't use non-booleans as conditions."

unit :: Monad m => m a -> m Value
unit = (>> return Unit)
