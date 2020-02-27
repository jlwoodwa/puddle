module Evaluation where

import AST
import Primitives
import Types

import Control.Lens
import Control.Monad.Except
import qualified Data.Map.Strict as M

evaluate :: Expr -> Puddle Value
evaluate (Value x) = return x
evaluate (Variable x) = do
  x' <- use $ varTable . at x
  maybe (throwError SymbolNotFound) return x'
evaluate (Assign s e) = do
  v <- evaluate e
  varTable %= M.insert s v
  return Unit
evaluate (Call f xs) = do
  f' <- use $ funTable . at f
  case f' of
    Just f'' -> f'' =<< mapM evaluate xs
    Nothing -> throwError SymbolNotFound
evaluate (Prim f xs) = do
  let f' = primTable ^. at f
  case f' of
    Just f'' -> do
      xs' <- mapM evaluate xs
      f'' xs'
    Nothing ->
      throwError $
      SomeException
        "Internal error: symbol recognized as primitive but not present in primTable"
evaluate (If b i e) = do
  b' <- evaluate b
  case b' of
    Bool True -> evaluate i
    Bool False -> evaluate e
    _ -> throwError $ TypeError "Can't use non-booleans as conditions."
evaluate (While b d) = do
  b' <- evaluate b
  case b' of
    Bool True -> do
      _ <- evaluate d
      evaluate (While b d)
    Bool False -> return Unit
    _ -> throwError $ TypeError "Can't use non-booleans as conditions."
evaluate (Print x) = evaluate x >>= liftIO . print >> return Unit
