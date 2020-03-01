module Primitives where

import Types

import Control.Monad.Except
import Data.Map.Strict

primTable :: Map Symb ([Value] -> Puddle Value)
primTable = fromList [("+", add), ("print", print')]

add :: [Value] -> Puddle Value
add [x, y] =
  case (x, y) of
    (Int x', Int y') -> return . Int $ x' + y'
    _ -> throwError $ TypeError "add requires its arguments to be integers"
add _ = throwError $ ArgError "add requires 2 arguments"

print' :: [Value] -> Puddle Value
print' [x] = liftIO $ print x >> return Unit
print' _ = throwError $ ArgError "print requires 1 argument"
