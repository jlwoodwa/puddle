{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict
import Data.Text

type Symb = Text

data Value
  = Unit
  | Bool Bool
  | Int Int
  deriving (Show)

data Exc
  = DivByZero
  | TypeError Text
  | ArgError Text
  | SymbolNotFound
  | SomeException Text
  deriving (Show)

newtype Puddle a =
  Puddle
    { runPuddle :: StateT Env (ExceptT Exc IO) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError Exc
           , MonadState Env
           , MonadIO
           )

type Function = [Value] -> Puddle Value

data Env =
  Env
    { _varTable :: Map Symb Value
    , _funTable :: Map Symb ([Value] -> Puddle Value)
    }

makeLenses ''Env
