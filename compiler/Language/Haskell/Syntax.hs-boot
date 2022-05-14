{-# LANGUAGE StandaloneDeriving #-}
module Language.Haskell.Syntax where

import GHC.Prelude (Show)
import GHC.Data.FastString

newtype ModuleName = ModuleName FastString

instance Show ModuleName
