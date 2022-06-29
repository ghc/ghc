module Language.Haskell.Syntax.ImpExp where

import Data.Eq
import Data.Ord
import Text.Show
import Data.Data

-- This boot file should be short lived: As soon as the dependency on
-- `GHC.Hs.Doc` is gone we'll no longer have cycles and can get rid this file.

data IsBootInterface = NotBoot | IsBoot

instance Eq IsBootInterface
instance Ord IsBootInterface
instance Show IsBootInterface
instance Data IsBootInterface
