module DodgyExports02
  ( Identity(..)   -- type constructor has out-of-scope children
  , Void(..)       -- type constructor has no children
  ) where

import Data.Void (Void)
import Data.Functor.Identity (Identity)
