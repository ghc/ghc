{-# LANGUAGE LinearTypes, GADTSyntax #-}

module T22546 where

import GHC.Types (Multiplicity (..))
import Data.Kind (Type)

data T :: Multiplicity -> Type where
  MkT :: () %m-> T m

unMkT :: T m %(n :: Multiplicity) -> ()
unMkT (MkT x) = x
