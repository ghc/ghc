{- test GHCi support for levity polymorphic UnliftedDatatypes -}
{-# LANGUAGE UnliftedDatatypes #-}

module T20849 where

import Data.Kind
import GHC.Exts

type Strict :: forall r. Type -> TYPE ('BoxedRep r)
data Strict a where
  Force :: a -> Strict @r a

x :: Int
x = 10
  where
    test :: Strict @Unlifted Int
    test = Force undefined
