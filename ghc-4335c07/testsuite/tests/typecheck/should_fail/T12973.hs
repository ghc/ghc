{-# LANGUAGE RebindableSyntax, TypeInType, ExplicitForAll #-}

module T12973 where

import qualified Prelude as P
import GHC.Exts

class Num (a :: TYPE r) where
  (+) :: a -> a -> a
  fromInteger :: P.Integer -> a

foo :: forall (a :: TYPE r). Num a => a
foo = 3 + 4


