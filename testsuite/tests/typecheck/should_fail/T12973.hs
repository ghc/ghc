{-# LANGUAGE RebindableSyntax, TypeInType, ExplicitForAll #-}

module T12973 where

import qualified Prelude as P
import GHC.Exts

class Num (a :: TYPEvis r) where
  (+) :: a -> a -> a
  fromInteger :: P.Integer -> a

foo :: forall (a :: TYPEvis r). Num a => a
foo = 3 + 4
