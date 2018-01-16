{-# Language RebindableSyntax, NoImplicitPrelude, MagicHash, RankNTypes,
             PolyKinds, ViewPatterns, TypeInType, FlexibleInstances #-}

module Main where

import Prelude hiding (Eq (..), Num(..))
import qualified Prelude as P
import GHC.Prim
import GHC.Types

class XNum (a :: TYPE rep) where
  (+) :: a -> a -> a
  fromInteger :: Integer -> a

instance P.Num a => XNum a where
  (+) = (P.+)
  fromInteger = P.fromInteger

instance XNum Int# where
  (+) = (+#)
  fromInteger i = case fromInteger i of
                   I# n -> n

u :: Bool
u = isTrue# v_
  where
    v_ :: forall rep (a :: TYPE rep). XNum a => a
    v_ = fromInteger 10

main = print u
