{-# Language DataKinds #-}
{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}
{-# Language KindSignatures #-}
{-# Language PolyKinds #-}

module T19883 where

import GHC.Types
import qualified Prelude

class Eq (a :: TYPE r) where
  (==) :: a -> a -> Bool

class Num (a :: TYPE r) where
  fromInteger :: Prelude.Integer -> a

roundDef :: forall (a :: TYPE IntRep) . (Eq a, Num a) => a -> ()
roundDef 0 = ()
