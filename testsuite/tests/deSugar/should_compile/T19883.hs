{-# Language DataKinds #-}
{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}

module T19883 where

import GHC.Types
import qualified Prelude

class Eq (a :: TYPE r) where
  (==) :: a -> a -> Bool

class Num (a :: TYPE r) where
  fromInteger :: Prelude.Integer -> a

roundDef :: forall (a :: TYPE IntRep) . (Eq a, Num a) => a -> ()
roundDef 0 = ()
