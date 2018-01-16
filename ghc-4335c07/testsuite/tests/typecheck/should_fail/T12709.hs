{-# Language MagicHash, PolyKinds, ViewPatterns, TypeInType, RebindableSyntax, NoImplicitPrelude #-}

module T12709 where

import GHC.Types
import Prelude hiding (Num (..))
import qualified Prelude as P
import GHC.Prim

data BoxUnbox = BUB Int Int#

class Num (a :: TYPE rep) where
  (+) :: a -> a -> a

  fromInteger :: Integer -> a

instance Num Int where
  (+) = (P.+)
  fromInteger = P.fromInteger

instance Num Int# where
  (+) = (+#)
  fromInteger (fromInteger -> I# n) = n

a :: BoxUnbox
a = let u :: Num (a :: TYPE rep) => a
        u = 1 + 2 + 3 + 4
     in
        BUB u u
