module GHC.Data.SizedSeq
  ( SizedSeq(..)
  , emptySS
  , addToSS
  , ssElts
  , sizeSS
  ) where

import Prelude -- See note [Why do we import Prelude here?]

data SizedSeq a = SizedSeq {-# UNPACK #-} !Word [a]

emptySS :: SizedSeq a
emptySS = SizedSeq 0 []

addToSS :: SizedSeq a -> a -> SizedSeq a
addToSS (SizedSeq n r_xs) x = SizedSeq (n+1) (x:r_xs)

ssElts :: SizedSeq a -> [a]
ssElts (SizedSeq _ r_xs) = reverse r_xs

sizeSS :: SizedSeq a -> Word
sizeSS (SizedSeq n _) = n
