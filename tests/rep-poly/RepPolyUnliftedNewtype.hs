{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnliftedNewtypes #-}

module RepPolyUnliftedNewtype where

import GHC.Exts
import GHC.Types (Multiplicity(..))

type C :: forall (r :: RuntimeRep). TYPE r -> Constraint
class C a
instance C Int#

type N :: forall (r :: RuntimeRep). TYPE r -> TYPE r
newtype C a => N a = MkN a

f1, f2, f3, f4, f5, f6, f7 :: Int# %Many -> N Int#
f1 = MkN
f2 = MkN @_
f3 = MkN @IntRep
f4 = MkN @_      @_
f5 = MkN @_      @Int#
f6 = MkN @IntRep @_
f7 = MkN @IntRep @Int#

g1, g2, g3, g4, g5, g6, g7 :: Int# %Many -> N Int#
g1 x = MkN               x
g2 x = MkN @_            x
g3 x = MkN @IntRep       x
g4 x = MkN @_      @_    x
g5 x = MkN @_      @Int# x
g6 x = MkN @IntRep @_    x
g7 x = MkN @IntRep @Int# x

h3, h5, h6, h7 :: _ => _ %Many -> N _
h3 = MkN @IntRep
h5 = MkN @_      @Int#
h6 = MkN @IntRep @_
h7 = MkN @IntRep @Int#

k1 (x :: Int#) = MkN               x
k2 (x :: Int#) = MkN @_            x
k3  x          = MkN @IntRep       x
k4 (x :: Int#) = MkN @_      @_    x
k5  x          = MkN @_      @Int# x
k6  x          = MkN @IntRep @_    x
k7  x          = MkN @IntRep @Int# x

l1 = (MkN :: Int# %Many -> N Int#)
