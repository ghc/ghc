{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}

module T25611d where

import GHC.Int (Int(I#))
import GHC.Word (Word(W#))
import GHC.Exts (Int#,Word#)
import GHC.Types

-- | A data family with a kind signature
data family T :: forall k. (k->v) -> k -> v
-- ensure the kind specialization is correctly handled in the GADT-style data instance
-- see Note [Kind inference for data family instances]
-- p will specialize differently in the two constructors
data instance T p q where
      MkkT :: forall r. r Int -> T r Int
      MkkV :: forall l. l Int# -> T l Int#

type N :: TYPE r -> TYPE r
newtype N a = MkN a

f :: Int# -> N Int#
f x = MkN x

g :: Int -> N Int
g x = MkN x

data family D :: Type -> k -> k
newtype instance D Int a = MkD a

f1 :: Int# -> D Int Int#
f1 x = MkD x

g1 :: Int -> D Int Int
g1 x = MkD x
