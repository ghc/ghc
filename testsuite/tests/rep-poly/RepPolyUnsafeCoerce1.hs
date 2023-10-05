{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

module RepPolyUnsafeCoerce1 where

import GHC.Exts

risky :: forall {r} (a :: TYPE (BoxedRep Unlifted)) (b :: TYPE r). a -> b
risky = unsafeCoerce#
-- This should be accepted: representation-polymorphism in the return type
-- is OK for unsafeCoerce#.

