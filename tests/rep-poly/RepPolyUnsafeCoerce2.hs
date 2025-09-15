{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module RepPolyUnsafeCoerce2 where

import GHC.Exts

type RR :: RuntimeRep
type family RR where
  RR = BoxedRep Unlifted

frisky :: forall (a :: TYPE (BoxedRep Unlifted)) (b :: TYPE RR). a -> b
frisky = unsafeCoerce#
  -- Like RepPolyUnsafeCoerce1 except that after rewriting we get
  -- a concrete runtime representation in the return type, so we should be OK.

