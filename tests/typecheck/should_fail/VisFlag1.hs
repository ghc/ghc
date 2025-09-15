module VisFlag1 where

import Data.Kind (Type)

type V :: forall k -> k -> Type
data V k (a :: k) = MkV

f :: forall {k} {a :: k} (hk :: forall j. j -> Type). hk a -> ()
f _ = ()

bad_tyapp :: ()
bad_tyapp = f @V MkV

bad_wild :: ()
bad_wild = f @_ MkV

bad_infer :: ()
bad_infer = f MkV
