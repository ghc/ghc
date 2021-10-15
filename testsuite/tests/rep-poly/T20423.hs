{-# LANGUAGE StandaloneKindSignatures, MagicHash, ExplicitForAll, PolyKinds,
             DataKinds, UnliftedDatatypes, PatternSynonyms #-}

module T20423 where

import GHC.Exts

type LPInt :: forall (l :: Levity) -> TYPE (BoxedRep l)
data LPInt l = MkI Int#

pattern MkIPS :: Int# -> LPInt lev
pattern MkIPS n = MkI n
