{-# LANGUAGE StandaloneKindSignatures, MagicHash, ExplicitForAll, PolyKinds,
             DataKinds, UnliftedDatatypes, PatternSynonyms, ViewPatterns,
             GADTs #-}

module T20426 where

import GHC.Exts

type LPGADT :: forall (l :: Levity) -> TYPE (BoxedRep l)
data LPGADT l where
  MkLifted   :: Int  -> LPGADT Lifted
  MkUnlifted :: Int# -> LPGADT Unlifted

getInt# :: LPGADT l -> Int#
getInt# (MkLifted (I# i#)) = i#
getInt# (MkUnlifted i#)    = i#
