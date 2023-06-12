{-# LANGUAGE StandaloneKindSignatures, MagicHash, DataKinds, UnliftedDatatypes #-}

module T20204 where

import GHC.Exts
import GHC.Types

type IntU :: UnliftedType
data IntU = IntU Int#

data Test = Test {-# UNPACK #-} IntU
