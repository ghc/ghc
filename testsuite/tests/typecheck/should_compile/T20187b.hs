{-# LANGUAGE StandaloneKindSignatures, MagicHash, DataKinds, UnliftedDatatypes #-}

module T20187b where

import GHC.Exts
import GHC.Types

type IntU :: UnliftedType
data IntU = IntU Int#

data T = T !IntU
