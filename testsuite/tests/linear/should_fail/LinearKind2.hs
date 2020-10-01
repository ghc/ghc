{-# LANGUAGE LinearTypes, KindSignatures, DataKinds #-}
module LinearKind2 where -- T18780

import GHC.Exts
import GHC.Types

data Two :: FUN One Type Type
