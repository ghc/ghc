{-# LANGUAGE LinearTypes, KindSignatures, DataKinds #-}
module LinearKind3 where -- T18780

import GHC.Exts
import GHC.Types

type K = Type %1 -> Type
data T :: K
