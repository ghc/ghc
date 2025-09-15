{-# LANGUAGE StandaloneKindSignatures #-}
module T24686a where

import GHC.Exts

-- This one crashed GHC too: see #24686

type T :: forall a (b:: TYPE a). b
data T
