{-# LANGUAGE DataKinds, StandaloneKindSignatures, TypeApplications, TypeFamilies #-}

module RepPolyArgument2 where

import GHC.Exts

type R :: RuntimeRep
type family R where { R = IntRep }

foo = undefined (undefined @R)
