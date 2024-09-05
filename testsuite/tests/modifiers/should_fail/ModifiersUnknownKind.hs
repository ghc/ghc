{-# LANGUAGE LinearTypes, RequiredTypeArguments, DataKinds, TypeData #-}

module ModifiersUnknownKind where

import GHC.Types (Multiplicity(..))

f1 :: a %m -> b
f1 = undefined

f2 :: a %(m :: Multiplicity) -> b %m -> c
f2 = undefined
