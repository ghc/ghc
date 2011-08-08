{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -XNoImplicitPrelude  #-}
{-# OPTIONS_GHC -XDeriveGeneric      #-}

module GHC.Unit where

import GHC.Generics (Generic)

default ()

{-
The Unit type is here because GHC.PrimopWrappers needs to use it in a
type signature.
-}

-- | The unit datatype @()@ has one non-undefined member, the nullary
-- constructor @()@.
data () = ()
        deriving Generic
