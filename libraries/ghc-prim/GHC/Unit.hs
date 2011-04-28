
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_GHC -XNoGenerics        #-}

module GHC.Unit where

import GHC.Generics ()

default ()

{-
The Unit type is here because GHC.PrimopWrappers needs to use it in a
type signature.
-}

-- | The unit datatype @()@ has one non-undefined member, the nullary
-- constructor @()@.
data () = ()
