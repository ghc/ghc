{-# OPTIONS -fno-implicit-prelude #-}
module GHC.Dynamic (
 	Dynamic, TypeRep, dynTypeRep, showsTypeRep
  ) where

import {-# SOURCE #-} Data.Dynamic	( Dynamic, dynTypeRep )
import {-# SOURCE #-} Data.Typeable	( TypeRep )
import GHC.Show		( ShowS )

showsTypeRep :: TypeRep -> ShowS
