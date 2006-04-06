{-# OPTIONS -fno-implicit-prelude #-}
module GHC.Dynamic (
 	Dynamic, TypeRep, dynTypeRep, showsTypeRep
  ) where

import Data.Dynamic	( Dynamic, dynTypeRep )
import Data.Typeable	( TypeRep )
import GHC.Show ( ShowS, shows )

showsTypeRep :: TypeRep -> ShowS
showsTypeRep = shows
