module Type where

import Unbound.Generics.LocallyNameless (Name)
import Data.Typeable

data TType

type TyName = Name TType

instance Typeable TType
