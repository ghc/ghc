module TypeRep where

import Outputable (Outputable)
import Data.Data (Data,Typeable)

data Type
data TyThing

type PredType = Type
type Kind = Type
type SuperKind = Type
type ThetaType = [PredType]

instance Outputable Type
instance Typeable Type
instance Data Type
