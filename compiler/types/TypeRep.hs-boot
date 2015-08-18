module TypeRep where

import Outputable (Outputable)

data Type
data TyThing

type PredType = Type
type Kind = Type
type SuperKind = Type
type ThetaType = [PredType]

instance Outputable Type
