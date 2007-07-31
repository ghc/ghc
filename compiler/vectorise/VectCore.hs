module VectCore (
  Vect, VVar, VExpr,

  vectorised, lifted,
  mapVect,

  vVar, mkVLams, mkVVarApps
) where

#include "HsVersions.h"

import CoreSyn
import Var

type Vect a = (a,a)
type VVar   = Vect Var
type VExpr  = Vect CoreExpr

vectorised :: Vect a -> a
vectorised = fst

lifted :: Vect a -> a
lifted = snd

mapVect :: (a -> b) -> Vect a -> Vect b
mapVect f (x,y) = (f x, f y)

vVar :: VVar -> VExpr
vVar = mapVect Var

mkVLams :: [VVar] -> VExpr -> VExpr
mkVLams vvs (ve,le) = (mkLams vs ve, mkLams ls le)
  where
    (vs,ls) = unzip vvs

mkVVarApps :: Var -> VExpr -> [VVar] -> VExpr
mkVVarApps lc (ve, le) vvs = (ve `mkVarApps` vs, le `mkVarApps` (lc : ls))
  where
    (vs,ls) = unzip vvs 


