module VectCore (
  Vect, VVar, VExpr, VBind,

  vectorised, lifted,
  mapVect,

  vNonRec, vRec,

  vVar, vType, vNote, vLet,
  vLams, vLamsWithoutLC, vVarApps
) where

#include "HsVersions.h"

import CoreSyn
import Type           ( Type )
import Var

type Vect a = (a,a)
type VVar   = Vect Var
type VExpr  = Vect CoreExpr
type VBind  = Vect CoreBind

vectorised :: Vect a -> a
vectorised = fst

lifted :: Vect a -> a
lifted = snd

mapVect :: (a -> b) -> Vect a -> Vect b
mapVect f (x,y) = (f x, f y)

zipWithVect :: (a -> b -> c) -> Vect a -> Vect b -> Vect c
zipWithVect f (x1,y1) (x2,y2) = (f x1 x2, f y1 y2)

vVar :: VVar -> VExpr
vVar = mapVect Var

vType :: Type -> VExpr
vType ty = (Type ty, Type ty)

vNote :: Note -> VExpr -> VExpr
vNote = mapVect . Note

vNonRec :: VVar -> VExpr -> VBind
vNonRec = zipWithVect NonRec

vRec :: [VVar] -> [VExpr] -> VBind
vRec vs es = (Rec (zip vvs ves), Rec (zip lvs les))
  where
    (vvs, lvs) = unzip vs
    (ves, les) = unzip es

vLet :: VBind -> VExpr -> VExpr
vLet = zipWithVect Let

vLams :: Var -> [VVar] -> VExpr -> VExpr
vLams lc vs (ve, le) = (mkLams vvs ve, mkLams (lc:lvs) le)
  where
    (vvs,lvs) = unzip vs

vLamsWithoutLC :: [VVar] -> VExpr -> VExpr
vLamsWithoutLC vvs (ve,le) = (mkLams vs ve, mkLams ls le)
  where
    (vs,ls) = unzip vvs

vVarApps :: Var -> VExpr -> [VVar] -> VExpr
vVarApps lc (ve, le) vvs = (ve `mkVarApps` vs, le `mkVarApps` (lc : ls))
  where
    (vs,ls) = unzip vvs 


