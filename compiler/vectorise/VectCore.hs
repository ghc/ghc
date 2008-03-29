module VectCore (
  Vect, VVar, VExpr, VBind,

  vectorised, lifted,
  mapVect,

  vNonRec, vRec,

  vVar, vType, vNote, vLet,
  vLams, vLamsWithoutLC, vVarApps,
  vCaseDEFAULT, vCaseProd
) where

#include "HsVersions.h"

import CoreSyn
import CoreUtils      ( exprType )
import DataCon        ( DataCon )
import Type           ( Type )
import Id             ( mkWildId )
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

vCaseDEFAULT :: VExpr -> VVar -> Type -> Type -> VExpr -> VExpr
vCaseDEFAULT (vscrut, lscrut) (vbndr, lbndr) vty lty (vbody, lbody)
  = (Case vscrut vbndr vty (mkDEFAULT vbody),
     Case lscrut lbndr lty (mkDEFAULT lbody))
  where
    mkDEFAULT e = [(DEFAULT, [], e)]

vCaseProd :: VExpr -> Type -> Type
          -> DataCon -> DataCon -> [Var] -> [VVar] -> VExpr -> VExpr
vCaseProd (vscrut, lscrut) vty lty vdc ldc sh_bndrs bndrs
          (vbody,lbody)
  = (Case vscrut (mkWildId $ exprType vscrut) vty
          [(DataAlt vdc, vbndrs, vbody)],
     Case lscrut (mkWildId $ exprType lscrut) lty
          [(DataAlt ldc, sh_bndrs ++ lbndrs, lbody)])
  where
    (vbndrs, lbndrs) = unzip bndrs
