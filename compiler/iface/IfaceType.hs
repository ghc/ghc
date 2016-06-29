{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


This module defines interface types and binders
-}

{-# LANGUAGE CPP, FlexibleInstances #-}
    -- FlexibleInstances for Binary (DefMethSpec IfaceType)

module IfaceType (
        IfExtName, IfLclName,

        IfaceType(..), IfacePredType, IfaceKind, IfaceCoercion(..),
        IfaceUnivCoProv(..),
        IfaceTyCon(..), IfaceTyConInfo(..),
        IfaceTyLit(..), IfaceTcArgs(..),
        IfaceContext, IfaceBndr(..), IfaceOneShot(..), IfaceLamBndr,
        IfaceTvBndr, IfaceIdBndr, IfaceTyConBinder,
        IfaceForAllBndr, ArgFlag(..),

        ifConstraintKind, ifTyConBinderTyVar, ifTyConBinderName,

        -- Equality testing
        IfRnEnv2, emptyIfRnEnv2, eqIfaceType, eqIfaceTypes,
        eqIfaceTcArgs, eqIfaceTvBndrs, isIfaceLiftedTypeKind,

        -- Conversion from Type -> IfaceType
        toIfaceType, toIfaceTypes, toIfaceKind, toIfaceTyVar,
        toIfaceContext, toIfaceBndr, toIfaceIdBndr,
        toIfaceTyCon, toIfaceTyCon_name,
        toIfaceTcArgs, toIfaceTvBndr, toIfaceTvBndrs,
        toIfaceForAllBndr,

        -- Conversion from IfaceTcArgs -> IfaceType
        tcArgsIfaceTypes,

        -- Conversion from Coercion -> IfaceCoercion
        toIfaceCoercion,

        -- Printing
        pprIfaceType, pprParendIfaceType,
        pprIfaceContext, pprIfaceContextArr,
        pprIfaceIdBndr, pprIfaceLamBndr, pprIfaceTvBndr, pprIfaceTyConBinders,
        pprIfaceBndrs, pprIfaceTcArgs, pprParendIfaceTcArgs,
        pprIfaceForAllPart, pprIfaceForAll, pprIfaceSigmaType,
        pprIfaceCoercion, pprParendIfaceCoercion,
        splitIfaceSigmaTy, pprIfaceTypeApp, pprUserIfaceForAll,

        suppressIfaceInvisibles,
        stripIfaceInvisVars,
        stripInvisArgs,
        substIfaceType, substIfaceTyVar, substIfaceTcArgs, mkIfaceTySubst,
        eqIfaceTvBndr
    ) where

#include "HsVersions.h"

import Coercion
import DataCon ( isTupleDataCon )
import TcType
import DynFlags
import TyCoRep  -- needs to convert core types to iface types
import TyCon hiding ( pprPromotionQuote )
import CoAxiom
import Id
import Var
-- import RnEnv( FastStringEnv, mkFsEnv, lookupFsEnv )
import TysWiredIn
import TysPrim
import PrelNames
import Name
import BasicTypes
import Binary
import Outputable
import FastString
import UniqSet
import VarEnv
import UniqFM
import Util

{-
************************************************************************
*                                                                      *
                Local (nested) binders
*                                                                      *
************************************************************************
-}

type IfLclName = FastString     -- A local name in iface syntax

type IfExtName = Name   -- An External or WiredIn Name can appear in IfaceSyn
                        -- (However Internal or System Names never should)

data IfaceBndr          -- Local (non-top-level) binders
  = IfaceIdBndr {-# UNPACK #-} !IfaceIdBndr
  | IfaceTvBndr {-# UNPACK #-} !IfaceTvBndr

type IfaceIdBndr  = (IfLclName, IfaceType)
type IfaceTvBndr  = (IfLclName, IfaceKind)

ifaceTvBndrName :: IfaceTvBndr -> IfLclName
ifaceTvBndrName (n,_) = n

type IfaceLamBndr = (IfaceBndr, IfaceOneShot)

data IfaceOneShot    -- See Note [Preserve OneShotInfo] in CoreTicy
  = IfaceNoOneShot   -- and Note [The oneShot function] in MkId
  | IfaceOneShot


{-
%************************************************************************
%*                                                                      *
                IfaceType
%*                                                                      *
%************************************************************************
-}

-------------------------------
type IfaceKind     = IfaceType

data IfaceType     -- A kind of universal type, used for types and kinds
  = IfaceTyVar    IfLclName               -- Type/coercion variable only, not tycon
  | IfaceLitTy    IfaceTyLit
  | IfaceAppTy    IfaceType IfaceType
  | IfaceFunTy    IfaceType IfaceType
  | IfaceDFunTy   IfaceType IfaceType
  | IfaceForAllTy IfaceForAllBndr IfaceType
  | IfaceTyConApp IfaceTyCon IfaceTcArgs  -- Not necessarily saturated
                                          -- Includes newtypes, synonyms, tuples
  | IfaceCastTy     IfaceType IfaceCoercion
  | IfaceCoercionTy IfaceCoercion
  | IfaceTupleTy                  -- Saturated tuples (unsaturated ones use IfaceTyConApp)
       TupleSort IfaceTyConInfo   -- A bit like IfaceTyCon
       IfaceTcArgs                -- arity = length args
          -- For promoted data cons, the kind args are omitted

type IfacePredType = IfaceType
type IfaceContext = [IfacePredType]

data IfaceTyLit
  = IfaceNumTyLit Integer
  | IfaceStrTyLit FastString
  deriving (Eq)

type IfaceTyConBinder = TyVarBndr IfaceTvBndr TyConBndrVis
type IfaceForAllBndr  = TyVarBndr IfaceTvBndr ArgFlag

-- See Note [Suppressing invisible arguments]
-- We use a new list type (rather than [(IfaceType,Bool)], because
-- it'll be more compact and faster to parse in interface
-- files. Rather than two bytes and two decisions (nil/cons, and
-- type/kind) there'll just be one.
data IfaceTcArgs
  = ITC_Nil
  | ITC_Vis   IfaceType IfaceTcArgs   -- "Vis" means show when pretty-printing
  | ITC_Invis IfaceKind IfaceTcArgs   -- "Invis" means don't show when pretty-printing
                                      --         except with -fprint-explicit-kinds

-- Encodes type constructors, kind constructors,
-- coercion constructors, the lot.
-- We have to tag them in order to pretty print them
-- properly.
data IfaceTyCon = IfaceTyCon { ifaceTyConName :: IfExtName
                             , ifaceTyConInfo :: IfaceTyConInfo }
    deriving (Eq)

data IfaceTyConInfo   -- Used to guide pretty-printing
                      -- and to disambiguate D from 'D (they share a name)
  = NoIfaceTyConInfo
  | IfacePromotedDataCon
    deriving (Eq)

data IfaceCoercion
  = IfaceReflCo       Role IfaceType
  | IfaceFunCo        Role IfaceCoercion IfaceCoercion
  | IfaceTyConAppCo   Role IfaceTyCon [IfaceCoercion]
  | IfaceAppCo        IfaceCoercion IfaceCoercion
  | IfaceForAllCo     IfaceTvBndr IfaceCoercion IfaceCoercion
  | IfaceCoVarCo      IfLclName
  | IfaceAxiomInstCo  IfExtName BranchIndex [IfaceCoercion]
  | IfaceUnivCo       IfaceUnivCoProv Role IfaceType IfaceType
  | IfaceSymCo        IfaceCoercion
  | IfaceTransCo      IfaceCoercion IfaceCoercion
  | IfaceNthCo        Int IfaceCoercion
  | IfaceLRCo         LeftOrRight IfaceCoercion
  | IfaceInstCo       IfaceCoercion IfaceCoercion
  | IfaceCoherenceCo  IfaceCoercion IfaceCoercion
  | IfaceKindCo       IfaceCoercion
  | IfaceSubCo        IfaceCoercion
  | IfaceAxiomRuleCo  IfLclName [IfaceCoercion]

data IfaceUnivCoProv
  = IfaceUnsafeCoerceProv
  | IfacePhantomProv IfaceCoercion
  | IfaceProofIrrelProv IfaceCoercion
  | IfacePluginProv String

-- this constant is needed for dealing with pretty-printing classes
ifConstraintKind :: IfaceKind
ifConstraintKind = IfaceTyConApp (IfaceTyCon { ifaceTyConName = getName constraintKindTyCon
                                             , ifaceTyConInfo = NoIfaceTyConInfo })
                                 ITC_Nil

{-
%************************************************************************
%*                                                                      *
                Functions over IFaceTypes
*                                                                      *
************************************************************************
-}

eqIfaceTvBndr :: IfaceTvBndr -> IfaceTvBndr -> Bool
eqIfaceTvBndr (occ1, _) (occ2, _) = occ1 == occ2

isIfaceLiftedTypeKind :: IfaceKind -> Bool
isIfaceLiftedTypeKind (IfaceTyConApp tc ITC_Nil)
  = isLiftedTypeKindTyConName (ifaceTyConName tc)
isIfaceLiftedTypeKind (IfaceTyConApp tc
                       (ITC_Vis (IfaceTyConApp ptr_rep_lifted ITC_Nil) ITC_Nil))
  =  ifaceTyConName tc      == tYPETyConName
  && ifaceTyConName ptr_rep_lifted `hasKey` ptrRepLiftedDataConKey
isIfaceLiftedTypeKind _ = False

splitIfaceSigmaTy :: IfaceType -> ([IfaceForAllBndr], [IfacePredType], IfaceType)
-- Mainly for printing purposes
splitIfaceSigmaTy ty
  = (bndrs, theta, tau)
  where
    (bndrs, rho)   = split_foralls ty
    (theta, tau)   = split_rho rho

    split_foralls (IfaceForAllTy bndr ty)
        = case split_foralls ty of { (bndrs, rho) -> (bndr:bndrs, rho) }
    split_foralls rho = ([], rho)

    split_rho (IfaceDFunTy ty1 ty2)
        = case split_rho ty2 of { (ps, tau) -> (ty1:ps, tau) }
    split_rho tau = ([], tau)

suppressIfaceInvisibles :: DynFlags -> [IfaceTyConBinder] -> [a] -> [a]
suppressIfaceInvisibles dflags tys xs
  | gopt Opt_PrintExplicitKinds dflags = xs
  | otherwise = suppress tys xs
    where
      suppress _       []      = []
      suppress []      a       = a
      suppress (k:ks) a@(_:xs)
        | isInvisibleTyConBinder k = suppress ks xs
        | otherwise                = a

stripIfaceInvisVars :: DynFlags -> [IfaceTyConBinder] -> [IfaceTyConBinder]
stripIfaceInvisVars dflags tyvars
  | gopt Opt_PrintExplicitKinds dflags = tyvars
  | otherwise = filterOut isInvisibleTyConBinder tyvars

-- | Extract a IfaceTvBndr from a IfaceTyConBinder
ifTyConBinderTyVar :: IfaceTyConBinder -> IfaceTvBndr
ifTyConBinderTyVar = binderVar

-- | Extract the variable name from a IfaceTyConBinder
ifTyConBinderName :: IfaceTyConBinder -> IfLclName
ifTyConBinderName tcb = ifaceTvBndrName (ifTyConBinderTyVar tcb)

ifTyVarsOfType :: IfaceType -> UniqSet IfLclName
ifTyVarsOfType ty
  = case ty of
      IfaceTyVar v -> unitUniqSet v
      IfaceAppTy fun arg
        -> ifTyVarsOfType fun `unionUniqSets` ifTyVarsOfType arg
      IfaceFunTy arg res
        -> ifTyVarsOfType arg `unionUniqSets` ifTyVarsOfType res
      IfaceDFunTy arg res
        -> ifTyVarsOfType arg `unionUniqSets` ifTyVarsOfType res
      IfaceForAllTy bndr ty
        -> let (free, bound) = ifTyVarsOfForAllBndr bndr in
           delListFromUniqSet (ifTyVarsOfType ty) bound `unionUniqSets` free
      IfaceTyConApp _ args -> ifTyVarsOfArgs args
      IfaceLitTy    _      -> emptyUniqSet
      IfaceCastTy ty co
        -> ifTyVarsOfType ty `unionUniqSets` ifTyVarsOfCoercion co
      IfaceCoercionTy co    -> ifTyVarsOfCoercion co
      IfaceTupleTy _ _ args -> ifTyVarsOfArgs args

ifTyVarsOfForAllBndr :: IfaceForAllBndr
                     -> ( UniqSet IfLclName   -- names used free in the binder
                        , [IfLclName] )       -- names bound by this binder
ifTyVarsOfForAllBndr (TvBndr (name, kind) _) = (ifTyVarsOfType kind, [name])

ifTyVarsOfArgs :: IfaceTcArgs -> UniqSet IfLclName
ifTyVarsOfArgs args = argv emptyUniqSet args
   where
     argv vs (ITC_Vis   t ts) = argv (vs `unionUniqSets` (ifTyVarsOfType t)) ts
     argv vs (ITC_Invis k ks) = argv (vs `unionUniqSets` (ifTyVarsOfType k)) ks
     argv vs ITC_Nil          = vs

ifTyVarsOfCoercion :: IfaceCoercion -> UniqSet IfLclName
ifTyVarsOfCoercion = go
  where
    go (IfaceReflCo _ ty)         = ifTyVarsOfType ty
    go (IfaceFunCo _ c1 c2)       = go c1 `unionUniqSets` go c2
    go (IfaceTyConAppCo _ _ cos)  = ifTyVarsOfCoercions cos
    go (IfaceAppCo c1 c2)         = go c1 `unionUniqSets` go c2
    go (IfaceForAllCo (bound, _) kind_co co)
     = go co `delOneFromUniqSet` bound `unionUniqSets` go kind_co
    go (IfaceCoVarCo cv)          = unitUniqSet cv
    go (IfaceAxiomInstCo _ _ cos) = ifTyVarsOfCoercions cos
    go (IfaceUnivCo p _ ty1 ty2)  = go_prov p `unionUniqSets`
                                    ifTyVarsOfType ty1 `unionUniqSets`
                                    ifTyVarsOfType ty2
    go (IfaceSymCo co)            = go co
    go (IfaceTransCo c1 c2)       = go c1 `unionUniqSets` go c2
    go (IfaceNthCo _ co)          = go co
    go (IfaceLRCo _ co)           = go co
    go (IfaceInstCo c1 c2)        = go c1 `unionUniqSets` go c2
    go (IfaceCoherenceCo c1 c2)   = go c1 `unionUniqSets` go c2
    go (IfaceKindCo co)           = go co
    go (IfaceSubCo co)            = go co
    go (IfaceAxiomRuleCo rule cos)
      = unionManyUniqSets
          [ unitUniqSet rule
          , ifTyVarsOfCoercions cos ]

    go_prov IfaceUnsafeCoerceProv    = emptyUniqSet
    go_prov (IfacePhantomProv co)    = go co
    go_prov (IfaceProofIrrelProv co) = go co
    go_prov (IfacePluginProv _)      = emptyUniqSet

ifTyVarsOfCoercions :: [IfaceCoercion] -> UniqSet IfLclName
ifTyVarsOfCoercions = foldr (unionUniqSets . ifTyVarsOfCoercion) emptyUniqSet

{-
Substitutions on IfaceType. This is only used during pretty-printing to construct
the result type of a GADT, and does not deal with binders (eg IfaceForAll), so
it doesn't need fancy capture stuff.
-}

type IfaceTySubst = FastStringEnv IfaceType

mkIfaceTySubst :: [IfaceTvBndr] -> [IfaceType] -> IfaceTySubst
mkIfaceTySubst tvs tys = mkFsEnv $ zipWithEqual "mkIfaceTySubst" (\(fs,_) ty -> (fs,ty)) tvs tys

substIfaceType :: IfaceTySubst -> IfaceType -> IfaceType
substIfaceType env ty
  = go ty
  where
    go (IfaceTyVar tv)        = substIfaceTyVar env tv
    go (IfaceAppTy  t1 t2)    = IfaceAppTy  (go t1) (go t2)
    go (IfaceFunTy  t1 t2)    = IfaceFunTy  (go t1) (go t2)
    go (IfaceDFunTy t1 t2)    = IfaceDFunTy (go t1) (go t2)
    go ty@(IfaceLitTy {})     = ty
    go (IfaceTyConApp tc tys) = IfaceTyConApp tc (substIfaceTcArgs env tys)
    go (IfaceTupleTy s i tys) = IfaceTupleTy s i (substIfaceTcArgs env tys)
    go (IfaceForAllTy {})     = pprPanic "substIfaceType" (ppr ty)
    go (IfaceCastTy ty co)    = IfaceCastTy (go ty) (go_co co)
    go (IfaceCoercionTy co)   = IfaceCoercionTy (go_co co)

    go_co (IfaceReflCo r ty)     = IfaceReflCo r (go ty)
    go_co (IfaceFunCo r c1 c2)   = IfaceFunCo r (go_co c1) (go_co c2)
    go_co (IfaceTyConAppCo r tc cos) = IfaceTyConAppCo r tc (go_cos cos)
    go_co (IfaceAppCo c1 c2)         = IfaceAppCo (go_co c1) (go_co c2)
    go_co (IfaceForAllCo {})         = pprPanic "substIfaceCoercion" (ppr ty)
    go_co (IfaceCoVarCo cv)          = IfaceCoVarCo cv
    go_co (IfaceAxiomInstCo a i cos) = IfaceAxiomInstCo a i (go_cos cos)
    go_co (IfaceUnivCo prov r t1 t2) = IfaceUnivCo (go_prov prov) r (go t1) (go t2)
    go_co (IfaceSymCo co)            = IfaceSymCo (go_co co)
    go_co (IfaceTransCo co1 co2)     = IfaceTransCo (go_co co1) (go_co co2)
    go_co (IfaceNthCo n co)          = IfaceNthCo n (go_co co)
    go_co (IfaceLRCo lr co)          = IfaceLRCo lr (go_co co)
    go_co (IfaceInstCo c1 c2)        = IfaceInstCo (go_co c1) (go_co c2)
    go_co (IfaceCoherenceCo c1 c2)   = IfaceCoherenceCo (go_co c1) (go_co c2)
    go_co (IfaceKindCo co)           = IfaceKindCo (go_co co)
    go_co (IfaceSubCo co)            = IfaceSubCo (go_co co)
    go_co (IfaceAxiomRuleCo n cos)   = IfaceAxiomRuleCo n (go_cos cos)

    go_cos = map go_co

    go_prov IfaceUnsafeCoerceProv    = IfaceUnsafeCoerceProv
    go_prov (IfacePhantomProv co)    = IfacePhantomProv (go_co co)
    go_prov (IfaceProofIrrelProv co) = IfaceProofIrrelProv (go_co co)
    go_prov (IfacePluginProv str)    = IfacePluginProv str

substIfaceTcArgs :: IfaceTySubst -> IfaceTcArgs -> IfaceTcArgs
substIfaceTcArgs env args
  = go args
  where
    go ITC_Nil            = ITC_Nil
    go (ITC_Vis ty tys)   = ITC_Vis   (substIfaceType env ty) (go tys)
    go (ITC_Invis ty tys) = ITC_Invis (substIfaceType env ty) (go tys)

substIfaceTyVar :: IfaceTySubst -> IfLclName -> IfaceType
substIfaceTyVar env tv
  | Just ty <- lookupFsEnv env tv = ty
  | otherwise                     = IfaceTyVar tv

{-
************************************************************************
*                                                                      *
                Equality over IfaceTypes
*                                                                      *
************************************************************************

Note [No kind check in ifaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We check iface types for equality only when checking the consistency
between two user-written signatures. In these cases, there is no possibility
for a kind mismatch. So we omit the kind check (which would be impossible to
write, anyway.)

-}

-- Like an RnEnv2, but mapping from FastString to deBruijn index
-- DeBruijn; see eqTypeX
type BoundVar = Int
data IfRnEnv2
  = IRV2 { ifenvL :: UniqFM BoundVar -- from FastString
         , ifenvR :: UniqFM BoundVar
         , ifenv_next :: BoundVar
         }

emptyIfRnEnv2 :: IfRnEnv2
emptyIfRnEnv2 = IRV2 { ifenvL = emptyUFM
                     , ifenvR = emptyUFM
                     , ifenv_next = 0 }

rnIfOccL :: IfRnEnv2 -> IfLclName -> Maybe BoundVar
rnIfOccL env = lookupUFM (ifenvL env)

rnIfOccR :: IfRnEnv2 -> IfLclName -> Maybe BoundVar
rnIfOccR env = lookupUFM (ifenvR env)

extendIfRnEnv2 :: IfRnEnv2 -> IfLclName -> IfLclName -> IfRnEnv2
extendIfRnEnv2 IRV2 { ifenvL = lenv
                    , ifenvR = renv
                    , ifenv_next = n } tv1 tv2
             = IRV2 { ifenvL = addToUFM lenv tv1 n
                    , ifenvR = addToUFM renv tv2 n
                    , ifenv_next = n + 1
                    }

-- See Note [No kind check in ifaces]
eqIfaceType :: IfRnEnv2 -> IfaceType -> IfaceType -> Bool
eqIfaceType env (IfaceTyVar tv1) (IfaceTyVar tv2) =
    case (rnIfOccL env tv1, rnIfOccR env tv2) of
        (Just v1, Just v2) -> v1 == v2
        (Nothing, Nothing) -> tv1 == tv2
        _ -> False
eqIfaceType _   (IfaceLitTy l1) (IfaceLitTy l2) = l1 == l2
eqIfaceType env (IfaceAppTy t11 t12) (IfaceAppTy t21 t22)
    = eqIfaceType env t11 t21 && eqIfaceType env t12 t22
eqIfaceType env (IfaceFunTy t11 t12) (IfaceFunTy t21 t22)
    = eqIfaceType env t11 t21 && eqIfaceType env t12 t22
eqIfaceType env (IfaceDFunTy t11 t12) (IfaceDFunTy t21 t22)
    = eqIfaceType env t11 t21 && eqIfaceType env t12 t22
eqIfaceType env (IfaceForAllTy bndr1 t1) (IfaceForAllTy bndr2 t2)
    = eqIfaceForAllBndr env bndr1 bndr2 (\env' -> eqIfaceType env' t1 t2)
eqIfaceType env (IfaceTyConApp tc1 tys1) (IfaceTyConApp tc2 tys2)
    = tc1 == tc2 && eqIfaceTcArgs env tys1 tys2
eqIfaceType env (IfaceTupleTy s1 tc1 tys1) (IfaceTupleTy s2 tc2 tys2)
    = s1 == s2 && tc1 == tc2 && eqIfaceTcArgs env tys1 tys2
eqIfaceType env (IfaceCastTy t1 _) (IfaceCastTy t2 _)
    = eqIfaceType env t1 t2
eqIfaceType _   (IfaceCoercionTy {}) (IfaceCoercionTy {})
    = True
eqIfaceType _ _ _ = False

eqIfaceTypes :: IfRnEnv2 -> [IfaceType] -> [IfaceType] -> Bool
eqIfaceTypes env tys1 tys2 = and (zipWith (eqIfaceType env) tys1 tys2)

eqIfaceForAllBndr :: IfRnEnv2 -> IfaceForAllBndr -> IfaceForAllBndr
                  -> (IfRnEnv2 -> Bool)  -- continuation
                  -> Bool
eqIfaceForAllBndr env (TvBndr (tv1, k1) vis1) (TvBndr (tv2, k2) vis2) k
  = eqIfaceType env k1 k2 && vis1 == vis2 &&
    k (extendIfRnEnv2 env tv1 tv2)

eqIfaceTcArgs :: IfRnEnv2 -> IfaceTcArgs -> IfaceTcArgs -> Bool
eqIfaceTcArgs _ ITC_Nil ITC_Nil = True
eqIfaceTcArgs env (ITC_Vis ty1 tys1) (ITC_Vis ty2 tys2)
    = eqIfaceType env ty1 ty2 && eqIfaceTcArgs env tys1 tys2
eqIfaceTcArgs env (ITC_Invis ty1 tys1) (ITC_Invis ty2 tys2)
    = eqIfaceType env ty1 ty2 && eqIfaceTcArgs env tys1 tys2
eqIfaceTcArgs _ _ _ = False

-- | Similar to 'eqTyVarBndrs', checks that tyvar lists
-- are the same length and have matching kinds; if so, extend the
-- 'IfRnEnv2'.  Returns 'Nothing' if they don't match.
eqIfaceTvBndrs :: IfRnEnv2 -> [IfaceTvBndr] -> [IfaceTvBndr] -> Maybe IfRnEnv2
eqIfaceTvBndrs env [] [] = Just env
eqIfaceTvBndrs env ((tv1, k1):tvs1) ((tv2, k2):tvs2)
  | eqIfaceType env k1 k2
  = eqIfaceTvBndrs (extendIfRnEnv2 env tv1 tv2) tvs1 tvs2
eqIfaceTvBndrs _ _ _ = Nothing

{-
************************************************************************
*                                                                      *
                Functions over IFaceTcArgs
*                                                                      *
************************************************************************
-}

stripInvisArgs :: DynFlags -> IfaceTcArgs -> IfaceTcArgs
stripInvisArgs dflags tys
  | gopt Opt_PrintExplicitKinds dflags = tys
  | otherwise = suppress_invis tys
    where
      suppress_invis c
        = case c of
            ITC_Invis _ ts -> suppress_invis ts
            _ -> c

toIfaceTcArgs :: TyCon -> [Type] -> IfaceTcArgs
-- See Note [Suppressing invisible arguments]
toIfaceTcArgs tc ty_args
  = go (mkEmptyTCvSubst in_scope) (tyConKind tc) ty_args
  where
    in_scope = mkInScopeSet (tyCoVarsOfTypes ty_args)

    go _   _                   []     = ITC_Nil
    go env ty                  ts
      | Just ty' <- coreView ty
      = go env ty' ts
    go env (ForAllTy (TvBndr tv vis) res) (t:ts)
      | isVisibleArgFlag vis = ITC_Vis   t' ts'
      | otherwise            = ITC_Invis t' ts'
      where
        t'  = toIfaceType t
        ts' = go (extendTvSubst env tv t) res ts

    go env (FunTy _ res) (t:ts) -- No type-class args in tycon apps
      = ITC_Vis (toIfaceType t) (go env res ts)

    go env (TyVarTy tv) ts
      | Just ki <- lookupTyVar env tv = go env ki ts
    go env kind (t:ts) = WARN( True, ppr tc $$ ppr (tyConKind tc) $$ ppr ty_args )
                         ITC_Vis (toIfaceType t) (go env kind ts) -- Ill-kinded

tcArgsIfaceTypes :: IfaceTcArgs -> [IfaceType]
tcArgsIfaceTypes ITC_Nil = []
tcArgsIfaceTypes (ITC_Invis t ts) = t : tcArgsIfaceTypes ts
tcArgsIfaceTypes (ITC_Vis   t ts) = t : tcArgsIfaceTypes ts

{-
Note [Suppressing invisible arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use the IfaceTcArgs to specify which of the arguments to a type
constructor should be displayed when pretty-printing, under
the control of -fprint-explicit-kinds.
See also Type.filterOutInvisibleTypes.
For example, given
    T :: forall k. (k->*) -> k -> *    -- Ordinary kind polymorphism
    'Just :: forall k. k -> 'Maybe k   -- Promoted
we want
  T * Tree Int    prints as    T Tree Int
  'Just *         prints as    Just *


************************************************************************
*                                                                      *
                Pretty-printing
*                                                                      *
************************************************************************
-}

pprIfaceInfixApp :: (TyPrec -> a -> SDoc) -> TyPrec -> SDoc -> a -> a -> SDoc
pprIfaceInfixApp pp p pp_tc ty1 ty2
  = maybeParen p FunPrec $
    sep [pp FunPrec ty1, pprInfixVar True pp_tc <+> pp FunPrec ty2]

pprIfacePrefixApp :: TyPrec -> SDoc -> [SDoc] -> SDoc
pprIfacePrefixApp p pp_fun pp_tys
  | null pp_tys = pp_fun
  | otherwise   = maybeParen p TyConPrec $
                  hang pp_fun 2 (sep pp_tys)

-- ----------------------------- Printing binders ------------------------------------

instance Outputable IfaceBndr where
    ppr (IfaceIdBndr bndr) = pprIfaceIdBndr bndr
    ppr (IfaceTvBndr bndr) = char '@' <+> pprIfaceTvBndr bndr

pprIfaceBndrs :: [IfaceBndr] -> SDoc
pprIfaceBndrs bs = sep (map ppr bs)

pprIfaceLamBndr :: IfaceLamBndr -> SDoc
pprIfaceLamBndr (b, IfaceNoOneShot) = ppr b
pprIfaceLamBndr (b, IfaceOneShot)   = ppr b <> text "[OneShot]"

pprIfaceIdBndr :: (IfLclName, IfaceType) -> SDoc
pprIfaceIdBndr (name, ty) = parens (ppr name <+> dcolon <+> ppr ty)

pprIfaceTvBndr :: IfaceTvBndr -> SDoc
pprIfaceTvBndr (tv, ki)
  | isIfaceLiftedTypeKind ki = ppr tv
  | otherwise                = parens (ppr tv <+> dcolon <+> ppr ki)

pprIfaceTyConBinders :: [IfaceTyConBinder] -> SDoc
pprIfaceTyConBinders = sep . map go
  where
    go tcb = pprIfaceTvBndr (ifTyConBinderTyVar tcb)

instance Binary IfaceBndr where
    put_ bh (IfaceIdBndr aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (IfaceTvBndr ab) = do
            putByte bh 1
            put_ bh ab
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      return (IfaceIdBndr aa)
              _ -> do ab <- get bh
                      return (IfaceTvBndr ab)

instance Binary IfaceOneShot where
    put_ bh IfaceNoOneShot = do
            putByte bh 0
    put_ bh IfaceOneShot = do
            putByte bh 1
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return IfaceNoOneShot
              _ -> do return IfaceOneShot

-- ----------------------------- Printing IfaceType ------------------------------------

---------------------------------
instance Outputable IfaceType where
  ppr ty = pprIfaceType ty

pprIfaceType, pprParendIfaceType ::IfaceType -> SDoc
pprIfaceType       = ppr_ty TopPrec
pprParendIfaceType = ppr_ty TyConPrec

ppr_ty :: TyPrec -> IfaceType -> SDoc
ppr_ty _         (IfaceTyVar tyvar)     = ppr tyvar
ppr_ty ctxt_prec (IfaceTyConApp tc tys) = sdocWithDynFlags (pprTyTcApp ctxt_prec tc tys)
ppr_ty _         (IfaceTupleTy s i tys) = pprTuple s i tys
ppr_ty _         (IfaceLitTy n)         = ppr_tylit n
        -- Function types
ppr_ty ctxt_prec (IfaceFunTy ty1 ty2)
  = -- We don't want to lose synonyms, so we mustn't use splitFunTys here.
    maybeParen ctxt_prec FunPrec $
    sep [ppr_ty FunPrec ty1, sep (ppr_fun_tail ty2)]
  where
    ppr_fun_tail (IfaceFunTy ty1 ty2)
      = (arrow <+> ppr_ty FunPrec ty1) : ppr_fun_tail ty2
    ppr_fun_tail other_ty
      = [arrow <+> pprIfaceType other_ty]

ppr_ty ctxt_prec (IfaceAppTy ty1 ty2)
  = maybeParen ctxt_prec TyConPrec $
    ppr_ty FunPrec ty1 <+> pprParendIfaceType ty2

ppr_ty ctxt_prec (IfaceCastTy ty co)
  = maybeParen ctxt_prec FunPrec $
    sep [ppr_ty FunPrec ty, text "`cast`", ppr_co FunPrec co]

ppr_ty ctxt_prec (IfaceCoercionTy co)
  = ppr_co ctxt_prec co

ppr_ty ctxt_prec ty
  = maybeParen ctxt_prec FunPrec (ppr_iface_sigma_type True ty)

instance Outputable IfaceTcArgs where
  ppr tca = pprIfaceTcArgs tca

pprIfaceTcArgs, pprParendIfaceTcArgs :: IfaceTcArgs -> SDoc
pprIfaceTcArgs  = ppr_tc_args TopPrec
pprParendIfaceTcArgs = ppr_tc_args TyConPrec

ppr_tc_args :: TyPrec -> IfaceTcArgs -> SDoc
ppr_tc_args ctx_prec args
 = let pprTys t ts = ppr_ty ctx_prec t <+> ppr_tc_args ctx_prec ts
   in case args of
        ITC_Nil        -> empty
        ITC_Vis   t ts -> pprTys t ts
        ITC_Invis t ts -> pprTys t ts

-------------------
ppr_iface_sigma_type :: Bool -> IfaceType -> SDoc
ppr_iface_sigma_type show_foralls_unconditionally ty
  = ppr_iface_forall_part show_foralls_unconditionally tvs theta (ppr tau)
  where
    (tvs, theta, tau) = splitIfaceSigmaTy ty

-------------------
pprIfaceForAllPart :: [IfaceForAllBndr] -> [IfaceType] -> SDoc -> SDoc
pprIfaceForAllPart tvs ctxt sdoc = ppr_iface_forall_part False tvs ctxt sdoc

pprIfaceForAllCoPart :: [(IfLclName, IfaceCoercion)] -> SDoc -> SDoc
pprIfaceForAllCoPart tvs sdoc = sep [ pprIfaceForAllCo tvs
                                    , sdoc ]

ppr_iface_forall_part :: Outputable a
                      => Bool -> [IfaceForAllBndr] -> [a] -> SDoc -> SDoc
ppr_iface_forall_part show_foralls_unconditionally tvs ctxt sdoc
  = sep [ if show_foralls_unconditionally
          then pprIfaceForAll tvs
          else pprUserIfaceForAll tvs
        , pprIfaceContextArr ctxt
        , sdoc]

-- | Render the "forall ... ." or "forall ... ->" bit of a type.
pprIfaceForAll :: [IfaceForAllBndr] -> SDoc
pprIfaceForAll [] = empty
pprIfaceForAll bndrs@(TvBndr _ vis : _)
  = add_separator (text "forall" <+> doc) <+> pprIfaceForAll bndrs'
  where
    (bndrs', doc) = ppr_itv_bndrs bndrs vis

    add_separator stuff = case vis of
                            Required -> stuff <+> arrow
                            _inv     -> stuff <>  dot


-- | Render the ... in @(forall ... .)@ or @(forall ... ->)@.
-- Returns both the list of not-yet-rendered binders and the doc.
-- No anonymous binders here!
ppr_itv_bndrs :: [IfaceForAllBndr]
             -> ArgFlag  -- ^ visibility of the first binder in the list
             -> ([IfaceForAllBndr], SDoc)
ppr_itv_bndrs all_bndrs@(bndr@(TvBndr _ vis) : bndrs) vis1
  | vis `sameVis` vis1 = let (bndrs', doc) = ppr_itv_bndrs bndrs vis1 in
                         (bndrs', pprIfaceForAllBndr bndr <+> doc)
  | otherwise   = (all_bndrs, empty)
ppr_itv_bndrs [] _ = ([], empty)

pprIfaceForAllCo :: [(IfLclName, IfaceCoercion)] -> SDoc
pprIfaceForAllCo []  = empty
pprIfaceForAllCo tvs = text "forall" <+> pprIfaceForAllCoBndrs tvs <> dot

pprIfaceForAllCoBndrs :: [(IfLclName, IfaceCoercion)] -> SDoc
pprIfaceForAllCoBndrs bndrs = hsep $ map pprIfaceForAllCoBndr bndrs

pprIfaceForAllBndr :: IfaceForAllBndr -> SDoc
pprIfaceForAllBndr (TvBndr tv Inferred) = sdocWithDynFlags $ \dflags ->
                                           if gopt Opt_PrintExplicitForalls dflags
                                           then braces $ pprIfaceTvBndr tv
                                           else pprIfaceTvBndr tv
pprIfaceForAllBndr (TvBndr tv _)        = pprIfaceTvBndr tv

pprIfaceForAllCoBndr :: (IfLclName, IfaceCoercion) -> SDoc
pprIfaceForAllCoBndr (tv, kind_co)
  = parens (ppr tv <+> dcolon <+> pprIfaceCoercion kind_co)

pprIfaceSigmaType :: IfaceType -> SDoc
pprIfaceSigmaType ty = ppr_iface_sigma_type False ty

pprUserIfaceForAll :: [IfaceForAllBndr] -> SDoc
pprUserIfaceForAll tvs
   = sdocWithDynFlags $ \dflags ->
     ppWhen (any tv_has_kind_var tvs || gopt Opt_PrintExplicitForalls dflags) $
     pprIfaceForAll tvs
   where
     tv_has_kind_var bndr
       = not (isEmptyUniqSet (fst (ifTyVarsOfForAllBndr bndr)))

-------------------

-- See equivalent function in TyCoRep.hs
pprIfaceTyList :: TyPrec -> IfaceType -> IfaceType -> SDoc
-- Given a type-level list (t1 ': t2), see if we can print
-- it in list notation [t1, ...].
-- Precondition: Opt_PrintExplicitKinds is off
pprIfaceTyList ctxt_prec ty1 ty2
  = case gather ty2 of
      (arg_tys, Nothing)
        -> char '\'' <> brackets (fsep (punctuate comma
                        (map (ppr_ty TopPrec) (ty1:arg_tys))))
      (arg_tys, Just tl)
        -> maybeParen ctxt_prec FunPrec $ hang (ppr_ty FunPrec ty1)
           2 (fsep [ colon <+> ppr_ty FunPrec ty | ty <- arg_tys ++ [tl]])
  where
    gather :: IfaceType -> ([IfaceType], Maybe IfaceType)
     -- (gather ty) = (tys, Nothing) means ty is a list [t1, .., tn]
     --             = (tys, Just tl) means ty is of form t1:t2:...tn:tl
    gather (IfaceTyConApp tc tys)
      | tcname == consDataConName
      , (ITC_Invis _ (ITC_Vis ty1 (ITC_Vis ty2 ITC_Nil))) <- tys
      , (args, tl) <- gather ty2
      = (ty1:args, tl)
      | tcname == nilDataConName
      = ([], Nothing)
      where tcname = ifaceTyConName tc
    gather ty = ([], Just ty)

pprIfaceTypeApp :: IfaceTyCon -> IfaceTcArgs -> SDoc
pprIfaceTypeApp tc args = sdocWithDynFlags (pprTyTcApp TopPrec tc args)

pprTyTcApp :: TyPrec -> IfaceTyCon -> IfaceTcArgs -> DynFlags -> SDoc
pprTyTcApp ctxt_prec tc tys dflags
  | ifaceTyConName tc `hasKey` ipClassKey
  , ITC_Vis (IfaceLitTy (IfaceStrTyLit n)) (ITC_Vis ty ITC_Nil) <- tys
  = char '?' <> ftext n <> text "::" <> ppr_ty TopPrec ty

  | ifaceTyConName tc == consDataConName
  , not (gopt Opt_PrintExplicitKinds dflags)
  , ITC_Invis _ (ITC_Vis ty1 (ITC_Vis ty2 ITC_Nil)) <- tys
  = pprIfaceTyList ctxt_prec ty1 ty2

  | ifaceTyConName tc == tYPETyConName
  , ITC_Vis (IfaceTyConApp ptr_rep ITC_Nil) ITC_Nil <- tys
  , ifaceTyConName ptr_rep `hasKey` ptrRepLiftedDataConKey
  = char '*'

  | ifaceTyConName tc == tYPETyConName
  , ITC_Vis (IfaceTyConApp ptr_rep ITC_Nil) ITC_Nil <- tys
  , ifaceTyConName ptr_rep `hasKey` ptrRepUnliftedDataConKey
  = char '#'

  | otherwise
  = ppr_iface_tc_app ppr_ty ctxt_prec tc tys_wo_kinds
  where
    tys_wo_kinds = tcArgsIfaceTypes $ stripInvisArgs dflags tys

pprIfaceCoTcApp :: TyPrec -> IfaceTyCon -> [IfaceCoercion] -> SDoc
pprIfaceCoTcApp ctxt_prec tc tys = ppr_iface_tc_app ppr_co ctxt_prec tc tys

ppr_iface_tc_app :: (TyPrec -> a -> SDoc) -> TyPrec -> IfaceTyCon -> [a] -> SDoc
ppr_iface_tc_app pp _ tc [ty]
  | n == listTyConName = pprPromotionQuote tc <> brackets (pp TopPrec ty)
  | n == parrTyConName = pprPromotionQuote tc <> paBrackets (pp TopPrec ty)
  where
    n = ifaceTyConName tc

ppr_iface_tc_app pp ctxt_prec tc tys
  | not (isSymOcc (nameOccName tc_name))
  = pprIfacePrefixApp ctxt_prec (ppr tc) (map (pp TyConPrec) tys)

  | [ty1,ty2] <- tys  -- Infix, two arguments;
                      -- we know nothing of precedence though
  = pprIfaceInfixApp pp ctxt_prec (ppr tc) ty1 ty2

  |  tc_name == starKindTyConName || tc_name == unliftedTypeKindTyConName
  || tc_name == unicodeStarKindTyConName
  = ppr tc   -- Do not wrap *, # in parens

  | otherwise
  = pprIfacePrefixApp ctxt_prec (parens (ppr tc)) (map (pp TyConPrec) tys)
  where
    tc_name = ifaceTyConName tc

pprTuple :: TupleSort -> IfaceTyConInfo -> IfaceTcArgs -> SDoc
pprTuple sort info args
  =   -- drop the RuntimeRep vars.
      -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
    let tys   = tcArgsIfaceTypes args
        args' = case sort of
                  UnboxedTuple -> drop (length tys `div` 2) tys
                  _            -> tys
    in
    pprPromotionQuoteI info <>
    tupleParens sort (pprWithCommas pprIfaceType args')

ppr_tylit :: IfaceTyLit -> SDoc
ppr_tylit (IfaceNumTyLit n) = integer n
ppr_tylit (IfaceStrTyLit n) = text (show n)

pprIfaceCoercion, pprParendIfaceCoercion :: IfaceCoercion -> SDoc
pprIfaceCoercion = ppr_co TopPrec
pprParendIfaceCoercion = ppr_co TyConPrec

ppr_co :: TyPrec -> IfaceCoercion -> SDoc
ppr_co _         (IfaceReflCo r ty) = angleBrackets (ppr ty) <> ppr_role r
ppr_co ctxt_prec (IfaceFunCo r co1 co2)
  = maybeParen ctxt_prec FunPrec $
    sep (ppr_co FunPrec co1 : ppr_fun_tail co2)
  where
    ppr_fun_tail (IfaceFunCo r co1 co2)
      = (arrow <> ppr_role r <+> ppr_co FunPrec co1) : ppr_fun_tail co2
    ppr_fun_tail other_co
      = [arrow <> ppr_role r <+> pprIfaceCoercion other_co]

ppr_co _         (IfaceTyConAppCo r tc cos)
  = parens (pprIfaceCoTcApp TopPrec tc cos) <> ppr_role r
ppr_co ctxt_prec (IfaceAppCo co1 co2)
  = maybeParen ctxt_prec TyConPrec $
    ppr_co FunPrec co1 <+> pprParendIfaceCoercion co2
ppr_co ctxt_prec co@(IfaceForAllCo {})
  = maybeParen ctxt_prec FunPrec (pprIfaceForAllCoPart tvs (pprIfaceCoercion inner_co))
  where
    (tvs, inner_co) = split_co co

    split_co (IfaceForAllCo (name, _) kind_co co')
      = let (tvs, co'') = split_co co' in ((name,kind_co):tvs,co'')
    split_co co' = ([], co')

ppr_co _         (IfaceCoVarCo covar)       = ppr covar

ppr_co ctxt_prec (IfaceUnivCo IfaceUnsafeCoerceProv r ty1 ty2)
  = maybeParen ctxt_prec TyConPrec $
    text "UnsafeCo" <+> ppr r <+>
    pprParendIfaceType ty1 <+> pprParendIfaceType ty2

ppr_co _         (IfaceUnivCo _ _ ty1 ty2)
  = angleBrackets ( ppr ty1 <> comma <+> ppr ty2 )

ppr_co ctxt_prec (IfaceInstCo co ty)
  = maybeParen ctxt_prec TyConPrec $
    text "Inst" <+> pprParendIfaceCoercion co
                        <+> pprParendIfaceCoercion ty

ppr_co ctxt_prec (IfaceAxiomRuleCo tc cos)
  = maybeParen ctxt_prec TyConPrec $ ppr tc <+> parens (interpp'SP cos)

ppr_co ctxt_prec (IfaceAxiomInstCo n i cos)
  = ppr_special_co ctxt_prec (ppr n <> brackets (ppr i)) cos
ppr_co ctxt_prec (IfaceSymCo co)
  = ppr_special_co ctxt_prec (text "Sym") [co]
ppr_co ctxt_prec (IfaceTransCo co1 co2)
  = ppr_special_co ctxt_prec  (text "Trans") [co1,co2]
ppr_co ctxt_prec (IfaceNthCo d co)
  = ppr_special_co ctxt_prec (text "Nth:" <> int d) [co]
ppr_co ctxt_prec (IfaceLRCo lr co)
  = ppr_special_co ctxt_prec (ppr lr) [co]
ppr_co ctxt_prec (IfaceSubCo co)
  = ppr_special_co ctxt_prec (text "Sub") [co]
ppr_co ctxt_prec (IfaceCoherenceCo co1 co2)
  = ppr_special_co ctxt_prec (text "Coh") [co1,co2]
ppr_co ctxt_prec (IfaceKindCo co)
  = ppr_special_co ctxt_prec (text "Kind") [co]

ppr_special_co :: TyPrec -> SDoc -> [IfaceCoercion] -> SDoc
ppr_special_co ctxt_prec doc cos
  = maybeParen ctxt_prec TyConPrec
               (sep [doc, nest 4 (sep (map pprParendIfaceCoercion cos))])

ppr_role :: Role -> SDoc
ppr_role r = underscore <> pp_role
  where pp_role = case r of
                    Nominal          -> char 'N'
                    Representational -> char 'R'
                    Phantom          -> char 'P'

-------------------
instance Outputable IfaceTyCon where
  ppr tc = pprPromotionQuote tc <> ppr (ifaceTyConName tc)

pprPromotionQuote :: IfaceTyCon -> SDoc
pprPromotionQuote tc = pprPromotionQuoteI (ifaceTyConInfo tc)

pprPromotionQuoteI  :: IfaceTyConInfo -> SDoc
pprPromotionQuoteI NoIfaceTyConInfo     = empty
pprPromotionQuoteI IfacePromotedDataCon = char '\''

instance Outputable IfaceCoercion where
  ppr = pprIfaceCoercion

instance Binary IfaceTyCon where
   put_ bh (IfaceTyCon n i) = put_ bh n >> put_ bh i

   get bh = do n <- get bh
               i <- get bh
               return (IfaceTyCon n i)

instance Binary IfaceTyConInfo where
   put_ bh NoIfaceTyConInfo     = putByte bh 0
   put_ bh IfacePromotedDataCon = putByte bh 1

   get bh =
     do i <- getByte bh
        case i of
          0 -> return NoIfaceTyConInfo
          _ -> return IfacePromotedDataCon

instance Outputable IfaceTyLit where
  ppr = ppr_tylit

instance Binary IfaceTyLit where
  put_ bh (IfaceNumTyLit n)  = putByte bh 1 >> put_ bh n
  put_ bh (IfaceStrTyLit n)  = putByte bh 2 >> put_ bh n

  get bh =
    do tag <- getByte bh
       case tag of
         1 -> do { n <- get bh
                 ; return (IfaceNumTyLit n) }
         2 -> do { n <- get bh
                 ; return (IfaceStrTyLit n) }
         _ -> panic ("get IfaceTyLit " ++ show tag)

instance Binary IfaceTcArgs where
  put_ bh tk =
    case tk of
      ITC_Vis   t ts -> putByte bh 0 >> put_ bh t >> put_ bh ts
      ITC_Invis t ts -> putByte bh 1 >> put_ bh t >> put_ bh ts
      ITC_Nil        -> putByte bh 2

  get bh =
    do c <- getByte bh
       case c of
         0 -> do
           t  <- get bh
           ts <- get bh
           return $! ITC_Vis t ts
         1 -> do
           t  <- get bh
           ts <- get bh
           return $! ITC_Invis t ts
         2 -> return ITC_Nil
         _ -> panic ("get IfaceTcArgs " ++ show c)

-------------------
pprIfaceContextArr :: Outputable a => [a] -> SDoc
-- Prints "(C a, D b) =>", including the arrow
pprIfaceContextArr []    = empty
pprIfaceContextArr preds = pprIfaceContext preds <+> darrow

pprIfaceContext :: Outputable a => [a] -> SDoc
pprIfaceContext []     = parens empty
pprIfaceContext [pred] = ppr pred -- No parens
pprIfaceContext preds  = parens (fsep (punctuate comma (map ppr preds)))

instance Binary IfaceType where
    put_ bh (IfaceForAllTy aa ab) = do
            putByte bh 0
            put_ bh aa
            put_ bh ab
    put_ bh (IfaceTyVar ad) = do
            putByte bh 1
            put_ bh ad
    put_ bh (IfaceAppTy ae af) = do
            putByte bh 2
            put_ bh ae
            put_ bh af
    put_ bh (IfaceFunTy ag ah) = do
            putByte bh 3
            put_ bh ag
            put_ bh ah
    put_ bh (IfaceDFunTy ag ah) = do
            putByte bh 4
            put_ bh ag
            put_ bh ah
    put_ bh (IfaceTyConApp tc tys)
      = do { putByte bh 5; put_ bh tc; put_ bh tys }
    put_ bh (IfaceCastTy a b)
      = do { putByte bh 6; put_ bh a; put_ bh b }
    put_ bh (IfaceCoercionTy a)
      = do { putByte bh 7; put_ bh a }
    put_ bh (IfaceTupleTy s i tys)
      = do { putByte bh 8; put_ bh s; put_ bh i; put_ bh tys }
    put_ bh (IfaceLitTy n)
      = do { putByte bh 9; put_ bh n }

    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      ab <- get bh
                      return (IfaceForAllTy aa ab)
              1 -> do ad <- get bh
                      return (IfaceTyVar ad)
              2 -> do ae <- get bh
                      af <- get bh
                      return (IfaceAppTy ae af)
              3 -> do ag <- get bh
                      ah <- get bh
                      return (IfaceFunTy ag ah)
              4 -> do ag <- get bh
                      ah <- get bh
                      return (IfaceDFunTy ag ah)
              5 -> do { tc <- get bh; tys <- get bh
                      ; return (IfaceTyConApp tc tys) }
              6 -> do { a <- get bh; b <- get bh
                      ; return (IfaceCastTy a b) }
              7 -> do { a <- get bh
                      ; return (IfaceCoercionTy a) }

              8 -> do { s <- get bh; i <- get bh; tys <- get bh
                      ; return (IfaceTupleTy s i tys) }
              _  -> do n <- get bh
                       return (IfaceLitTy n)

instance Binary IfaceCoercion where
  put_ bh (IfaceReflCo a b) = do
          putByte bh 1
          put_ bh a
          put_ bh b
  put_ bh (IfaceFunCo a b c) = do
          putByte bh 2
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceTyConAppCo a b c) = do
          putByte bh 3
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceAppCo a b) = do
          putByte bh 4
          put_ bh a
          put_ bh b
  put_ bh (IfaceForAllCo a b c) = do
          putByte bh 5
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceCoVarCo a) = do
          putByte bh 6
          put_ bh a
  put_ bh (IfaceAxiomInstCo a b c) = do
          putByte bh 7
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceUnivCo a b c d) = do
          putByte bh 8
          put_ bh a
          put_ bh b
          put_ bh c
          put_ bh d
  put_ bh (IfaceSymCo a) = do
          putByte bh 9
          put_ bh a
  put_ bh (IfaceTransCo a b) = do
          putByte bh 10
          put_ bh a
          put_ bh b
  put_ bh (IfaceNthCo a b) = do
          putByte bh 11
          put_ bh a
          put_ bh b
  put_ bh (IfaceLRCo a b) = do
          putByte bh 12
          put_ bh a
          put_ bh b
  put_ bh (IfaceInstCo a b) = do
          putByte bh 13
          put_ bh a
          put_ bh b
  put_ bh (IfaceCoherenceCo a b) = do
          putByte bh 14
          put_ bh a
          put_ bh b
  put_ bh (IfaceKindCo a) = do
          putByte bh 15
          put_ bh a
  put_ bh (IfaceSubCo a) = do
          putByte bh 16
          put_ bh a
  put_ bh (IfaceAxiomRuleCo a b) = do
          putByte bh 17
          put_ bh a
          put_ bh b

  get bh = do
      tag <- getByte bh
      case tag of
           1 -> do a <- get bh
                   b <- get bh
                   return $ IfaceReflCo a b
           2 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceFunCo a b c
           3 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceTyConAppCo a b c
           4 -> do a <- get bh
                   b <- get bh
                   return $ IfaceAppCo a b
           5 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceForAllCo a b c
           6 -> do a <- get bh
                   return $ IfaceCoVarCo a
           7 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceAxiomInstCo a b c
           8 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   d <- get bh
                   return $ IfaceUnivCo a b c d
           9 -> do a <- get bh
                   return $ IfaceSymCo a
           10-> do a <- get bh
                   b <- get bh
                   return $ IfaceTransCo a b
           11-> do a <- get bh
                   b <- get bh
                   return $ IfaceNthCo a b
           12-> do a <- get bh
                   b <- get bh
                   return $ IfaceLRCo a b
           13-> do a <- get bh
                   b <- get bh
                   return $ IfaceInstCo a b
           14-> do a <- get bh
                   b <- get bh
                   return $ IfaceCoherenceCo a b
           15-> do a <- get bh
                   return $ IfaceKindCo a
           16-> do a <- get bh
                   return $ IfaceSubCo a
           17-> do a <- get bh
                   b <- get bh
                   return $ IfaceAxiomRuleCo a b
           _ -> panic ("get IfaceCoercion " ++ show tag)

instance Binary IfaceUnivCoProv where
  put_ bh IfaceUnsafeCoerceProv = putByte bh 1
  put_ bh (IfacePhantomProv a) = do
          putByte bh 2
          put_ bh a
  put_ bh (IfaceProofIrrelProv a) = do
          putByte bh 3
          put_ bh a
  put_ bh (IfacePluginProv a) = do
          putByte bh 4
          put_ bh a

  get bh = do
      tag <- getByte bh
      case tag of
           1 -> return $ IfaceUnsafeCoerceProv
           2 -> do a <- get bh
                   return $ IfacePhantomProv a
           3 -> do a <- get bh
                   return $ IfaceProofIrrelProv a
           4 -> do a <- get bh
                   return $ IfacePluginProv a
           _ -> panic ("get IfaceUnivCoProv " ++ show tag)


instance Binary (DefMethSpec IfaceType) where
    put_ bh VanillaDM     = putByte bh 0
    put_ bh (GenericDM t) = putByte bh 1 >> put_ bh t
    get bh = do
            h <- getByte bh
            case h of
              0 -> return VanillaDM
              _ -> do { t <- get bh; return (GenericDM t) }

{-
************************************************************************
*                                                                      *
        Conversion from Type to IfaceType
*                                                                      *
************************************************************************
-}

----------------
toIfaceTvBndr :: TyVar -> IfaceTvBndr
toIfaceTvBndr tyvar   = ( occNameFS (getOccName tyvar)
                        , toIfaceKind (tyVarKind tyvar)
                        )

toIfaceIdBndr :: Id -> (IfLclName, IfaceType)
toIfaceIdBndr id      = (occNameFS (getOccName id),    toIfaceType (idType id))

toIfaceTvBndrs :: [TyVar] -> [IfaceTvBndr]
toIfaceTvBndrs = map toIfaceTvBndr

toIfaceBndr :: Var -> IfaceBndr
toIfaceBndr var
  | isId var  = IfaceIdBndr (toIfaceIdBndr var)
  | otherwise = IfaceTvBndr (toIfaceTvBndr var)

toIfaceKind :: Type -> IfaceType
toIfaceKind = toIfaceType

---------------------
toIfaceType :: Type -> IfaceType
-- Synonyms are retained in the interface type
toIfaceType (TyVarTy tv)      = IfaceTyVar (toIfaceTyVar tv)
toIfaceType (AppTy t1 t2)     = IfaceAppTy (toIfaceType t1) (toIfaceType t2)
toIfaceType (LitTy n)         = IfaceLitTy (toIfaceTyLit n)
toIfaceType (ForAllTy b t)    = IfaceForAllTy (toIfaceForAllBndr b) (toIfaceType t)
toIfaceType (FunTy t1 t2)
  | isPredTy t1 = IfaceDFunTy (toIfaceType t1) (toIfaceType t2)
  | otherwise   = IfaceFunTy  (toIfaceType t1) (toIfaceType t2)
toIfaceType (CastTy ty co)      = IfaceCastTy (toIfaceType ty) (toIfaceCoercion co)
toIfaceType (CoercionTy co)     = IfaceCoercionTy (toIfaceCoercion co)

toIfaceType (TyConApp tc tys)  -- Look for the two sorts of saturated tuple
  | Just sort <- tyConTuple_maybe tc
  , n_tys == arity
  = IfaceTupleTy sort NoIfaceTyConInfo (toIfaceTcArgs tc tys)

  | Just dc <- isPromotedDataCon_maybe tc
  , isTupleDataCon dc
  , n_tys == 2*arity
  = IfaceTupleTy BoxedTuple IfacePromotedDataCon (toIfaceTcArgs tc (drop arity tys))

  | otherwise
  = IfaceTyConApp (toIfaceTyCon tc) (toIfaceTcArgs tc tys)
  where
    arity = tyConArity tc
    n_tys = length tys

toIfaceTyVar :: TyVar -> FastString
toIfaceTyVar = occNameFS . getOccName

toIfaceCoVar :: CoVar -> FastString
toIfaceCoVar = occNameFS . getOccName

toIfaceForAllBndr :: TyVarBinder -> IfaceForAllBndr
toIfaceForAllBndr (TvBndr v vis) = TvBndr (toIfaceTvBndr v) vis

----------------
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTyCon tc
  = IfaceTyCon tc_name info
  where
    tc_name = tyConName tc
    info | isPromotedDataCon tc = IfacePromotedDataCon
         | otherwise            = NoIfaceTyConInfo

toIfaceTyCon_name :: Name -> IfaceTyCon
toIfaceTyCon_name n = IfaceTyCon n NoIfaceTyConInfo
  -- Used for the "rough-match" tycon stuff,
  -- where pretty-printing is not an issue

toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceTyLit (NumTyLit x) = IfaceNumTyLit x
toIfaceTyLit (StrTyLit x) = IfaceStrTyLit x

----------------
toIfaceTypes :: [Type] -> [IfaceType]
toIfaceTypes ts = map toIfaceType ts

----------------
toIfaceContext :: ThetaType -> IfaceContext
toIfaceContext = toIfaceTypes

----------------
toIfaceCoercion :: Coercion -> IfaceCoercion
toIfaceCoercion (Refl r ty)         = IfaceReflCo r (toIfaceType ty)
toIfaceCoercion (TyConAppCo r tc cos)
  | tc `hasKey` funTyConKey
  , [arg,res] <- cos                = IfaceFunCo r (toIfaceCoercion arg) (toIfaceCoercion res)
  | otherwise                       = IfaceTyConAppCo r (toIfaceTyCon tc)
                                                        (map toIfaceCoercion cos)
toIfaceCoercion (AppCo co1 co2)     = IfaceAppCo  (toIfaceCoercion co1)
                                                  (toIfaceCoercion co2)
toIfaceCoercion (ForAllCo tv k co)  = IfaceForAllCo (toIfaceTvBndr tv)
                                                    (toIfaceCoercion k)
                                                    (toIfaceCoercion co)
toIfaceCoercion (CoVarCo cv)        = IfaceCoVarCo  (toIfaceCoVar cv)
toIfaceCoercion (AxiomInstCo con ind cos)
                                    = IfaceAxiomInstCo (coAxiomName con) ind
                                                       (map toIfaceCoercion cos)
toIfaceCoercion (UnivCo p r t1 t2)  = IfaceUnivCo (toIfaceUnivCoProv p) r
                                                  (toIfaceType t1)
                                                  (toIfaceType t2)
toIfaceCoercion (SymCo co)          = IfaceSymCo (toIfaceCoercion co)
toIfaceCoercion (TransCo co1 co2)   = IfaceTransCo (toIfaceCoercion co1)
                                                   (toIfaceCoercion co2)
toIfaceCoercion (NthCo d co)        = IfaceNthCo d (toIfaceCoercion co)
toIfaceCoercion (LRCo lr co)        = IfaceLRCo lr (toIfaceCoercion co)
toIfaceCoercion (InstCo co arg)     = IfaceInstCo (toIfaceCoercion co)
                                                  (toIfaceCoercion arg)
toIfaceCoercion (CoherenceCo c1 c2) = IfaceCoherenceCo (toIfaceCoercion c1)
                                                       (toIfaceCoercion c2)
toIfaceCoercion (KindCo c)          = IfaceKindCo (toIfaceCoercion c)
toIfaceCoercion (SubCo co)          = IfaceSubCo (toIfaceCoercion co)
toIfaceCoercion (AxiomRuleCo co cs) = IfaceAxiomRuleCo (coaxrName co)
                                          (map toIfaceCoercion cs)

toIfaceUnivCoProv :: UnivCoProvenance -> IfaceUnivCoProv
toIfaceUnivCoProv UnsafeCoerceProv    = IfaceUnsafeCoerceProv
toIfaceUnivCoProv (PhantomProv co)    = IfacePhantomProv (toIfaceCoercion co)
toIfaceUnivCoProv (ProofIrrelProv co) = IfaceProofIrrelProv (toIfaceCoercion co)
toIfaceUnivCoProv (PluginProv str)    = IfacePluginProv str
toIfaceUnivCoProv (HoleProv h) = pprPanic "toIfaceUnivCoProv hit a hole" (ppr h)
