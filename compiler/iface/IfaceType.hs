{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


This module defines interface types and binders
-}

{-# LANGUAGE CPP, FlexibleInstances, BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
    -- FlexibleInstances for Binary (DefMethSpec IfaceType)

module IfaceType (
        IfExtName, IfLclName,

        IfaceType(..), IfacePredType, IfaceKind, IfaceCoercion(..),
        IfaceUnivCoProv(..),
        IfaceTyCon(..), IfaceTyConInfo(..), IfaceTyConSort(..), IsPromoted(..),
        IfaceTyLit(..), IfaceTcArgs(..),
        IfaceContext, IfaceBndr(..), IfaceOneShot(..), IfaceLamBndr,
        IfaceTvBndr, IfaceIdBndr, IfaceTyConBinder,
        IfaceForAllBndr, ArgFlag(..), ShowForAllFlag(..),

        ifTyConBinderTyVar, ifTyConBinderName,

        -- Equality testing
        isIfaceLiftedTypeKind,

        -- Conversion from IfaceTcArgs -> [IfaceType]
        tcArgsIfaceTypes,

        -- Printing
        pprIfaceType, pprParendIfaceType, pprPrecIfaceType,
        pprIfaceContext, pprIfaceContextArr,
        pprIfaceIdBndr, pprIfaceLamBndr, pprIfaceTvBndr, pprIfaceTyConBinders,
        pprIfaceBndrs, pprIfaceTcArgs, pprParendIfaceTcArgs,
        pprIfaceForAllPart, pprIfaceForAll, pprIfaceSigmaType,
        pprIfaceTyLit,
        pprIfaceCoercion, pprParendIfaceCoercion,
        splitIfaceSigmaTy, pprIfaceTypeApp, pprUserIfaceForAll,
        pprIfaceCoTcApp, pprTyTcApp, pprIfacePrefixApp,

        suppressIfaceInvisibles,
        stripIfaceInvisVars,
        stripInvisArgs,

        mkIfaceTySubst, substIfaceTyVar, substIfaceTcArgs, inDomIfaceTySubst
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} TysWiredIn ( liftedRepDataConTyCon )

import DynFlags
import TyCon hiding ( pprPromotionQuote )
import CoAxiom
import Var
import PrelNames
import Name
import BasicTypes
import Binary
import Outputable
import FastString
import FastStringEnv
import Util

import Data.Maybe( isJust )
import Data.List (foldl')

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
  = IfaceFreeTyVar TyVar                -- See Note [Free tyvars in IfaceType]
  | IfaceTyVar     IfLclName            -- Type/coercion variable only, not tycon
  | IfaceLitTy     IfaceTyLit
  | IfaceAppTy     IfaceType IfaceType
  | IfaceFunTy     IfaceType IfaceType
  | IfaceDFunTy    IfaceType IfaceType
  | IfaceForAllTy  IfaceForAllBndr IfaceType
  | IfaceTyConApp  IfaceTyCon IfaceTcArgs  -- Not necessarily saturated
                                           -- Includes newtypes, synonyms, tuples
  | IfaceCastTy     IfaceType IfaceCoercion
  | IfaceCoercionTy IfaceCoercion

  | IfaceTupleTy                  -- Saturated tuples (unsaturated ones use IfaceTyConApp)
       TupleSort                  -- What sort of tuple?
       IsPromoted                 -- A bit like IfaceTyCon
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

instance Monoid IfaceTcArgs where
  mempty = ITC_Nil
  ITC_Nil `mappend` xs           = xs
  ITC_Vis ty rest `mappend` xs   = ITC_Vis ty (rest `mappend` xs)
  ITC_Invis ki rest `mappend` xs = ITC_Invis ki (rest `mappend` xs)

-- Encodes type constructors, kind constructors,
-- coercion constructors, the lot.
-- We have to tag them in order to pretty print them
-- properly.
data IfaceTyCon = IfaceTyCon { ifaceTyConName :: IfExtName
                             , ifaceTyConInfo :: IfaceTyConInfo }
    deriving (Eq)

-- | Is a TyCon a promoted data constructor or just a normal type constructor?
data IsPromoted = IsNotPromoted | IsPromoted
    deriving (Eq)

-- | The various types of TyCons which have special, built-in syntax.
data IfaceTyConSort = IfaceNormalTyCon          -- ^ a regular tycon

                    | IfaceTupleTyCon !Arity !TupleSort
                      -- ^ e.g. @(a, b, c)@ or @(#a, b, c#)@.
                      -- The arity is the tuple width, not the tycon arity
                      -- (which is twice the width in the case of unboxed
                      -- tuples).

                    | IfaceSumTyCon !Arity
                      -- ^ e.g. @(a | b | c)@

                    | IfaceEqualityTyCon !Bool
                      -- ^ a type equality. 'True' indicates kind-homogeneous.
                      -- See Note [Equality predicates in IfaceType] for
                      -- details.
                    deriving (Eq)

{- Note [Free tyvars in IfaceType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nowadays (since Nov 16, 2016) we pretty-print a Type by converting to an
IfaceType and pretty printing that.  This eliminates a lot of
pretty-print duplication, and it matches what we do with
pretty-printing TyThings.

It works fine for closed types, but when printing debug traces (e.g.
when using -ddump-tc-trace) we print a lot of /open/ types.  These
types are full of TcTyVars, and it's absolutely crucial to print them
in their full glory, with their unique, TcTyVarDetails etc.

So we simply embed a TyVar in IfaceType with the IfaceFreeTyVar constructor.
Note that:

* We never expect to serialise an IfaceFreeTyVar into an interface file, nor
  to deserialise one.  IfaceFreeTyVar is used only in the "convert to IfaceType
  and then pretty-print" pipeline.

We do the same for covars, naturally.

Note [Equality predicates in IfaceType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC has several varieties of type equality (see Note [The equality types story]
in TysPrim for details) which all must be rendered with different surface syntax
during pretty-printing. Which syntax we use depends upon,

 1. Which predicate tycon was used
 2. Whether the types being compared are of the same kind.

Unfortunately, determining (2) from an IfaceType isn't possible since we can't
see through type synonyms. Consequently, we need to record whether the equality
is homogeneous or not in IfaceTyConSort for the purposes of pretty-printing.

Namely we handle these cases,

    Predicate               Homogeneous        Heterogeneous
    ----------------        -----------        -------------
    eqTyCon                 ~                  N/A
    heqTyCon                ~                  ~~
    eqPrimTyCon             ~#                 ~~
    eqReprPrimTyCon         Coercible          Coercible

See Note [The equality types story] in TysPrim.
-}

data IfaceTyConInfo   -- Used to guide pretty-printing
                      -- and to disambiguate D from 'D (they share a name)
  = IfaceTyConInfo { ifaceTyConIsPromoted :: IsPromoted
                   , ifaceTyConSort       :: IfaceTyConSort }
    deriving (Eq)

data IfaceCoercion
  = IfaceReflCo       Role IfaceType
  | IfaceFunCo        Role IfaceCoercion IfaceCoercion
  | IfaceTyConAppCo   Role IfaceTyCon [IfaceCoercion]
  | IfaceAppCo        IfaceCoercion IfaceCoercion
  | IfaceForAllCo     IfaceTvBndr IfaceCoercion IfaceCoercion
  | IfaceFreeCoVar    CoVar       -- See Note [Free tyvars in IfaceType]
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
  | IfaceHoleProv Unique
    -- ^ See Note [Holes in IfaceUnivCoProv]

{-
Note [Holes in IfaceUnivCoProv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking fails the typechecker will produce a HoleProv UnivCoProv to
stand in place of the unproven assertion. While we generally don't want to let
these unproven assertions leak into interface files, we still need to be able to
pretty-print them as we use IfaceType's pretty-printer to render Types. For this
reason IfaceUnivCoProv has a IfaceHoleProv constructor; however, we fails when
asked to serialize to a IfaceHoleProv to ensure that they don't end up in an
interface file. To avoid an import loop between IfaceType and TyCoRep we only
keep the hole's Unique, since that is all we need to print.
-}

{-
%************************************************************************
%*                                                                      *
                Functions over IFaceTypes
*                                                                      *
************************************************************************
-}

ifaceTyConHasKey :: IfaceTyCon -> Unique -> Bool
ifaceTyConHasKey tc key = ifaceTyConName tc `hasKey` key

isIfaceLiftedTypeKind :: IfaceKind -> Bool
isIfaceLiftedTypeKind (IfaceTyConApp tc ITC_Nil)
  = isLiftedTypeKindTyConName (ifaceTyConName tc)
isIfaceLiftedTypeKind (IfaceTyConApp tc
                       (ITC_Vis (IfaceTyConApp ptr_rep_lifted ITC_Nil) ITC_Nil))
  =  tc `ifaceTyConHasKey` tYPETyConKey
  && ptr_rep_lifted `ifaceTyConHasKey` liftedRepDataConKey
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

ifTypeIsVarFree :: IfaceType -> Bool
-- Returns True if the type definitely has no variables at all
-- Just used to control pretty printing
ifTypeIsVarFree ty = go ty
  where
    go (IfaceTyVar {})         = False
    go (IfaceFreeTyVar {})     = False
    go (IfaceAppTy fun arg)    = go fun && go arg
    go (IfaceFunTy arg res)    = go arg && go res
    go (IfaceDFunTy arg res)   = go arg && go res
    go (IfaceForAllTy {})      = False
    go (IfaceTyConApp _ args)  = go_args args
    go (IfaceTupleTy _ _ args) = go_args args
    go (IfaceLitTy _)          = True
    go (IfaceCastTy {})        = False -- Safe
    go (IfaceCoercionTy {})    = False -- Safe

    go_args ITC_Nil = True
    go_args (ITC_Vis   arg args) = go arg && go_args args
    go_args (ITC_Invis arg args) = go arg && go_args args

{- Note [Substitution on IfaceType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Substitutions on IfaceType are done only during pretty-printing to
construct the result type of a GADT, and does not deal with binders
(eg IfaceForAll), so it doesn't need fancy capture stuff.  -}

type IfaceTySubst = FastStringEnv IfaceType -- Note [Substitution on IfaceType]

mkIfaceTySubst :: [(IfLclName,IfaceType)] -> IfaceTySubst
-- See Note [Substitution on IfaceType]
mkIfaceTySubst eq_spec = mkFsEnv eq_spec

inDomIfaceTySubst :: IfaceTySubst -> IfaceTvBndr -> Bool
-- See Note [Substitution on IfaceType]
inDomIfaceTySubst subst (fs, _) = isJust (lookupFsEnv subst fs)

substIfaceType :: IfaceTySubst -> IfaceType -> IfaceType
-- See Note [Substitution on IfaceType]
substIfaceType env ty
  = go ty
  where
    go (IfaceFreeTyVar tv)    = IfaceFreeTyVar tv
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
    go_co (IfaceFreeCoVar cv)        = IfaceFreeCoVar cv
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
    go_prov (IfaceHoleProv h)        = IfaceHoleProv h

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

tcArgsIfaceTypes :: IfaceTcArgs -> [IfaceType]
tcArgsIfaceTypes ITC_Nil = []
tcArgsIfaceTypes (ITC_Invis t ts) = t : tcArgsIfaceTypes ts
tcArgsIfaceTypes (ITC_Vis   t ts) = t : tcArgsIfaceTypes ts

ifaceVisTcArgsLength :: IfaceTcArgs -> Int
ifaceVisTcArgsLength = go 0
  where
    go !n ITC_Nil            = n
    go n  (ITC_Vis _ rest)   = go (n+1) rest
    go n  (ITC_Invis _ rest) = go n rest

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

if_print_coercions :: SDoc  -- ^ if printing coercions
                   -> SDoc  -- ^ otherwise
                   -> SDoc
if_print_coercions yes no
  = sdocWithDynFlags $ \dflags ->
    getPprStyle $ \style ->
    if gopt Opt_PrintExplicitCoercions dflags
         || dumpStyle style || debugStyle style
    then yes
    else no

pprIfaceInfixApp :: TyPrec -> SDoc -> SDoc -> SDoc -> SDoc
pprIfaceInfixApp ctxt_prec pp_tc pp_ty1 pp_ty2
  = maybeParen ctxt_prec TyOpPrec $
    sep [pp_ty1, pp_tc <+> pp_ty2]

pprIfacePrefixApp :: TyPrec -> SDoc -> [SDoc] -> SDoc
pprIfacePrefixApp ctxt_prec pp_fun pp_tys
  | null pp_tys = pp_fun
  | otherwise   = maybeParen ctxt_prec TyConPrec $
                  hang pp_fun 2 (sep pp_tys)

-- ----------------------------- Printing binders ------------------------------------

instance Outputable IfaceBndr where
    ppr (IfaceIdBndr bndr) = pprIfaceIdBndr bndr
    ppr (IfaceTvBndr bndr) = char '@' <+> pprIfaceTvBndr False bndr

pprIfaceBndrs :: [IfaceBndr] -> SDoc
pprIfaceBndrs bs = sep (map ppr bs)

pprIfaceLamBndr :: IfaceLamBndr -> SDoc
pprIfaceLamBndr (b, IfaceNoOneShot) = ppr b
pprIfaceLamBndr (b, IfaceOneShot)   = ppr b <> text "[OneShot]"

pprIfaceIdBndr :: IfaceIdBndr -> SDoc
pprIfaceIdBndr (name, ty) = parens (ppr name <+> dcolon <+> ppr ty)

pprIfaceTvBndr :: Bool -> IfaceTvBndr -> SDoc
pprIfaceTvBndr use_parens (tv, ki)
  | isIfaceLiftedTypeKind ki = ppr tv
  | otherwise                = maybe_parens (ppr tv <+> dcolon <+> ppr ki)
  where
    maybe_parens | use_parens = parens
                 | otherwise  = id

pprIfaceTyConBinders :: [IfaceTyConBinder] -> SDoc
pprIfaceTyConBinders = sep . map go
  where
    go tcb = pprIfaceTvBndr True (ifTyConBinderTyVar tcb)

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

pprIfaceType, pprParendIfaceType :: IfaceType -> SDoc
pprIfaceType       = pprPrecIfaceType TopPrec
pprParendIfaceType = pprPrecIfaceType TyConPrec

pprPrecIfaceType :: TyPrec -> IfaceType -> SDoc
pprPrecIfaceType prec ty = eliminateRuntimeRep (ppr_ty prec) ty

ppr_ty :: TyPrec -> IfaceType -> SDoc
ppr_ty _         (IfaceFreeTyVar tyvar) = ppr tyvar  -- This is the main reson for IfaceFreeTyVar!
ppr_ty _         (IfaceTyVar tyvar)     = ppr tyvar  -- See Note [TcTyVars in IfaceType]
ppr_ty ctxt_prec (IfaceTyConApp tc tys) = pprTyTcApp ctxt_prec tc tys
ppr_ty _         (IfaceTupleTy i p tys) = pprTuple i p tys
ppr_ty _         (IfaceLitTy n)         = pprIfaceTyLit n
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
  = if_print_coercions
      ppr_app_ty
      ppr_app_ty_no_casts
  where
    ppr_app_ty =
        maybeParen ctxt_prec TyConPrec
        $ ppr_ty FunPrec ty1 <+> ppr_ty TyConPrec ty2

    -- Strip any casts from the head of the application
    ppr_app_ty_no_casts =
        case split_app_tys ty1 (ITC_Vis ty2 ITC_Nil) of
          (IfaceCastTy head _, args) -> ppr_ty ctxt_prec (mk_app_tys head args)
          _                          -> ppr_app_ty

    split_app_tys :: IfaceType -> IfaceTcArgs -> (IfaceType, IfaceTcArgs)
    split_app_tys (IfaceAppTy t1 t2) args = split_app_tys t1 (t2 `ITC_Vis` args)
    split_app_tys head               args = (head, args)

    mk_app_tys :: IfaceType -> IfaceTcArgs -> IfaceType
    mk_app_tys (IfaceTyConApp tc tys1) tys2 =
        IfaceTyConApp tc (tys1 `mappend` tys2)
    mk_app_tys t1                      tys2 =
        foldl' IfaceAppTy t1 (tcArgsIfaceTypes tys2)

ppr_ty ctxt_prec (IfaceCastTy ty co)
  = if_print_coercions
      (parens (ppr_ty TopPrec ty <+> text "|>" <+> ppr co))
      (ppr_ty ctxt_prec ty)

ppr_ty ctxt_prec (IfaceCoercionTy co)
  = if_print_coercions
      (ppr_co ctxt_prec co)
      (text "<>")

ppr_ty ctxt_prec ty
  = maybeParen ctxt_prec FunPrec (pprIfaceSigmaType ShowForAllMust ty)

{-
Note [Defaulting RuntimeRep variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RuntimeRep variables are considered by many (most?) users to be little more than
syntactic noise. When the notion was introduced there was a signficant and
understandable push-back from those with pedagogy in mind, which argued that
RuntimeRep variables would throw a wrench into nearly any teach approach since
they appear in even the lowly ($) function's type,

    ($) :: forall (w :: RuntimeRep) a (b :: TYPE w). (a -> b) -> a -> b

which is significantly less readable than its non RuntimeRep-polymorphic type of

    ($) :: (a -> b) -> a -> b

Moreover, unboxed types don't appear all that often in run-of-the-mill Haskell
programs, so it makes little sense to make all users pay this syntactic
overhead.

For this reason it was decided that we would hide RuntimeRep variables for now
(see #11549). We do this by defaulting all type variables of kind RuntimeRep to
PtrLiftedRep. This is done in a pass right before pretty-printing
(defaultRuntimeRepVars, controlled by -fprint-explicit-runtime-reps)
-}

-- | Default 'RuntimeRep' variables to 'LiftedPtr'. e.g.
--
-- @
-- ($) :: forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r).
--        (a -> b) -> a -> b
-- @
--
-- turns in to,
--
-- @ ($) :: forall a (b :: *). (a -> b) -> a -> b @
--
-- We do this to prevent RuntimeRep variables from incurring a significant
-- syntactic overhead in otherwise simple type signatures (e.g. ($)). See
-- Note [Defaulting RuntimeRep variables] and #11549 for further discussion.
--
defaultRuntimeRepVars :: IfaceType -> IfaceType
defaultRuntimeRepVars = go emptyFsEnv
  where
    go :: FastStringEnv () -> IfaceType -> IfaceType
    go subs (IfaceForAllTy bndr ty)
      | isRuntimeRep var_kind
      = let subs' = extendFsEnv subs var ()
        in go subs' ty
      | otherwise
      = IfaceForAllTy (TvBndr (var, go subs var_kind) (binderArgFlag bndr))
        (go subs ty)
      where
        var :: IfLclName
        (var, var_kind) = binderVar bndr

    go subs (IfaceTyVar tv)
      | tv `elemFsEnv` subs
      = IfaceTyConApp liftedRep ITC_Nil

    go subs (IfaceFunTy kind ty)
      = IfaceFunTy (go subs kind) (go subs ty)

    go subs (IfaceAppTy x y)
      = IfaceAppTy (go subs x) (go subs y)

    go subs (IfaceDFunTy x y)
      = IfaceDFunTy (go subs x) (go subs y)

    go subs (IfaceCastTy x co)
      = IfaceCastTy (go subs x) co

    go _ other = other

    liftedRep :: IfaceTyCon
    liftedRep =
        IfaceTyCon dc_name (IfaceTyConInfo IsPromoted IfaceNormalTyCon)
      where dc_name = getName liftedRepDataConTyCon

    isRuntimeRep :: IfaceType -> Bool
    isRuntimeRep (IfaceTyConApp tc _) =
        tc `ifaceTyConHasKey` runtimeRepTyConKey
    isRuntimeRep _ = False

eliminateRuntimeRep :: (IfaceType -> SDoc) -> IfaceType -> SDoc
eliminateRuntimeRep f ty = sdocWithDynFlags $ \dflags ->
    if gopt Opt_PrintExplicitRuntimeReps dflags
      then f ty
      else f (defaultRuntimeRepVars ty)

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
pprIfaceForAllPart :: [IfaceForAllBndr] -> [IfacePredType] -> SDoc -> SDoc
pprIfaceForAllPart tvs ctxt sdoc
  = ppr_iface_forall_part ShowForAllWhen tvs ctxt sdoc

pprIfaceForAllCoPart :: [(IfLclName, IfaceCoercion)] -> SDoc -> SDoc
pprIfaceForAllCoPart tvs sdoc
  = sep [ pprIfaceForAllCo tvs, sdoc ]

ppr_iface_forall_part :: ShowForAllFlag
                      -> [IfaceForAllBndr] -> [IfacePredType] -> SDoc -> SDoc
ppr_iface_forall_part show_forall tvs ctxt sdoc
  = sep [ case show_forall of
            ShowForAllMust -> pprIfaceForAll tvs
            ShowForAllWhen -> pprUserIfaceForAll tvs
        , pprIfaceContextArr ctxt
        , sdoc]

-- | Render the "forall ... ." or "forall ... ->" bit of a type.
pprIfaceForAll :: [IfaceForAllBndr] -> SDoc
pprIfaceForAll [] = empty
pprIfaceForAll bndrs@(TvBndr _ vis : _)
  = add_separator (forAllLit <+> doc) <+> pprIfaceForAll bndrs'
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
                                           then braces $ pprIfaceTvBndr False tv
                                           else pprIfaceTvBndr True tv
pprIfaceForAllBndr (TvBndr tv _)        = pprIfaceTvBndr True tv

pprIfaceForAllCoBndr :: (IfLclName, IfaceCoercion) -> SDoc
pprIfaceForAllCoBndr (tv, kind_co)
  = parens (ppr tv <+> dcolon <+> pprIfaceCoercion kind_co)

-- | Show forall flag
--
-- Unconditionally show the forall quantifier with ('ShowForAllMust')
-- or when ('ShowForAllWhen') the names used are free in the binder
-- or when compiling with -fprint-explicit-foralls.
data ShowForAllFlag = ShowForAllMust | ShowForAllWhen

pprIfaceSigmaType :: ShowForAllFlag -> IfaceType -> SDoc
pprIfaceSigmaType show_forall ty
  = ppr_iface_forall_part show_forall tvs theta (ppr tau)
  where
    (tvs, theta, tau) = splitIfaceSigmaTy ty

pprUserIfaceForAll :: [IfaceForAllBndr] -> SDoc
pprUserIfaceForAll tvs
   = sdocWithDynFlags $ \dflags ->
     ppWhen (any tv_has_kind_var tvs || gopt Opt_PrintExplicitForalls dflags) $
     pprIfaceForAll tvs
   where
     tv_has_kind_var (TvBndr (_,kind) _) = not (ifTypeIsVarFree kind)


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
      | tc `ifaceTyConHasKey` consDataConKey
      , (ITC_Invis _ (ITC_Vis ty1 (ITC_Vis ty2 ITC_Nil))) <- tys
      , (args, tl) <- gather ty2
      = (ty1:args, tl)
      | tc `ifaceTyConHasKey` nilDataConKey
      = ([], Nothing)
    gather ty = ([], Just ty)

pprIfaceTypeApp :: TyPrec -> IfaceTyCon -> IfaceTcArgs -> SDoc
pprIfaceTypeApp prec tc args = pprTyTcApp prec tc args

pprTyTcApp :: TyPrec -> IfaceTyCon -> IfaceTcArgs -> SDoc
pprTyTcApp ctxt_prec tc tys =
    sdocWithDynFlags $ \dflags ->
    getPprStyle $ \style ->
    pprTyTcApp' ctxt_prec tc tys dflags style

pprTyTcApp' :: TyPrec -> IfaceTyCon -> IfaceTcArgs
            -> DynFlags -> PprStyle -> SDoc
pprTyTcApp' ctxt_prec tc tys dflags style
  | ifaceTyConName tc `hasKey` ipClassKey
  , ITC_Vis (IfaceLitTy (IfaceStrTyLit n)) (ITC_Vis ty ITC_Nil) <- tys
  = maybeParen ctxt_prec FunPrec
    $ char '?' <> ftext n <> text "::" <> ppr_ty TopPrec ty

  | IfaceTupleTyCon arity sort <- ifaceTyConSort info
  , not (debugStyle style)
  , arity == ifaceVisTcArgsLength tys
  = pprTuple sort (ifaceTyConIsPromoted info) tys

  | IfaceSumTyCon arity <- ifaceTyConSort info
  = pprSum arity (ifaceTyConIsPromoted info) tys

  | tc `ifaceTyConHasKey` consDataConKey
  , not (gopt Opt_PrintExplicitKinds dflags)
  , ITC_Invis _ (ITC_Vis ty1 (ITC_Vis ty2 ITC_Nil)) <- tys
  = pprIfaceTyList ctxt_prec ty1 ty2

  | tc `ifaceTyConHasKey` tYPETyConKey
  , ITC_Vis (IfaceTyConApp rep ITC_Nil) ITC_Nil <- tys
  , rep `ifaceTyConHasKey` liftedRepDataConKey
  = kindStar

  | otherwise
  = sdocWithPprDebug $ \dbg ->
    if | not dbg && tc `ifaceTyConHasKey` errorMessageTypeErrorFamKey
         -- Suppress detail unles you _really_ want to see
         -> text "(TypeError ...)"

       | Just doc <- ppr_equality ctxt_prec tc (tcArgsIfaceTypes tys)
         -> doc

       | otherwise
         -> ppr_iface_tc_app ppr_ty ctxt_prec tc tys_wo_kinds
  where
    info = ifaceTyConInfo tc
    tys_wo_kinds = tcArgsIfaceTypes $ stripInvisArgs dflags tys

-- | Pretty-print a type-level equality.
--
-- See Note [Equality predicates in IfaceType]
-- and Note [The equality types story] in TysPrim
ppr_equality :: TyPrec -> IfaceTyCon -> [IfaceType] -> Maybe SDoc
ppr_equality ctxt_prec tc args
  | hetero_eq_tc
  , [k1, k2, t1, t2] <- args
  = Just $ print_equality (k1, k2, t1, t2)

  | hom_eq_tc
  , [k, t1, t2] <- args
  = Just $ print_equality (k, k, t1, t2)

  | otherwise
  = Nothing
  where
    homogeneous = case ifaceTyConSort $ ifaceTyConInfo tc of
                    IfaceEqualityTyCon hom -> hom
                    _other -> pprPanic "ppr_equality: homogeneity" (ppr tc)
    tc_name = ifaceTyConName tc
    pp = ppr_ty
    hom_eq_tc = tc_name `hasKey` eqTyConKey            -- (~)
    hetero_eq_tc = tc_name `hasKey` eqPrimTyConKey     -- (~#)
                || tc_name `hasKey` eqReprPrimTyConKey -- (~R#)
                || tc_name `hasKey` heqTyConKey        -- (~~)
    print_equality args =
        sdocWithDynFlags $ \dflags ->
        getPprStyle      $ \style  ->
        print_equality' args style dflags

    print_equality' (ki1, ki2, ty1, ty2) style dflags
      | print_eqs
      = ppr_infix_eq (ppr tc)

      | hetero_eq_tc
      , print_kinds || not homogeneous
      = ppr_infix_eq (text "~~")

      | otherwise
      = if tc_name `hasKey` eqReprPrimTyConKey
        then pprIfacePrefixApp ctxt_prec (text "Coercible")
                               [pp TyConPrec ty1, pp TyConPrec ty2]
        else pprIfaceInfixApp ctxt_prec (char '~')
                 (pp TyOpPrec ty1) (pp TyOpPrec ty2)
      where
        ppr_infix_eq eq_op
           = pprIfaceInfixApp ctxt_prec eq_op
                 (parens (pp TopPrec ty1 <+> dcolon <+> pp TyOpPrec ki1))
                 (parens (pp TopPrec ty2 <+> dcolon <+> pp TyOpPrec ki2))

        print_kinds = gopt Opt_PrintExplicitKinds dflags
        print_eqs   = gopt Opt_PrintEqualityRelations dflags ||
                      dumpStyle style || debugStyle style


pprIfaceCoTcApp :: TyPrec -> IfaceTyCon -> [IfaceCoercion] -> SDoc
pprIfaceCoTcApp ctxt_prec tc tys = ppr_iface_tc_app ppr_co ctxt_prec tc tys

ppr_iface_tc_app :: (TyPrec -> a -> SDoc) -> TyPrec -> IfaceTyCon -> [a] -> SDoc
ppr_iface_tc_app pp _ tc [ty]
  | tc `ifaceTyConHasKey` listTyConKey = pprPromotionQuote tc <> brackets (pp TopPrec ty)
  | tc `ifaceTyConHasKey` parrTyConKey = pprPromotionQuote tc <> paBrackets (pp TopPrec ty)

ppr_iface_tc_app pp ctxt_prec tc tys
  |  tc `ifaceTyConHasKey` starKindTyConKey
  || tc `ifaceTyConHasKey` liftedTypeKindTyConKey
  || tc `ifaceTyConHasKey` unicodeStarKindTyConKey
  = kindStar   -- Handle unicode; do not wrap * in parens

  | not (isSymOcc (nameOccName (ifaceTyConName tc)))
  = pprIfacePrefixApp ctxt_prec (ppr tc) (map (pp TyConPrec) tys)

  | [ty1,ty2] <- tys  -- Infix, two arguments;
                      -- we know nothing of precedence though
  = pprIfaceInfixApp ctxt_prec (ppr tc)
                     (pp TyOpPrec ty1) (pp TyOpPrec ty2)

  | otherwise
  = pprIfacePrefixApp ctxt_prec (parens (ppr tc)) (map (pp TyConPrec) tys)

pprSum :: Arity -> IsPromoted -> IfaceTcArgs -> SDoc
pprSum _arity is_promoted args
  =   -- drop the RuntimeRep vars.
      -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
    let tys   = tcArgsIfaceTypes args
        args' = drop (length tys `div` 2) tys
    in pprPromotionQuoteI is_promoted
       <> sumParens (pprWithBars (ppr_ty TopPrec) args')

pprTuple :: TupleSort -> IsPromoted -> IfaceTcArgs -> SDoc
pprTuple ConstraintTuple IsNotPromoted ITC_Nil
  = text "() :: Constraint"

-- All promoted constructors have kind arguments
pprTuple sort IsPromoted args
  = let tys = tcArgsIfaceTypes args
        args' = drop (length tys `div` 2) tys
    in pprPromotionQuoteI IsPromoted <>
       tupleParens sort (pprWithCommas pprIfaceType args')

pprTuple sort promoted args
  =   -- drop the RuntimeRep vars.
      -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
    let tys   = tcArgsIfaceTypes args
        args' = case sort of
                  UnboxedTuple -> drop (length tys `div` 2) tys
                  _            -> tys
    in
    pprPromotionQuoteI promoted <>
    tupleParens sort (pprWithCommas pprIfaceType args')

pprIfaceTyLit :: IfaceTyLit -> SDoc
pprIfaceTyLit (IfaceNumTyLit n) = integer n
pprIfaceTyLit (IfaceStrTyLit n) = text (show n)

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
  = maybeParen ctxt_prec FunPrec $
    pprIfaceForAllCoPart tvs (pprIfaceCoercion inner_co)
  where
    (tvs, inner_co) = split_co co

    split_co (IfaceForAllCo (name, _) kind_co co')
      = let (tvs, co'') = split_co co' in ((name,kind_co):tvs,co'')
    split_co co' = ([], co')

-- Why these two? See Note [TcTyVars in IfaceType]
ppr_co _         (IfaceFreeCoVar covar)     = ppr covar
ppr_co _         (IfaceCoVarCo covar)       = ppr covar

ppr_co ctxt_prec (IfaceUnivCo IfaceUnsafeCoerceProv r ty1 ty2)
  = maybeParen ctxt_prec TyConPrec $
    text "UnsafeCo" <+> ppr r <+>
    pprParendIfaceType ty1 <+> pprParendIfaceType ty2

ppr_co _ctxt_prec (IfaceUnivCo (IfaceHoleProv u) _ _ _)
 = braces $ ppr u

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
  = maybeParen ctxt_prec TyOpPrec $
    ppr_co TyOpPrec co1 <+> semi <+> ppr_co TyOpPrec co2
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
pprPromotionQuote tc =
    pprPromotionQuoteI $ ifaceTyConIsPromoted $ ifaceTyConInfo tc

pprPromotionQuoteI  :: IsPromoted -> SDoc
pprPromotionQuoteI IsNotPromoted = empty
pprPromotionQuoteI IsPromoted    = char '\''

instance Outputable IfaceCoercion where
  ppr = pprIfaceCoercion

instance Binary IfaceTyCon where
   put_ bh (IfaceTyCon n i) = put_ bh n >> put_ bh i

   get bh = do n <- get bh
               i <- get bh
               return (IfaceTyCon n i)

instance Binary IsPromoted where
   put_ bh IsNotPromoted = putByte bh 0
   put_ bh IsPromoted    = putByte bh 1

   get bh = do
       n <- getByte bh
       case n of
         0 -> return IsNotPromoted
         1 -> return IsPromoted
         _ -> fail "Binary(IsPromoted): fail)"

instance Binary IfaceTyConSort where
   put_ bh IfaceNormalTyCon             = putByte bh 0
   put_ bh (IfaceTupleTyCon arity sort) = putByte bh 1 >> put_ bh arity >> put_ bh sort
   put_ bh (IfaceSumTyCon arity)        = putByte bh 2 >> put_ bh arity
   put_ bh (IfaceEqualityTyCon hom)
     | hom                              = putByte bh 3
     | otherwise                        = putByte bh 4

   get bh = do
       n <- getByte bh
       case n of
         0 -> return IfaceNormalTyCon
         1 -> IfaceTupleTyCon <$> get bh <*> get bh
         2 -> IfaceSumTyCon <$> get bh
         3 -> return $ IfaceEqualityTyCon True
         4 -> return $ IfaceEqualityTyCon False
         _ -> fail "Binary(IfaceTyConSort): fail"

instance Binary IfaceTyConInfo where
   put_ bh (IfaceTyConInfo i s) = put_ bh i >> put_ bh s

   get bh = IfaceTyConInfo <$> get bh <*> get bh

instance Outputable IfaceTyLit where
  ppr = pprIfaceTyLit

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

-- Some notes about printing contexts
--
-- In the event that we are printing a singleton context (e.g. @Eq a@) we can
-- omit parentheses. However, we must take care to set the precedence correctly
-- to TyOpPrec, since something like @a :~: b@ must be parenthesized (see
-- #9658).
--
-- When printing a larger context we use 'fsep' instead of 'sep' so that
-- the context doesn't get displayed as a giant column. Rather than,
--  instance (Eq a,
--            Eq b,
--            Eq c,
--            Eq d,
--            Eq e,
--            Eq f,
--            Eq g,
--            Eq h,
--            Eq i,
--            Eq j,
--            Eq k,
--            Eq l) =>
--           Eq (a, b, c, d, e, f, g, h, i, j, k, l)
--
-- we want
--
--  instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--            Eq j, Eq k, Eq l) =>
--           Eq (a, b, c, d, e, f, g, h, i, j, k, l)



-- | Prints "(C a, D b) =>", including the arrow.
-- Used when we want to print a context in a type, so we
-- use FunPrec to decide whether to parenthesise a singleton
-- predicate; e.g.   Num a => a -> a
pprIfaceContextArr :: [IfacePredType] -> SDoc
pprIfaceContextArr []     = empty
pprIfaceContextArr [pred] = ppr_ty FunPrec pred <+> darrow
pprIfaceContextArr preds  = ppr_parend_preds preds <+> darrow

-- | Prints a context or @()@ if empty
-- You give it the context precedence
pprIfaceContext :: TyPrec -> [IfacePredType] -> SDoc
pprIfaceContext _    []     = text "()"
pprIfaceContext prec [pred] = ppr_ty prec pred
pprIfaceContext _    preds  = ppr_parend_preds preds

ppr_parend_preds :: [IfacePredType] -> SDoc
ppr_parend_preds preds = parens (fsep (punctuate comma (map ppr preds)))

instance Binary IfaceType where
    put_ _ (IfaceFreeTyVar tv)
       = pprPanic "Can't serialise IfaceFreeTyVar" (ppr tv)

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
  put_ _ (IfaceFreeCoVar cv)
       = pprPanic "Can't serialise IfaceFreeCoVar" (ppr cv)
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
  put_ _  (IfaceHoleProv _) =
          pprPanic "Binary(IfaceUnivCoProv) hit a hole" empty
  -- See Note [Holes in IfaceUnivCoProv]

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
