{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


This module defines interface types and binders
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
  -- FlexibleInstances for Binary (DefMethSpec IfaceType)
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- For NFData instance
{-# LANGUAGE UndecidableInstances #-}

module GHC.Iface.Type (
        IfExtName, IfLclName,

        IfaceType'(..), IfaceKind',
        IfaceContext', IfacePredType',
        IfaceCoercion'(..),IfaceMCoercion'(..),
        IfaceUnivCoProv'(..),
        IfaceMult',
        IfaceAppArgs'(..),
        IfaceBndr'(..),
        IfaceTvBndr', IfaceIdBndr',
        IfaceTyConBinder', IfaceForAllBndr', IfaceForAllSpecBndr',
        XXIfaceType, XXIfaceCoercion,

        IfaceType, IfaceKind,
        IfaceContext, IfacePredType,
        IfaceCoercion,IfaceMCoercion,
        IfaceUnivCoProv,
        IfaceMult,
        IfaceAppArgs,
        IfaceBndr,
        IfaceTvBndr, IfaceIdBndr,
        IfaceTyConBinder, IfaceForAllBndr, IfaceForAllSpecBndr,

        IfaceTyCon(..),
        IfaceTyConInfo(..), mkIfaceTyConInfo,
        IfaceTyConSort(..),
        IfaceTyLit(..),
        IfaceOneShot(..), IfaceLamBndr,
        ArgFlag(..), AnonArgFlag(..),
        mkIfaceForAllTvBndr,
        mkIfaceTyConKind,
        ifaceForAllSpecToBndrs, ifaceForAllSpecToBndr,

        ifForAllBndrVar, ifForAllBndrName, ifaceBndrName,
        ifTyConBinderVar, ifTyConBinderName,

        -- Equality testing
        isIfaceLiftedTypeKind,
        isIfaceTyConAppLiftedTypeKind,

        ifaceTyConHasKey,

        -- Conversion from IfaceAppArgs to IfaceTypes/ArgFlags
        appArgsIfaceTypes, appArgsIfaceTypesArgFlags,
        ifaceVisAppArgsLength,

        -- Printing
        PrintExplicitKinds(..),
        pprIfaceTyLit,
        pprPromotionQuote,
        pprPromotionQuoteI,

        suppressIfaceInvisibles,
        stripIfaceInvisVars,
        stripInvisArgs,

        splitIfaceSigmaTy, splitIfaceReqForallTy,

        mkIfaceTySubst, substIfaceTyVar, substIfaceAppArgs, inDomIfaceTySubst,

        many_ty
    ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Builtin.Types ( manyDataConTyCon )

import GHC.Core.TyCon hiding ( pprPromotionQuote )
import GHC.Core.Coercion.Axiom
import GHC.Types.Var
import GHC.Builtin.Names
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Utils.Binary
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Utils.Misc
import GHC.Utils.Panic

import Language.Haskell.Syntax.Extension ( DataConCantHappen, dataConCantHappen )

import Data.Maybe( isJust )
import qualified Data.Semigroup as Semi
import Control.DeepSeq

{-
************************************************************************
*                                                                      *
                Local (nested) binders
*                                                                      *
************************************************************************
-}

type IfLclName = FastString     -- A local name in iface syntax

type IfExtName = Name   -- An External or WiredIn Name can appear in Iface syntax
                        -- (However Internal or System Names never should)

data IfaceBndr' p        -- Local (non-top-level) binders
  = IfaceIdBndr {-# UNPACK #-} !(IfaceIdBndr' p)
  | IfaceTvBndr {-# UNPACK #-} !(IfaceTvBndr' p)

type IfaceIdBndr' p  = (IfaceType' p, IfLclName, IfaceType' p)
type IfaceTvBndr' p  = (IfLclName, IfaceKind' p)

type IfaceBndr = IfaceBndr' TtgIface

type IfaceTvBndr = IfaceTvBndr' TtgIface
type IfaceIdBndr = IfaceIdBndr' TtgIface

ifaceTvBndrName :: IfaceTvBndr' p -> IfLclName
ifaceTvBndrName (n,_) = n

ifaceIdBndrName :: IfaceIdBndr' p -> IfLclName
ifaceIdBndrName (_,n,_) = n

ifaceBndrName :: IfaceBndr' p -> IfLclName
ifaceBndrName (IfaceTvBndr bndr) = ifaceTvBndrName bndr
ifaceBndrName (IfaceIdBndr bndr) = ifaceIdBndrName bndr

ifaceBndrType :: IfaceBndr' p -> IfaceType' p
ifaceBndrType (IfaceIdBndr (_, _, t)) = t
ifaceBndrType (IfaceTvBndr (_, t)) = t

type IfaceLamBndr = (IfaceBndr, IfaceOneShot)

data IfaceOneShot    -- See Note [Preserve OneShotInfo] in "GHC.Core.Tidy"
  = IfaceNoOneShot   -- and Note [The oneShot function] in "GHC.Types.Id.Make"
  | IfaceOneShot

instance Outputable IfaceOneShot where
  ppr IfaceNoOneShot = text "NoOneShotInfo"
  ppr IfaceOneShot = text "OneShot"

{-
%************************************************************************
%*                                                                      *
                IfaceType
%*                                                                      *
%************************************************************************
-}

-------------------------------
type IfaceKind     = IfaceType

-- | A kind of universal type, used for types and kinds.
--
-- Any time a 'Type' is pretty-printed, it is first converted to an 'IfaceType'
-- before being printed. See Note [Pretty printing via Iface syntax] in "GHC.Types.TyThing.Ppr"
data IfaceType' p
  = IfaceTyVar     IfLclName            -- Type/coercion variable only, not tycon
  | IfaceLitTy     IfaceTyLit
  | IfaceAppTy     (IfaceType' p) (IfaceAppArgs' p)
                             -- See Note [Suppressing invisible arguments] for
                             -- an explanation of why the second field isn't
                             -- IfaceType, analogous to AppTy.
  | IfaceFunTy     AnonArgFlag (IfaceMult' p) (IfaceType' p) (IfaceType' p)
  | IfaceForAllTy  (IfaceForAllBndr' p) (IfaceType' p)
  | IfaceTyConApp  IfaceTyCon (IfaceAppArgs' p) -- Not necessarily saturated
                                                -- Includes newtypes, synonyms, tuples
  | IfaceCastTy     (IfaceType' p) (IfaceCoercion' p)
  | IfaceCoercionTy (IfaceCoercion' p)

  | IfaceTupleTy                  -- Saturated tuples (unsaturated ones use IfaceTyConApp)
       TupleSort                  -- What sort of tuple?
       PromotionFlag                 -- A bit like IfaceTyCon
       (IfaceAppArgs' p)          -- arity = length args
          -- For promoted data cons, the kind args are omitted
          -- Why have this? Only for efficiency: IfaceTupleTy can omit the
          -- type arguments, as they can be recreated when deserializing.
          -- In an experiment, removing IfaceTupleTy resulted in a 0.75% regression
          -- in interface file size (in GHC's boot libraries).
          -- See !3987.
  | XIfaceType (XXIfaceType p)

type IfaceKind' p = IfaceType' p
type IfaceMult' p = IfaceType' p

type family XXIfaceType p

data TtgIface
type instance XXIfaceType TtgIface = DataConCantHappen
type IfaceType = IfaceType' TtgIface
type IfaceMult = IfaceType

type IfacePredType' p = IfaceType' p
type IfaceContext' p = [IfacePredType' p]

type IfacePredType = IfacePredType' TtgIface
type IfaceContext = IfaceContext' TtgIface

data IfaceTyLit
  = IfaceNumTyLit Integer
  | IfaceStrTyLit FastString
  | IfaceCharTyLit Char
  deriving (Eq)

type IfaceTyConBinder' p    = VarBndr (IfaceBndr' p) TyConBndrVis
type IfaceForAllBndr' p     = VarBndr (IfaceBndr' p) ArgFlag
type IfaceForAllSpecBndr' p = VarBndr (IfaceBndr' p) Specificity

type IfaceTyConBinder    = IfaceTyConBinder' TtgIface
type IfaceForAllBndr     = IfaceForAllBndr' TtgIface
type IfaceForAllSpecBndr = IfaceForAllSpecBndr' TtgIface

-- | Make an 'IfaceForAllBndr' from an 'IfaceTvBndr'.
mkIfaceForAllTvBndr :: ArgFlag -> IfaceTvBndr' p -> IfaceForAllBndr' p
mkIfaceForAllTvBndr vis var = Bndr (IfaceTvBndr var) vis

-- | Build the 'tyConKind' from the binders and the result kind.
-- Keep in sync with 'mkTyConKind' in "GHC.Core.TyCon".
mkIfaceTyConKind :: [IfaceTyConBinder' p] -> IfaceKind' p -> IfaceKind' p
mkIfaceTyConKind bndrs res_kind = foldr mk res_kind bndrs
  where
    mk :: IfaceTyConBinder' p -> IfaceKind' p -> IfaceKind' p
    mk (Bndr tv (AnonTCB af))   k = IfaceFunTy af many_ty (ifaceBndrType tv) k
    mk (Bndr tv (NamedTCB vis)) k = IfaceForAllTy (Bndr tv vis) k

ifaceForAllSpecToBndrs :: [IfaceForAllSpecBndr' p] -> [IfaceForAllBndr' p]
ifaceForAllSpecToBndrs = map ifaceForAllSpecToBndr

ifaceForAllSpecToBndr :: IfaceForAllSpecBndr' p -> IfaceForAllBndr' p
ifaceForAllSpecToBndr (Bndr tv spec) = Bndr tv (Invisible spec)

-- | Stores the arguments in a type application as a list.
-- See @Note [Suppressing invisible arguments]@.
data IfaceAppArgs' p
  = IA_Nil
  | IA_Arg (IfaceType' p) -- The type argument

           ArgFlag      -- The argument's visibility. We store this here so
                        -- that we can:
                        --
                        -- 1. Avoid pretty-printing invisible (i.e., specified
                        --    or inferred) arguments when
                        --    -fprint-explicit-kinds isn't enabled, or
                        -- 2. When -fprint-explicit-kinds *is*, enabled, print
                        --    specified arguments in @(...) and inferred
                        --    arguments in @{...}.

           (IfaceAppArgs' p) -- The rest of the arguments

instance Semi.Semigroup (IfaceAppArgs' p) where
  IA_Nil <> xs              = xs
  IA_Arg ty argf rest <> xs = IA_Arg ty argf (rest Semi.<> xs)

instance Monoid (IfaceAppArgs' p) where
  mempty = IA_Nil
  mappend = (Semi.<>)

type IfaceAppArgs = IfaceAppArgs' TtgIface

-- Encodes type constructors, kind constructors,
-- coercion constructors, the lot.
-- We have to tag them in order to pretty print them
-- properly.
data IfaceTyCon = IfaceTyCon { ifaceTyConName :: IfExtName
                             , ifaceTyConInfo :: IfaceTyConInfo }
    deriving (Eq)

-- | The various types of TyCons which have special, built-in syntax.
data IfaceTyConSort = IfaceNormalTyCon          -- ^ a regular tycon

                    | IfaceTupleTyCon !Arity !TupleSort
                      -- ^ a tuple, e.g. @(a, b, c)@ or @(#a, b, c#)@.
                      -- The arity is the tuple width, not the tycon arity
                      -- (which is twice the width in the case of unboxed
                      -- tuples).

                    | IfaceSumTyCon !Arity
                      -- ^ an unboxed sum, e.g. @(# a | b | c #)@

                    | IfaceEqualityTyCon
                      -- ^ A heterogeneous equality TyCon
                      --   (i.e. eqPrimTyCon, eqReprPrimTyCon, heqTyCon)
                      -- that is actually being applied to two types
                      -- of the same kind.  This affects pretty-printing
                      -- only: see Note [Equality predicates in IfaceType]
                    deriving (Eq)

instance Outputable IfaceTyConSort where
  ppr IfaceNormalTyCon         = text "normal"
  ppr (IfaceTupleTyCon n sort) = ppr sort <> colon <> ppr n
  ppr (IfaceSumTyCon n)        = text "sum:" <> ppr n
  ppr IfaceEqualityTyCon       = text "equality"

{- Note [Free tyvars in IfaceType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nowadays (since Nov 16, 2016) we pretty-print a Type by converting to
an IfaceType and pretty printing that.  This eliminates a lot of
pretty-print duplication, and it matches what we do with pretty-
printing TyThings. See Note [Pretty printing via Iface syntax] in GHC.Types.TyThing.Ppr.

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
in GHC.Builtin.Types.Prim for details).  In an effort to avoid confusing users, we suppress
the differences during pretty printing unless certain flags are enabled.
Here is how each equality predicate* is printed in homogeneous and
heterogeneous contexts, depending on which combination of the
-fprint-explicit-kinds and -fprint-equality-relations flags is used:

--------------------------------------------------------------------------------------------
|         Predicate             |        Neither flag        |    -fprint-explicit-kinds   |
|-------------------------------|----------------------------|-----------------------------|
| a ~ b         (homogeneous)   |        a ~ b               | (a :: Type) ~  (b :: Type)  |
| a ~~ b,       homogeneously   |        a ~ b               | (a :: Type) ~  (b :: Type)  |
| a ~~ b,       heterogeneously |        a ~~ c              | (a :: Type) ~~ (c :: k)     |
| a ~# b,       homogeneously   |        a ~ b               | (a :: Type) ~  (b :: Type)  |
| a ~# b,       heterogeneously |        a ~~ c              | (a :: Type) ~~ (c :: k)     |
| Coercible a b (homogeneous)   |        Coercible a b       | Coercible @Type a b         |
| a ~R# b,      homogeneously   |        Coercible a b       | Coercible @Type a b         |
| a ~R# b,      heterogeneously |        a ~R# b             | (a :: Type) ~R# (c :: k)    |
|-------------------------------|----------------------------|-----------------------------|
|         Predicate             | -fprint-equality-relations |          Both flags         |
|-------------------------------|----------------------------|-----------------------------|
| a ~ b         (homogeneous)   |        a ~  b              | (a :: Type) ~  (b :: Type)  |
| a ~~ b,       homogeneously   |        a ~~ b              | (a :: Type) ~~ (b :: Type)  |
| a ~~ b,       heterogeneously |        a ~~ c              | (a :: Type) ~~ (c :: k)     |
| a ~# b,       homogeneously   |        a ~# b              | (a :: Type) ~# (b :: Type)  |
| a ~# b,       heterogeneously |        a ~# c              | (a :: Type) ~# (c :: k)     |
| Coercible a b (homogeneous)   |        Coercible a b       | Coercible @Type a b         |
| a ~R# b,      homogeneously   |        a ~R# b             | (a :: Type) ~R# (b :: Type) |
| a ~R# b,      heterogeneously |        a ~R# b             | (a :: Type) ~R# (c :: k)    |
--------------------------------------------------------------------------------------------

(* There is no heterogeneous, representational, lifted equality counterpart
to (~~). There could be, but there seems to be no use for it.)

This table adheres to the following rules:

A. With -fprint-equality-relations, print the true equality relation.
B. Without -fprint-equality-relations:
     i. If the equality is representational and homogeneous, use Coercible.
    ii. Otherwise, if the equality is representational, use ~R#.
   iii. If the equality is nominal and homogeneous, use ~.
    iv. Otherwise, if the equality is nominal, use ~~.
C. With -fprint-explicit-kinds, print kinds on both sides of an infix operator,
   as above; or print the kind with Coercible.
D. Without -fprint-explicit-kinds, don't print kinds.

A hetero-kinded equality is used homogeneously when it is applied to two
identical kinds. Unfortunately, determining this from an IfaceType isn't
possible since we can't see through type synonyms. Consequently, we need to
record whether this particular application is homogeneous in IfaceTyConSort
for the purposes of pretty-printing.

See Note [The equality types story] in GHC.Builtin.Types.Prim.
-}

data IfaceTyConInfo   -- Used to guide pretty-printing
                      -- and to disambiguate D from 'D (they share a name)
  = IfaceTyConInfo { ifaceTyConIsPromoted :: PromotionFlag
                   , ifaceTyConSort       :: IfaceTyConSort }
    deriving (Eq)

-- This smart constructor allows sharing of the two most common
-- cases. See #19194
mkIfaceTyConInfo :: PromotionFlag -> IfaceTyConSort -> IfaceTyConInfo
mkIfaceTyConInfo IsPromoted  IfaceNormalTyCon = IfaceTyConInfo IsPromoted  IfaceNormalTyCon
mkIfaceTyConInfo NotPromoted IfaceNormalTyCon = IfaceTyConInfo NotPromoted IfaceNormalTyCon
mkIfaceTyConInfo prom        sort             = IfaceTyConInfo prom        sort

data IfaceMCoercion' p
  = IfaceMRefl
  | IfaceMCo (IfaceCoercion' p)

data IfaceCoercion' p
  = IfaceReflCo       (IfaceType' p)
  | IfaceGReflCo      Role (IfaceType' p) (IfaceMCoercion' p)
  | IfaceFunCo        Role (IfaceCoercion' p) (IfaceCoercion' p) (IfaceCoercion' p)
  | IfaceTyConAppCo   Role IfaceTyCon [(IfaceCoercion' p)]
  | IfaceAppCo        (IfaceCoercion' p) (IfaceCoercion' p)
  | IfaceForAllCo     IfaceBndr (IfaceCoercion' p) (IfaceCoercion' p)
  | IfaceCoVarCo      IfLclName
  | IfaceAxiomInstCo  IfExtName BranchIndex [(IfaceCoercion' p)]
  | IfaceAxiomRuleCo  IfLclName [(IfaceCoercion' p)]
       -- There are only a fixed number of CoAxiomRules, so it suffices
       -- to use an IfaceLclName to distinguish them.
       -- See Note [Adding built-in type families] in GHC.Builtin.Types.Literals
  | IfaceUnivCo       (IfaceUnivCoProv' p) Role (IfaceType' p) (IfaceType' p)
  | IfaceSymCo        (IfaceCoercion' p)
  | IfaceTransCo      (IfaceCoercion' p) (IfaceCoercion' p)
  | IfaceNthCo        Int (IfaceCoercion' p)
  | IfaceLRCo         LeftOrRight (IfaceCoercion' p)
  | IfaceInstCo       (IfaceCoercion' p) (IfaceCoercion' p)
  | IfaceKindCo       (IfaceCoercion' p)
  | IfaceSubCo        (IfaceCoercion' p)
  | XIfaceCoercion    (XXIfaceCoercion p)

type family XXIfaceCoercion p

type instance XXIfaceCoercion TtgIface = DataConCantHappen

data IfaceUnivCoProv' p
  = IfacePhantomProv (IfaceCoercion' p)
  | IfaceProofIrrelProv (IfaceCoercion' p)
  | IfacePluginProv String
  | IfaceCorePrepProv Bool  -- See defn of CorePrepProv

type IfaceMCoercion = IfaceMCoercion' TtgIface

type IfaceCoercion = IfaceCoercion' TtgIface

type IfaceUnivCoProv = IfaceUnivCoProv' TtgIface

{- Note [Holes in IfaceCoercion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking fails the typechecker will produce a HoleCo to stand
in place of the unproven assertion. While we generally don't want to
let these unproven assertions leak into interface files, we still need
to be able to pretty-print them as we use IfaceType's pretty-printer
to render Types. For this reason IfaceCoercion has a IfaceHoleCo
constructor; however, we fails when asked to serialize to a
IfaceHoleCo to ensure that they don't end up in an interface file.


%************************************************************************
%*                                                                      *
                Functions over IFaceTypes
*                                                                      *
************************************************************************
-}

ifaceTyConHasKey :: IfaceTyCon -> Unique -> Bool
ifaceTyConHasKey tc key = ifaceTyConName tc `hasKey` key

-- | Given a kind K, is K of the form (TYPE ('BoxedRep 'LiftedRep))?
isIfaceLiftedTypeKind :: IfaceKind' p -> Bool
isIfaceLiftedTypeKind (IfaceTyConApp tc IA_Nil)
  = isLiftedTypeKindTyConName (ifaceTyConName tc)
isIfaceLiftedTypeKind (IfaceTyConApp tc1 args1)
  = isIfaceTyConAppLiftedTypeKind tc1 args1
isIfaceLiftedTypeKind _ = False

-- | Given a kind constructor K and arguments A, returns true if
-- both of the following statements are true:
--
-- * K is TYPE
-- * A is a singleton IfaceAppArgs of the form ('BoxedRep 'Lifted)
--
-- For the second condition, we must also check for the type
-- synonym LiftedRep.
isIfaceTyConAppLiftedTypeKind :: IfaceTyCon -> IfaceAppArgs' p -> Bool
isIfaceTyConAppLiftedTypeKind tc1 args1
  | tc1 `ifaceTyConHasKey` tYPETyConKey
  , IA_Arg soleArg1 Required IA_Nil <- args1
  , IfaceTyConApp rep args2 <- soleArg1 =
    if | rep `ifaceTyConHasKey` boxedRepDataConKey
       , IA_Arg soleArg2 Required IA_Nil <- args2
       , IfaceTyConApp lev IA_Nil <- soleArg2
       , lev `ifaceTyConHasKey` liftedDataConKey -> True
       | rep `ifaceTyConHasKey` liftedRepTyConKey
       , IA_Nil <- args2 -> True
       | otherwise -> False
  | otherwise = False

splitIfaceSigmaTy :: IfaceType' p -> ([IfaceForAllBndr' p], [IfacePredType' p], IfaceType' p)
-- Mainly for printing purposes
--
-- Here we split nested IfaceSigmaTy properly.
--
-- @
-- forall t. T t => forall m a b. M m => (a -> m b) -> t a -> m (t b)
-- @
--
-- If you called @splitIfaceSigmaTy@ on this type:
--
-- @
-- ([t, m, a, b], [T t, M m], (a -> m b) -> t a -> m (t b))
-- @
splitIfaceSigmaTy ty
  = case (bndrs, theta) of
      ([], []) -> (bndrs, theta, tau)
      _        -> let (bndrs', theta', tau') = splitIfaceSigmaTy tau
                   in (bndrs ++ bndrs', theta ++ theta', tau')
  where
    (bndrs, rho)   = split_foralls ty
    (theta, tau)   = split_rho rho

    split_foralls (IfaceForAllTy bndr ty)
        | isInvisibleArgFlag (binderArgFlag bndr)
        = case split_foralls ty of { (bndrs, rho) -> (bndr:bndrs, rho) }
    split_foralls rho = ([], rho)

    split_rho (IfaceFunTy InvisArg _ ty1 ty2)
        = case split_rho ty2 of { (ps, tau) -> (ty1:ps, tau) }
    split_rho tau = ([], tau)

splitIfaceReqForallTy :: IfaceType' p -> ([IfaceForAllBndr' p], IfaceType' p)
splitIfaceReqForallTy (IfaceForAllTy bndr ty)
  | isVisibleArgFlag (binderArgFlag bndr)
  = case splitIfaceReqForallTy ty of { (bndrs, rho) -> (bndr:bndrs, rho) }
splitIfaceReqForallTy rho = ([], rho)

newtype PrintExplicitKinds = PrintExplicitKinds Bool

suppressIfaceInvisibles :: PrintExplicitKinds -> [IfaceTyConBinder' p] -> [a] -> [a]
suppressIfaceInvisibles (PrintExplicitKinds True) _tys xs = xs
suppressIfaceInvisibles (PrintExplicitKinds False) tys xs = suppress tys xs
    where
      suppress _       []      = []
      suppress []      a       = a
      suppress (k:ks) (x:xs)
        | isInvisibleTyConBinder k =     suppress ks xs
        | otherwise                = x : suppress ks xs

stripIfaceInvisVars :: PrintExplicitKinds -> [IfaceTyConBinder' p] -> [IfaceTyConBinder' p]
stripIfaceInvisVars (PrintExplicitKinds True)  tyvars = tyvars
stripIfaceInvisVars (PrintExplicitKinds False) tyvars
  = filterOut isInvisibleTyConBinder tyvars

-- | Extract an 'IfaceBndr' from an 'IfaceForAllBndr'.
ifForAllBndrVar :: IfaceForAllBndr' p -> IfaceBndr' p
ifForAllBndrVar = binderVar

-- | Extract the variable name from an 'IfaceForAllBndr'.
ifForAllBndrName :: IfaceForAllBndr' p -> IfLclName
ifForAllBndrName fab = ifaceBndrName (ifForAllBndrVar fab)

-- | Extract an 'IfaceBndr' from an 'IfaceTyConBinder'.
ifTyConBinderVar :: IfaceTyConBinder' p -> IfaceBndr' p
ifTyConBinderVar = binderVar

-- | Extract the variable name from an 'IfaceTyConBinder'.
ifTyConBinderName :: IfaceTyConBinder' p -> IfLclName
ifTyConBinderName tcb = ifaceBndrName (ifTyConBinderVar tcb)

{- Note [Substitution on IfaceType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Substitutions on IfaceType are done only during pretty-printing to
construct the result type of a GADT, and does not deal with binders
(eg IfaceForAll), so it doesn't need fancy capture stuff.  -}

type IfaceTySubst' p = FastStringEnv (IfaceType' p) -- Note [Substitution on IfaceType]

mkIfaceTySubst :: [(IfLclName, IfaceType' p)] -> IfaceTySubst' p
-- See Note [Substitution on IfaceType]
mkIfaceTySubst eq_spec = mkFsEnv eq_spec

inDomIfaceTySubst :: IfaceTySubst' p -> IfaceTvBndr' p -> Bool
-- See Note [Substitution on IfaceType]
inDomIfaceTySubst subst (fs, _) = isJust (lookupFsEnv subst fs)

substIfaceType :: Outputable (IfaceType' p) => IfaceTySubst' p -> IfaceType' p -> IfaceType' p
-- See Note [Substitution on IfaceType]
substIfaceType env ty
  = go ty
  where
    go ty@(XIfaceType _)      = ty -- assume extension unsubstituted for now
    go (IfaceTyVar tv)        = substIfaceTyVar env tv
    go (IfaceAppTy  t ts)     = IfaceAppTy  (go t) (substIfaceAppArgs env ts)
    go (IfaceFunTy af w t1 t2)  = IfaceFunTy af (go w) (go t1) (go t2)
    go ty@(IfaceLitTy {})     = ty
    go (IfaceTyConApp tc tys) = IfaceTyConApp tc (substIfaceAppArgs env tys)
    go (IfaceTupleTy s i tys) = IfaceTupleTy s i (substIfaceAppArgs env tys)
    go (IfaceForAllTy {})     = pprPanic "substIfaceType" (ppr ty)
    go (IfaceCastTy ty co)    = IfaceCastTy (go ty) (go_co co)
    go (IfaceCoercionTy co)   = IfaceCoercionTy (go_co co)

    go_mco IfaceMRefl    = IfaceMRefl
    go_mco (IfaceMCo co) = IfaceMCo $ go_co co

    go_co co@(XIfaceCoercion _)      = co -- assume extension unsubstituted for now
    go_co (IfaceReflCo ty)           = IfaceReflCo (go ty)
    go_co (IfaceGReflCo r ty mco)    = IfaceGReflCo r (go ty) (go_mco mco)
    go_co (IfaceFunCo r w c1 c2)     = IfaceFunCo r (go_co w) (go_co c1) (go_co c2)
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
    go_co (IfaceKindCo co)           = IfaceKindCo (go_co co)
    go_co (IfaceSubCo co)            = IfaceSubCo (go_co co)
    go_co (IfaceAxiomRuleCo n cos)   = IfaceAxiomRuleCo n (go_cos cos)

    go_cos = map go_co

    go_prov (IfacePhantomProv co)    = IfacePhantomProv (go_co co)
    go_prov (IfaceProofIrrelProv co) = IfaceProofIrrelProv (go_co co)
    go_prov co@(IfacePluginProv _)   = co
    go_prov co@(IfaceCorePrepProv _) = co

substIfaceAppArgs :: Outputable (IfaceType' p) => IfaceTySubst' p -> IfaceAppArgs' p -> IfaceAppArgs' p
substIfaceAppArgs env args
  = go args
  where
    go IA_Nil              = IA_Nil
    go (IA_Arg ty arg tys) = IA_Arg (substIfaceType env ty) arg (go tys)

substIfaceTyVar :: IfaceTySubst' p -> IfLclName -> IfaceType' p
substIfaceTyVar env tv
  | Just ty <- lookupFsEnv env tv = ty
  | otherwise                     = IfaceTyVar tv


{-
************************************************************************
*                                                                      *
                Functions over IfaceAppArgs
*                                                                      *
************************************************************************
-}

stripInvisArgs :: PrintExplicitKinds -> IfaceAppArgs' p -> IfaceAppArgs' p
stripInvisArgs (PrintExplicitKinds True)  tys = tys
stripInvisArgs (PrintExplicitKinds False) tys = suppress_invis tys
    where
      suppress_invis c
        = case c of
            IA_Nil -> IA_Nil
            IA_Arg t argf ts
              |  isVisibleArgFlag argf
              -> IA_Arg t argf $ suppress_invis ts
              -- Keep recursing through the remainder of the arguments, as it's
              -- possible that there are remaining invisible ones.
              -- See the "In type declarations" section of Note [VarBndrs,
              -- TyCoVarBinders, TyConBinders, and visibility] in GHC.Core.TyCo.Rep.
              |  otherwise
              -> suppress_invis ts

appArgsIfaceTypes :: IfaceAppArgs' p -> [IfaceType' p]
appArgsIfaceTypes IA_Nil = []
appArgsIfaceTypes (IA_Arg t _ ts) = t : appArgsIfaceTypes ts

appArgsIfaceTypesArgFlags :: IfaceAppArgs' p -> [(IfaceType' p, ArgFlag)]
appArgsIfaceTypesArgFlags IA_Nil = []
appArgsIfaceTypesArgFlags (IA_Arg t a ts)
                                 = (t, a) : appArgsIfaceTypesArgFlags ts

ifaceVisAppArgsLength :: IfaceAppArgs' p -> Int
ifaceVisAppArgsLength = go 0
  where
    go !n IA_Nil = n
    go n  (IA_Arg _ argf rest)
      | isVisibleArgFlag argf = go (n+1) rest
      | otherwise             = go n rest

-- | The type 'Many :: Multiplicity'.
many_ty :: IfaceType' p
many_ty =
    IfaceTyConApp (IfaceTyCon dc_name (mkIfaceTyConInfo IsPromoted IfaceNormalTyCon))
                  IA_Nil
  where dc_name = getName manyDataConTyCon

{-
************************************************************************
*                                                                      *
                Pretty-printing
*                                                                      *
************************************************************************
-}

instance Outputable IfaceTyLit where
  ppr = pprIfaceTyLit

pprIfaceTyLit :: IfaceTyLit -> SDoc
pprIfaceTyLit (IfaceNumTyLit n) = integer n
pprIfaceTyLit (IfaceStrTyLit n) = text (show n)
pprIfaceTyLit (IfaceCharTyLit c) = text (show c)

instance Outputable IfaceTyCon where
  ppr tc = pprPromotionQuote tc <> ppr (ifaceTyConName tc)

instance Outputable IfaceTyConInfo where
  ppr (IfaceTyConInfo { ifaceTyConIsPromoted = prom
                      , ifaceTyConSort       = sort })
    = angleBrackets $ ppr prom <> comma <+> ppr sort

pprPromotionQuote :: IfaceTyCon -> SDoc
pprPromotionQuote tc =
    pprPromotionQuoteI $ ifaceTyConIsPromoted $ ifaceTyConInfo tc

pprPromotionQuoteI  :: PromotionFlag -> SDoc
pprPromotionQuoteI NotPromoted = empty
pprPromotionQuoteI IsPromoted    = char '\''

{-
************************************************************************
*                                                                      *
                Instances
*                                                                      *
************************************************************************
-}


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
    put_ bh IfaceNoOneShot =
            putByte bh 0
    put_ bh IfaceOneShot =
            putByte bh 1
    get bh = do
            h <- getByte bh
            case h of
              0 -> return IfaceNoOneShot
              _ -> return IfaceOneShot

instance Binary IfaceTyCon where
   put_ bh (IfaceTyCon n i) = put_ bh n >> put_ bh i

   get bh = do n <- get bh
               i <- get bh
               return (IfaceTyCon n i)

instance Binary IfaceTyConSort where
   put_ bh IfaceNormalTyCon             = putByte bh 0
   put_ bh (IfaceTupleTyCon arity sort) = putByte bh 1 >> put_ bh arity >> put_ bh sort
   put_ bh (IfaceSumTyCon arity)        = putByte bh 2 >> put_ bh arity
   put_ bh IfaceEqualityTyCon           = putByte bh 3

   get bh = do
       n <- getByte bh
       case n of
         0 -> return IfaceNormalTyCon
         1 -> IfaceTupleTyCon <$> get bh <*> get bh
         2 -> IfaceSumTyCon <$> get bh
         _ -> return IfaceEqualityTyCon

instance Binary IfaceTyConInfo where
   put_ bh (IfaceTyConInfo i s) = put_ bh i >> put_ bh s

   get bh = mkIfaceTyConInfo <$> get bh <*> get bh

instance Binary IfaceTyLit where
  put_ bh (IfaceNumTyLit n)   = putByte bh 1 >> put_ bh n
  put_ bh (IfaceStrTyLit n)   = putByte bh 2 >> put_ bh n
  put_ bh (IfaceCharTyLit n)  = putByte bh 3 >> put_ bh n

  get bh =
    do tag <- getByte bh
       case tag of
         1 -> do { n <- get bh
                 ; return (IfaceNumTyLit n) }
         2 -> do { n <- get bh
                 ; return (IfaceStrTyLit n) }
         3 -> do { n <- get bh
                 ; return (IfaceCharTyLit n) }
         _ -> panic ("get IfaceTyLit " ++ show tag)

instance Binary IfaceAppArgs where
  put_ bh tk =
    case tk of
      IA_Arg t a ts -> putByte bh 0 >> put_ bh t >> put_ bh a >> put_ bh ts
      IA_Nil        -> putByte bh 1

  get bh =
    do c <- getByte bh
       case c of
         0 -> do
           t  <- get bh
           a  <- get bh
           ts <- get bh
           return $! IA_Arg t a ts
         1 -> return IA_Nil
         _ -> panic ("get IfaceAppArgs " ++ show c)

instance Binary IfaceType where
    put_ _  (XIfaceType x) = dataConCantHappen x
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
    put_ bh (IfaceFunTy af aw ag ah) = do
            putByte bh 3
            put_ bh af
            put_ bh aw
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
              3 -> do af <- get bh
                      aw <- get bh
                      ag <- get bh
                      ah <- get bh
                      return (IfaceFunTy af aw ag ah)
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

instance Binary IfaceMCoercion where
  put_ bh IfaceMRefl =
          putByte bh 1
  put_ bh (IfaceMCo co) = do
          putByte bh 2
          put_ bh co

  get bh = do
    tag <- getByte bh
    case tag of
         1 -> return IfaceMRefl
         2 -> do a <- get bh
                 return $ IfaceMCo a
         _ -> panic ("get IfaceMCoercion " ++ show tag)

instance Binary IfaceCoercion where
  put_ _  (XIfaceCoercion x) = dataConCantHappen x
  put_ bh (IfaceReflCo a) = do
          putByte bh 1
          put_ bh a
  put_ bh (IfaceGReflCo a b c) = do
          putByte bh 2
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceFunCo a w b c) = do
          putByte bh 3
          put_ bh a
          put_ bh w
          put_ bh b
          put_ bh c
  put_ bh (IfaceTyConAppCo a b c) = do
          putByte bh 4
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceAppCo a b) = do
          putByte bh 5
          put_ bh a
          put_ bh b
  put_ bh (IfaceForAllCo a b c) = do
          putByte bh 6
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceCoVarCo a) = do
          putByte bh 7
          put_ bh a
  put_ bh (IfaceAxiomInstCo a b c) = do
          putByte bh 8
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceUnivCo a b c d) = do
          putByte bh 9
          put_ bh a
          put_ bh b
          put_ bh c
          put_ bh d
  put_ bh (IfaceSymCo a) = do
          putByte bh 10
          put_ bh a
  put_ bh (IfaceTransCo a b) = do
          putByte bh 11
          put_ bh a
          put_ bh b
  put_ bh (IfaceNthCo a b) = do
          putByte bh 12
          put_ bh a
          put_ bh b
  put_ bh (IfaceLRCo a b) = do
          putByte bh 13
          put_ bh a
          put_ bh b
  put_ bh (IfaceInstCo a b) = do
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
                   return $ IfaceReflCo a
           2 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceGReflCo a b c
           3 -> do a <- get bh
                   w <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceFunCo a w b c
           4 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceTyConAppCo a b c
           5 -> do a <- get bh
                   b <- get bh
                   return $ IfaceAppCo a b
           6 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceForAllCo a b c
           7 -> do a <- get bh
                   return $ IfaceCoVarCo a
           8 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceAxiomInstCo a b c
           9 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   d <- get bh
                   return $ IfaceUnivCo a b c d
           10-> do a <- get bh
                   return $ IfaceSymCo a
           11-> do a <- get bh
                   b <- get bh
                   return $ IfaceTransCo a b
           12-> do a <- get bh
                   b <- get bh
                   return $ IfaceNthCo a b
           13-> do a <- get bh
                   b <- get bh
                   return $ IfaceLRCo a b
           14-> do a <- get bh
                   b <- get bh
                   return $ IfaceInstCo a b
           15-> do a <- get bh
                   return $ IfaceKindCo a
           16-> do a <- get bh
                   return $ IfaceSubCo a
           17-> do a <- get bh
                   b <- get bh
                   return $ IfaceAxiomRuleCo a b
           _ -> panic ("get IfaceCoercion " ++ show tag)

instance Binary IfaceUnivCoProv where
  put_ bh (IfacePhantomProv a) = do
          putByte bh 1
          put_ bh a
  put_ bh (IfaceProofIrrelProv a) = do
          putByte bh 2
          put_ bh a
  put_ bh (IfacePluginProv a) = do
          putByte bh 3
          put_ bh a
  put_ bh (IfaceCorePrepProv a) = do
          putByte bh 4
          put_ bh a

  get bh = do
      tag <- getByte bh
      case tag of
           1 -> do a <- get bh
                   return $ IfacePhantomProv a
           2 -> do a <- get bh
                   return $ IfaceProofIrrelProv a
           3 -> do a <- get bh
                   return $ IfacePluginProv a
           4 -> do a <- get bh
                   return (IfaceCorePrepProv a)
           _ -> panic ("get IfaceUnivCoProv " ++ show tag)


instance Binary (DefMethSpec IfaceType) where
    put_ bh VanillaDM     = putByte bh 0
    put_ bh (GenericDM t) = putByte bh 1 >> put_ bh t
    get bh = do
            h <- getByte bh
            case h of
              0 -> return VanillaDM
              _ -> do { t <- get bh; return (GenericDM t) }

instance (NFData (XXIfaceType p), NFData (XXIfaceCoercion p)) => NFData (IfaceType' p) where
  rnf = \case
    XIfaceType f1 -> rnf f1
    IfaceTyVar f1 -> rnf f1
    IfaceLitTy f1 -> rnf f1
    IfaceAppTy f1 f2 -> rnf f1 `seq` rnf f2
    IfaceFunTy f1 f2 f3 f4 -> f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4
    IfaceForAllTy f1 f2 -> f1 `seq` rnf f2
    IfaceTyConApp f1 f2 -> rnf f1 `seq` rnf f2
    IfaceCastTy f1 f2 -> rnf f1 `seq` rnf f2
    IfaceCoercionTy f1 -> rnf f1
    IfaceTupleTy f1 f2 f3 -> f1 `seq` f2 `seq` rnf f3

instance NFData IfaceTyLit where
  rnf = \case
    IfaceNumTyLit f1 -> rnf f1
    IfaceStrTyLit f1 -> rnf f1
    IfaceCharTyLit f1 -> rnf f1

instance (NFData (XXIfaceType p), NFData (XXIfaceCoercion p)) => NFData (IfaceCoercion' p) where
  rnf = \case
    XIfaceCoercion f1 -> rnf f1
    IfaceReflCo f1 -> rnf f1
    IfaceGReflCo f1 f2 f3 -> f1 `seq` rnf f2 `seq` rnf f3
    IfaceFunCo f1 f2 f3 f4 -> f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4
    IfaceTyConAppCo f1 f2 f3 -> f1 `seq` rnf f2 `seq` rnf f3
    IfaceAppCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceForAllCo f1 f2 f3 -> rnf f1 `seq` rnf f2 `seq` rnf f3
    IfaceCoVarCo f1 -> rnf f1
    IfaceAxiomInstCo f1 f2 f3 -> rnf f1 `seq` rnf f2 `seq` rnf f3
    IfaceAxiomRuleCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceUnivCo f1 f2 f3 f4 -> rnf f1 `seq` f2 `seq` rnf f3 `seq` rnf f4
    IfaceSymCo f1 -> rnf f1
    IfaceTransCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceNthCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceLRCo f1 f2 -> f1 `seq` rnf f2
    IfaceInstCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceKindCo f1 -> rnf f1
    IfaceSubCo f1 -> rnf f1

instance NFData (IfaceUnivCoProv' p) where
  rnf x = seq x ()

instance NFData (XXIfaceType p) => NFData (IfaceMCoercion' p) where
  rnf x = seq x ()

instance NFData IfaceOneShot where
  rnf x = seq x ()

instance NFData IfaceTyConSort where
  rnf = \case
    IfaceNormalTyCon -> ()
    IfaceTupleTyCon arity sort -> rnf arity `seq` sort `seq` ()
    IfaceSumTyCon arity -> rnf arity
    IfaceEqualityTyCon -> ()

instance NFData IfaceTyConInfo where
  rnf (IfaceTyConInfo f s) = f `seq` rnf s

instance NFData IfaceTyCon where
  rnf (IfaceTyCon nm info) = rnf nm `seq` rnf info

instance NFData IfaceBndr where
  rnf = \case
    IfaceIdBndr id_bndr -> rnf id_bndr
    IfaceTvBndr tv_bndr -> rnf tv_bndr

instance (NFData (XXIfaceType p), NFData (XXIfaceCoercion p)) => NFData (IfaceAppArgs' p) where
  rnf = \case
    IA_Nil -> ()
    IA_Arg f1 f2 f3 -> rnf f1 `seq` f2 `seq` rnf f3

instance NFData IfaceUnivCoProv where
  rnf x = seq x ()
