{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


This module defines interface types and binders
-}


{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module GHC.Iface.Type (
        IfExtName,
        IfLclName(..), mkIfLclName, ifLclNameFS,

        IfaceType(..), IfacePredType, IfaceKind, IfaceCoercion(..),
        IfaceMCoercion(..),
        IfaceUnivCoProv(..),
        IfaceMult,
        IfaceTyCon(..),
        IfaceTyConInfo(..), mkIfaceTyConInfo,
        IfaceTyConSort(..),
        IfaceTyLit(..), IfaceAppArgs(..),
        IfaceContext, IfaceBndr(..), IfaceOneShot(..), IfaceLamBndr,
        IfaceTvBndr, IfaceIdBndr, IfaceTyConBinder,
        IfaceForAllSpecBndr,
        IfaceForAllBndr, ForAllTyFlag(..), FunTyFlag(..), ShowForAllFlag(..),
        ShowSub(..), ShowHowMuch(..), AltPpr(..),
        mkIfaceForAllTvBndr,
        mkIfaceTyConKind,
        ifaceForAllSpecToBndrs, ifaceForAllSpecToBndr,

        ifForAllBndrVar, ifForAllBndrName, ifaceBndrName,
        ifTyConBinderVar, ifTyConBinderName,

        -- Binary utilities
        putIfaceType, getIfaceType, ifaceTypeSharedByte,
        -- Equality testing
        isIfaceLiftedTypeKind,

        -- Conversion from IfaceAppArgs to IfaceTypes/ForAllTyFlags
        appArgsIfaceTypes, appArgsIfaceTypesForAllTyFlags,

        -- Printing
        SuppressBndrSig(..),
        UseBndrParens(..),
        PrintExplicitKinds(..),
        pprIfaceType, pprParendIfaceType, pprPrecIfaceType,
        pprIfaceContext, pprIfaceContextArr,
        pprIfaceIdBndr, pprIfaceLamBndr, pprIfaceTvBndr, pprIfaceTyConBinders,
        pprIfaceBndrs, pprIfaceAppArgs, pprParendIfaceAppArgs,
        pprIfaceForAllPart, pprIfaceForAllPartMust, pprIfaceForAll,
        pprIfaceSigmaType, pprIfaceTyLit,
        pprIfaceCoercion, pprParendIfaceCoercion,
        splitIfaceSigmaTy, pprIfaceTypeApp, pprUserIfaceForAll,
        pprIfaceCoTcApp, pprTyTcApp, pprIfacePrefixApp,
        isIfaceRhoType,

        suppressIfaceInvisibles,
        stripIfaceInvisVars,
        stripInvisArgs,

        mkIfaceTySubst, substIfaceTyVar, substIfaceAppArgs, inDomIfaceTySubst,

        many_ty, pprTypeArrow
    ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Builtin.Types
                                 ( coercibleTyCon, heqTyCon
                                 , constraintKindTyConName
                                 , tupleTyConName
                                 , tupleDataConName
                                 , manyDataConTyCon
                                 , liftedRepTyCon, liftedDataConTyCon
                                 , sumTyCon )
import GHC.Core.Type ( isRuntimeRepTy, isMultiplicityTy, isLevityTy, funTyFlagTyCon )
import GHC.Core.TyCo.Rep( CoSel )
import GHC.Core.TyCo.Compare( eqForAllVis )
import GHC.Core.TyCon hiding ( pprPromotionQuote )
import GHC.Core.Coercion.Axiom
import GHC.Types.Var
import GHC.Builtin.Names
import {-# SOURCE #-} GHC.Builtin.Types ( liftedTypeKindTyConName )
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Utils.Binary
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Utils.Misc
import GHC.Utils.Panic
import {-# SOURCE #-} GHC.Tc.Utils.TcType ( isMetaTyVar, isTyConableTyVar )

import Data.Maybe (isJust)
import Data.Proxy
import qualified Data.Semigroup as Semi
import Data.Word (Word8)
import Control.Arrow (first)
import Control.DeepSeq
import Control.Monad ((<$!>))

{-
************************************************************************
*                                                                      *
                Local (nested) binders
*                                                                      *
************************************************************************
-}

-- | A local name in iface syntax
newtype IfLclName = IfLclName
  { getIfLclName :: LexicalFastString
  } deriving (Eq, Ord, Show)

ifLclNameFS :: IfLclName -> FastString
ifLclNameFS = getLexicalFastString . getIfLclName

mkIfLclName :: FastString -> IfLclName
mkIfLclName = IfLclName . LexicalFastString

type IfExtName = Name   -- An External or WiredIn Name can appear in Iface syntax
                        -- (However Internal or System Names never should)

data IfaceBndr          -- Local (non-top-level) binders
  = IfaceIdBndr {-# UNPACK #-} !IfaceIdBndr
  | IfaceTvBndr {-# UNPACK #-} !IfaceTvBndr
  deriving (Eq, Ord)


type IfaceIdBndr  = (IfaceType, IfLclName, IfaceType)
type IfaceTvBndr  = (IfLclName, IfaceKind)

ifaceTvBndrName :: IfaceTvBndr -> IfLclName
ifaceTvBndrName (n,_) = n

ifaceIdBndrName :: IfaceIdBndr -> IfLclName
ifaceIdBndrName (_,n,_) = n

ifaceBndrName :: IfaceBndr -> IfLclName
ifaceBndrName (IfaceTvBndr bndr) = ifaceTvBndrName bndr
ifaceBndrName (IfaceIdBndr bndr) = ifaceIdBndrName bndr

ifaceBndrType :: IfaceBndr -> IfaceType
ifaceBndrType (IfaceIdBndr (_, _, t)) = t
ifaceBndrType (IfaceTvBndr (_, t)) = t

type IfaceLamBndr = (IfaceBndr, IfaceOneShot)

data IfaceOneShot    -- See Note [Preserve OneShotInfo] in "GHC.Core.Tidy"
  = IfaceNoOneShot   -- and Note [oneShot magic] in "GHC.Types.Id.Make"
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
data IfaceType
  = IfaceFreeTyVar TyVar                -- See Note [Free TyVars and CoVars in IfaceType]
  | IfaceTyVar     IfLclName            -- Type/coercion variable only, not tycon
  | IfaceLitTy     IfaceTyLit
  | IfaceAppTy     IfaceType IfaceAppArgs
                             -- See Note [Suppressing invisible arguments] for
                             -- an explanation of why the second field isn't
                             -- IfaceType, analogous to AppTy.
  | IfaceFunTy     FunTyFlag IfaceMult IfaceType IfaceType
  | IfaceForAllTy  IfaceForAllBndr IfaceType
  | IfaceTyConApp  IfaceTyCon IfaceAppArgs  -- Not necessarily saturated
                                            -- Includes newtypes, synonyms, tuples
  | IfaceCastTy     IfaceType IfaceCoercion
  | IfaceCoercionTy IfaceCoercion

  | IfaceTupleTy                  -- Saturated tuples (unsaturated ones use IfaceTyConApp)
       TupleSort                  -- What sort of tuple?
       PromotionFlag                 -- A bit like IfaceTyCon
       IfaceAppArgs               -- arity = length args
          -- For promoted data cons, the kind args are omitted
          -- Why have this? Only for efficiency: IfaceTupleTy can omit the
          -- type arguments, as they can be recreated when deserializing.
          -- In an experiment, removing IfaceTupleTy resulted in a 0.75% regression
          -- in interface file size (in GHC's boot libraries).
          -- See !3987.
  deriving (Eq, Ord)
  -- See Note [Ord instance of IfaceType]

{-
Note [Ord instance of IfaceType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need an 'Ord' instance to have a 'Map' keyed by 'IfaceType'. This 'Map' is
required for implementing the deduplication table during interface file
serialisation.
See Note [Deduplication during iface binary serialisation] for the implementation details.

We experimented with a 'TrieMap' based implementation, but it seems to be
slower than using a straight-forward 'Map IfaceType'.
The experiments loaded the full agda library into a ghci session with the
following scenarios:

* normal: a plain ghci session.
* cold: a ghci session that uses '-fwrite-if-simplified-core -fforce-recomp',
  forcing a cold-cache.
* warm: a subsequent ghci session that uses a warm cache for
  '-fwrite-if-simplified-core', e.g. nothing needs to be recompiled.

The implementation was up to 5% slower in some execution runs. However, on
'lib:Cabal', the performance difference between 'Map IfaceType' and
'TrieMap IfaceType' was negligible.

We share our implementation of the 'TrieMap' in the ticket #24816, so that
further performance analysis and improvements don't need to start from scratch.
-}

type IfaceMult = IfaceType

type IfacePredType = IfaceType
type IfaceContext = [IfacePredType]

data IfaceTyLit
  = IfaceNumTyLit Integer
  | IfaceStrTyLit LexicalFastString
  | IfaceCharTyLit Char
  deriving (Eq, Ord)

type IfaceTyConBinder    = VarBndr IfaceBndr TyConBndrVis
type IfaceForAllBndr     = VarBndr IfaceBndr ForAllTyFlag
type IfaceForAllSpecBndr = VarBndr IfaceBndr Specificity

-- | Make an 'IfaceForAllBndr' from an 'IfaceTvBndr'.
mkIfaceForAllTvBndr :: ForAllTyFlag -> IfaceTvBndr -> IfaceForAllBndr
mkIfaceForAllTvBndr vis var = Bndr (IfaceTvBndr var) vis

-- | Build the 'tyConKind' from the binders and the result kind.
-- Keep in sync with 'mkTyConKind' in "GHC.Core.TyCon".
mkIfaceTyConKind :: [IfaceTyConBinder] -> IfaceKind -> IfaceKind
mkIfaceTyConKind bndrs res_kind = foldr mk res_kind bndrs
  where
    mk :: IfaceTyConBinder -> IfaceKind -> IfaceKind
    mk (Bndr tv AnonTCB)        k = IfaceFunTy FTF_T_T many_ty (ifaceBndrType tv) k
    mk (Bndr tv (NamedTCB vis)) k = IfaceForAllTy (Bndr tv vis) k

ifaceForAllSpecToBndrs :: [IfaceForAllSpecBndr] -> [IfaceForAllBndr]
ifaceForAllSpecToBndrs = map ifaceForAllSpecToBndr

ifaceForAllSpecToBndr :: IfaceForAllSpecBndr -> IfaceForAllBndr
ifaceForAllSpecToBndr (Bndr tv spec) = Bndr tv (Invisible spec)

-- | Stores the arguments in a type application as a list.
-- See @Note [Suppressing invisible arguments]@.
data IfaceAppArgs
  = IA_Nil
  | IA_Arg IfaceType    -- The type argument

           ForAllTyFlag      -- The argument's visibility. We store this here so
                        -- that we can:
                        --
                        -- 1. Avoid pretty-printing invisible (i.e., specified
                        --    or inferred) arguments when
                        --    -fprint-explicit-kinds isn't enabled, or
                        -- 2. When -fprint-explicit-kinds *is*, enabled, print
                        --    specified arguments in @(...) and inferred
                        --    arguments in @{...}.

           IfaceAppArgs -- The rest of the arguments
  deriving (Eq, Ord)

instance Semi.Semigroup IfaceAppArgs where
  IA_Nil <> xs              = xs
  IA_Arg ty argf rest <> xs = IA_Arg ty argf (rest Semi.<> xs)

instance Monoid IfaceAppArgs where
  mempty = IA_Nil
  mappend = (Semi.<>)

-- Encodes type constructors, kind constructors,
-- coercion constructors, the lot.
-- We have to tag them in order to pretty print them
-- properly.
data IfaceTyCon = IfaceTyCon { ifaceTyConName :: IfExtName
                             , ifaceTyConInfo :: !IfaceTyConInfo
                             -- ^ We add a bang to this field as heap analysis
                             -- showed that this constructor retains a thunk to
                             -- a value that is usually shared.
                             --
                             -- See !12200 for how this bang saved ~10% residency
                             -- when loading 'mi_extra_decls' on the agda
                             -- code base.
                             --
                             -- See Note [Sharing IfaceTyConInfo] for why
                             -- sharing is so important for 'IfaceTyConInfo'.
                             }
    deriving (Eq, Ord)

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
                    deriving (Eq, Ord)

instance Outputable IfaceTyConSort where
  ppr IfaceNormalTyCon         = text "normal"
  ppr (IfaceTupleTyCon n sort) = ppr sort <> colon <> ppr n
  ppr (IfaceSumTyCon n)        = text "sum:" <> ppr n
  ppr IfaceEqualityTyCon       = text "equality"

{- Note [Free TyVars and CoVars in IfaceType]
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

data IfaceTyConInfo   -- Used only to guide pretty-printing
  = IfaceTyConInfo { ifaceTyConIsPromoted :: PromotionFlag
                      -- A PromotionFlag value of IsPromoted indicates
                      -- that the type constructor came from a data
                      -- constructor promoted by -XDataKinds, and thus
                      -- should be printed as 'D to distinguish it from
                      -- an existing type constructor D.
                   , ifaceTyConSort       :: IfaceTyConSort }
    deriving (Eq, Ord)

-- | This smart constructor allows sharing of the two most common
-- cases. See Note [Sharing IfaceTyConInfo]
mkIfaceTyConInfo :: PromotionFlag -> IfaceTyConSort -> IfaceTyConInfo
mkIfaceTyConInfo IsPromoted  IfaceNormalTyCon = promotedNormalTyConInfo
mkIfaceTyConInfo NotPromoted IfaceNormalTyCon = notPromotedNormalTyConInfo
mkIfaceTyConInfo prom        sort             = IfaceTyConInfo prom sort

{-# NOINLINE promotedNormalTyConInfo #-}
-- | See Note [Sharing IfaceTyConInfo]
promotedNormalTyConInfo :: IfaceTyConInfo
promotedNormalTyConInfo = IfaceTyConInfo IsPromoted IfaceNormalTyCon

{-# NOINLINE notPromotedNormalTyConInfo #-}
-- | See Note [Sharing IfaceTyConInfo]
notPromotedNormalTyConInfo :: IfaceTyConInfo
notPromotedNormalTyConInfo = IfaceTyConInfo NotPromoted IfaceNormalTyCon

{-
Note [Sharing IfaceTyConInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'IfaceTyConInfo' occurs an awful lot in 'ModIface', see #19194 for an example.
But almost all of them are

   IfaceTyConInfo IsPromoted IfaceNormalTyCon
   IfaceTyConInfo NotPromoted IfaceNormalTyCon.

The smart constructor `mkIfaceTyConInfo` arranges to share these instances,
thus:

  promotedNormalTyConInfo    = IfaceTyConInfo IsPromoted  IfaceNormalTyCon
  notPromotedNormalTyConInfo = IfaceTyConInfo NotPromoted IfaceNormalTyCon

  mkIfaceTyConInfo IsPromoted  IfaceNormalTyCon = promotedNormalTyConInfo
  mkIfaceTyConInfo NotPromoted IfaceNormalTyCon = notPromotedNormalTyConInfo
  mkIfaceTyConInfo prom        sort             = IfaceTyConInfo prom sort

But ALAS, the (nested) CPR transform can lose this sharing, completely
negating the effect of `mkIfaceTyConInfo`: see #24530 and #19326.

Sticking-plaster solution: add a NOINLINE pragma to those top-level constants.
When we fix the CPR bug we can remove the NOINLINE pragmas.

This one change leads to an 15% reduction in residency for GHC when embedding
'mi_extra_decls': see !12222.
-}

data IfaceMCoercion
  = IfaceMRefl
  | IfaceMCo IfaceCoercion deriving (Eq, Ord)

data IfaceCoercion
  = IfaceReflCo       IfaceType
  | IfaceGReflCo      Role IfaceType (IfaceMCoercion)
  | IfaceFunCo        Role IfaceCoercion IfaceCoercion IfaceCoercion
  | IfaceTyConAppCo   Role IfaceTyCon [IfaceCoercion]
  | IfaceAppCo        IfaceCoercion IfaceCoercion
  | IfaceForAllCo     IfaceBndr !ForAllTyFlag !ForAllTyFlag IfaceCoercion IfaceCoercion
  | IfaceCoVarCo      IfLclName
  | IfaceAxiomInstCo  IfExtName BranchIndex [IfaceCoercion]
  | IfaceAxiomRuleCo  IfLclName [IfaceCoercion]
       -- ^ There are only a fixed number of CoAxiomRules, so it suffices
       -- to use an IfaceLclName to distinguish them.
       -- See Note [Adding built-in type families] in GHC.Builtin.Types.Literals
  | IfaceUnivCo       IfaceUnivCoProv Role IfaceType IfaceType
  | IfaceSymCo        IfaceCoercion
  | IfaceTransCo      IfaceCoercion IfaceCoercion
  | IfaceSelCo        CoSel IfaceCoercion
  | IfaceLRCo         LeftOrRight IfaceCoercion
  | IfaceInstCo       IfaceCoercion IfaceCoercion
  | IfaceKindCo       IfaceCoercion
  | IfaceSubCo        IfaceCoercion
  | IfaceFreeCoVar    CoVar    -- ^ See Note [Free TyVars and CoVars in IfaceType]
  | IfaceHoleCo       CoVar    -- ^ See Note [Holes in IfaceCoercion]
  deriving (Eq, Ord)

data IfaceUnivCoProv
  = IfacePhantomProv IfaceCoercion
  | IfaceProofIrrelProv IfaceCoercion
  | IfacePluginProv String [IfLclName] [Var]
    -- ^ Local covars and open (free) covars resp
    -- See Note [Free TyVars and CoVars in IfaceType]
  deriving (Eq, Ord)


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
                Functions over IfaceTypes
*                                                                      *
************************************************************************
-}

ifaceTyConHasKey :: IfaceTyCon -> Unique -> Bool
ifaceTyConHasKey tc key = ifaceTyConName tc `hasKey` key

-- | Returns true for Type or (TYPE LiftedRep)
isIfaceLiftedTypeKind :: IfaceKind -> Bool
isIfaceLiftedTypeKind (IfaceTyConApp tc args)
  | tc `ifaceTyConHasKey` liftedTypeKindTyConKey
  , IA_Nil <- args
  = True  -- Type

  | tc `ifaceTyConHasKey` tYPETyConKey
  , IA_Arg arg1 Required IA_Nil <- args
  , isIfaceLiftedRep arg1
  = True  -- TYPE Lifted

isIfaceLiftedTypeKind _ = False

-- | Returns true for Constraint or (CONSTRAINT LiftedRep)
isIfaceConstraintKind :: IfaceKind -> Bool
isIfaceConstraintKind (IfaceTyConApp tc args)
  | tc `ifaceTyConHasKey` constraintKindTyConKey
  , IA_Nil <- args
  = True  -- Type

  | tc `ifaceTyConHasKey` cONSTRAINTTyConKey
  , IA_Arg arg1 Required IA_Nil <- args
  , isIfaceLiftedRep arg1
  = True  -- TYPE Lifted

isIfaceConstraintKind _ = False

isIfaceLiftedRep :: IfaceKind -> Bool
-- Returns true for LiftedRep, or BoxedRep Lifted
isIfaceLiftedRep (IfaceTyConApp tc args)
  | tc `ifaceTyConHasKey` liftedRepTyConKey
  , IA_Nil <- args
  = True  -- LiftedRep

  | tc `ifaceTyConHasKey` boxedRepDataConKey
  , IA_Arg arg1 Required IA_Nil <- args
  , isIfaceLifted arg1
  = True  -- TYPE Lifted

isIfaceLiftedRep _ = False

isIfaceLifted :: IfaceKind -> Bool
-- Returns true for Lifted
isIfaceLifted (IfaceTyConApp tc args)
  | tc `ifaceTyConHasKey` liftedDataConKey
  , IA_Nil <- args
  = True
isIfaceLifted _ = False

splitIfaceSigmaTy :: IfaceType -> ([IfaceForAllBndr], [IfacePredType], IfaceType)
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
        | isInvisibleForAllTyFlag (binderFlag bndr)
        = case split_foralls ty of { (bndrs, rho) -> (bndr:bndrs, rho) }
    split_foralls rho = ([], rho)

    split_rho (IfaceFunTy af _ ty1 ty2)
        | isInvisibleFunArg af
        = case split_rho ty2 of { (ps, tau) -> (ty1:ps, tau) }
    split_rho tau = ([], tau)

splitIfaceReqForallTy :: IfaceType -> ([IfaceForAllBndr], IfaceType)
splitIfaceReqForallTy (IfaceForAllTy bndr ty)
  | isVisibleForAllTyFlag (binderFlag bndr)
  = case splitIfaceReqForallTy ty of { (bndrs, rho) -> (bndr:bndrs, rho) }
splitIfaceReqForallTy rho = ([], rho)

suppressIfaceInvisibles :: PrintExplicitKinds -> [IfaceTyConBinder] -> [a] -> [a]
suppressIfaceInvisibles (PrintExplicitKinds True) _tys xs = xs
suppressIfaceInvisibles (PrintExplicitKinds False) tys xs = suppress tys xs
    where
      suppress _       []      = []
      suppress []      a       = a
      suppress (k:ks) (x:xs)
        | isInvisibleTyConBinder k =     suppress ks xs
        | otherwise                = x : suppress ks xs

stripIfaceInvisVars :: PrintExplicitKinds -> [IfaceTyConBinder] -> [IfaceTyConBinder]
stripIfaceInvisVars (PrintExplicitKinds True)  tyvars = tyvars
stripIfaceInvisVars (PrintExplicitKinds False) tyvars
  = filterOut isInvisibleTyConBinder tyvars

-- | Extract an 'IfaceBndr' from an 'IfaceForAllBndr'.
ifForAllBndrVar :: IfaceForAllBndr -> IfaceBndr
ifForAllBndrVar = binderVar

-- | Extract the variable name from an 'IfaceForAllBndr'.
ifForAllBndrName :: IfaceForAllBndr -> IfLclName
ifForAllBndrName fab = ifaceBndrName (ifForAllBndrVar fab)

-- | Extract an 'IfaceBndr' from an 'IfaceTyConBinder'.
ifTyConBinderVar :: IfaceTyConBinder -> IfaceBndr
ifTyConBinderVar = binderVar

-- | Extract the variable name from an 'IfaceTyConBinder'.
ifTyConBinderName :: IfaceTyConBinder -> IfLclName
ifTyConBinderName tcb = ifaceBndrName (ifTyConBinderVar tcb)

ifTypeIsVarFree :: IfaceType -> Bool
-- Returns True if the type definitely has no variables at all
-- Just used to control pretty printing
ifTypeIsVarFree ty = go ty
  where
    go (IfaceTyVar {})         = False
    go (IfaceFreeTyVar {})     = False
    go (IfaceAppTy fun args)   = go fun && go_args args
    go (IfaceFunTy _ w arg res) = go w && go arg && go res
    go (IfaceForAllTy {})      = False
    go (IfaceTyConApp _ args)  = go_args args
    go (IfaceTupleTy _ _ args) = go_args args
    go (IfaceLitTy _)          = True
    go (IfaceCastTy {})        = False -- Safe
    go (IfaceCoercionTy {})    = False -- Safe

    go_args IA_Nil = True
    go_args (IA_Arg arg _ args) = go arg && go_args args

{- Note [Substitution on IfaceType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Substitutions on IfaceType are done only during pretty-printing to
construct the result type of a GADT, and does not deal with binders
(eg IfaceForAll), so it doesn't need fancy capture stuff.  -}

type IfaceTySubst = FastStringEnv IfaceType -- Note [Substitution on IfaceType]

mkIfaceTySubst :: [(IfLclName,IfaceType)] -> IfaceTySubst
-- See Note [Substitution on IfaceType]
mkIfaceTySubst eq_spec = mkFsEnv (map (first ifLclNameFS) eq_spec)

inDomIfaceTySubst :: IfaceTySubst -> IfaceTvBndr -> Bool
-- See Note [Substitution on IfaceType]
inDomIfaceTySubst subst (fs, _) = isJust (lookupFsEnv subst (ifLclNameFS fs))

substIfaceType :: IfaceTySubst -> IfaceType -> IfaceType
-- See Note [Substitution on IfaceType]
substIfaceType env ty
  = go ty
  where
    go (IfaceFreeTyVar tv)    = IfaceFreeTyVar tv
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

    go_co (IfaceReflCo ty)           = IfaceReflCo (go ty)
    go_co (IfaceGReflCo r ty mco)    = IfaceGReflCo r (go ty) (go_mco mco)
    go_co (IfaceFunCo r w c1 c2)     = IfaceFunCo r (go_co w) (go_co c1) (go_co c2)
    go_co (IfaceTyConAppCo r tc cos) = IfaceTyConAppCo r tc (go_cos cos)
    go_co (IfaceAppCo c1 c2)         = IfaceAppCo (go_co c1) (go_co c2)
    go_co (IfaceForAllCo {})         = pprPanic "substIfaceCoercion" (ppr ty)
    go_co (IfaceFreeCoVar cv)        = IfaceFreeCoVar cv
    go_co (IfaceCoVarCo cv)          = IfaceCoVarCo cv
    go_co (IfaceHoleCo cv)           = IfaceHoleCo cv
    go_co (IfaceAxiomInstCo a i cos) = IfaceAxiomInstCo a i (go_cos cos)
    go_co (IfaceUnivCo prov r t1 t2) = IfaceUnivCo (go_prov prov) r (go t1) (go t2)
    go_co (IfaceSymCo co)            = IfaceSymCo (go_co co)
    go_co (IfaceTransCo co1 co2)     = IfaceTransCo (go_co co1) (go_co co2)
    go_co (IfaceSelCo n co)          = IfaceSelCo n (go_co co)
    go_co (IfaceLRCo lr co)          = IfaceLRCo lr (go_co co)
    go_co (IfaceInstCo c1 c2)        = IfaceInstCo (go_co c1) (go_co c2)
    go_co (IfaceKindCo co)           = IfaceKindCo (go_co co)
    go_co (IfaceSubCo co)            = IfaceSubCo (go_co co)
    go_co (IfaceAxiomRuleCo n cos)   = IfaceAxiomRuleCo n (go_cos cos)

    go_cos = map go_co

    go_prov (IfacePhantomProv co)    = IfacePhantomProv (go_co co)
    go_prov (IfaceProofIrrelProv co) = IfaceProofIrrelProv (go_co co)
    go_prov co@(IfacePluginProv _ _ _) = co

substIfaceAppArgs :: IfaceTySubst -> IfaceAppArgs -> IfaceAppArgs
substIfaceAppArgs env args
  = go args
  where
    go IA_Nil              = IA_Nil
    go (IA_Arg ty arg tys) = IA_Arg (substIfaceType env ty) arg (go tys)

substIfaceTyVar :: IfaceTySubst -> IfLclName -> IfaceType
substIfaceTyVar env tv
  | Just ty <- lookupFsEnv env (ifLclNameFS tv) = ty
  | otherwise                     = IfaceTyVar tv


{-
************************************************************************
*                                                                      *
                Functions over IfaceAppArgs
*                                                                      *
************************************************************************
-}

stripInvisArgs :: PrintExplicitKinds -> IfaceAppArgs -> IfaceAppArgs
stripInvisArgs (PrintExplicitKinds True)  tys = tys
stripInvisArgs (PrintExplicitKinds False) tys = suppress_invis tys
    where
      suppress_invis c
        = case c of
            IA_Nil -> IA_Nil
            IA_Arg t argf ts
              |  isVisibleForAllTyFlag argf
              -> IA_Arg t argf $ suppress_invis ts
              -- Keep recursing through the remainder of the arguments, as it's
              -- possible that there are remaining invisible ones.
              -- See the "In type declarations" section of Note [VarBndrs,
              -- ForAllTyBinders, TyConBinders, and visibility] in GHC.Core.TyCo.Rep.
              |  otherwise
              -> suppress_invis ts

appArgsIfaceTypes :: IfaceAppArgs -> [IfaceType]
appArgsIfaceTypes IA_Nil = []
appArgsIfaceTypes (IA_Arg t _ ts) = t : appArgsIfaceTypes ts

appArgsIfaceTypesForAllTyFlags :: IfaceAppArgs -> [(IfaceType, ForAllTyFlag)]
appArgsIfaceTypesForAllTyFlags IA_Nil = []
appArgsIfaceTypesForAllTyFlags (IA_Arg t a ts)
                                 = (t, a) : appArgsIfaceTypesForAllTyFlags ts

ifaceVisAppArgsLength :: IfaceAppArgs -> Int
ifaceVisAppArgsLength = go 0
  where
    go !n IA_Nil = n
    go n  (IA_Arg _ argf rest)
      | isVisibleForAllTyFlag argf = go (n+1) rest
      | otherwise             = go n rest

ifaceAppArgsLength :: IfaceAppArgs -> Int
ifaceAppArgsLength = go 0
  where
    go !n IA_Nil = n
    go !n (IA_Arg _ _ ts) = go (n + 1) ts

{-
Note [Suppressing invisible arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use the IfaceAppArgs data type to specify which of the arguments to a type
should be displayed when pretty-printing, under the control of
-fprint-explicit-kinds.
See also Type.filterOutInvisibleTypes.
For example, given

    T :: forall k. (k->*) -> k -> *    -- Ordinary kind polymorphism
    'Just :: forall k. k -> 'Maybe k   -- Promoted

we want

    T * Tree Int    prints as    T Tree Int
    'Just *         prints as    Just *

For type constructors (IfaceTyConApp), IfaceAppArgs is a quite natural fit,
since the corresponding Core constructor:

    data Type
      = ...
      | TyConApp TyCon [Type]

Already puts all of its arguments into a list. So when converting a Type to an
IfaceType (see toIfaceAppArgsX in GHC.Core.ToIface), we simply use the kind of
the TyCon (which is cached) to guide the process of converting the argument
Types into an IfaceAppArgs list.

We also want this behavior for IfaceAppTy, since given:

    data Proxy (a :: k)
    f :: forall (t :: forall a. a -> Type). Proxy Type (t Bool True)

We want to print the return type as `Proxy (t True)` without the use of
-fprint-explicit-kinds (#15330). Accomplishing this is trickier than in the
tycon case, because the corresponding Core constructor for IfaceAppTy:

    data Type
      = ...
      | AppTy Type Type

Only stores one argument at a time. Therefore, when converting an AppTy to an
IfaceAppTy (in toIfaceTypeX in GHC.CoreToIface), we:

1. Flatten the chain of AppTys down as much as possible
2. Use typeKind to determine the function Type's kind
3. Use this kind to guide the process of converting the argument Types into an
   IfaceAppArgs list.

By flattening the arguments like this, we obtain two benefits:

(a) We can reuse the same machinery to pretty-print IfaceTyConApp arguments as
    we do IfaceTyApp arguments, which means that we only need to implement the
    logic to filter out invisible arguments once.
(b) Unlike for tycons, finding the kind of a type in general (through typeKind)
    is not a constant-time operation, so by flattening the arguments first, we
    decrease the number of times we have to call typeKind.

Note [Pretty-printing invisible arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note [Suppressing invisible arguments] is all about how to avoid printing
invisible arguments when the -fprint-explicit-kinds flag is disables. Well,
what about when it's enabled? Then we can and should print invisible kind
arguments, and this Note explains how we do it.

As two running examples, consider the following code:

  {-# LANGUAGE PolyKinds #-}
  data T1 a
  data T2 (a :: k)

When displaying these types (with -fprint-explicit-kinds on), we could just
do the following:

  T1 k a
  T2 k a

That certainly gets the job done. But it lacks a crucial piece of information:
is the `k` argument inferred or specified? To communicate this, we use visible
kind application syntax to distinguish the two cases:

  T1 @{k} a
  T2 @k   a

Here, @{k} indicates that `k` is an inferred argument, and @k indicates that
`k` is a specified argument. (See
Note [VarBndrs, ForAllTyBinders, TyConBinders, and visibility] in GHC.Core.TyCo.Rep for
a lengthier explanation on what "inferred" and "specified" mean.)

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
  = sdocOption sdocPrintExplicitCoercions $ \print_co ->
    getPprStyle $ \style ->
    getPprDebug $ \debug ->
    if print_co || dumpStyle style || debug
    then yes
    else no

pprIfaceInfixApp :: PprPrec -> SDoc -> SDoc -> SDoc -> SDoc
pprIfaceInfixApp ctxt_prec pp_tc pp_ty1 pp_ty2
  = maybeParen ctxt_prec opPrec $
    sep [pp_ty1, pp_tc <+> pp_ty2]

pprIfacePrefixApp :: PprPrec -> SDoc -> [SDoc] -> SDoc
pprIfacePrefixApp ctxt_prec pp_fun pp_tys
  | null pp_tys = pp_fun
  | otherwise   = maybeParen ctxt_prec appPrec $
                  hang pp_fun 2 (sep pp_tys)

isIfaceRhoType :: IfaceType -> Bool
isIfaceRhoType (IfaceForAllTy _ _)   = False
isIfaceRhoType (IfaceFunTy af _ _ _) = isVisibleFunArg af
isIfaceRhoType _ = True

-- ----------------------------- Printing binders ------------------------------------

instance Outputable IfaceBndr where
    ppr (IfaceIdBndr bndr) = pprIfaceIdBndr bndr
    ppr (IfaceTvBndr bndr) = char '@' <> pprIfaceTvBndr bndr (SuppressBndrSig False)
                                                             (UseBndrParens False)

pprIfaceBndrs :: [IfaceBndr] -> SDoc
pprIfaceBndrs bs = sep (map ppr bs)

pprIfaceLamBndr :: IfaceLamBndr -> SDoc
pprIfaceLamBndr (b, IfaceNoOneShot) = ppr b
pprIfaceLamBndr (b, IfaceOneShot)   = ppr b <> text "[OneShot]"

pprIfaceIdBndr :: IfaceIdBndr -> SDoc
pprIfaceIdBndr (w, name, ty) = parens (ppr name <> brackets (ppr_ty_nested w) <+> dcolon <+> ppr_ty_nested ty)

{- Note [Suppressing binder signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When printing the binders in a 'forall', we want to keep the kind annotations:

    forall (a :: k). blah
              ^^^^
              good

On the other hand, when we print the binders of a data declaration in :info,
the kind information would be redundant due to the standalone kind signature:

   type F :: Symbol -> Type
   type F (s :: Symbol) = blah
             ^^^^^^^^^
             redundant

Here we'd like to omit the kind annotation:

   type F :: Symbol -> Type
   type F s = blah

Note [Printing type abbreviations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normally, we pretty-print
   `TYPE       'LiftedRep` as `Type` (or `*`)
   `CONSTRAINT 'LiftedRep` as `Constraint`
   `FUN 'Many`             as `(->)`
This way, error messages don't refer to representation polymorphism
or linearity if it is not necessary.  Normally we'd would represent
these types using their synonyms (see GHC.Core.Type
Note [Using synonyms to compress types]), but in the :kind! GHCi
command we specifically expand synonyms (see GHC.Tc.Module.tcRnExpr).
So here in the pretty-printing we effectively collapse back Type
and Constraint to their synonym forms.  A bit confusing!

However, when printing the definition of Type, Constraint or (->) with :info,
this would give confusing output: `type (->) = (->)` (#18594).
Solution: detect when we are in :info and disable displaying the synonym
with the SDoc option sdocPrintTypeAbbreviations.
If you are creating a similar synonym, make sure it is listed in pprIfaceDecl,
see reference to this Note.

If there will be a need, in the future we could expose it as a flag
-fprint-type-abbreviations or even three separate flags controlling
TYPE 'LiftedRep, CONSTRAINT 'LiftedRep and FUN 'Many.
-}

-- | Do we want to suppress kind annotations on binders?
-- See Note [Suppressing binder signatures]
newtype SuppressBndrSig = SuppressBndrSig Bool

newtype UseBndrParens      = UseBndrParens Bool
newtype PrintExplicitKinds = PrintExplicitKinds Bool

pprIfaceTvBndr :: IfaceTvBndr -> SuppressBndrSig -> UseBndrParens -> SDoc
pprIfaceTvBndr (tv, ki) (SuppressBndrSig suppress_sig) (UseBndrParens use_parens)
  | suppress_sig             = ppr tv
  | isIfaceLiftedTypeKind ki = ppr tv
  | otherwise                = maybe_parens (ppr tv <+> dcolon <+> ppr_ty_nested ki)
  where
    maybe_parens | use_parens = parens
                 | otherwise  = id

pprIfaceTyConBinders :: SuppressBndrSig -> [IfaceTyConBinder] -> SDoc
pprIfaceTyConBinders suppress_sig = sep . map go
  where
    go :: IfaceTyConBinder -> SDoc
    go (Bndr (IfaceIdBndr bndr) _) = pprIfaceIdBndr bndr
    go (Bndr (IfaceTvBndr bndr) vis) =
      -- See Note [Pretty-printing invisible arguments]
      case vis of
        AnonTCB            -> ppr_bndr (UseBndrParens True)
        NamedTCB Required  -> ppr_bndr (UseBndrParens True)
        NamedTCB Specified -> char '@' <> ppr_bndr (UseBndrParens True)
        NamedTCB Inferred  -> char '@' <> braces (ppr_bndr (UseBndrParens False))
      where
        ppr_bndr = pprIfaceTvBndr bndr suppress_sig

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

-- ----------------------------- Printing IfaceType ------------------------------------

---------------------------------
instance Outputable IfaceType where
  ppr ty = pprIfaceType ty

-- The purpose of 'ppr_ty_nested' is to distinguish calls that should not
-- trigger 'hideNonStandardTypes', see Note [Defaulting RuntimeRep variables]
-- wrinkle (W2).
pprIfaceType, pprParendIfaceType, ppr_ty_nested :: IfaceType -> SDoc
pprIfaceType       = pprPrecIfaceType topPrec
pprParendIfaceType = pprPrecIfaceType appPrec
ppr_ty_nested = ppr_ty topPrec

pprPrecIfaceType :: PprPrec -> IfaceType -> SDoc
-- We still need `hideNonStandardTypes`, since the `pprPrecIfaceType` may be
-- called from other places, besides `:type` and `:info`.
pprPrecIfaceType prec ty =
  hideNonStandardTypes (ppr_ty prec) ty

pprTypeArrow :: FunTyFlag -> IfaceMult -> SDoc
pprTypeArrow af mult
  = pprArrow (mb_conc, pprPrecIfaceType) af mult
  where
    mb_conc (IfaceTyConApp tc _) = Just tc
    mb_conc _                    = Nothing

pprArrow :: (a -> Maybe IfaceTyCon, PprPrec -> a -> SDoc)
         -> FunTyFlag -> a -> SDoc
-- Prints a thin arrow (->) with its multiplicity
-- Used for both FunTy and FunCo, hence higher order arguments
pprArrow (mb_conc, ppr_mult) af mult
  | isFUNArg af
  = case mb_conc mult of
      Just tc | tc `ifaceTyConHasKey` manyDataConKey -> arrow
              | tc `ifaceTyConHasKey` oneDataConKey  -> lollipop
      _ -> text "%" <> ppr_mult appPrec mult <+> arrow
  | otherwise
  = ppr (funTyFlagTyCon af)

ppr_ty :: PprPrec -> IfaceType -> SDoc
ppr_ty ctxt_prec ty
  | not (isIfaceRhoType ty)             = ppr_sigma ShowForAllMust ctxt_prec ty
ppr_ty _         (IfaceForAllTy {})     = panic "ppr_ty"  -- Covered by not.isIfaceRhoType
ppr_ty _         (IfaceFreeTyVar tyvar) = ppr tyvar  -- This is the main reason for IfaceFreeTyVar!
ppr_ty _         (IfaceTyVar tyvar)     = ppr tyvar  -- See Note [Free TyVars and CoVars in IfaceType]
ppr_ty ctxt_prec (IfaceTyConApp tc tys) = pprTyTcApp ctxt_prec tc tys
ppr_ty ctxt_prec (IfaceTupleTy i p tys) = ppr_tuple ctxt_prec i p tys -- always fully saturated
ppr_ty _         (IfaceLitTy n)         = pprIfaceTyLit n

        -- Function types
ppr_ty ctxt_prec ty@(IfaceFunTy af w ty1 ty2)  -- Should be a visible argument
  = assertPpr (isVisibleFunArg af) (ppr ty) $  -- Ensured by isIfaceRhoType above
    -- We want to print a chain of arrows in a column
    --     type1
    --     -> type2
    --     -> type3
    maybeParen ctxt_prec funPrec $
    sep [ppr_ty funPrec ty1, sep (ppr_fun_tail w ty2)]
  where
    ppr_fun_tail wthis (IfaceFunTy af wnext ty1 ty2)
      | isVisibleFunArg af
      = (pprTypeArrow af wthis <+> ppr_ty funPrec ty1) : ppr_fun_tail wnext ty2
    ppr_fun_tail wthis other_ty
      = [pprTypeArrow af wthis <+> ppr_ty_nested other_ty]

ppr_ty ctxt_prec (IfaceAppTy t ts)
  = if_print_coercions
      ppr_app_ty
      ppr_app_ty_no_casts
  where
    ppr_app_ty =
        sdocOption sdocPrintExplicitKinds $ \print_kinds ->
        let tys_wo_kinds = appArgsIfaceTypesForAllTyFlags $ stripInvisArgs
                              (PrintExplicitKinds print_kinds) ts
        in pprIfacePrefixApp ctxt_prec
                             (ppr_ty funPrec t)
                             (map (ppr_app_arg appPrec) tys_wo_kinds)


    -- Strip any casts from the head of the application
    ppr_app_ty_no_casts =
        case t of
          IfaceCastTy head _ -> ppr_ty ctxt_prec (mk_app_tys head ts)
          _                  -> ppr_app_ty

    mk_app_tys :: IfaceType -> IfaceAppArgs -> IfaceType
    mk_app_tys (IfaceTyConApp tc tys1) tys2 =
        IfaceTyConApp tc (tys1 `mappend` tys2)
    mk_app_tys t1 tys2 = IfaceAppTy t1 tys2

ppr_ty ctxt_prec (IfaceCastTy ty co)
  = if_print_coercions
      (parens (ppr_ty topPrec ty <+> text "|>" <+> ppr co))
      (ppr_ty ctxt_prec ty)

ppr_ty ctxt_prec (IfaceCoercionTy co)
  = if_print_coercions
      (ppr_co ctxt_prec co)
      (text "<>")

{- Note [Defaulting RuntimeRep variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RuntimeRep variables are considered by many (most?) users to be little
more than syntactic noise. When the notion was introduced there was a
significant and understandable push-back from those with pedagogy in
mind, which argued that RuntimeRep variables would throw a wrench into
nearly any teach approach since they appear in even the lowly ($)
function's type,

    ($) :: forall (w :: RuntimeRep) a (b :: TYPE w). (a -> b) -> a -> b

which is significantly less readable than its non RuntimeRep-polymorphic type of

    ($) :: (a -> b) -> a -> b

Moreover, unboxed types don't appear all that often in run-of-the-mill
Haskell programs, so it makes little sense to make all users pay this
syntactic overhead.

For this reason it was decided that we would hide RuntimeRep variables
for now (see #11549). We do this right in the pretty-printer, by pre-processing
the type we are about to print, to default any type variables of kind RuntimeRep
that are bound by toplevel invisible quantification to LiftedRep.
Likewise, we default Multiplicity variables to Many and Levity variables to
Lifted.

This is done in a pass right before pretty-printing
(defaultIfaceTyVarsOfKind, controlled by
-fprint-explicit-runtime-reps and -XLinearTypes)

This applies to /quantified/ variables like 'w' above.  What about
variables that are /free/ in the type being printed, which certainly
happens in error messages.  Suppose (#16074, #19361) we are reporting a
mismatch between skolems
          (a :: RuntimeRep) ~ (b :: RuntimeRep)
        or
          (m :: Multiplicity) ~ Many
We certainly don't want to say "Can't match LiftedRep with LiftedRep" or
"Can't match Many with Many"!

But if we are printing the type
    (forall (a :: TYPE r). blah)
we do want to turn that (free) r into LiftedRep, so it prints as
    (forall a. blah)

We use isMetaTyVar to distinguish between those two situations:
metavariables are converted, skolem variables are not.

There's one exception though: TyVarTv metavariables should not be defaulted,
as they appear during kind-checking of "newtype T :: TYPE r where..."
(test T18357a). Therefore, we additionally test for isTyConableTyVar.

Wrinkles:

(W1) The loop 'go' in 'defaultIfaceTyVarsOfKind' passes a Bool flag, 'rank1',
     around that indicates whether we haven't yet descended into the arguments
     of a function type.
     This is used to decide whether newly bound variables are eligible for
     defaulting – we do not want contravariant foralls to be defaulted because
     that would result in an incorrect, rather than specialized, type.
     For example:
       ∀ p (r1 :: RuntimeRep) . (∀ (r2 :: RuntimeRep) . p r2) -> p r1
     We want to default 'r1', but not 'r2'.
     When examining the first forall, 'rank1' is True.
     The toplevel function type is matched as IfaceFunTy, where we recurse into
     'go' by passing False for 'rank1'.
     The forall in the first argument then skips adding a substitution for 'r2'.

(W2) 'defaultIfaceTyVarsOfKind' ought to be called only once when printing a
     type.
     A few components of the printing machinery used to invoke 'ppr' on types
     nested in secondary structures like IfaceBndr, which would repeat the
     defaulting process, but treating the type as if it were top-level, causing
     unwanted defaulting.
     In order to prevent future developers from using 'ppr' again or being
     confused that @ppr_ty topPrec@ is used, we introduced a marker function,
     'ppr_ty_nested'.
-}

-- | Default 'RuntimeRep' variables to 'LiftedRep',
--   'Levity' variables to 'Lifted', and 'Multiplicity'
--   variables to 'Many'. For example:
--
-- @
-- ($) :: forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r).
--        (a -> b) -> a -> b
-- Just :: forall (k :: Multiplicity) a. a % k -> Maybe a
-- @
--
-- turns in to,
--
-- @ ($) :: forall a (b :: *). (a -> b) -> a -> b @
-- @ Just :: forall a . a -> Maybe a @
--
-- We do this to prevent RuntimeRep, Levity and Multiplicity variables from
-- incurring a significant syntactic overhead in otherwise simple
-- type signatures (e.g. ($)). See Note [Defaulting RuntimeRep variables]
-- and #11549 for further discussion.
defaultIfaceTyVarsOfKind :: Bool -- ^ default 'RuntimeRep'/'Levity' variables?
                         -> Bool -- ^ default 'Multiplicity' variables?
                         -> IfaceType -> IfaceType
defaultIfaceTyVarsOfKind def_rep def_mult ty = go emptyFsEnv True ty
  where
    go :: FastStringEnv IfaceType -- Set of enclosing forall-ed RuntimeRep/Levity/Multiplicity variables
       -> Bool -- Are we in a toplevel forall, where defaulting is allowed?
       -> IfaceType
       -> IfaceType
    go subs True (IfaceForAllTy (Bndr (IfaceTvBndr (var, var_kind)) argf) ty)
     | isInvisibleForAllTyFlag argf  -- Don't default *visible* quantification
                                     -- or we get the mess in #13963
     , Just substituted_ty <- check_substitution var_kind
      = let subs' = extendFsEnv subs (ifLclNameFS var) substituted_ty
            -- Record that we should replace it with LiftedRep/Lifted/Many,
            -- and recurse, discarding the forall
        in go subs' True ty

    go subs rank1 (IfaceForAllTy bndr ty)
      = IfaceForAllTy (go_ifacebndr subs bndr) (go subs rank1 ty)

    go subs _ ty@(IfaceTyVar tv) = case lookupFsEnv subs (ifLclNameFS tv) of
      Just s -> s
      Nothing -> ty

    go _ _ ty@(IfaceFreeTyVar tv)
      -- See Note [Defaulting RuntimeRep variables], about free vars
      | def_rep
      , GHC.Core.Type.isRuntimeRepTy (tyVarKind tv)
      , isMetaTyVar tv
      , isTyConableTyVar tv
      = liftedRep_ty
      | def_rep
      , GHC.Core.Type.isLevityTy (tyVarKind tv)
      , isMetaTyVar tv
      , isTyConableTyVar tv
      = lifted_ty
      | def_mult
      , GHC.Core.Type.isMultiplicityTy (tyVarKind tv)
      , isMetaTyVar tv
      , isTyConableTyVar tv
      = many_ty
      | otherwise
      = ty

    go subs _ (IfaceTyConApp tc tc_args)
      = IfaceTyConApp tc (go_args subs tc_args)

    go subs _ (IfaceTupleTy sort is_prom tc_args)
      = IfaceTupleTy sort is_prom (go_args subs tc_args)

    go subs rank1 (IfaceFunTy af w arg res)
      = IfaceFunTy af (go subs False w) (go subs False arg) (go subs rank1 res)

    go subs _ (IfaceAppTy t ts)
      = IfaceAppTy (go subs False t) (go_args subs ts)

    go subs rank1 (IfaceCastTy x co)
      = IfaceCastTy (go subs rank1 x) co

    go _ _ ty@(IfaceLitTy {}) = ty
    go _ _ ty@(IfaceCoercionTy {}) = ty

    go_ifacebndr :: FastStringEnv IfaceType -> IfaceForAllBndr -> IfaceForAllBndr
    go_ifacebndr subs (Bndr (IfaceIdBndr (w, n, t)) argf)
      = Bndr (IfaceIdBndr (w, n, go subs False t)) argf
    go_ifacebndr subs (Bndr (IfaceTvBndr (n, t)) argf)
      = Bndr (IfaceTvBndr (n, go subs False t)) argf

    go_args :: FastStringEnv IfaceType -> IfaceAppArgs -> IfaceAppArgs
    go_args _ IA_Nil = IA_Nil
    go_args subs (IA_Arg ty argf args)
      = IA_Arg (go subs False ty) argf (go_args subs args)

    check_substitution :: IfaceType -> Maybe IfaceType
    check_substitution (IfaceTyConApp tc _)
        | def_rep
        , tc `ifaceTyConHasKey` runtimeRepTyConKey
        = Just liftedRep_ty
        | def_rep
        , tc `ifaceTyConHasKey` levityTyConKey
        = Just lifted_ty
        | def_mult
        , tc `ifaceTyConHasKey` multiplicityTyConKey
        = Just many_ty
    check_substitution _ = Nothing

-- | The type ('BoxedRep 'Lifted), also known as LiftedRep.
liftedRep_ty :: IfaceType
liftedRep_ty =
  IfaceTyConApp liftedRep IA_Nil
  where
    liftedRep :: IfaceTyCon
    liftedRep = IfaceTyCon tc_name (mkIfaceTyConInfo NotPromoted IfaceNormalTyCon)
      where tc_name = getName liftedRepTyCon

-- | The type 'Lifted :: Levity'.
lifted_ty :: IfaceType
lifted_ty =
    IfaceTyConApp (IfaceTyCon dc_name (mkIfaceTyConInfo IsPromoted IfaceNormalTyCon))
                  IA_Nil
  where dc_name = getName liftedDataConTyCon

-- | The type 'Many :: Multiplicity'.
many_ty :: IfaceType
many_ty = IfaceTyConApp (IfaceTyCon dc_name (mkIfaceTyConInfo IsPromoted IfaceNormalTyCon))
                        IA_Nil
  where dc_name = getName manyDataConTyCon

hideNonStandardTypes :: (IfaceType -> SDoc) -> IfaceType -> SDoc
hideNonStandardTypes f ty
  = sdocOption sdocPrintExplicitRuntimeReps $ \printExplicitRuntimeReps ->
    sdocOption sdocLinearTypes $ \linearTypes ->
    getPprStyle      $ \sty    ->
    let def_rep  = not printExplicitRuntimeReps
        def_mult = not linearTypes
    in if userStyle sty
       then f (defaultIfaceTyVarsOfKind def_rep def_mult ty)
       else f ty

instance Outputable IfaceAppArgs where
  ppr tca = pprIfaceAppArgs tca

pprIfaceAppArgs, pprParendIfaceAppArgs :: IfaceAppArgs -> SDoc
pprIfaceAppArgs  = ppr_app_args topPrec
pprParendIfaceAppArgs = ppr_app_args appPrec

ppr_app_args :: PprPrec -> IfaceAppArgs -> SDoc
ppr_app_args ctx_prec = go
  where
    go :: IfaceAppArgs -> SDoc
    go IA_Nil             = empty
    go (IA_Arg t argf ts) = ppr_app_arg ctx_prec (t, argf) <+> go ts

-- See Note [Pretty-printing invisible arguments]
ppr_app_arg :: PprPrec -> (IfaceType, ForAllTyFlag) -> SDoc
ppr_app_arg ctx_prec (t, argf) =
  sdocOption sdocPrintExplicitKinds $ \print_kinds ->
  case argf of
       Required  -> ppr_ty ctx_prec t
       Specified |  print_kinds
                 -> char '@' <> ppr_ty appPrec t
       Inferred  |  print_kinds
                 -> char '@' <> braces (ppr_ty_nested t)
       _         -> empty

-------------------
pprIfaceForAllPart :: [IfaceForAllBndr] -> [IfacePredType] -> SDoc -> SDoc
pprIfaceForAllPart tvs ctxt sdoc
  = ppr_iface_forall_part ShowForAllWhen tvs ctxt sdoc

-- | Like 'pprIfaceForAllPart', but always uses an explicit @forall@.
pprIfaceForAllPartMust :: [IfaceForAllBndr] -> [IfacePredType] -> SDoc -> SDoc
pprIfaceForAllPartMust tvs ctxt sdoc
  = ppr_iface_forall_part ShowForAllMust tvs ctxt sdoc

pprIfaceForAllCoPart :: [(IfLclName, IfaceCoercion, ForAllTyFlag, ForAllTyFlag)]
                     -> SDoc -> SDoc
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
pprIfaceForAll bndrs@(Bndr _ vis : _)
  = sep [ add_separator (forAllLit <+> fsep docs)
        , pprIfaceForAll bndrs' ]
  where
    (bndrs', docs) = ppr_itv_bndrs bndrs vis

    add_separator stuff = case vis of
                            Required -> stuff <+> arrow
                            _inv     -> stuff <>  dot


-- | Render the ... in @(forall ... .)@ or @(forall ... ->)@.
-- Returns both the list of not-yet-rendered binders and the doc.
-- No anonymous binders here!
ppr_itv_bndrs :: [IfaceForAllBndr]
             -> ForAllTyFlag  -- ^ visibility of the first binder in the list
             -> ([IfaceForAllBndr], [SDoc])
ppr_itv_bndrs all_bndrs@(bndr@(Bndr _ vis) : bndrs) vis1
  | vis `eqForAllVis` vis1 = let (bndrs', doc) = ppr_itv_bndrs bndrs vis1 in
                             (bndrs', pprIfaceForAllBndr bndr : doc)
  | otherwise              = (all_bndrs, [])
ppr_itv_bndrs [] _ = ([], [])

pprIfaceForAllCo :: [(IfLclName, IfaceCoercion, ForAllTyFlag, ForAllTyFlag)] -> SDoc
pprIfaceForAllCo []  = empty
pprIfaceForAllCo tvs = text "forall" <+> pprIfaceForAllCoBndrs tvs <> dot

pprIfaceForAllCoBndrs :: [(IfLclName, IfaceCoercion, ForAllTyFlag, ForAllTyFlag)] -> SDoc
pprIfaceForAllCoBndrs bndrs = hsep $ map pprIfaceForAllCoBndr bndrs

pprIfaceForAllBndr :: IfaceForAllBndr -> SDoc
pprIfaceForAllBndr bndr =
  case bndr of
    Bndr (IfaceTvBndr tv) Inferred ->
      braces $ pprIfaceTvBndr tv suppress_sig (UseBndrParens False)
    Bndr (IfaceTvBndr tv) _ ->
      pprIfaceTvBndr tv suppress_sig (UseBndrParens True)
    Bndr (IfaceIdBndr idv) _ -> pprIfaceIdBndr idv
  where
    -- See Note [Suppressing binder signatures]
    suppress_sig = SuppressBndrSig False

pprIfaceForAllCoBndr :: (IfLclName, IfaceCoercion, ForAllTyFlag, ForAllTyFlag) -> SDoc
pprIfaceForAllCoBndr (tv, kind_co, visL, visR)
  = parens (ppr tv <> pp_vis <+> dcolon <+> pprIfaceCoercion kind_co)
  where
    pp_vis | visL == coreTyLamForAllTyFlag
           , visR == coreTyLamForAllTyFlag
           = empty
           | otherwise
           = ppr visL <> char '~' <> ppr visR    -- "[spec]~[reqd]"

-- | Show forall flag
--
-- Unconditionally show the forall quantifier with ('ShowForAllMust')
-- or when ('ShowForAllWhen') the names used are free in the binder
-- or when compiling with -fprint-explicit-foralls.
data ShowForAllFlag = ShowForAllMust | ShowForAllWhen

data ShowSub
  = ShowSub
      { ss_how_much :: ShowHowMuch
      , ss_forall :: ShowForAllFlag }

-- See Note [Printing IfaceDecl binders]
-- The alternative pretty printer referred to in the note.
newtype AltPpr = AltPpr (Maybe (OccName -> SDoc))

data ShowHowMuch
  = ShowHeader AltPpr -- ^ Header information only, not rhs
  | ShowSome (Maybe (OccName -> Bool)) AltPpr
  -- ^ Show the declaration and its RHS. The @Maybe@ predicate
  -- allows filtering of the sub-components which should be printing;
  -- any sub-components filtered out will be elided with @...@.
  | ShowIface
  -- ^ Everything including GHC-internal information (used in --show-iface)

instance Outputable ShowHowMuch where
  ppr (ShowHeader _) = text "ShowHeader"
  ppr ShowIface      = text "ShowIface"
  ppr (ShowSome _ _) = text "ShowSome"

pprIfaceSigmaType :: ShowForAllFlag -> IfaceType -> SDoc
pprIfaceSigmaType show_forall ty
  = hideNonStandardTypes (ppr_sigma show_forall topPrec) ty

ppr_sigma :: ShowForAllFlag -> PprPrec -> IfaceType -> SDoc
ppr_sigma show_forall ctxt_prec iface_ty
  = maybeParen ctxt_prec funPrec $
    let (invis_tvs, theta, tau) = splitIfaceSigmaTy iface_ty
        (req_tvs, tau') = splitIfaceReqForallTy tau
          -- splitIfaceSigmaTy is recursive, so it will gather the binders after
          -- the theta, i.e.  forall a. theta => forall b. tau
          -- will give you    ([a,b], theta, tau).
          --
          -- This isn't right when it comes to visible forall (see
          --  testsuite/tests/polykinds/T18522-ppr),
          -- so we split off required binders separately,
          -- using splitIfaceReqForallTy.
          --
          -- An alternative solution would be to make splitIfaceSigmaTy
          -- non-recursive (see #18458).
          -- Then it could handle both invisible and required binders, and
          -- splitIfaceReqForallTy wouldn't be necessary here.
    in ppr_iface_forall_part show_forall invis_tvs theta $
       sep [pprIfaceForAll req_tvs, ppr_ty_nested tau']

pprUserIfaceForAll :: [IfaceForAllBndr] -> SDoc
pprUserIfaceForAll tvs
   = sdocOption sdocPrintExplicitForalls $ \print_foralls ->
     -- See Note [When to print foralls] in this module.
     ppWhen (any tv_has_kind_var tvs
             || any tv_is_required tvs
             || print_foralls) $
     pprIfaceForAll tvs
   where
     tv_has_kind_var (Bndr (IfaceTvBndr (_,kind)) _)
       = not (ifTypeIsVarFree kind)
     tv_has_kind_var _ = False

     tv_is_required = isVisibleForAllTyFlag . binderFlag

{-
Note [When to print foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We opt to explicitly pretty-print `forall`s if any of the following
criteria are met:

1. -fprint-explicit-foralls is on.

2. A bound type variable has a polymorphic kind. E.g.,

     forall k (a::k). Proxy a -> Proxy a

   Since a's kind mentions a variable k, we print the foralls.

3. A bound type variable is a visible argument (#14238).
   Suppose we are printing the kind of:

     T :: forall k -> k -> Type

   The "forall k ->" notation means that this kind argument is required.
   That is, it must be supplied at uses of T. E.g.,

     f :: T (Type->Type)  Monad -> Int

   So we print an explicit "T :: forall k -> k -> Type",
   because omitting it and printing "T :: k -> Type" would be
   utterly misleading.

   See Note [VarBndrs, ForAllTyBinders, TyConBinders, and visibility]
   in GHC.Core.TyCo.Rep.

N.B. Until now (Aug 2018) we didn't check anything for coercion variables.

Note [Printing foralls in type family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use the same criteria as in Note [When to print foralls] to determine
whether a type family instance should be pretty-printed with an explicit
`forall`. Example:

  type family Foo (a :: k) :: k where
    Foo Maybe       = []
    Foo (a :: Type) = Int
    Foo a           = a

Without -fprint-explicit-foralls enabled, this will be pretty-printed as:

type family Foo (a :: k) :: k where
  Foo Maybe = []
  Foo a = Int
  forall k (a :: k). Foo a = a

Note that only the third equation has an explicit forall, since it has a type
variable with a non-Type kind. (If -fprint-explicit-foralls were enabled, then
the second equation would be preceded with `forall a.`.)

There is one tricky point in the implementation: what visibility
do we give the type variables in a type family instance? Type family instances
only store type *variables*, not type variable *binders*, and only the latter
has visibility information. We opt to default the visibility of each of these
type variables to Specified because users can't ever instantiate these
variables manually, so the choice of visibility is only relevant to
pretty-printing. (This is why the `k` in `forall k (a :: k). ...` above is
printed the way it is, even though it wasn't written explicitly in the
original source code.)

We adopt the same strategy for data family instances. Example:

  data family DF (a :: k)
  data instance DF '[a, b] = DFList

That data family instance is pretty-printed as:

  data instance forall j (a :: j) (b :: j). DF '[a, b] = DFList

This is despite that the representation tycon for this data instance (call it
$DF:List) actually has different visibilities for its binders.
However, the visibilities of these binders are utterly irrelevant to the
programmer, who cares only about the specificity of variables in `DF`'s type,
not $DF:List's type. Therefore, we opt to pretty-print all variables in data
family instances as Specified.

Note [Printing promoted type constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this GHCi session (#14343)
    > _ :: Proxy '[ 'True ]
    error:
      Found hole: _ :: Proxy '['True]

This would be bad, because the '[' looks like a character literal.

A similar issue arises if the element is a character literal (#22488)
    ghci> type T = '[ 'x' ]
    ghci> :kind! T
    T :: [Char]
    = '['x']

Solution: in type-level lists and tuples, add a leading space
if the first element is printed with a single quote.
-}


-------------------

-- See equivalent function in "GHC.Core.TyCo.Rep"
pprIfaceTyList :: PprPrec -> IfaceType -> IfaceType -> SDoc
-- Given a type-level list (t1 ': t2), see if we can print
-- it in list notation [t1, ...].
-- Precondition: Opt_PrintExplicitKinds is off
pprIfaceTyList ctxt_prec ty1 ty2
  = case gather ty2 of
      (arg_tys, Nothing)
        ->
        sdocWithContext $ \ctx ->
          let
            items  = ty1:arg_tys
            eos    = isListEmptyOrSingleton items
            ticked = promTick (sdocStyle ctx) (PromotedItemListSyntax eos)
            (preBracket, postBracket) =
              if ticked
              then (char '\'', spaceIfSingleQuote)
              else (empty, id)
          in
            preBracket <> brackets (postBracket (fsep
                          (punctuate comma (map (ppr_ty topPrec) items))))
      (arg_tys, Just tl)
        -> maybeParen ctxt_prec funPrec $ hang (ppr_ty funPrec ty1)
           2 (fsep [ colon <+> ppr_ty funPrec ty | ty <- arg_tys ++ [tl]])
  where
    gather :: IfaceType -> ([IfaceType], Maybe IfaceType)
     -- (gather ty) = (tys, Nothing) means ty is a list [t1, .., tn]
     --             = (tys, Just tl) means ty is of form t1:t2:...tn:tl
    gather (IfaceTyConApp tc tys)
      | tc `ifaceTyConHasKey` consDataConKey
      , IA_Arg _ argf (IA_Arg ty1 Required (IA_Arg ty2 Required IA_Nil)) <- tys
      , isInvisibleForAllTyFlag argf
      , (args, tl) <- gather ty2
      = (ty1:args, tl)
      | tc `ifaceTyConHasKey` nilDataConKey
      = ([], Nothing)
    gather ty = ([], Just ty)

pprIfaceTypeApp :: PprPrec -> IfaceTyCon -> IfaceAppArgs -> SDoc
pprIfaceTypeApp prec tc args = pprTyTcApp prec tc args

pprTyTcApp :: PprPrec -> IfaceTyCon -> IfaceAppArgs -> SDoc
pprTyTcApp ctxt_prec tc tys =
    sdocOption sdocPrintExplicitKinds $ \print_kinds ->
    sdocOption sdocPrintTypeAbbreviations $ \print_type_abbreviations ->
    getPprDebug $ \debug ->

    if | ifaceTyConName tc `hasKey` ipClassKey
       , IA_Arg (IfaceLitTy (IfaceStrTyLit n))
                Required (IA_Arg ty Required IA_Nil) <- tys
       -> maybeParen ctxt_prec funPrec
         $ char '?' <> ftext (getLexicalFastString n) <> text "::" <> ppr_ty topPrec ty

       | IfaceTupleTyCon arity sort <- ifaceTyConSort info
       , not debug
       , arity == ifaceVisAppArgsLength tys
       -> ppr_tuple ctxt_prec sort (ifaceTyConIsPromoted info) tys
           -- NB: ppr_tuple requires a saturated tuple.

       | IfaceSumTyCon arity <- ifaceTyConSort info
       , not debug
       , arity == ifaceVisAppArgsLength tys
       -> ppr_sum ctxt_prec (ifaceTyConIsPromoted info) tys
           -- NB: ppr_sum requires a saturated unboxed sum.

       | tc `ifaceTyConHasKey` consDataConKey
       , False <- print_kinds
       , IA_Arg _ argf (IA_Arg ty1 Required (IA_Arg ty2 Required IA_Nil)) <- tys
       , isInvisibleForAllTyFlag argf
       -> pprIfaceTyList ctxt_prec ty1 ty2

       | isIfaceLiftedTypeKind (IfaceTyConApp tc tys)
       , print_type_abbreviations  -- See Note [Printing type abbreviations]
       -> ppr_kind_type ctxt_prec

       | isIfaceConstraintKind (IfaceTyConApp tc tys)
       , print_type_abbreviations  -- See Note [Printing type abbreviations]
       -> pprPrefixOcc constraintKindTyConName

       | tc `ifaceTyConHasKey` fUNTyConKey
       , IA_Arg (IfaceTyConApp rep IA_Nil) Required args <- tys
       , rep `ifaceTyConHasKey` manyDataConKey
       , print_type_abbreviations  -- See Note [Printing type abbreviations]
       -> pprIfacePrefixApp ctxt_prec (parens arrow) (map (ppr_app_arg appPrec) $
          appArgsIfaceTypesForAllTyFlags $
          stripInvisArgs (PrintExplicitKinds print_kinds) args)
          -- Use appArgsIfaceTypesForAllTyFlags to print invisible arguments
          -- correctly (#19310)

       | tc `ifaceTyConHasKey` errorMessageTypeErrorFamKey
       , not debug
         -- Suppress detail unless you _really_ want to see
       -> text "(TypeError ...)"

       | Just doc <- ppr_equality ctxt_prec tc (appArgsIfaceTypes tys)
       -> doc

       | otherwise
       -> ppr_iface_tc_app ppr_app_arg ctxt_prec tc $
          appArgsIfaceTypesForAllTyFlags $ stripInvisArgs (PrintExplicitKinds print_kinds) tys
  where
    info = ifaceTyConInfo tc

ppr_kind_type :: PprPrec -> SDoc
ppr_kind_type ctxt_prec = sdocOption sdocStarIsType $ \case
   False -> pprPrefixOcc liftedTypeKindTyConName
   True  -> maybeParen ctxt_prec starPrec $
              unicodeSyntax (char '★') (char '*')

-- | Pretty-print a type-level equality.
-- Returns (Just doc) if the argument is a /saturated/ application
-- of   eqTyCon          (~)
--      eqPrimTyCon      (~#)
--      eqReprPrimTyCon  (~R#)
--      heqTyCon         (~~)
--
-- See Note [Equality predicates in IfaceType]
-- and Note [The equality types story] in GHC.Builtin.Types.Prim
ppr_equality :: PprPrec -> IfaceTyCon -> [IfaceType] -> Maybe SDoc
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
    homogeneous = tc_name `hasKey` eqTyConKey -- (~)
               || hetero_tc_used_homogeneously
      where
        hetero_tc_used_homogeneously
          = case ifaceTyConSort $ ifaceTyConInfo tc of
                          IfaceEqualityTyCon -> True
                          _other             -> False
             -- True <=> a heterogeneous equality whose arguments
             --          are (in this case) of the same kind

    tc_name = ifaceTyConName tc
    pp = ppr_ty
    hom_eq_tc = tc_name `hasKey` eqTyConKey            -- (~)
    hetero_eq_tc = tc_name `hasKey` eqPrimTyConKey     -- (~#)
                || tc_name `hasKey` eqReprPrimTyConKey -- (~R#)
                || tc_name `hasKey` heqTyConKey        -- (~~)
    nominal_eq_tc = tc_name `hasKey` heqTyConKey       -- (~~)
                 || tc_name `hasKey` eqPrimTyConKey    -- (~#)
    print_equality args =
        sdocOption sdocPrintExplicitKinds $ \print_kinds ->
        sdocOption sdocPrintEqualityRelations $ \print_eqs ->
        getPprStyle      $ \style  ->
        getPprDebug      $ \debug  ->
        print_equality' args print_kinds
          (print_eqs || dumpStyle style || debug)

    print_equality' (ki1, ki2, ty1, ty2) print_kinds print_eqs
      | -- If -fprint-equality-relations is on, just print the original TyCon
        print_eqs
      = ppr_infix_eq (ppr tc)

      | -- Homogeneous use of heterogeneous equality (ty1 ~~ ty2)
        --                 or unlifted equality      (ty1 ~# ty2)
        nominal_eq_tc, homogeneous
      = ppr_infix_eq (text "~")

      | -- Heterogeneous use of unlifted equality (ty1 ~# ty2)
        not homogeneous
      = ppr_infix_eq (ppr heqTyCon)

      | -- Homogeneous use of representational unlifted equality (ty1 ~R# ty2)
        tc_name `hasKey` eqReprPrimTyConKey, homogeneous
      = let ki | print_kinds = [pp appPrec ki1]
               | otherwise   = []
        in pprIfacePrefixApp ctxt_prec (ppr coercibleTyCon)
                            (ki ++ [pp appPrec ty1, pp appPrec ty2])

        -- The other cases work as you'd expect
      | otherwise
      = ppr_infix_eq (ppr tc)
      where
        ppr_infix_eq :: SDoc -> SDoc
        ppr_infix_eq eq_op = pprIfaceInfixApp ctxt_prec eq_op
                               (pp_ty_ki ty1 ki1) (pp_ty_ki ty2 ki2)
          where
            pp_ty_ki ty ki
              | print_kinds
              = parens (pp topPrec ty <+> dcolon <+> pp opPrec ki)
              | otherwise
              = pp opPrec ty


pprIfaceCoTcApp :: PprPrec -> IfaceTyCon -> [IfaceCoercion] -> SDoc
pprIfaceCoTcApp ctxt_prec tc tys =
  ppr_iface_tc_app (\prec (co, _) -> ppr_co prec co) ctxt_prec tc
    (map (, Required) tys)
    -- We are trying to re-use ppr_iface_tc_app here, which requires its
    -- arguments to be accompanied by visibilities. But visibility is
    -- irrelevant when printing coercions, so just default everything to
    -- Required.

-- | Pretty-prints an application of a type constructor to some arguments
-- (whose visibilities are known). This is polymorphic (over @a@) since we use
-- this function to pretty-print two different things:
--
-- 1. Types (from `pprTyTcApp'`)
--
-- 2. Coercions (from 'pprIfaceCoTcApp')
ppr_iface_tc_app :: (PprPrec -> (a, ForAllTyFlag) -> SDoc)
                 -> PprPrec -> IfaceTyCon -> [(a, ForAllTyFlag)] -> SDoc

ppr_iface_tc_app pp ctxt_prec tc tys =
  sdocOption sdocListTuplePuns $ \listTuplePuns ->
  if | listTuplePuns, tc `ifaceTyConHasKey` listTyConKey, [ty] <- tys
     -> brackets (pp topPrec ty)

     | tc `ifaceTyConHasKey` liftedTypeKindTyConKey
     -> ppr_kind_type ctxt_prec

     | isSymOcc (nameOccName (ifaceTyConName tc))

     , [ ty1@(_, Required), ty2@(_, Required) ] <- tys
         -- Infix, two visible arguments (we know nothing of precedence though).
         -- Don't apply this special case if one of the arguments is invisible,
         -- lest we print something like (@LiftedRep -> @LiftedRep) (#15941).
     -> pprIfaceInfixApp ctxt_prec (pprIfaceTyCon tc) (pp opPrec ty1) (pp opPrec ty2)

     | otherwise
     -> pprIfacePrefixApp ctxt_prec (pprParendIfaceTyCon tc) (map (pp appPrec) tys)

data TupleOrSum = IsSum | IsTuple TupleSort
  deriving (Eq)

-- | Pretty-print a boxed tuple datacon in regular tuple syntax.
-- Used when -XListTuplePuns is disabled.
ppr_tuple_no_pun :: PprPrec -> [IfaceType] -> SDoc
ppr_tuple_no_pun ctxt_prec = \case
  [t] -> maybeParen ctxt_prec appPrec (text "MkSolo" <+> pprPrecIfaceType appPrec t)
  tys -> tupleParens BoxedTuple (pprWithCommas pprIfaceType tys)

-- | Pretty-print an unboxed tuple or sum type in its parenthesized, punned, form.
-- Used when -XListTuplePuns is enabled.
--
-- The tycon should be saturated:
-- as many visible arguments as the arity of the sum or tuple.
--
-- NB: this always strips off the invisible 'RuntimeRep' arguments,
-- even with `-fprint-explicit-runtime-reps` and `-fprint-explicit-kinds`.
ppr_tuple_sum_pun :: PprPrec -> TupleOrSum -> PromotionFlag -> IfaceType -> Arity -> [IfaceType] -> SDoc
ppr_tuple_sum_pun ctxt_prec sort promoted tc arity tys
  | IsSum <- sort
  = sumParens (pprWithBars (ppr_ty topPrec) tys)

  |  IsTuple ConstraintTuple <- sort
  ,  NotPromoted <- promoted
  ,  arity == 0
  = maybeParen ctxt_prec sigPrec $
    text "() :: Constraint"

  -- Special-case unary boxed tuples so that they are pretty-printed as
  -- `Solo x`, not `(x)`
  | IsTuple BoxedTuple <- sort
  , arity == 1
  = pprPrecIfaceType ctxt_prec tc

  | IsTuple tupleSort <- sort
  = pprPromotionQuoteI promoted <>
    tupleParens tupleSort (quote_space (pprWithCommas pprIfaceType tys))
  where
    quote_space = case promoted of
      IsPromoted -> spaceIfSingleQuote
      NotPromoted -> id

-- | Pretty-print an unboxed tuple or sum type either in the punned or unpunned form,
-- depending on whether -XListTuplePuns is enabled.
ppr_tuple_sum :: PprPrec -> TupleOrSum -> PromotionFlag -> IfaceAppArgs -> SDoc
ppr_tuple_sum ctxt_prec sort is_promoted args =
  sdocOption sdocListTuplePuns $ \case
    True -> ppr_tuple_sum_pun ctxt_prec sort is_promoted prefix_tc arity non_rep_tys
    False
      | IsPromoted <- is_promoted
      , IsTuple BoxedTuple <- sort
      -> ppr_tuple_no_pun ctxt_prec non_rep_tys
      | otherwise
      -> pprPrecIfaceType ctxt_prec prefix_tc
  where
    -- This tycon is used to print in prefix notation for the punned Solo
    -- case and the unabbreviated case.
    prefix_tc = IfaceTyConApp (IfaceTyCon (mk_name arity) info) args

    info = mkIfaceTyConInfo NotPromoted IfaceNormalTyCon

    mk_name = case (sort, is_promoted) of
      (IsTuple BoxedTuple, IsPromoted) -> tupleDataConName Boxed
      (IsTuple s, _) -> tupleTyConName s
      (IsSum, _) -> tyConName . sumTyCon

    -- drop the RuntimeRep vars.
    -- See Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
    non_rep_tys = if strip_reps then drop arity all_tys else all_tys

    arity = if strip_reps then count `div` 2 else count

    count = length all_tys

    all_tys = appArgsIfaceTypes args

    strip_reps = case is_promoted of
      IsPromoted -> True
      NotPromoted -> strip_reps_sort

    strip_reps_sort = case sort of
      IsTuple BoxedTuple -> False
      IsTuple UnboxedTuple -> True
      IsTuple ConstraintTuple -> False
      IsSum -> True

-- | Pretty-print an unboxed sum type.
-- The sum should be saturated: as many visible arguments as the arity of
-- the sum.
ppr_sum :: PprPrec -> PromotionFlag -> IfaceAppArgs -> SDoc
ppr_sum ctxt_prec = ppr_tuple_sum ctxt_prec IsSum

-- | Pretty-print a tuple type (boxed tuple, constraint tuple, unboxed tuple).
-- The tuple should be saturated: as many visible arguments as the arity of
-- the tuple.
ppr_tuple :: PprPrec -> TupleSort -> PromotionFlag -> IfaceAppArgs -> SDoc
ppr_tuple ctxt_prec sort = ppr_tuple_sum ctxt_prec (IsTuple sort)

pprIfaceTyLit :: IfaceTyLit -> SDoc
pprIfaceTyLit (IfaceNumTyLit n) = integer n
pprIfaceTyLit (IfaceStrTyLit n) = text (show n)
pprIfaceTyLit (IfaceCharTyLit c) = text (show c)

pprIfaceCoercion, pprParendIfaceCoercion :: IfaceCoercion -> SDoc
pprIfaceCoercion = ppr_co topPrec
pprParendIfaceCoercion = ppr_co appPrec

ppr_co :: PprPrec -> IfaceCoercion -> SDoc
ppr_co _         (IfaceReflCo ty) = angleBrackets (ppr ty) <> ppr_role Nominal
ppr_co _         (IfaceGReflCo r ty IfaceMRefl)
  = angleBrackets (ppr ty) <> ppr_role r
ppr_co ctxt_prec (IfaceGReflCo r ty (IfaceMCo co))
  = ppr_special_co ctxt_prec
    (text "GRefl" <+> ppr r <+> pprParendIfaceType ty) [co]

ppr_co ctxt_prec (IfaceFunCo r co_mult co1 co2)
  = maybeParen ctxt_prec funPrec $
    sep (ppr_co funPrec co1 : ppr_fun_tail co_mult co2)
  where
    ppr_fun_tail co_mult1 (IfaceFunCo r co_mult2 co1 co2)
      = (ppr_arrow co_mult1 <> ppr_role r <+> ppr_co funPrec co1)
        : ppr_fun_tail co_mult2 co2
    ppr_fun_tail co_mult1 other_co
      = [ppr_arrow co_mult1 <> ppr_role r <+> pprIfaceCoercion other_co]

    ppr_arrow = pprArrow (mb_conc, ppr_co) visArgTypeLike
    mb_conc (IfaceTyConAppCo _ tc _) = Just tc
    mb_conc _                        = Nothing

ppr_co _         (IfaceTyConAppCo r tc cos)
  = parens (pprIfaceCoTcApp topPrec tc cos) <> ppr_role r
ppr_co ctxt_prec (IfaceAppCo co1 co2)
  = maybeParen ctxt_prec appPrec $
    ppr_co funPrec co1 <+> pprParendIfaceCoercion co2
ppr_co ctxt_prec co@(IfaceForAllCo {})
  = maybeParen ctxt_prec funPrec $
    -- FIXME: collect and pretty-print visibility info?
    pprIfaceForAllCoPart tvs (pprIfaceCoercion inner_co)
  where
    (tvs, inner_co) = split_co co

    split_co (IfaceForAllCo (IfaceTvBndr (name, _)) visL visR kind_co co')
      = let (tvs, co'') = split_co co' in ((name,kind_co,visL,visR):tvs,co'')
    split_co (IfaceForAllCo (IfaceIdBndr (_, name, _)) visL visR kind_co co')
      = let (tvs, co'') = split_co co' in ((name,kind_co,visL,visR):tvs,co'')
    split_co co' = ([], co')

-- Why these three? See Note [Free TyVars and CoVars in IfaceType]
ppr_co _ (IfaceFreeCoVar covar) = ppr covar
ppr_co _ (IfaceCoVarCo covar)   = ppr covar
ppr_co _ (IfaceHoleCo covar)    = braces (ppr covar)

ppr_co _ (IfaceUnivCo prov role ty1 ty2)
  = text "Univ" <> (parens $
      sep [ ppr role <+> pprIfaceUnivCoProv prov
          , dcolon <+>  ppr ty1 <> comma <+> ppr ty2 ])

ppr_co ctxt_prec (IfaceInstCo co ty)
  = maybeParen ctxt_prec appPrec $
    text "Inst" <+> sep [ pprParendIfaceCoercion co
                        , pprParendIfaceCoercion ty ]

ppr_co ctxt_prec (IfaceAxiomRuleCo tc cos)
  = maybeParen ctxt_prec appPrec $ ppr tc <+> parens (interpp'SP cos)

ppr_co ctxt_prec (IfaceAxiomInstCo n i cos)
  = ppr_special_co ctxt_prec (ppr n <> brackets (ppr i)) cos
ppr_co ctxt_prec (IfaceSymCo co)
  = ppr_special_co ctxt_prec (text "Sym") [co]
ppr_co ctxt_prec (IfaceTransCo co1 co2)
    -- chain nested TransCo
  = let ppr_trans (IfaceTransCo c1 c2) = semi <+> ppr_co topPrec c1 : ppr_trans c2
        ppr_trans c                    = [semi <+> ppr_co opPrec c]
    in maybeParen ctxt_prec opPrec $
        vcat (ppr_co topPrec co1 : ppr_trans co2)
ppr_co ctxt_prec (IfaceSelCo d co)
  = ppr_special_co ctxt_prec (text "SelCo:" <> ppr d) [co]
ppr_co ctxt_prec (IfaceLRCo lr co)
  = ppr_special_co ctxt_prec (ppr lr) [co]
ppr_co ctxt_prec (IfaceSubCo co)
  = ppr_special_co ctxt_prec (text "Sub") [co]
ppr_co ctxt_prec (IfaceKindCo co)
  = ppr_special_co ctxt_prec (text "Kind") [co]

ppr_special_co :: PprPrec -> SDoc -> [IfaceCoercion] -> SDoc
ppr_special_co ctxt_prec doc cos
  = maybeParen ctxt_prec appPrec
               (sep [doc, nest 4 (sep (map pprParendIfaceCoercion cos))])

ppr_role :: Role -> SDoc
ppr_role r = underscore <> pp_role
  where pp_role = case r of
                    Nominal          -> char 'N'
                    Representational -> char 'R'
                    Phantom          -> char 'P'

------------------
pprIfaceUnivCoProv :: IfaceUnivCoProv -> SDoc
pprIfaceUnivCoProv (IfacePhantomProv co)
  = text "phantom" <+> pprParendIfaceCoercion co
pprIfaceUnivCoProv (IfaceProofIrrelProv co)
  = text "irrel" <+> pprParendIfaceCoercion co
pprIfaceUnivCoProv (IfacePluginProv s cvs fcvs)
  = hang (text "plugin") 2 (sep [doubleQuotes (text s), ppr cvs, ppr fcvs])

-------------------
instance Outputable IfLclName where
  ppr = ppr . ifLclNameFS

instance Outputable IfaceTyCon where
  ppr = pprIfaceTyCon

-- | Print an `IfaceTyCon` with a promotion tick if needed, without parens,
-- suitable for use in infix contexts
pprIfaceTyCon :: IfaceTyCon -> SDoc
pprIfaceTyCon tc = pprPromotionQuote tc <> ppr (ifaceTyConName tc)

-- | Print an `IfaceTyCon` with a promotion tick if needed, possibly with parens,
-- suitable for use in prefix contexts
pprParendIfaceTyCon :: IfaceTyCon -> SDoc
pprParendIfaceTyCon tc = pprPromotionQuote tc <> pprPrefixVar (isSymOcc (nameOccName tc_name)) (ppr tc_name)
  where tc_name = ifaceTyConName tc

instance Outputable IfaceTyConInfo where
  ppr (IfaceTyConInfo { ifaceTyConIsPromoted = prom
                      , ifaceTyConSort       = sort })
    = angleBrackets $ ppr prom <> comma <+> ppr sort

pprPromotionQuote :: IfaceTyCon -> SDoc
pprPromotionQuote tc =
  getPprStyle $ \sty ->
    let
      name   = getOccName (ifaceTyConName tc)
      ticked =
        case ifaceTyConIsPromoted (ifaceTyConInfo tc) of
          NotPromoted -> False
          IsPromoted  -> promTick sty (PromotedItemDataCon name)
    in
      if ticked
      then char '\''
      else empty

pprPromotionQuoteI  :: PromotionFlag -> SDoc
pprPromotionQuoteI NotPromoted = empty
pprPromotionQuoteI IsPromoted  = char '\''

instance Outputable IfaceCoercion where
  ppr = pprIfaceCoercion

instance Binary IfaceTyCon where
  put_ bh (IfaceTyCon n i) = put_ bh n >> put_ bh i

  get bh = do
    n <- get bh
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

   get bh = mkIfaceTyConInfo <$!> get bh <*> get bh
    -- We want to make sure, when reading from disk, as the most common case
    -- is supposed to be shared. Any thunk adds an additional indirection
    -- making sharing less useful.
    --
    -- See !12200 for how this bang and the one in 'IfaceTyCon' reduces the
    -- residency by ~10% when loading 'mi_extra_decls' from disk.

instance Outputable IfaceTyLit where
  ppr = pprIfaceTyLit

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
  put_ bh tk = do
    -- Int is variable length encoded so only
    -- one byte for small lists.
    put_ bh (ifaceAppArgsLength tk)
    go tk
    where
      go IA_Nil = pure ()
      go (IA_Arg a b t) = do
        put_ bh a
        put_ bh b
        go t

  get bh = do
    n <- get bh :: IO Int
    go n
    where
      go 0 = return IA_Nil
      go c = do
        a <- get bh
        b <- get bh
        IA_Arg a b <$> go (c - 1)

-------------------

-- Some notes about printing contexts
--
-- In the event that we are printing a singleton context (e.g. @Eq a@) we can
-- omit parentheses. However, we must take care to set the precedence correctly
-- to opPrec, since something like @a :~: b@ must be parenthesized (see
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
-- use 'funPrec' to decide whether to parenthesise a singleton
-- predicate; e.g.   Num a => a -> a
pprIfaceContextArr :: [IfacePredType] -> SDoc
pprIfaceContextArr []     = empty
pprIfaceContextArr [pred] = ppr_ty funPrec pred <+> darrow
pprIfaceContextArr preds  = ppr_parend_preds preds <+> darrow

-- | Prints a context or @()@ if empty
-- You give it the context precedence
pprIfaceContext :: PprPrec -> [IfacePredType] -> SDoc
pprIfaceContext _    []     = text "()"
pprIfaceContext prec [pred] = ppr_ty prec pred
pprIfaceContext _    preds  = ppr_parend_preds preds

ppr_parend_preds :: [IfacePredType] -> SDoc
ppr_parend_preds preds = parens (fsep (punctuate comma (map ppr preds)))

instance Binary IfaceType where
   put_ bh ty =
    case findUserDataWriter Proxy bh of
      tbl -> putEntry tbl bh ty

   get bh = getIfaceTypeShared bh

-- | This is the byte tag we expect to read when the next
-- value is not an 'IfaceType' value, but an offset into a
-- lookup table.
-- See Note [Deduplication during iface binary serialisation].
--
-- Must not overlap with any byte tag in 'getIfaceType'.
ifaceTypeSharedByte :: Word8
ifaceTypeSharedByte = 99

-- | Like 'getIfaceType' but checks for a specific byte tag
-- that indicates that we won't be able to read a 'IfaceType' value
-- but rather an offset into a lookup table. Consequentially,
-- we look up the value for the 'IfaceType' in the look up table.
--
-- See Note [Deduplication during iface binary serialisation]
-- for details.
getIfaceTypeShared :: ReadBinHandle -> IO IfaceType
getIfaceTypeShared bh = do
  start <- tellBinReader bh
  tag <- getByte bh
  if ifaceTypeSharedByte == tag
    then case findUserDataReader Proxy bh of
            tbl -> getEntry tbl bh
    else seekBinReader bh start >> getIfaceType bh

-- | Serialises an 'IfaceType' to the given 'WriteBinHandle'.
--
-- Serialising inner 'IfaceType''s uses the 'Binary.put' of 'IfaceType' which may be using
-- a deduplication table. See Note [Deduplication during iface binary serialisation].
putIfaceType :: WriteBinHandle -> IfaceType -> IO ()
putIfaceType _ (IfaceFreeTyVar tv)
  = pprPanic "Can't serialise IfaceFreeTyVar" (ppr tv)
  -- See Note [Free TyVars and CoVars in IfaceType]

putIfaceType bh (IfaceForAllTy aa ab) = do
        putByte bh 0
        put_ bh aa
        put_ bh ab
putIfaceType bh (IfaceTyVar ad) = do
        putByte bh 1
        put_ bh ad
putIfaceType bh (IfaceAppTy ae af) = do
        putByte bh 2
        put_ bh ae
        put_ bh af
putIfaceType bh (IfaceFunTy af aw ag ah) = do
        putByte bh 3
        put_ bh af
        put_ bh aw
        put_ bh ag
        put_ bh ah
putIfaceType bh (IfaceTyConApp tc tys)
  = do { putByte bh 5; put_ bh tc; put_ bh tys }
putIfaceType bh (IfaceCastTy a b)
  = do { putByte bh 6; put_ bh a; put_ bh b }
putIfaceType bh (IfaceCoercionTy a)
  = do { putByte bh 7; put_ bh a }
putIfaceType bh (IfaceTupleTy s i tys)
  = do { putByte bh 8; put_ bh s; put_ bh i; put_ bh tys }
putIfaceType bh (IfaceLitTy n)
  = do { putByte bh 9; put_ bh n }

-- | Deserialises an 'IfaceType' from the given 'ReadBinHandle'.
--
-- Reading inner 'IfaceType''s uses the 'Binary.get' of 'IfaceType' which may be using
-- a deduplication table. See Note [Deduplication during iface binary serialisation].
getIfaceType :: HasCallStack => ReadBinHandle -> IO IfaceType
getIfaceType bh = do
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

instance Binary IfLclName where
  put_ bh = put_ bh . ifLclNameFS

  get bh = do
    fs <- get bh
    pure $ IfLclName $ LexicalFastString fs

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
  put_ bh (IfaceForAllCo a visL visR b c) = do
          putByte bh 6
          put_ bh a
          put_ bh visL
          put_ bh visR
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
  put_ bh (IfaceSelCo a b) = do
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
  put_ _ (IfaceFreeCoVar cv)
       = pprPanic "Can't serialise IfaceFreeCoVar" (ppr cv)
           -- See Note [Free TyVars and CoVars in IfaceType]
  put_ _  (IfaceHoleCo cv)
       = pprPanic "Can't serialise IfaceHoleCo" (ppr cv)
           -- See Note [Holes in IfaceCoercion]

  get bh = do
      tag <- getByte bh
      case tag of
           1 -> do a <- get bh
                   return $ IfaceReflCo a
           2 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceGReflCo a b c
           3 -> do a  <- get bh
                   w  <- get bh
                   b  <- get bh
                   c  <- get bh
                   return $ IfaceFunCo a w b c
           4 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceTyConAppCo a b c
           5 -> do a <- get bh
                   b <- get bh
                   return $ IfaceAppCo a b
           6 -> do a <- get bh
                   visL <- get bh
                   visR <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceForAllCo a visL visR b c
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
                   return $ IfaceSelCo a b
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
  put_ bh (IfacePluginProv a cvs fcvs) = do
          putByte bh 3
          put_ bh a
          -- See Note [Free TyVars and CoVars in IfaceType]
          assertPpr (null fcvs) (ppr cvs $$ ppr fcvs) $
            put_ bh cvs

  get bh = do
      tag <- getByte bh
      case tag of
           1 -> do a <- get bh
                   return $ IfacePhantomProv a
           2 -> do a <- get bh
                   return $ IfaceProofIrrelProv a
           3 -> do a <- get bh
                   cvs <- get bh
                   return $ IfacePluginProv a cvs []
           _ -> panic ("get IfaceUnivCoProv " ++ show tag)


instance Binary (DefMethSpec IfaceType) where
    put_ bh VanillaDM     = putByte bh 0
    put_ bh (GenericDM t) = putByte bh 1 >> put_ bh t
    get bh = do
            h <- getByte bh
            case h of
              0 -> return VanillaDM
              _ -> do { t <- get bh; return (GenericDM t) }

instance NFData IfaceType where
  rnf = \case
    IfaceFreeTyVar f1 -> f1 `seq` ()
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

instance NFData IfaceCoercion where
  rnf = \case
    IfaceReflCo f1 -> rnf f1
    IfaceGReflCo f1 f2 f3 -> f1 `seq` rnf f2 `seq` rnf f3
    IfaceFunCo f1 f2 f3 f4 -> f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4
    IfaceTyConAppCo f1 f2 f3 -> f1 `seq` rnf f2 `seq` rnf f3
    IfaceAppCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceForAllCo f1 f2 f3 f4 f5 -> rnf f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4 `seq` rnf f5
    IfaceCoVarCo f1 -> rnf f1
    IfaceAxiomInstCo f1 f2 f3 -> rnf f1 `seq` rnf f2 `seq` rnf f3
    IfaceAxiomRuleCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceUnivCo f1 f2 f3 f4 -> rnf f1 `seq` f2 `seq` rnf f3 `seq` rnf f4
    IfaceSymCo f1 -> rnf f1
    IfaceTransCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceSelCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceLRCo f1 f2 -> f1 `seq` rnf f2
    IfaceInstCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceKindCo f1 -> rnf f1
    IfaceSubCo f1 -> rnf f1
    IfaceFreeCoVar f1 -> f1 `seq` ()
    IfaceHoleCo f1 -> f1 `seq` ()

instance NFData IfaceUnivCoProv where
  rnf x = seq x ()

instance NFData IfaceMCoercion where
  rnf x = seq x ()

instance NFData IfaceOneShot where
  rnf x = seq x ()

instance NFData IfaceTyConSort where
  rnf = \case
    IfaceNormalTyCon -> ()
    IfaceTupleTyCon arity sort -> rnf arity `seq` sort `seq` ()
    IfaceSumTyCon arity -> rnf arity
    IfaceEqualityTyCon -> ()

instance NFData IfLclName where
  rnf (IfLclName lfs) = rnf lfs

instance NFData IfaceTyConInfo where
  rnf (IfaceTyConInfo f s) = f `seq` rnf s

instance NFData IfaceTyCon where
  rnf (IfaceTyCon nm info) = rnf nm `seq` rnf info

instance NFData IfaceBndr where
  rnf = \case
    IfaceIdBndr id_bndr -> rnf id_bndr
    IfaceTvBndr tv_bndr -> rnf tv_bndr

instance NFData IfaceAppArgs where
  rnf = \case
    IA_Nil -> ()
    IA_Arg f1 f2 f3 -> rnf f1 `seq` f2 `seq` rnf f3
