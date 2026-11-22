{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


This module defines interface types and binders
-}


{-# LANGUAGE MultiWayIf, OverloadedRecordDot #-}
module GHC.Iface.Type (
        IfExtName,
        IfLclName(..), mkIfLclName, ifLclNameFS,

        IfaceType(..), IfacePredType, IfaceKind, IfaceCoercion(..),
        IfaceUnivCoDep(..),
        IfaceAxiomRule(..),IfaceMCoercion(..),
        IfaceMult, IfaceMatchability,
        IfaceTyCon(..),
        IfaceTyConInfo(..), mkIfaceTyConInfo,
        IfaceTyConSort(..),
        IfaceTyLit(..), IfaceAppArgs(..),
        IfaceContext, IfaceBndr(..), IfaceOneShot(..), IfaceLamBndr,
        IfaceTvBndr, IfaceIdBndr, IfaceTyConBinder,
        IfaceForAllSpecBndr,
        IfaceForAllBndr, IfaceForAllTyFlag(..),
        ForAllTyFlag(..), FunTyFlag(..), ShowForAllFlag(..),
        ShowSub(..), ShowHowMuch(..), AltPpr(..),
        mkIfaceForAllTvBndr,
        mkIfaceTyConKind,
        ifaceForAllSpecToBndrs, ifaceForAllSpecToBndr,

        ifForAllBndrVar, ifForAllBndrName, ifaceBndrName,
        ifTyConBinderVar, ifTyConBinderName,

        -- Binary utilities
        putIfaceType, getIfaceType, ifaceTypeSharedByte,
        IfaceEvidence(..), IfaceEvidenceVersion, requireIfaceEvidenceVersion,
        -- Equality testing
        isIfaceLiftedTypeKind,

        -- Conversion from IfaceAppArgs to IfaceTypes/ForAllTyFlags
        appArgsIfaceTypes, appArgsIfaceTypesForAllTyFlags,

        -- Printing
        SuppressBndrSig(..),
        UseBndrParens(..),
        PrintExplicitKinds(..),
        PrintArityInvisibles(..),
        pprIfaceType, pprParendIfaceType, pprPrecIfaceType,
        pprIfaceContext, pprIfaceContextArr,
        pprIfaceIdBndr, pprIfaceLamBndr, pprIfaceTvBndr, pprIfaceTyConBinders,
        pprIfaceBndrs, pprIfaceAppArgs, pprParendIfaceAppArgs,
        pprIfaceForAllPart, pprIfaceForAllPartMust, pprIfaceForAll,
        pprIfaceForAllWithMatchability, pprUserIfaceForAllWithMatchability,
        pprIfaceSigmaType, pprIfaceTyLit,
        pprIfaceCoercion, pprParendIfaceCoercion,
        splitIfaceSigmaTy, pprIfaceTypeApp, pprUserIfaceForAll,
        pprIfaceCoTcApp, pprTyTcApp, pprIfacePrefixApp, stripLiftedTypeOwner, pprLiftedTypeLambdaSite,
        isIfaceRhoType,

        suppressIfaceInvisibles,
        visibleTypeVarOccurencies,
        stripIfaceInvisVars,
        stripInvisArgs,

        mkIfaceTySubst, substIfaceTyVar, substIfaceAppArgs, inDomIfaceTySubst,

        many_ty, ifaceMatchable, ifaceUnmatchable, pprTypeArrow
    ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Builtin.WiredIn.Types
                                 ( coercibleTyCon, heqTyCon
                                 , constraintKindTyConName
                                 , tupleTyConName
                                 , tupleDataConName
                                 , manyDataConTyCon
                                 , matchableDataConTyCon, unmatchableDataConTyCon
                                 , liftedRepTyCon, liftedDataConTyCon
                                 , liftedTypeKindTyConName, sumTyCon,  )
import GHC.Base ( Multiplicity(..) )
import GHC.Core.Multiplicity ( pprArrowWithModifiers )
import GHC.Core.Type ( isRuntimeRepTy, isMultiplicityTy, isLevityTy )
import GHC.Core.TyCo.Rep( CoSel, UnivCoProvenance(..) )
import GHC.Core.TyCon hiding ( pprPromotionQuote )
import GHC.Core.Coercion.Axiom
import GHC.Types.Var
import GHC.Builtin.KnownKeys
import GHC.Builtin.Modules (gHC_INTERNAL_TYPENATS)
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Utils.Binary
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Data.FastString.Env (delFromFsEnv, nonDetFoldFsEnv)
import GHC.Utils.Misc
import GHC.Utils.Panic
import {-# SOURCE #-} GHC.Tc.Utils.TcType ( isMetaTyVar, isTyConableTyVar )

import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import qualified Data.Semigroup as Semi
import Data.Word (Word8)
import Control.Arrow (first)
import Control.DeepSeq
import Control.Monad ((<$!>), when)
import Data.List (dropWhileEnd, stripPrefix)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

{-
************************************************************************
*                                                                      *
                Local (nested) binders
*                                                                      *
************************************************************************
-}

type IfExtName = Name -- Always an External, KnownKey, or WiredIn Name
                      -- Never an Internal of System Name

-- | A local name in iface syntax
newtype IfLclName = IfLclName
  { getIfLclName :: LexicalFastString
  } deriving (Eq, Ord, Show)

data IfaceBndr          -- Local (non-top-level) binders
  = IfaceIdBndr {-# UNPACK #-} !IfaceIdBndr
  | IfaceTvBndr {-# UNPACK #-} !IfaceTvBndr
  deriving (Eq, Ord)

type IfaceIdBndr  = (IfaceType, IfLclName, IfaceType)  -- (multiplicity, name, type)
type IfaceTvBndr  = (IfLclName, IfaceKind)

type IfaceLamBndr = (IfaceBndr, IfaceOneShot)

data IfaceOneShot    -- See Note [Preserve OneShotInfo] in "GHC.Core.Tidy"
  = IfaceNoOneShot   -- and Note [oneShot magic] in "GHC.Types.Id.Make"
  | IfaceOneShot


ifLclNameFS :: IfLclName -> FastString
ifLclNameFS = getLexicalFastString . getIfLclName

mkIfLclName :: FastString -> IfLclName
mkIfLclName = IfLclName . LexicalFastString

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

-- | Type-valued matchability metadata for an arrow or a forall.
type IfaceMatchability = IfaceType

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
  | IfaceFunTy     FunTyFlag IfaceMatchability IfaceMult IfaceType IfaceType
      -- See Note [Ordinary-arrow interface metadata].
  | IfaceForAllTy  IfaceMatchability IfaceForAllBndr IfaceType
      -- See Note [Forall interface metadata].
  -- See Note [Retained Boolean interface types].
  | IfaceExistsTy ExistsTyFlag IfaceTvBndr IfaceType
  | IfaceDepFunTy !DepFunTyFlag IfaceIdBndr IfaceType
      -- See Note [Retained interface visibility].
  | IfaceTermVar IfLclName
  | IfaceFreeTermVar Id                 -- Diagnostics only, like IfaceFreeTyVar
  | IfaceTermAppTy IfLclName IfaceType  -- See Note [Opaque call indices in interfaces]
  | IfaceFreeTermAppTy Id IfaceType    -- Diagnostics only, like IfaceFreeTermVar
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
  | IfaceDepDictTy IfaceIdBndr IfaceType
  | IfaceClassAppTy Name IfaceType IfaceType IfaceType
      -- See Note [Scoped class selectors in interfaces].
  deriving (Eq, Ord)
  -- See Note [Ord instance of IfaceType]

{- Note [Scoped class selectors in interfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An IfaceDepDictTy binds its Id only in the result type, like IfaceDepFunTy.
An IfaceClassAppTy stores a global class method, its type argument and two
lexical references. The reader derives the result kind from the method and
checks the types of both references. The method is part of the declaration's
ABI dependencies. Free references can occur in diagnostic types only.
-}

{- Note [Forall interface metadata]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IfaceForAllTy stores matchability before its binder and body. The annotation
is outside the binder scope. IfaceForAllCo stores its nominal metadata proof
before its binder-kind proof. Both proofs are outside the binder scope.
Read and validate these fields before installing the binder.

Keep metadata in equality, serialization, dependencies and telescope views.
Binder visibility does not determine matchability. The common interface
version rejects the old payload before decoding these fields. HIE has its
own version and stores the metadata as a type-table child.

Print an annotation after its group's separator: forall a. @ma body, or
forall a -> @ma body. The annotation remains outside the whole group.
Combine adjacent binders only when their annotations and visibilities agree.
Keep symbolic annotations when defaulting display variables.
Semantic declaration kinds come from ifKind, not from binder-only lists.
See Note [Default forall matchability in diagnostics] for user-facing types.
-}

{- Note [Ordinary-arrow interface metadata]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IfaceFunTy stores matchability before multiplicity. IfaceFunCo stores its
proof in the same position. Both fields participate in interface sharing,
fingerprints, free names and substitution. The common payload version has
a distinct suffix in GHC.Settings.Constants. A previous payload must fail
at the file header before a reader decodes either new field.
-}

{- Note [Retained Boolean interface types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A retained binder uses the existing local Id format. Its multiplicity and
domain are outside its scope. Its body can refer to its local name through
IfaceTermVar. The reader allocates a fresh Id and resolves those occurrences
in the Id environment, separately from type variables. Do not serialise Id
Uniques or use IfaceForAllTy: this argument is a retained runtime value.

As with ordinary interface binders, tidy the type before conversion so that
distinct nested binders have distinct local spellings. The derived Eq and Ord
instances compare this named syntax for binary sharing. They do not perform
alpha equivalence. Free runtime Ids used by diagnostics have the separate
IfaceFreeTermVar form. That form must not enter an interface file.

Tags 10 and 11 require the fork's distinct configured compiler version.
Old readers must reject the file in the whole-file version check before
type decoding.
See docs/dependent-haskell/REFERENCES.md#agda-provenance for the comparison
with Agda's explicit binder and variable serialization.
-}

{- Note [Retained interface visibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IfaceDepFunTy stores its visibility before the local Id binder. Visibility
selects source argument syntax. Both flags retain a runtime value. Preserve
this flag in sharing, renaming, type traversals and the binary payload.
The common interface version must reject previous payloads before decoding.
The DepFunTyFlag decoder rejects unknown flag tags.

The reader must use mkDepFunTyWithVis. A vacuous visible binder can become an
ordinary arrow. A vacuous hidden binder must remain retained. Keep the same
Many, lifted-domain, scope and result-kind checks for both forms.

Print a visible binder with an arrow and a hidden binder with a dot. This is
a diagnostic spelling; it adds no source grammar. IfaceDepFunCo preserves
both endpoint flags for diagnostics, including a change of visibility.
Native coercion serialization and import remain blocked.
-}

{- Note [Opaque call indices in interfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IfaceTermAppTy stores a local function name and its index argument with tag 12.
The enclosing telescope writes the function's domain before that name enters
scope. The importer resolves the actual local Id and checks its Many arrow,
lifted argument and result types, and the kind of the index argument.
An IfaceAppTy can supply the remaining arguments of a curried call.
Check each argument against the next unrestricted value arrow.
The result kind comes from the head that was actually decoded. When a
call supplies a type argument, check that kind against the application's
telescope. Replacing a Bool result with a scoped type variable cannot
make the call a valid argument of Bool kind. A cast reports the right kind
of its coercion. Before constructing the cast, check that its nominal
coercion starts at the actual kind of the call. Constructor fields can refer
only to earlier fields. An IfaceDepFunTy binder can hold an opaque lifted
value. Check its arrow when an IfaceTermAppTy uses it as a call head.
See Note [Exported function-valued retained signatures]
in GHC.Tc.Gen.HsType. This encoding adds no runtime call or function equality.

An open diagnostic head must keep its actual Id. CoreToIface selects
IfaceFreeTermAppTy when the head belongs to its free-variable set. Keep the
argument for printing and traversal, but reject this form in the raw writer
and importer. Do not encode an open head as a local spelling that could resolve
to a different binder. Shared-type table writers use the same raw writer.

Tag 12 requires the fork's distinct configured interface version. Native call
coercions and unfolding generation remain separately gated. See the telescope
scope comparison in docs/dependent-haskell/REFERENCES.md under
function-valued-retained-package-fields-prior-art.
-}

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
  | IfaceIntPrimTyLit Integer
  | IfaceWordPrimTyLit Integer
  deriving (Eq, Ord)

type IfaceTyConBinder    = VarBndr IfaceBndr TyConBndrVis
type IfaceForAllBndr     = VarBndr IfaceBndr IfaceForAllTyFlag
type IfaceForAllSpecBndr = VarBndr IfaceBndr Specificity

-- | The 'ForAllTyFlag' used in 'IfaceType' and 'IfaceCoercion'.
--
-- 'ForAllTyFlag' deliberately has no 'Eq' or 'Ord' instances
-- (see Note [Comparing visibility] in "GHC.Core.TyCo.Compare").  But
-- 'IfaceType' needs 'Eq' and 'Ord' instances for deduplication during
-- interface-file serialisation; see Note [Ord instance of IfaceType].
-- Those instances must compare /exactly/, distinguishing 'Specified' from
-- 'Inferred'; otherwise (forall a. ty) and (forall {a}. ty) would be
-- deduplicated into the same table entry.
newtype IfaceForAllTyFlag = IfaceForAllTyFlag { ifaceForAllTyFlag :: ForAllTyFlag }

instance Eq IfaceForAllTyFlag where
  f1 == f2 = compare f1 f2 == EQ

instance Ord IfaceForAllTyFlag where
  -- Exact comparison; see Note [Ord instance of IfaceType]
  compare (IfaceForAllTyFlag f1) (IfaceForAllTyFlag f2) = cmpForAllTyFlagExactly f1 f2

instance Outputable IfaceForAllTyFlag where
  ppr (IfaceForAllTyFlag f) = ppr f

instance Binary IfaceForAllTyFlag where
  put_ bh (IfaceForAllTyFlag f) = put_ bh f
  get bh = IfaceForAllTyFlag <$> get bh

instance NFData IfaceForAllTyFlag where
  rnf (IfaceForAllTyFlag f) = rnf f

-- | Make an 'IfaceForAllBndr' from an 'IfaceTvBndr'.
mkIfaceForAllTvBndr :: ForAllTyFlag -> IfaceTvBndr -> IfaceForAllBndr
mkIfaceForAllTvBndr vis var = Bndr (IfaceTvBndr var) (IfaceForAllTyFlag vis)

-- | Build a fresh 'tyConKind' with explicit prefix matchability.
-- Use the stored full kind when reconstructing a declaration.
-- Keep in sync with 'mkTyConKind' in "GHC.Core.TyCon".
mkIfaceTyConKind :: IfaceMatchability -> [IfaceTyConBinder] -> IfaceKind -> IfaceKind
mkIfaceTyConKind ma bndrs res_kind = foldr mk res_kind bndrs
  where
    mk :: IfaceTyConBinder -> IfaceKind -> IfaceKind
    mk (Bndr tv AnonTCB)        k = IfaceFunTy FTF_T_T ma many_ty (ifaceBndrType tv) k
    mk (Bndr tv (NamedTCB vis)) k = IfaceForAllTy ma (Bndr tv (IfaceForAllTyFlag vis)) k

ifaceForAllSpecToBndrs :: [IfaceForAllSpecBndr] -> [IfaceForAllBndr]
ifaceForAllSpecToBndrs = map ifaceForAllSpecToBndr

ifaceForAllSpecToBndr :: IfaceForAllSpecBndr -> IfaceForAllBndr
ifaceForAllSpecToBndr (Bndr tv spec) = Bndr tv (IfaceForAllTyFlag (Invisible spec))

-- | Stores the arguments in a type application as a list.
-- See @Note [Suppressing invisible arguments]@.
data IfaceAppArgs
  = IA_Nil
  | IA_Arg IfaceType    -- The type argument

           IfaceForAllTyFlag -- The argument's visibility. We store this here so
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

                    | IfacePartialSynonym -- ^ A synonym applied below its arity.

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
  ppr IfacePartialSynonym      = text "partial synonym"

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
in GHC.Builtin.WiredIn.Prim for details).  In an effort to avoid confusing users, we suppress
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

See Note [The equality types story] in GHC.Builtin.WiredIn.Prim.
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
  | IfaceFunCo        Role IfaceCoercion IfaceCoercion IfaceCoercion IfaceCoercion
  | IfaceDepFunCo     !DepFunTyFlag !DepFunTyFlag IfaceIdBndr IfaceMCoercion IfaceCoercion
      -- The runtime binder scopes over the body only.
  | IfaceTermAppCo    IfLclName IfaceCoercion -- Diagnostic form; no wire encoding
  | IfaceTyConAppCo   Role IfaceTyCon [IfaceCoercion]
  | IfaceAppCo        IfaceCoercion IfaceCoercion
  | IfaceForAllCo     IfaceBndr !IfaceForAllTyFlag !IfaceForAllTyFlag IfaceCoercion IfaceMCoercion IfaceCoercion
  | IfaceCoVarCo      IfLclName
  | IfaceAxiomCo      IfaceAxiomRule [IfaceCoercion]
       -- ^ There are only a fixed number of CoAxiomRules, so it suffices
       -- to use an IfaceLclName to distinguish them.
       -- See Note [Adding built-in type families] in GHC.Builtin.WiredIn.TypeLits
  | IfaceUnivCo       UnivCoProvenance Role IfaceType IfaceType [IfaceUnivCoDep]
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
  -- Why Ord?  See Note [Ord instance of IfaceType]

-- | A single dependency of an 'IfaceUnivCo'.
--
-- See Note [UnivCoDeps] in GHC.Core.TyCo.Rep.
data IfaceUnivCoDep
  = IfaceUnivCoDepCoVar     IfLclName
      -- ^ An in-scope coercion variable (bound by an enclosing binder).
      --
      -- This is the only form that occurs in interface files.
  | IfaceUnivCoDepFreeCoVar CoVar
      -- ^ A free coercion variable.
      --
      -- See Note [Free TyVars and CoVars in IfaceType]
  | IfaceUnivCoDepHole      CoVar
      -- ^ A coercion hole.
      --
      -- See Note [Holes in IfaceCoercion]
  deriving (Eq, Ord)

data IfaceAxiomRule
  = IfaceAR_X IfLclName               -- Built-in
  | IfaceAR_B IfExtName BranchIndex   -- Branched
  | IfaceAR_U IfExtName               -- Unbranched
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

The dependencies of a UnivCo are subject to the same treatment, via
IfaceUnivCoDepHole.


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
  , IA_Arg arg1 (IfaceForAllTyFlag Required) IA_Nil <- args
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
  , IA_Arg arg1 (IfaceForAllTyFlag Required) IA_Nil <- args
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
  , IA_Arg arg1 (IfaceForAllTyFlag Required) IA_Nil <- args
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

splitIfaceSigmaTy :: IfaceType -> ([(IfaceMatchability, IfaceForAllBndr)], [IfacePredType], IfaceType)
-- Mainly for printing purposes
--
-- Here we split nested IfaceSigmaTy properly.
--
-- @
-- forall t. @M T t => forall m a b. @U M m => (a -> m b) -> t a -> m (t b)
-- @
--
-- If you called @splitIfaceSigmaTy@ on this type:
--
-- @
-- ([(M,t), (U,m), (U,a), (U,b)], [T t, M m], (a -> m b) -> t a -> m (t b))
-- @
splitIfaceSigmaTy ty
  = case (bndrs, theta) of
      ([], []) -> (bndrs, theta, tau)
      _        -> let (bndrs', theta', tau') = splitIfaceSigmaTy tau
                   in (bndrs ++ bndrs', theta ++ theta', tau')
  where
    (bndrs, rho)   = split_foralls ty
    (theta, tau)   = split_rho rho

    split_foralls (IfaceForAllTy ma bndr ty)
        | isInvisibleForAllTyFlag (ifaceForAllTyFlag (binderFlag bndr))
        = case split_foralls ty of { (bndrs, rho) -> ((ma,bndr):bndrs, rho) }
    split_foralls rho = ([], rho)

    split_rho (IfaceFunTy af _ _ ty1 ty2)
        | isInvisibleFunArg af
        = case split_rho ty2 of { (ps, tau) -> (ty1:ps, tau) }
    split_rho tau = ([], tau)

splitIfaceReqForallTy :: IfaceType -> ([(IfaceMatchability, IfaceForAllBndr)], IfaceType)
splitIfaceReqForallTy (IfaceForAllTy ma bndr ty)
  | isVisibleForAllTyFlag (ifaceForAllTyFlag (binderFlag bndr))
  = case splitIfaceReqForallTy ty of { (bndrs, rho) -> ((ma,bndr):bndrs, rho) }
splitIfaceReqForallTy rho = ([], rho)

newtype PrintArityInvisibles = MkPrintArityInvisibles Bool

-- See Note [Print invisible binders in interface declarations]
-- for the definition of what binders are considered insignificant
suppressIfaceInvisibles :: PrintArityInvisibles
                        -> PrintExplicitKinds
                        -> Set.Set IfLclName
                        -> [IfaceTyConBinder]
                        -> [a]
                        -> [a]
suppressIfaceInvisibles _ (PrintExplicitKinds True) _ _tys xs = xs

suppressIfaceInvisibles -- This case is semantically the same as the third case, but it should be way f
  (MkPrintArityInvisibles False) (PrintExplicitKinds False) mentioned_vars tys xs
  | Set.null mentioned_vars = suppress tys xs
    where
      suppress _       []      = []
      suppress []      a       = a
      suppress (k:ks) (x:xs)
        | isInvisibleTyConBinder k =     suppress ks xs
        | otherwise                = x : suppress ks xs

suppressIfaceInvisibles
  (MkPrintArityInvisibles arity_invisibles)
  (PrintExplicitKinds False) mentioned_vars tys xs
  = map snd (suppress (zip tys xs))
    where
      -- Consider this example:
      --   type T :: forall k1 k2. Type
      --   type T @a @b = b
      -- `@a` is not mentioned on the RHS. However, we can't just
      -- drop it because implicit argument positioning matters.
      --
      -- Hence just drop the end
      only_mentioned_binders = dropWhileEnd (not . is_binder_mentioned)

      is_binder_mentioned (bndr, _) = ifTyConBinderName bndr `Set.member` mentioned_vars

      suppress_invisibles group =
        applyWhen invis_group only_mentioned_binders bndrs
        where
          bndrs       = NonEmpty.toList group
          invis_group = is_invisible_bndr (NonEmpty.head group)

      suppress_invisible_groups [] = []
      suppress_invisible_groups [group] =
          if arity_invisibles
            then NonEmpty.toList group -- the last group affects arity
            else suppress_invisibles group
      suppress_invisible_groups (group : groups)
        = suppress_invisibles group ++ suppress_invisible_groups groups

      suppress
        = suppress_invisible_groups            -- Filter out insignificant invisible binders
        . NonEmpty.groupWith is_invisible_bndr -- Find chunks of @-binders
        . filterOut          is_inferred_bndr  -- We don't want to display @{binders}

      is_inferred_bndr = isInferredTyConBinder . fst
      is_invisible_bndr = isInvisibleTyConBinder . fst

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
    go (IfaceTermVar {})       = False
    go (IfaceFreeTermVar {})   = False
    go (IfaceTermAppTy {})     = False
    go (IfaceFreeTermAppTy {}) = False
    go (IfaceExistsTy {})      = False
    go (IfaceDepFunTy {})      = False
    go (IfaceDepDictTy {})     = False
    go (IfaceClassAppTy {})    = False
    go (IfaceAppTy fun args)   = go fun && go_args args
    go (IfaceFunTy _ ma w arg res) = go ma && go w && go arg && go res
    go (IfaceForAllTy {})      = False
    go (IfaceTyConApp _ args)  = go_args args
    go (IfaceTupleTy _ _ args) = go_args args
    go (IfaceLitTy _)          = True
    go (IfaceCastTy {})        = False -- Safe
    go (IfaceCoercionTy {})    = False -- Safe

    go_args IA_Nil = True
    go_args (IA_Arg arg _ args) = go arg && go_args args

visibleTypeVarOccurencies :: IfaceType -> Set.Set IfLclName
-- Returns True if the type contains this name. Doesn't count
-- invisible application
-- Just used to control pretty printing
visibleTypeVarOccurencies = go
  where
    (<>) = Set.union

    go (IfaceTyVar var)         = Set.singleton var
    go (IfaceFreeTyVar {})      = mempty
    go (IfaceTermVar {})        = mempty
    go (IfaceFreeTermVar {})    = mempty
    go (IfaceTermAppTy _ arg)   = go arg
    go (IfaceFreeTermAppTy _ arg) = go arg
    go (IfaceExistsTy _ (_, arg) res) = go arg <> go res
    go (IfaceDepFunTy _vis (w, _, arg) res) = go w <> go arg <> go res
    go (IfaceDepDictTy (w, _, arg) res) = go w <> go arg <> go res
    go (IfaceClassAppTy _ _ dict arg) = go dict <> go arg
    go (IfaceAppTy fun args)    = go fun <> go_args args
    go (IfaceFunTy _ ma w arg res) = go ma <> go w <> go arg <> go res
    go (IfaceForAllTy ma bndr ty) = go ma <> go (ifaceBndrType $ binderVar bndr) <> go ty
    go (IfaceTyConApp _ args)   = go_args args
    go (IfaceTupleTy _ _ args)  = go_args args
    go (IfaceLitTy _)           = mempty
    go (IfaceCastTy {})         = mempty -- Safe
    go (IfaceCoercionTy {})     = mempty -- Safe

    go_args IA_Nil = mempty
    go_args (IA_Arg arg (IfaceForAllTyFlag Required) args) = go arg <> go_args args
    go_args (IA_Arg _arg _ args) = go_args args

{- Note [Substitution on IfaceType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Substitutions on IfaceType are done only during pretty-printing to
construct the result type of a GADT. An existential can occur in a
GADT index. Its binder scopes over its payload only. Freshen that binder
if its name occurs in the substitution range. Reserve all names in the
payload when generating the replacement, including nested binders.
A set of all names is sufficient and avoids a second scoped traversal
to compute free variables. The existing forall restrictions remain. -}

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
    go ty@(IfaceTermVar {})  = ty
    go ty@(IfaceFreeTermVar {}) = ty
    go (IfaceTermAppTy fun arg) = IfaceTermAppTy fun (go arg)
    go (IfaceFreeTermAppTy fun arg) = IfaceFreeTermAppTy fun (go arg)
    go (IfaceExistsTy flag (name, kind) body)
      = IfaceExistsTy flag (name', go kind) (substIfaceType body_env body)
      where
        without_binder = delFromFsEnv env (ifLclNameFS name)
        range_names = nonDetFoldFsEnv (Set.union . ifaceLocalNames) Set.empty without_binder
        used = range_names `Set.union` ifaceLocalNames body
        name'
          | Set.member name range_names
          = head [ candidate
                 | n <- [(1 :: Int)..]
                 , let candidate = mkIfLclName (mkFastString (unpackFS (ifLclNameFS name) ++ show n))
                 , Set.notMember candidate used ]
          | otherwise = name
        body_env
          | name' == name = without_binder
          | otherwise = extendFsEnv without_binder (ifLclNameFS name) (IfaceTyVar name')
    go (IfaceClassAppTy method inst dict arg) = IfaceClassAppTy method (go inst) dict arg
    go (IfaceDepDictTy {})   = pprPanic "substIfaceType: scoped dictionary binder" (ppr ty)
    go (IfaceDepFunTy {})    = pprPanic "substIfaceType: native dependent Core not implemented" (ppr ty)
    go (IfaceTyVar tv)        = substIfaceTyVar env tv
    go (IfaceAppTy  t ts)     = IfaceAppTy  (go t) (substIfaceAppArgs env ts)
    go (IfaceFunTy af ma w t1 t2) = IfaceFunTy af (go ma) (go w) (go t1) (go t2)
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
    go_co (IfaceFunCo r ma w c1 c2) = IfaceFunCo r (go_co ma) (go_co w) (go_co c1) (go_co c2)
    go_co (IfaceDepFunCo {})
      = panic "substIfaceCoercion: native dependent coercion substitution is not implemented"
    go_co (IfaceTermAppCo fun co) = IfaceTermAppCo fun (go_co co)
    go_co (IfaceTyConAppCo r tc cos) = IfaceTyConAppCo r tc (go_cos cos)
    go_co (IfaceAppCo c1 c2)         = IfaceAppCo (go_co c1) (go_co c2)
    go_co (IfaceForAllCo {})         = pprPanic "substIfaceCoercion" (ppr ty)
    go_co (IfaceFreeCoVar cv)        = IfaceFreeCoVar cv
    go_co (IfaceCoVarCo cv)          = IfaceCoVarCo cv
    go_co (IfaceHoleCo cv)           = IfaceHoleCo cv
    go_co (IfaceUnivCo p r t1 t2 ds) = IfaceUnivCo p r (go t1) (go t2) ds
    go_co (IfaceSymCo co)            = IfaceSymCo (go_co co)
    go_co (IfaceTransCo co1 co2)     = IfaceTransCo (go_co co1) (go_co co2)
    go_co (IfaceSelCo n co)          = IfaceSelCo n (go_co co)
    go_co (IfaceLRCo lr co)          = IfaceLRCo lr (go_co co)
    go_co (IfaceInstCo c1 c2)        = IfaceInstCo (go_co c1) (go_co c2)
    go_co (IfaceKindCo co)           = IfaceKindCo (go_co co)
    go_co (IfaceSubCo co)            = IfaceSubCo (go_co co)
    go_co (IfaceAxiomCo n cos)       = IfaceAxiomCo n (go_cos cos)

    go_cos = map go_co

-- | All local names appearing in a type, including hidden arguments and casts.
ifaceLocalNames :: IfaceType -> Set.Set IfLclName
ifaceLocalNames = go
  where
    union = Set.union
    names = Set.unions
    one = Set.singleton
    go = \case
      IfaceTyVar var -> one var
      IfaceTermVar var -> one var
      IfaceTermAppTy fun arg -> one fun `union` go arg
      IfaceFreeTermAppTy _ arg -> go arg
      IfaceExistsTy _ (var, arg) body -> names [one var, go arg, go body]
      IfaceDepFunTy _ (w, var, arg) body -> names [one var, go w, go arg, go body]
      IfaceDepDictTy (w, var, pred_ty) body -> names [one var, go w, go pred_ty, go body]
      IfaceClassAppTy _ inst dict arg -> names [go inst, go dict, go arg]
      IfaceAppTy fun args -> go fun `union` go_args args
      IfaceFunTy _ ma w arg res -> names (map go [ma, w, arg, res])
      IfaceForAllTy ma (Bndr b _) body -> names [go ma, bndr b, go body]
      IfaceTyConApp _ args -> go_args args
      IfaceTupleTy _ _ args -> go_args args
      IfaceCastTy ty co -> go ty `union` go_co co
      IfaceCoercionTy co -> go_co co
      IfaceFreeTyVar{} -> Set.empty
      IfaceFreeTermVar{} -> Set.empty
      IfaceLitTy{} -> Set.empty

    go_args IA_Nil = Set.empty
    go_args (IA_Arg arg _ args) = go arg `union` go_args args

    bndr (IfaceTvBndr (v, k)) = one v `union` go k
    bndr (IfaceIdBndr (w, v, ty)) = names [one v, go w, go ty]

    go_mco IfaceMRefl = Set.empty
    go_mco (IfaceMCo co) = go_co co

    go_co = \case
      IfaceReflCo ty -> go ty
      IfaceGReflCo _ ty mco -> go ty `union` go_mco mco
      IfaceFunCo _ ma w arg res -> names (map go_co [ma, w, arg, res])
      IfaceDepFunCo _ _ b k body -> names [bndr (IfaceIdBndr b), go_mco k, go_co body]
      IfaceTermAppCo fun co -> one fun `union` go_co co
      IfaceTyConAppCo _ _ cos -> names (map go_co cos)
      IfaceAppCo fun arg -> go_co fun `union` go_co arg
      IfaceForAllCo b _ _ ma k body -> names [bndr b, go_co ma, go_mco k, go_co body]
      IfaceCoVarCo v -> one v
      IfaceAxiomCo _ cos -> names (map go_co cos)
      IfaceUnivCo _ _ ty1 ty2 deps -> names (go ty1 : go ty2 : map dep deps)
      IfaceSymCo co -> go_co co
      IfaceTransCo co1 co2 -> go_co co1 `union` go_co co2
      IfaceSelCo _ co -> go_co co
      IfaceLRCo _ co -> go_co co
      IfaceInstCo co1 co2 -> go_co co1 `union` go_co co2
      IfaceKindCo co -> go_co co
      IfaceSubCo co -> go_co co
      IfaceFreeCoVar{} -> Set.empty
      IfaceHoleCo{} -> Set.empty

    dep (IfaceUnivCoDepCoVar v) = one v
    dep IfaceUnivCoDepFreeCoVar{} = Set.empty
    dep IfaceUnivCoDepHole{} = Set.empty

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
              |  isVisibleForAllTyFlag (ifaceForAllTyFlag argf)
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
                                 = (t, ifaceForAllTyFlag a) : appArgsIfaceTypesForAllTyFlags ts

ifaceVisAppArgsLength :: IfaceAppArgs -> Int
ifaceVisAppArgsLength = go 0
  where
    go !n IA_Nil = n
    go n  (IA_Arg _ argf rest)
      | isVisibleForAllTyFlag (ifaceForAllTyFlag argf) = go (n+1) rest
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
isIfaceRhoType (IfaceForAllTy {})    = False
isIfaceRhoType (IfaceFunTy af _ _ _ _) = isVisibleFunArg af
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

-- | Render the stored visibility of a retained runtime binder.
-- See Note [Retained interface visibility].
pprIfaceDepFunArrow :: DepFunTyFlag -> SDoc
pprIfaceDepFunArrow DepFunVisible = arrow
pprIfaceDepFunArrow DepFunInvisible = dot

{- Note [Ordinary arrow matchability in user output]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Proposal #242 (section "Showing matchabilities") introduces
-fprint-explicit-matchabilities. User output without Modifiers can
omit fixed M and U on ordinary arrows. For example, show the kind of
Maybe as Type -> Type. These arrows can have either matchability.

Show both annotations when the explicit flag is set. The mismatch
reporter can also set that flag. See Note [Showing invisible bits of
types in error messages] in GHC.Tc.Errors.Ppr. Modifiers output shows M
and symbolic matchabilities. When user output shows a matchability,
print it as a source modifier, even if Modifiers is disabled. For
example, GHCi can show a matchability variable inferred in an imported
family's kind. Keep the existing notation for debug and dump output.

https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#showing-matchabilities
-}

pprTypeArrow :: FunTyFlag -> IfaceMatchability -> IfaceMult -> SDoc
pprTypeArrow af ma mult =
  getPprStyle $ \style ->
  getPprDebug $ \debug ->
  sdocOption sdocModifiers $ \modifiers ->
  sdocOption sdocPrintExplicitMatchabilities $ \explicit ->
    let user_arrow = userStyle style && isFUNArg af
        -- See Note [Ordinary arrow matchability in user output].
        show_ma
          | user_arrow && explicit = True
          | user_arrow && not debug && not modifiers && ma == ifaceMatchable = False
          | otherwise = ma /= ifaceUnmatchable
    in if user_arrow && (modifiers || not debug)
      then pprArrowWithModifiers ([pprPrecIfaceType appPrec ma | show_ma] ++ mods) af arr
      else pprArrowWithModifiers mods af arr
             <+> ppWhen show_ma (char '@' <> parens (pprPrecIfaceType appPrec ma))
  where
    (arr, mods) = case mult of
      IfaceTyConApp tc IA_Nil
        | tc `ifaceTyConHasKey` manyDataConKey
        -> (Many, [])
        | tc `ifaceTyConHasKey` oneDataConKey
        -> (One, [])
      _ -> (Many, [pprPrecIfaceType appPrec mult])

ppr_ty :: PprPrec -> IfaceType -> SDoc
ppr_ty ctxt_prec ty
  | not (isIfaceRhoType ty)             = ppr_sigma ShowForAllMust ctxt_prec ty
ppr_ty _         (IfaceForAllTy {})     = panic "ppr_ty"  -- Covered by not.isIfaceRhoType
ppr_ty _         (IfaceFreeTyVar tyvar) = ppr tyvar  -- This is the main reason for IfaceFreeTyVar!
ppr_ty _         (IfaceTyVar tyvar)     = ppr tyvar  -- See Note [Free TyVars and CoVars in IfaceType]
ppr_ty _         (IfaceTermVar var)     = ppr var
ppr_ty _         (IfaceFreeTermVar var) = ppr var
ppr_ty ctxt_prec (IfaceTermAppTy fun arg)
  = maybeParen ctxt_prec appPrec (ppr fun <+> ppr_ty appPrec arg)
ppr_ty ctxt_prec (IfaceFreeTermAppTy fun arg)
  = maybeParen ctxt_prec appPrec (ppr fun <+> ppr_ty appPrec arg)
ppr_ty ctxt_prec (IfaceExistsTy flag (var, arg) res)
  = maybeParen ctxt_prec funPrec $
      pprExistsTy flag (parens (ppr var <+> dcolon <+> ppr_ty_nested arg)) (ppr_ty_nested res)
ppr_ty ctxt_prec (IfaceDepDictTy (_, var, pred_ty) res)
  = maybeParen ctxt_prec funPrec $
    sep [parens (ppr var <+> dcolon <+> ppr_ty_nested pred_ty) <+> darrow, ppr_ty_nested res]
ppr_ty ctxt_prec (IfaceClassAppTy method inst dict arg)
  = maybeParen ctxt_prec appPrec $
    ppr method <+> (char '@' <> ppr_ty appPrec inst)
               <+> ppr_ty appPrec dict <+> ppr_ty appPrec arg
ppr_ty ctxt_prec (IfaceDepFunTy vis (_, var, arg) res)
  = maybeParen ctxt_prec funPrec $
    case vis of
      DepFunVisible   -> sep [binder, arrow <+> body]
      DepFunInvisible -> sep [binder <> dot, body]
  where
    binder = text "foreach" <+> parens (ppr var <+> dcolon <+> ppr_ty_nested arg)
    body = ppr_ty_nested res
ppr_ty ctxt_prec (IfaceTyConApp tc tys) = pprTyTcApp ctxt_prec tc tys
ppr_ty ctxt_prec (IfaceTupleTy i p tys) = ppr_tuple ctxt_prec i p tys -- always fully saturated
ppr_ty _         (IfaceLitTy n)         = pprIfaceTyLit n

        -- Function types
ppr_ty ctxt_prec ty@(IfaceFunTy af ma w ty1 ty2)  -- Should be a visible argument
  = assertPpr (isVisibleFunArg af) (ppr ty) $  -- Ensured by isIfaceRhoType above
    -- We want to print a chain of arrows in a column
    --     type1
    --     -> type2
    --     -> type3
    maybeParen ctxt_prec funPrec $
    sep [ppr_ty funPrec ty1, sep (ppr_fun_tail ma w ty2)]
  where
    ppr_fun_tail mathis wthis (IfaceFunTy af manext wnext ty1 ty2)
      | isVisibleFunArg af
      = (pprTypeArrow af mathis wthis <+> ppr_ty funPrec ty1) : ppr_fun_tail manext wnext ty2
    ppr_fun_tail mathis wthis other_ty
      = [pprTypeArrow af mathis wthis <+> ppr_ty_nested other_ty]

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
    go subs True (IfaceForAllTy ma (Bndr (IfaceTvBndr (var, var_kind)) argf) ty)
     | isInvisibleForAllTyFlag (ifaceForAllTyFlag argf)  -- Don't default *visible* quantification
                                     -- or we get the mess in #13963
     , ma == ifaceMatchable
     , Just substituted_ty <- check_substitution var_kind
      = let subs' = extendFsEnv subs (ifLclNameFS var) substituted_ty
            -- Record that we should replace it with LiftedRep/Lifted/Many,
            -- and recurse, discarding the forall
        in go subs' True ty

    go subs rank1 (IfaceForAllTy ma bndr ty)
      = IfaceForAllTy (go subs False ma) (go_ifacebndr subs bndr) (go subs rank1 ty)

    go subs rank1 (IfaceExistsTy flag (var, arg) res)
      = IfaceExistsTy flag (var, go subs False arg) (go subs rank1 res)

    go subs rank1 (IfaceDepFunTy vis (w, var, arg) res)
      = IfaceDepFunTy vis (go subs False w, var, go subs False arg) (go subs rank1 res)

    go subs rank1 (IfaceDepDictTy (w, var, pred_ty) res)
      = IfaceDepDictTy (go subs False w, var, go subs False pred_ty) (go subs rank1 res)
    go subs rank1 (IfaceClassAppTy method inst dict arg)
      = IfaceClassAppTy method (go subs rank1 inst) dict arg

    go _ _ ty@(IfaceTermVar {}) = ty
    go _ _ ty@(IfaceFreeTermVar {}) = ty
    go subs rank1 (IfaceTermAppTy fun arg) = IfaceTermAppTy fun (go subs rank1 arg)
    go subs rank1 (IfaceFreeTermAppTy fun arg) = IfaceFreeTermAppTy fun (go subs rank1 arg)

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

    go subs rank1 (IfaceFunTy af ma w arg res)
      = IfaceFunTy af (go subs False ma) (go subs False w) (go subs False arg) (go subs rank1 res)

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

-- | The promoted matchable arrow classification.
ifaceMatchable :: IfaceMatchability
ifaceMatchable = ifaceMatchability matchableDataConTyCon

-- | The promoted unmatchable arrow classification.
ifaceUnmatchable :: IfaceMatchability
ifaceUnmatchable = ifaceMatchability unmatchableDataConTyCon

-- | Construct an interface matchability constant from its promoted TyCon.
ifaceMatchability :: TyCon -> IfaceMatchability
ifaceMatchability tc = IfaceTyConApp
  (IfaceTyCon (getName tc) (mkIfaceTyConInfo IsPromoted IfaceNormalTyCon)) IA_Nil

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
    go (IA_Arg t argf ts) = ppr_app_arg ctx_prec (t, ifaceForAllTyFlag argf) <+> go ts

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
pprIfaceForAllPart :: [(IfaceMatchability, IfaceForAllBndr)] -> [IfacePredType] -> SDoc -> SDoc
pprIfaceForAllPart tvs ctxt sdoc
  = ppr_iface_forall_part ShowForAllWhen tvs ctxt sdoc

-- | Like 'pprIfaceForAllPart', but always uses an explicit @forall@.
pprIfaceForAllPartMust :: [(IfaceMatchability, IfaceForAllBndr)] -> [IfacePredType] -> SDoc -> SDoc
pprIfaceForAllPartMust tvs ctxt sdoc
  = ppr_iface_forall_part ShowForAllMust tvs ctxt sdoc

pprIfaceForAllCoPart :: [(IfaceBndr, ForAllTyFlag, ForAllTyFlag, IfaceCoercion, IfaceMCoercion)]
                     -> SDoc -> SDoc
pprIfaceForAllCoPart tvs sdoc = sep [ pprIfaceForAllCo tvs, sdoc ]

ppr_iface_forall_part :: ShowForAllFlag
                      -> [(IfaceMatchability, IfaceForAllBndr)] -> [IfacePredType] -> SDoc -> SDoc
ppr_iface_forall_part show_forall tvs ctxt sdoc
  = sep [ case show_forall of
            ShowForAllMust -> pprIfaceForAllWithMatchability tvs
            ShowForAllWhen -> pprUserIfaceForAllWithMatchability tvs
            ShowForAllDFun -> pprIfaceForAllWithDefault dfun_binder tvs
        , pprIfaceContextArr ctxt
        , sdoc]
  where
    -- See Note [Dictionary function quantifiers in instance headers].
    dfun_binder (ma, Bndr (IfaceTvBndr _) (IfaceForAllTyFlag Specified)) =
      ma == ifaceUnmatchable
    dfun_binder _ = False

-- | Render a semantic forall telescope with its matchability metadata.
-- See Note [Forall interface metadata].
pprIfaceForAllWithMatchability :: [(IfaceMatchability, IfaceForAllBndr)] -> SDoc
pprIfaceForAllWithMatchability bndrs
  = pprDefaultUserForAll bndrs (pprIfaceForAll (map snd bndrs)) (go bndrs)
  where
    go [] = empty
    go ((ma,bndr):bndrs)
      = sep [ add_separator (forAllLit <+> fsep (map pprIfaceForAllBndr group))
            <+> char '@' <> pprParendIfaceType ma
            , go rest ]
      where
        vis = ifaceForAllTyFlag (binderFlag bndr)
        (same, rest) = span same_group bndrs
        group = bndr : map snd same
        same_group (ma', bndr')
          = ma == ma' && vis `eqForAllVis` ifaceForAllTyFlag (binderFlag bndr')
        add_separator stuff = case vis of
          Required -> stuff <+> arrow
          _        -> stuff <> dot

-- | Apply user-facing quantifier rules without hiding non-default metadata.
pprUserIfaceForAllWithMatchability :: [(IfaceMatchability, IfaceForAllBndr)] -> SDoc
pprUserIfaceForAllWithMatchability bndrs
  = getPprStyle $ \style ->
    sdocOption sdocPrintExplicitMatchabilities $ \explicit ->
      if userStyle style && explicit
      then pprIfaceForAllWithMatchability bndrs
      else pprDefaultUserForAll bndrs (pprUserIfaceForAll (map snd bndrs)) $
             pprIfaceForAllWithDefault ((== ifaceMatchable) . fst) bndrs

-- | Use ordinary forall syntax for a default source-type telescope.
-- See Note [Default forall matchability in diagnostics].
pprDefaultUserForAll :: [(IfaceMatchability, IfaceForAllBndr)] -> SDoc -> SDoc -> SDoc
pprDefaultUserForAll bndrs ordinary annotated
  = getPprStyle $ \style ->
    getPprDebug $ \debug ->
    sdocOption sdocPrintExplicitKinds $ \print_kinds ->
    sdocOption sdocPrintExplicitMatchabilities $ \explicit ->
    sdocOption sdocModifiers $ \modifiers ->
      let implicit_ma (ma, bndr) =
            ma == ifaceUnmatchable
            || (ma == ifaceMatchable && not modifiers &&
                (not print_kinds || isInvisibleForAllTyFlag (ifaceForAllTyFlag (binderFlag bndr))))
      in if userStyle style && not debug && not explicit
            && all implicit_ma bndrs
         then ordinary else annotated

{- Note [Default forall matchability in diagnostics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Without Modifiers, user output omits fixed M and U on forall binders
as it does on ordinary arrows. For example, an ill-scoped data telescope
still shows its required binders without exposing their internal matchability.
Keep symbolic indices visible. With Modifiers, show a fixed M binder.
Explicit kinds still show fixed M on visible binders in a telescope.
A skolem escape diagnostic prints kind applications to show the escaping
variable. This does not need to show fixed matchability on invisible
binders. The explicit matchability flag and debug output show all annotations.
See Note [Ordinary arrow matchability in user output].
-}

-- | Apply the usual forall display rules when every binder has its contextual default.
pprIfaceForAllWithDefault :: ((IfaceMatchability, IfaceForAllBndr) -> Bool)
                          -> [(IfaceMatchability, IfaceForAllBndr)] -> SDoc
pprIfaceForAllWithDefault has_default bndrs
  | not (all has_default bndrs)
  = pprIfaceForAllWithMatchability bndrs
  | otherwise
  = sdocOption sdocPrintExplicitForalls $ \print_foralls ->
      ppWhen (print_foralls || any significant (map snd bndrs)) $
        pprIfaceForAllWithMatchability bndrs
  where
    significant (Bndr bndr (IfaceForAllTyFlag vis))
      = isVisibleForAllTyFlag vis || not (ifTypeIsVarFree (ifaceBndrType bndr))

-- | Render the "forall ... ." or "forall ... ->" bit of a type.
pprIfaceForAll :: [IfaceForAllBndr] -> SDoc
pprIfaceForAll [] = empty
pprIfaceForAll bndrs@(Bndr _ (IfaceForAllTyFlag vis) : _)
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
ppr_itv_bndrs all_bndrs@(bndr@(Bndr _ (IfaceForAllTyFlag vis)) : bndrs) vis1
  | vis `eqForAllVis` vis1 = let (bndrs', doc) = ppr_itv_bndrs bndrs vis1 in
                             (bndrs', pprIfaceForAllBndr bndr : doc)
  | otherwise              = (all_bndrs, [])
ppr_itv_bndrs [] _ = ([], [])

pprIfaceForAllCo :: [(IfaceBndr, ForAllTyFlag, ForAllTyFlag, IfaceCoercion, IfaceMCoercion)] -> SDoc
pprIfaceForAllCo []  = empty
pprIfaceForAllCo (bndr@(_,_,_,ma_co,_):bndrs)
  = sep [ text "forall" <+> pprIfaceForAllCoBndr bndr <> dot
            <+> char '@' <> pprParendIfaceCoercion ma_co
        , pprIfaceForAllCo bndrs ]

pprIfaceForAllBndr :: IfaceForAllBndr -> SDoc
pprIfaceForAllBndr bndr =
  case bndr of
    Bndr (IfaceTvBndr tv) (IfaceForAllTyFlag Inferred) ->
      braces $ pprIfaceTvBndr tv suppress_sig (UseBndrParens False)
    Bndr (IfaceTvBndr tv) _ ->
      pprIfaceTvBndr tv suppress_sig (UseBndrParens True)
    Bndr (IfaceIdBndr idv) _ -> pprIfaceIdBndr idv
  where
    -- See Note [Suppressing binder signatures]
    suppress_sig = SuppressBndrSig False

pprIfaceForAllCoBndr :: (IfaceBndr, ForAllTyFlag, ForAllTyFlag, IfaceCoercion, IfaceMCoercion) -> SDoc
pprIfaceForAllCoBndr (tcv, visL, visR, _ma_co, kind_mco)
  = parens (ppr (ifaceBndrName tcv) <> pp_vis
            <+> text "::~" <+> pprIfaceCoercion kind_co)
    -- We print (tcv ::~ kind_co), with the "::~" reminding us the type of tcv
    -- isn't kind_co; rather it's (coercionLKind kind_co).  We used "::" previously
    -- which grievously confused me.
  where
    kind_co = case kind_mco of
                   IfaceMRefl  -> IfaceReflCo (ifaceBndrType tcv)
                   IfaceMCo co -> co
    pp_vis | isSpecifiedForAllTyFlag visL   -- Omit the visibilities if both are
           , isSpecifiedForAllTyFlag visR   -- coreTyLamForAllTyFlag (= Specified)
           = empty
           | otherwise
           = ppr visL <> char '~' <> ppr visR    -- "[spec]~[reqd]"

-- | Show forall flag
--
-- Unconditionally show the forall quantifier with ('ShowForAllMust')
-- or when ('ShowForAllWhen') the names used are free in the binder
-- or when compiling with -fprint-explicit-foralls.
data ShowForAllFlag = ShowForAllMust | ShowForAllWhen | ShowForAllDFun

{- Note [Dictionary function quantifiers in instance headers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcMkDFunSigmaTy in GHC.Tc.Utils.TcType gives each dictionary function a
specified, unmatchable forall for each instance type variable. This metadata
does not record whether the instance declaration specified matchability.
An instance header can omit these quantifiers under the rules in
Note [When to print foralls]. When a quantifier is shown, print its actual
matchability. Apply this default only to the outer dictionary telescope.
-}

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
       sep [pprIfaceForAllWithMatchability req_tvs, ppr_ty_nested tau']

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

     tv_is_required = isVisibleForAllTyFlag . ifaceForAllTyFlag . binderFlag

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
      , IA_Arg _ argf (IA_Arg ty1 (IfaceForAllTyFlag Required) (IA_Arg ty2 (IfaceForAllTyFlag Required) IA_Nil)) <- tys
      , isInvisibleForAllTyFlag (ifaceForAllTyFlag argf)
      , (args, tl) <- gather ty2
      = (ty1:args, tl)
      | tc `ifaceTyConHasKey` nilDataConKey
      = ([], Nothing)
    gather ty = ([], Just ty)

pprIfaceTypeApp :: PprPrec -> IfaceTyCon -> IfaceAppArgs -> SDoc
pprIfaceTypeApp prec tc args = pprTyTcApp prec tc args

{- Note [Printing partial arrow matchability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FUN and (->) have a leading specified matchability argument. A partial
application remains a TyConApp, so it does not use the FunTy arrow printer.
Print that argument even when other invisible arguments are hidden. Otherwise
FUN @M Many and FUN @U Many print alike. The same rule preserves variables
and computed metadata. Keep the remaining argument visibilities unchanged.
Render the matchability argument directly. ppr_app_arg hides specified arguments
when explicit kind printing is off, even if its caller retains the argument.
See Note [Arrow matchability in Haddock] in Haddock.Convert for source ASTs.
-}

-- | Remove the prefix and private index from a lifted occurrence.
stripLiftedTypeOwner :: String -> String -> Maybe String
stripLiftedTypeOwner prefix occurrence = do
  key <- stripPrefix prefix occurrence
  case break (== ':') (reverse key) of
    (_ : _, ':' : owner) | not (null owner) -> Just (reverse owner)
    _ -> Nothing

-- | Identify the signature or open family equation that contains a lifted lambda.
pprLiftedTypeLambdaSite :: String -> Maybe SDoc
pprLiftedTypeLambdaSite occurrence
  | Just value <- stripLiftedTypeOwner "$signature-lambda:" owner
  = Just (text "type lambda in the signature for" <+> quotes (text value))
  | Just family <- stripLiftedTypeOwner "R:$callback:" owner
  = Just (text "type lambda in an instance of" <+> quotes (text family))
  | otherwise = Nothing
  where
    owner = fromMaybe occurrence (stripLiftedTypeOwner "$lambda:" occurrence)

pprTyTcApp :: PprPrec -> IfaceTyCon -> IfaceAppArgs -> SDoc
pprTyTcApp ctxt_prec tc tys =
    sdocOption sdocPrintExplicitKinds $ \print_kinds ->
    sdocOption sdocPrintTypeAbbreviations $ \print_type_abbreviations ->
    getPprDebug $ \debug ->
    getPprStyle $ \style ->

    if | ifaceTyConName tc `hasKey` ipClassKey
       , IA_Arg (IfaceLitTy (IfaceStrTyLit n))
                (IfaceForAllTyFlag Required) (IA_Arg ty (IfaceForAllTyFlag Required) IA_Nil) <- tys
       -> maybeParen ctxt_prec funPrec
         $ char '?' <> ftext (getLexicalFastString n) <> dcolon <> ppr_ty topPrec ty

       | userStyle style, not debug, print_type_abbreviations
       , Just site <- pprLiftedTypeLambdaSite (occNameString (nameOccName (ifaceTyConName tc)))
       -> pprIfacePrefixApp ctxt_prec (parens site)
            (map (ppr_app_arg appPrec) (appArgsIfaceTypesForAllTyFlags $
              stripInvisArgs (PrintExplicitKinds print_kinds) tys))

       | userStyle style, not debug, print_type_abbreviations
       , Just owner <- stripLiftedTypeOwner "$signature-case:"
                        (occNameString (nameOccName (ifaceTyConName tc)))
       -> maybeParen ctxt_prec funPrec $
            text "the type case in the signature for" <+> quotes (text owner)

       | userStyle style, not debug, print_type_abbreviations
       , Just owner <- stripLiftedTypeOwner "$field-case:"
                        (occNameString (nameOccName (ifaceTyConName tc)))
       -> maybeParen ctxt_prec funPrec $
            text "the type case in a field of" <+> quotes (text owner)

       -- See Note [Registered type numerals] in GHC.Tc.Gen.HsType.
       | userStyle style, not debug, not print_kinds, print_type_abbreviations
       , let name = ifaceTyConName tc
       , nameOccName name == mkTcOcc "NaturalLiteral"
       , nameModule_maybe name == Just gHC_INTERNAL_TYPENATS
       , IA_Arg target (IfaceForAllTyFlag Required)
           (IA_Arg (IfaceLitTy lit@IfaceNumTyLit{}) (IfaceForAllTyFlag Required) IA_Nil) <- tys
       -> maybeParen ctxt_prec sigPrec $ pprIfaceTyLit lit <+> dcolon <+> pprIfaceType target

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
       , IA_Arg _ argf (IA_Arg ty1 (IfaceForAllTyFlag Required) (IA_Arg ty2 (IfaceForAllTyFlag Required) IA_Nil)) <- tys
       , isInvisibleForAllTyFlag (ifaceForAllTyFlag argf)
       -> pprIfaceTyList ctxt_prec ty1 ty2

       | isIfaceLiftedTypeKind (IfaceTyConApp tc tys)
       , print_type_abbreviations  -- See Note [Printing type abbreviations]
       -> ppr_kind_type ctxt_prec

       | isIfaceConstraintKind (IfaceTyConApp tc tys)
       , print_type_abbreviations  -- See Note [Printing type abbreviations]
       -> pprPrefixOcc constraintKindTyConName

       -- See Note [Printing partial arrow matchability].
       | tc `ifaceTyConHasKey` fUNTyConKey
         || tc `ifaceTyConHasKey` unrestrictedFunTyConKey
       , IA_Arg ma ma_flag args <- tys
       , isInvisibleForAllTyFlag (ifaceForAllTyFlag ma_flag)
       -> pprIfacePrefixApp ctxt_prec (pprParendIfaceTyCon tc) $
          (char '@' <> ppr_ty appPrec ma) :
          map (ppr_app_arg appPrec) (appArgsIfaceTypesForAllTyFlags $
            stripInvisArgs (PrintExplicitKinds print_kinds) args)

       | tc `ifaceTyConHasKey` errorMessageTypeErrorFamKey
       , not debug
         -- Suppress detail unless you _really_ want to see
       -> text "(TypeError ...)"

       | Just doc <- ppr_equality ctxt_prec tc (appArgsIfaceTypes tys)
       -> doc

       -- See Note [The types Any and UnusedType], specifically (Any6) and (Any7)
       | ifaceTyConName tc `hasKey` unusedTypeTyConKey
       , ((arg_k, _) : (IfaceLitTy (IfaceStrTyLit arg_nm), _) : args_usr)
         <- appArgsIfaceTypesForAllTyFlags tys
         -- if arg_k is a kind with more than 0 arguments, then _ might not be [] here
       , userStyle style
       -> ppr_iface_unused_ty_tycon ctxt_prec arg_k arg_nm args_usr

       | otherwise
       -> ppr_iface_tc_app ppr_app_arg ctxt_prec tc $
          appArgsIfaceTypesForAllTyFlags $ stripInvisArgs (PrintExplicitKinds print_kinds) tys
  where
    info = ifaceTyConInfo tc

ppr_kind_type :: PprPrec -> SDoc
ppr_kind_type ctxt_prec = sdocOption sdocStarIsType $ \case
   False -> pprPrefixOcc liftedTypeKindTyConName
   True  -> maybeParen ctxt_prec starPrec starLit

-- | user-style printer that pretty-prints an 'UnusedType @k "foo_3" to foo_3.
-- If -fprint-explicit-kinds or -fprint-explicit-runtime-reps are set, instead
-- prints them to (foo3 :: k).
-- See Note [The types Any and UnusedType], specifically (Any6) and (Any7) for why this is useful.
ppr_iface_unused_ty_tycon :: PprPrec -> IfaceType -> LexicalFastString -> [(IfaceType, ForAllTyFlag)] -> SDoc
ppr_iface_unused_ty_tycon ctxt_prec arg_k arg_nm args_usr
  = sdocOption sdocPrintExplicitKinds       $ \print_kinds ->
    sdocOption sdocPrintExplicitRuntimeReps $ \print_reps  ->
      if print_kinds || print_reps
      then prettyMeta $ \nm ->
             maybeParen sig_prec sigPrec $ nm <+> text "::" <+> pprIfaceType arg_k
      else prettyMeta id
  where sig_prec = if null args_usr then ctxt_prec else appPrec
        prettyMeta add_ty
          = pprIfacePrefixApp ctxt_prec (add_ty $ ppr arg_nm)
          $ map (ppr_app_arg appPrec) args_usr

-- | Pretty-print a type-level equality.
-- Returns (Just doc) if the argument is a /saturated/ application
-- of   eqTyCon          (~)
--      eqPrimTyCon      (~#)
--      eqReprPrimTyCon  (~R#)
--      heqTyCon         (~~)
--
-- See Note [Equality predicates in IfaceType]
-- and Note [The equality types story] in GHC.Builtin.WiredIn.Prim
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
  sdocOption (isSymOccForPpr (nameOccName (ifaceTyConName tc))) $ \sym ->
  if | listTuplePuns, tc `ifaceTyConHasKey` listTyConKey, [ty] <- tys
     -> brackets (pp topPrec ty)

     | tc `ifaceTyConHasKey` liftedTypeKindTyConKey
     -> ppr_kind_type ctxt_prec

     | sym
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
pprIfaceTyLit (IfaceIntPrimTyLit n) = integer n <> char '#'
pprIfaceTyLit (IfaceWordPrimTyLit n) = integer n <> text "##"

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

ppr_co ctxt_prec (IfaceTermAppCo fun co)
  = maybeParen ctxt_prec appPrec (text "TermAppCo" <+> ppr fun <+> ppr_co appPrec co)

ppr_co ctxt_prec (IfaceDepFunCo visL visR (_, var, arg) domain co)
  = maybeParen ctxt_prec funPrec $
    sep [ text "foreach" <+> parens (ppr var <+> dcolon <+> ppr_ty_nested arg <+> ppr_domain domain)
        , visibility <+> pprIfaceCoercion co ]
  where
    visibility
      | visL == visR = pprIfaceDepFunArrow visL
      | otherwise = braces (pprIfaceDepFunArrow visL <> char '~' <> pprIfaceDepFunArrow visR)
    ppr_domain IfaceMRefl = empty
    ppr_domain (IfaceMCo proof) = text "|>" <+> pprIfaceCoercion proof

ppr_co ctxt_prec (IfaceFunCo r co_ma co_mult co1 co2)
  = maybeParen ctxt_prec funPrec $
    sep (ppr_co funPrec co1 : ppr_fun_tail co_ma co_mult co2)
  where
    ppr_fun_tail co_ma1 co_mult1 (IfaceFunCo r co_ma2 co_mult2 co1 co2)
      = (ppr_arrow co_ma1 co_mult1 <> ppr_role r <+> ppr_co funPrec co1)
        : ppr_fun_tail co_ma2 co_mult2 co2
    ppr_fun_tail co_ma1 co_mult1 other_co
      = [ppr_arrow co_ma1 co_mult1 <> ppr_role r <+> pprIfaceCoercion other_co]

    ppr_arrow ma w = ppr_mult_arrow w <+> ppr_matchability ma
    ppr_matchability (IfaceReflCo ty) | ty == ifaceUnmatchable = empty
    ppr_matchability ma = char '@' <> parens (ppr_co appPrec ma)

    ppr_mult_arrow (IfaceReflCo (IfaceTyConApp tc IA_Nil))
      | tc `ifaceTyConHasKey` manyDataConKey
      = pprArrowWithModifiers [] visArgTypeLike Many
      | tc `ifaceTyConHasKey` oneDataConKey
      = pprArrowWithModifiers [] visArgTypeLike One
    ppr_mult_arrow w =
      pprArrowWithModifiers [ppr_co appPrec w] visArgTypeLike Many

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

    split_co (IfaceForAllCo bndr visL visR ma_co kind_co co')
      = let (tvs, co'') = split_co co' in ((bndr,ifaceForAllTyFlag visL,ifaceForAllTyFlag visR,ma_co,kind_co):tvs,co'')
    split_co co' = ([], co')

-- Why these three? See Note [Free TyVars and CoVars in IfaceType]
ppr_co _ (IfaceFreeCoVar covar) = ppr covar
ppr_co _ (IfaceCoVarCo covar)   = ppr covar
ppr_co _ (IfaceHoleCo covar)    = braces (ppr covar)

ppr_co _ (IfaceUnivCo prov role ty1 ty2 ds)
  = text "Univ" <> (parens $
      sep [ ppr role <+> ppr prov <> ppr ds
          , dcolon <+> ppr ty1 <> text "~#" <+> ppr ty2 ])

ppr_co ctxt_prec (IfaceInstCo co ty)
  = maybeParen ctxt_prec appPrec $
    text "Inst" <+> sep [ pprParendIfaceCoercion co
                        , pprParendIfaceCoercion ty ]

ppr_co ctxt_prec (IfaceAxiomCo ax cos)
  | null cos  = pprIfAxRule ax  -- Don't add parens
  | otherwise = ppr_special_co ctxt_prec (pprIfAxRule ax) cos
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

-- Rendered as for the corresponding IfaceCoercion constructors above
instance Outputable IfaceUnivCoDep where
  ppr (IfaceUnivCoDepCoVar     covar) = ppr covar
  ppr (IfaceUnivCoDepFreeCoVar covar) = ppr covar
  ppr (IfaceUnivCoDepHole      covar) = braces (ppr covar)

pprIfAxRule :: IfaceAxiomRule -> SDoc
pprIfAxRule (IfaceAR_X n)   = ppr n
pprIfAxRule (IfaceAR_U n)   = ppr n
pprIfAxRule (IfaceAR_B n i) = ppr n <> brackets (int i)

ppr_role :: Role -> SDoc
ppr_role r = underscore <> pp_role
  where pp_role = case r of
                    Nominal          -> char 'N'
                    Representational -> char 'R'
                    Phantom          -> char 'P'

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
pprParendIfaceTyCon tc = pprPromotionQuote tc <> pprPrefixName (ifaceTyConName tc)

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
   put_ bh IfacePartialSynonym          = do
     requireIfaceEvidenceVersion bh PartialSynonymEvidence
     putByte bh 4

   get bh = do
       n <- getByte bh
       case n of
         0 -> return IfaceNormalTyCon
         1 -> IfaceTupleTyCon <$> get bh <*> get bh
         2 -> IfaceSumTyCon <$> get bh
         3 -> return IfaceEqualityTyCon
         4 -> return IfacePartialSynonym
         _ -> panic "get IfaceTyConSort"

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
  put_ bh (IfaceIntPrimTyLit n) = do
    requireIfaceEvidenceVersion bh ExperimentalIntPrimIndexEvidence
    putByte bh 4
    put_ bh n
  put_ bh (IfaceWordPrimTyLit n) = do
    requireIfaceEvidenceVersion bh ExperimentalWordPrimIndexEvidence
    putByte bh 5
    put_ bh n

  get bh =
    do tag <- getByte bh
       case tag of
         1 -> do { n <- get bh
                 ; return (IfaceNumTyLit n) }
         2 -> do { n <- get bh
                 ; return (IfaceStrTyLit n) }
         3 -> do { n <- get bh
                 ; return (IfaceCharTyLit n) }
         4 -> IfaceIntPrimTyLit <$> get bh
         5 -> IfaceWordPrimTyLit <$> get bh
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

putIfaceType _ (IfaceFreeTermVar var)
  = pprPanic "Can't serialise IfaceFreeTermVar" (ppr var)
putIfaceType _ (IfaceFreeTermAppTy fun _)
  = pprPanic "Can't serialise IfaceFreeTermAppTy" (ppr fun)
putIfaceType bh (IfaceTermAppTy fun arg)
  = do { putByte bh 12; put_ bh fun; put_ bh arg }
putIfaceType bh (IfaceExistsTy flag bndr body)
  = do { case flag of
           ExplicitExists -> do
             requireIfaceEvidenceVersion bh ErasedExistentialEvidence
             putByte bh 13
           ImplicitExists -> do
             requireIfaceEvidenceVersion bh ImplicitExistentialEvidence
             putByte bh 14
       ; put_ bh bndr; put_ bh body }
putIfaceType bh (IfaceDepFunTy vis bndr body)
  = do { putByte bh 10; put_ bh vis; put_ bh bndr; put_ bh body }
putIfaceType bh (IfaceDepDictTy bndr body)
  = do { requireIfaceEvidenceVersion bh ExperimentalScopedDictionaryEvidence
       ; putByte bh 15; put_ bh bndr; put_ bh body }
putIfaceType bh (IfaceClassAppTy method inst dict@IfaceTermVar{} arg@IfaceTermVar{})
  = do { requireIfaceEvidenceVersion bh ExperimentalScopedDictionaryEvidence
       ; putByte bh 16; put_ bh method; put_ bh inst; put_ bh dict; put_ bh arg }
putIfaceType _ ty@IfaceClassAppTy{}
  = pprPanic "Cannot serialise an unscoped class selector" (ppr ty)
putIfaceType bh (IfaceTermVar var)
  = do { putByte bh 11; put_ bh var }

putIfaceType bh (IfaceForAllTy ma aa ab) = do
        putByte bh 0
        put_ bh ma
        put_ bh aa
        put_ bh ab
putIfaceType bh (IfaceTyVar ad) = do
        putByte bh 1
        put_ bh ad
putIfaceType bh (IfaceAppTy ae af) = do
        putByte bh 2
        put_ bh ae
        put_ bh af
putIfaceType bh (IfaceFunTy af am aw ag ah) = do
        putByte bh 3
        put_ bh af
        put_ bh am
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
              0 -> do ma <- get bh
                      aa <- get bh
                      ab <- get bh
                      return (IfaceForAllTy ma aa ab)
              1 -> do ad <- get bh
                      return (IfaceTyVar ad)
              2 -> do ae <- get bh
                      af <- get bh
                      return (IfaceAppTy ae af)
              3 -> do af <- get bh
                      am <- get bh
                      aw <- get bh
                      ag <- get bh
                      ah <- get bh
                      return (IfaceFunTy af am aw ag ah)
              5 -> do { tc <- get bh; tys <- get bh
                      ; return (IfaceTyConApp tc tys) }
              6 -> do { a <- get bh; b <- get bh
                      ; return (IfaceCastTy a b) }
              7 -> do { a <- get bh
                      ; return (IfaceCoercionTy a) }

              8 -> do { s <- get bh; i <- get bh; tys <- get bh
                      ; return (IfaceTupleTy s i tys) }
              9  -> do n <- get bh
                       return (IfaceLitTy n)
              10 -> do { vis <- get bh; bndr <- get bh; body <- get bh
                       ; return (IfaceDepFunTy vis bndr body) }
              11 -> IfaceTermVar <$> get bh
              12 -> IfaceTermAppTy <$> get bh <*> get bh
              13 -> IfaceExistsTy ExplicitExists <$> get bh <*> get bh
              14 -> IfaceExistsTy ImplicitExists <$> get bh <*> get bh
              15 -> IfaceDepDictTy <$> get bh <*> get bh
              16 -> IfaceClassAppTy <$> get bh <*> get bh <*> get bh <*> get bh
              _  -> ioError (userError ("getIfaceType: invalid type tag " ++ show h))

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

-- | A wire requirement recorded when the writer emits a retained form.
data IfaceEvidence
  = DepFunCoEvidence | ConstructorCaseEvidence | RetainedSpecRuleEvidence
  | IndexAliasEvidence | ErasedExistentialEvidence | ScopedSigmaEvidence
  | RetainedDefaultEvidence | ImplicitExistentialEvidence | ErasedUserRuleEvidence
  | LexicalPackageRuleEvidence | OrphanErasedUserRuleEvidence | AnchoredErasedRuleEvidence
  | RetainedUserRuleEvidence | ImportedRetainedRuleEvidence
  | IndexedConstructorCaseEvidence | IndexedConstructorFieldsEvidence
  | IndexedConstructorOperandEvidence
  | IndexedConstructorExistentialCaseEvidence | GeneralIndexedConstructorOperandEvidence
  | RetainedUserUnfoldingEvidence | LinearUnpackEvidence | RetainedCharOperandEvidence
  | MatchableProjectionEvidence
  | RetainedNaturalOperandEvidence
  | ExperimentalScopedDictionaryEvidence | ExperimentalScopedDictionaryCoreEvidence
  | RetainedDataFamilyConOperandEvidence
  | TerminalGadtForAllEvidence
  | TerminalGadtContextEvidence
  | TerminalGadtTrailingContextEvidence
  | TerminalGadtContextOnlyEvidence
  | InteriorGadtContextEvidence
  | InteriorGadtForAllEvidence
  | RepeatedInteriorGadtForAllEvidence
  | OrderedGadtTelescopeEvidence
  | InferredOrderedGadtBindersEvidence
  | ExtendedOrderedGadtTelescopeEvidence
  | ExperimentalIntPrimIndexEvidence
  | ScopedLambdaEvidence
  | RetainedConstructorContextEvidence
  | HomeRuleInterfaceEvidence
  | PartialSynonymEvidence
  | SourceProofRuleEvidence
  | SourceProofRuleAliasEvidence
  | SourceProofRuleAxiomEvidence
  | SourceProofRuleInfoEvidence
  | SourceProofRuleArgumentsEvidence
  | PolymorphicSourceMethodEvidence
  | SourceProofRuleBranchesEvidence
  | ExperimentalWordPrimIndexEvidence
  deriving (Bounded, Enum, Eq, Ord)

-- | Requirements of an interface payload. See Note [Versioning retained evidence]
-- in GHC.Iface.Binary.
type IfaceEvidenceVersion = Set.Set IfaceEvidence

-- | Record a feature only while writing the full interface.
requireIfaceEvidenceVersion :: WriteBinHandle -> IfaceEvidence -> IO ()
requireIfaceEvidenceVersion bh required =
  case findOptionalUserDataWriter (Proxy :: Proxy IfaceEvidence) bh of
    Just notify -> putEntry notify bh required
    Nothing -> pure () -- Fingerprinting does not write a version header.

instance Binary IfaceCoercion where
  put_ _ (IfaceTermAppCo {})
       = ioError (userError "Opaque runtime call congruence interfaces are not implemented.")
  put_ bh (IfaceDepFunCo visL visR bndr domain body) = do
       requireIfaceEvidenceVersion bh DepFunCoEvidence
       putByte bh 18
       put_ bh visL
       put_ bh visR
       put_ bh bndr
       put_ bh domain
       put_ bh body
  put_ bh (IfaceReflCo a) = do
          putByte bh 1
          put_ bh a
  put_ bh (IfaceGReflCo a b c) = do
          putByte bh 2
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceFunCo a m w b c) = do
          putByte bh 3
          put_ bh a
          put_ bh m
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
  put_ bh (IfaceForAllCo a visL visR ma b c) = do
          putByte bh 6
          put_ bh a
          put_ bh visL
          put_ bh visR
          put_ bh ma
          put_ bh b
          put_ bh c
  put_ bh (IfaceCoVarCo a) = do
          putByte bh 7
          put_ bh a
  put_ bh (IfaceUnivCo a b c d deps) = do
          putByte bh 9
          put_ bh a
          put_ bh b
          put_ bh c
          put_ bh d
          put_ bh deps
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
  put_ bh (IfaceAxiomCo a b) = do
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
                   m  <- get bh
                   w  <- get bh
                   b  <- get bh
                   c  <- get bh
                   return $ IfaceFunCo a m w b c
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
                   ma <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceForAllCo a visL visR ma b c
           7 -> do a <- get bh
                   return $ IfaceCoVarCo a
           9 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   d <- get bh
                   deps <- get bh
                   return $ IfaceUnivCo a b c d deps
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
                   return $ IfaceAxiomCo a b
           18 -> IfaceDepFunCo <$> get bh <*> get bh <*> get bh <*> get bh <*> get bh
           _ -> panic ("get IfaceCoercion " ++ show tag)

-- | Only 'IfaceUnivCoDepCoVar' can be serialised, so no tag is written.
instance Binary IfaceUnivCoDep where
  put_ bh (IfaceUnivCoDepCoVar n) = put_ bh n
  put_ _ (IfaceUnivCoDepFreeCoVar cv)
       = pprPanic "Can't serialise IfaceUnivCoDepFreeCoVar" (ppr cv)
           -- See Note [Free TyVars and CoVars in IfaceType]
  put_ _ (IfaceUnivCoDepHole cv)
       = pprPanic "Can't serialise IfaceUnivCoDepHole" (ppr cv)
           -- See Note [Holes in IfaceCoercion]

  get bh = IfaceUnivCoDepCoVar <$> get bh

instance Binary IfaceAxiomRule where
  put_ bh (IfaceAR_X n) = do
    let uses flag = ifLclNameFS n `elem`
          [existsCoRuleName flag Nominal, existsCoRuleName flag Representational]
    when (uses ExplicitExists) $
      requireIfaceEvidenceVersion bh ErasedExistentialEvidence
    when (uses ImplicitExists) $
      requireIfaceEvidenceVersion bh ImplicitExistentialEvidence
    when (isJust (matchableProjectionDirection (ifLclNameFS n))) $
      requireIfaceEvidenceVersion bh MatchableProjectionEvidence
    when (ifLclNameFS n == scopedSigmaClosureName) $
      requireIfaceEvidenceVersion bh ScopedSigmaEvidence
    when (ifLclNameFS n == scopedLambdaClosureName) $
      requireIfaceEvidenceVersion bh ScopedLambdaEvidence
    putByte bh 0
    put_ bh n
  put_ bh (IfaceAR_U n)   = putByte bh 1 >> put_ bh n
  put_ bh (IfaceAR_B n i) = putByte bh 2 >> put_ bh n >> put_ bh i

  get bh = do h <- getByte bh
              case h of
                0 -> do { n <- get bh; return (IfaceAR_X n) }
                1 -> do { n <- get bh; return (IfaceAR_U n) }
                _ -> do { n <- get bh; i <- get bh; return (IfaceAR_B n i) }

instance Binary (DefMethSpec IfaceType) where
    put_ bh VanillaDM     = putByte bh 0
    put_ bh (GenericDM t) = putByte bh 1 >> put_ bh t
    get bh = do
            h <- getByte bh
            case h of
              0 -> return VanillaDM
              _ -> do { t <- get bh; return (GenericDM t) }

instance NFData (DefMethSpec IfaceType) where
  rnf = \case
    VanillaDM -> ()
    GenericDM t -> rnf t

instance NFData IfaceType where
  rnf = \case
    IfaceFreeTyVar f1 -> f1 `seq` ()
    IfaceExistsTy _ bndr res -> rnf bndr `seq` rnf res
    IfaceDepFunTy vis bndr res -> vis `seq` rnf bndr `seq` rnf res
    IfaceDepDictTy bndr res -> rnf bndr `seq` rnf res
    IfaceClassAppTy method inst dict arg -> rnf method `seq` rnf inst `seq` rnf dict `seq` rnf arg
    IfaceTermVar var -> rnf var
    IfaceFreeTermVar var -> var `seq` ()
    IfaceTermAppTy fun arg -> rnf fun `seq` rnf arg
    IfaceFreeTermAppTy fun arg -> fun `seq` rnf arg
    IfaceTyVar f1 -> rnf f1
    IfaceLitTy f1 -> rnf f1
    IfaceAppTy f1 f2 -> rnf f1 `seq` rnf f2
    IfaceFunTy f1 f2 f3 f4 f5 -> rnf f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4 `seq` rnf f5
    IfaceForAllTy f1 f2 f3 -> rnf f1 `seq` rnf f2 `seq` rnf f3
    IfaceTyConApp f1 f2 -> rnf f1 `seq` rnf f2
    IfaceCastTy f1 f2 -> rnf f1 `seq` rnf f2
    IfaceCoercionTy f1 -> rnf f1
    IfaceTupleTy f1 f2 f3 -> rnf f1 `seq` rnf f2 `seq` rnf f3

instance NFData IfaceTyLit where
  rnf = \case
    IfaceNumTyLit f1 -> rnf f1
    IfaceIntPrimTyLit f1 -> rnf f1
    IfaceWordPrimTyLit f1 -> rnf f1
    IfaceStrTyLit f1 -> rnf f1
    IfaceCharTyLit f1 -> rnf f1

instance NFData IfaceCoercion where
  rnf = \case
    IfaceDepFunCo visL visR bndr domain co -> visL `seq` visR `seq` rnf bndr `seq` rnf domain `seq` rnf co
    IfaceTermAppCo fun co -> rnf fun `seq` rnf co
    IfaceReflCo f1 -> rnf f1
    IfaceGReflCo f1 f2 f3 -> rnf f1 `seq` rnf f2 `seq` rnf f3
    IfaceFunCo f1 f2 f3 f4 f5 -> rnf f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4 `seq` rnf f5
    IfaceTyConAppCo f1 f2 f3 -> rnf f1 `seq` rnf f2 `seq` rnf f3
    IfaceAppCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceForAllCo f1 f2 f3 f4 f5 f6 -> rnf f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4 `seq` rnf f5 `seq` rnf f6
    IfaceCoVarCo f1 -> rnf f1
    IfaceAxiomCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceUnivCo f1 f2 f3 f4 deps -> rnf f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4 `seq` rnf deps
    IfaceSymCo f1 -> rnf f1
    IfaceTransCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceSelCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceLRCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceInstCo f1 f2 -> rnf f1 `seq` rnf f2
    IfaceKindCo f1 -> rnf f1
    IfaceSubCo f1 -> rnf f1
    -- These are not deeply forced because they are not used in ModIface,
    -- these constructors are for pretty-printing.
    -- See Note [Free TyVars and CoVars in IfaceType]
    -- See Note [Holes in IfaceCoercion]
    IfaceFreeCoVar f1 -> f1 `seq` ()
    IfaceHoleCo f1 -> f1 `seq` ()

instance NFData IfaceUnivCoDep where
  rnf = \case
    IfaceUnivCoDepCoVar f1 -> rnf f1
    -- As for IfaceFreeCoVar and IfaceHoleCo above, these are only used for
    -- pretty-printing, so they are not deeply forced.
    IfaceUnivCoDepFreeCoVar f1 -> f1 `seq` ()
    IfaceUnivCoDepHole f1 -> f1 `seq` ()

instance NFData IfaceAxiomRule where
  rnf = \case
    IfaceAR_X n   -> rnf n
    IfaceAR_U n   -> rnf n
    IfaceAR_B n i -> rnf n `seq` rnf i

instance NFData IfaceMCoercion where
  rnf IfaceMRefl = ()
  rnf (IfaceMCo c) = rnf c

instance NFData IfaceOneShot where
  rnf IfaceOneShot = ()
  rnf IfaceNoOneShot = ()

instance NFData IfaceTyConSort where
  rnf = \case
    IfaceNormalTyCon -> ()
    IfaceTupleTyCon arity sort -> rnf arity `seq` rnf sort `seq` ()
    IfaceSumTyCon arity -> rnf arity
    IfaceEqualityTyCon -> ()
    IfacePartialSynonym -> ()

instance NFData IfLclName where
  rnf (IfLclName lfs) = rnf lfs

instance NFData IfaceTyConInfo where
  rnf (IfaceTyConInfo f s) = rnf f `seq` rnf s

instance NFData IfaceTyCon where
  rnf (IfaceTyCon nm info) = rnf nm `seq` rnf info

instance NFData IfaceBndr where
  rnf = \case
    IfaceIdBndr id_bndr -> rnf id_bndr
    IfaceTvBndr tv_bndr -> rnf tv_bndr

instance NFData IfaceAppArgs where
  rnf = \case
    IA_Nil -> ()
    IA_Arg f1 f2 f3 -> rnf f1 `seq` rnf f2 `seq` rnf f3
