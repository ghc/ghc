{-
(c) The AQUA Project, Glasgow University, 1994-1998


Wired-in knowledge about primitive types
-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | This module defines TyCons that can't be expressed in Haskell.
--   They are all, therefore, wired-in TyCons.  C.f module "GHC.Builtin.Types"
module GHC.Builtin.Types.Prim(
        mkTemplateKindVar, mkTemplateKindVars,
        mkTemplateTyVars, mkTemplateTyVarsFrom,
        mkTemplateKiTyVars, mkTemplateKiTyVar,

        mkTemplateTyConBinders, mkTemplateKindTyConBinders,
        mkTemplateAnonTyConBinders,

        alphaTyVars, alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar,
        alphaTyVarSpec, betaTyVarSpec, gammaTyVarSpec, deltaTyVarSpec,
        alphaTys, alphaTy, betaTy, gammaTy, deltaTy,
        alphaTyVarsUnliftedRep, alphaTyVarUnliftedRep,
        alphaTysUnliftedRep, alphaTyUnliftedRep,
        runtimeRep1TyVar, runtimeRep2TyVar, runtimeRep3TyVar,
        runtimeRep1TyVarInf, runtimeRep2TyVarInf,
        runtimeRep1Ty, runtimeRep2Ty, runtimeRep3Ty,
        levity1TyVar, levity2TyVar,
        levity1TyVarInf, levity2TyVarInf,
        levity1Ty, levity2Ty,

        alphaConstraintTyVar, alphaConstraintTy,

        openAlphaTyVar, openBetaTyVar, openGammaTyVar,
        openAlphaTyVarSpec, openBetaTyVarSpec, openGammaTyVarSpec,
        openAlphaTy, openBetaTy, openGammaTy,

        levPolyAlphaTyVar, levPolyBetaTyVar,
        levPolyAlphaTyVarSpec, levPolyBetaTyVarSpec,
        levPolyAlphaTy, levPolyBetaTy,

        multiplicityTyVar1, multiplicityTyVar2,

        -- Kind constructors...
        tYPETyCon, tYPETyConName, tYPEKind,
        cONSTRAINTTyCon, cONSTRAINTTyConName, cONSTRAINTKind,

        -- Arrows
        funTyFlagTyCon, isArrowTyCon,
        fUNTyCon,       fUNTyConName,
        ctArrowTyCon, ctArrowTyConName,
        ccArrowTyCon, ccArrowTyConName,
        tcArrowTyCon, tcArrowTyConName,

        unexposedPrimTyCons, exposedPrimTyCons, primTyCons,

        charPrimTyCon,          charPrimTy, charPrimTyConName,
        intPrimTyCon,           intPrimTy, intPrimTyConName,
        wordPrimTyCon,          wordPrimTy, wordPrimTyConName,
        addrPrimTyCon,          addrPrimTy, addrPrimTyConName,
        floatPrimTyCon,         floatPrimTy, floatPrimTyConName,
        doublePrimTyCon,        doublePrimTy, doublePrimTyConName,

        statePrimTyCon,         mkStatePrimTy,
        realWorldTyCon,         realWorldTy,
        realWorldStatePrimTy,   realWorldMutableByteArrayPrimTy,

        proxyPrimTyCon,         mkProxyPrimTy,

        arrayPrimTyCon, mkArrayPrimTy,
        byteArrayPrimTyCon,     byteArrayPrimTy,
        smallArrayPrimTyCon, mkSmallArrayPrimTy,
        mutableArrayPrimTyCon, mkMutableArrayPrimTy,
        mutableByteArrayPrimTyCon, mkMutableByteArrayPrimTy,
        smallMutableArrayPrimTyCon, mkSmallMutableArrayPrimTy,
        mutVarPrimTyCon, mkMutVarPrimTy,

        mVarPrimTyCon,                  mkMVarPrimTy,
        ioPortPrimTyCon,                mkIOPortPrimTy,
        tVarPrimTyCon,                  mkTVarPrimTy,
        stablePtrPrimTyCon,             mkStablePtrPrimTy,
        stableNamePrimTyCon,            mkStableNamePrimTy,
        compactPrimTyCon,               compactPrimTy,
        bcoPrimTyCon,                   bcoPrimTy,
        weakPrimTyCon,                  mkWeakPrimTy,
        threadIdPrimTyCon,              threadIdPrimTy,
        stackSnapshotPrimTyCon,         stackSnapshotPrimTy,
        promptTagPrimTyCon,             mkPromptTagPrimTy,

        int8PrimTyCon,          int8PrimTy, int8PrimTyConName,
        word8PrimTyCon,         word8PrimTy, word8PrimTyConName,

        int16PrimTyCon,         int16PrimTy, int16PrimTyConName,
        word16PrimTyCon,        word16PrimTy, word16PrimTyConName,

        int32PrimTyCon,         int32PrimTy, int32PrimTyConName,
        word32PrimTyCon,        word32PrimTy, word32PrimTyConName,

        int64PrimTyCon,         int64PrimTy, int64PrimTyConName,
        word64PrimTyCon,        word64PrimTy, word64PrimTyConName,

        eqPrimTyCon,            -- ty1 ~# ty2
        eqReprPrimTyCon,        -- ty1 ~R# ty2  (at role Representational)
        eqPhantPrimTyCon,       -- ty1 ~P# ty2  (at role Phantom)
        equalityTyCon,

        -- * SIMD
#include "primop-vector-tys-exports.hs-incl"
  ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Builtin.Types
  ( runtimeRepTy, levityTy, unboxedTupleKind, liftedTypeKind, unliftedTypeKind
  , boxedRepDataConTyCon, vecRepDataConTyCon
  , liftedRepTy, unliftedRepTy, zeroBitRepTy
  , intRepDataConTy
  , int8RepDataConTy, int16RepDataConTy, int32RepDataConTy, int64RepDataConTy
  , wordRepDataConTy
  , word16RepDataConTy, word8RepDataConTy, word32RepDataConTy, word64RepDataConTy
  , addrRepDataConTy
  , floatRepDataConTy, doubleRepDataConTy
  , vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy
  , vec64DataConTy
  , int8ElemRepDataConTy, int16ElemRepDataConTy, int32ElemRepDataConTy
  , int64ElemRepDataConTy, word8ElemRepDataConTy, word16ElemRepDataConTy
  , word32ElemRepDataConTy, word64ElemRepDataConTy, floatElemRepDataConTy
  , doubleElemRepDataConTy
  , multiplicityTy
  , constraintKind )

import {-# SOURCE #-} GHC.Types.TyThing( mkATyCon )
import {-# SOURCE #-} GHC.Core.Type ( mkTyConApp, getLevity )

import GHC.Core.TyCon
import GHC.Core.TyCo.Rep -- Doesn't need special access, but this is easier to avoid
                         -- import loops which show up if you import Type instead

import GHC.Types.Var    ( TyVarBinder, TyVar,binderVar, binderVars
                        , mkTyVar, mkTyVarBinder, mkTyVarBinders )
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Types.Unique

import GHC.Builtin.Uniques
import GHC.Builtin.Names
import GHC.Utils.Misc ( changeLast )
import GHC.Utils.Panic ( assertPpr )
import GHC.Utils.Outputable

import GHC.Data.FastString
import Data.Char

{- *********************************************************************
*                                                                      *
             Building blocks
*                                                                      *
********************************************************************* -}

mk_TYPE_app :: Type -> Type
mk_TYPE_app rep = mkTyConApp tYPETyCon [rep]

mk_CONSTRAINT_app :: Type -> Type
mk_CONSTRAINT_app rep = mkTyConApp cONSTRAINTTyCon [rep]

mkPrimTc :: FastString -> Unique -> TyCon -> Name
mkPrimTc = mkGenPrimTc UserSyntax

mkBuiltInPrimTc :: FastString -> Unique -> TyCon -> Name
mkBuiltInPrimTc = mkGenPrimTc BuiltInSyntax

mkGenPrimTc :: BuiltInSyntax -> FastString -> Unique -> TyCon -> Name
mkGenPrimTc built_in_syntax occ key tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS occ)
                  key
                  (mkATyCon tycon)
                  built_in_syntax

-- | Create a primitive 'TyCon' with the given 'Name',
-- arguments of kind 'Type` with the given 'Role's,
-- and the given result kind representation.
--
-- Only use this in "GHC.Builtin.Types.Prim".
pcPrimTyCon :: Name
            -> [Role] -> RuntimeRepType -> TyCon
pcPrimTyCon name roles res_rep
  = mkPrimTyCon name binders result_kind roles
  where
    bndr_kis    = liftedTypeKind <$ roles
    binders     = mkTemplateAnonTyConBinders bndr_kis
    result_kind = mk_TYPE_app res_rep

-- | Create a primitive nullary 'TyCon' with the given 'Name'
-- and result kind representation.
--
-- Only use this in "GHC.Builtin.Types.Prim".
pcPrimTyCon0 :: Name -> RuntimeRepType -> TyCon
pcPrimTyCon0 name res_rep
  = pcPrimTyCon name [] res_rep

-- | Create a primitive 'TyCon' like 'pcPrimTyCon', except the last
-- argument is levity-polymorphic, where the levity argument is
-- implicit and comes before other arguments
--
-- Only use this in "GHC.Builtin.Types.Prim".
pcPrimTyCon_LevPolyLastArg :: Name
                           -> [Role] -- ^ roles of the arguments (must be non-empty),
                                     -- not including the implicit argument of kind 'Levity',
                                     -- which always has 'Nominal' role
                           -> RuntimeRepType  -- ^ representation of the fully-applied type
                           -> TyCon
pcPrimTyCon_LevPolyLastArg name roles res_rep
  = mkPrimTyCon name binders result_kind (Nominal : roles)
    where
      result_kind = mk_TYPE_app res_rep
      lev_bndr = mkNamedTyConBinder Inferred levity1TyVar
      binders  = lev_bndr : mkTemplateAnonTyConBinders anon_bndr_kis
      lev_tv   = mkTyVarTy (binderVar lev_bndr)

      -- [ Type, ..., Type, TYPE (BoxedRep l) ]
      anon_bndr_kis = changeLast (liftedTypeKind <$ roles) $
                      mk_TYPE_app $
                      mkTyConApp boxedRepDataConTyCon [lev_tv]


{- *********************************************************************
*                                                                      *
           Primitive type constructors
*                                                                      *
********************************************************************* -}

{- Note Note [Unexposed TyCons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A few primitive TyCons are "unexposed", meaning:
* We don't want users to be able to write them (see #15209);
  i.e. they aren't in scope, ever.  In particular they do not
  appear in the exports of GHC.Prim: see GHC.Builtin.Utils.ghcPrimExports

* We don't want users to see them in GHCi's @:browse@ output (see #12023).
-}

primTyCons :: [TyCon]
primTyCons = unexposedPrimTyCons ++ exposedPrimTyCons

-- | Primitive 'TyCon's that are defined in GHC.Prim but not "exposed".
-- See Note [Unexposed TyCons]
unexposedPrimTyCons :: [TyCon]
unexposedPrimTyCons
  = [ eqPrimTyCon      -- (~#)
    , eqReprPrimTyCon  -- (~R#)
    , eqPhantPrimTyCon -- (~P#)

    -- These arrows are un-exposed for now
    , ctArrowTyCon  -- (=>)
    , ccArrowTyCon  -- (==>)
    , tcArrowTyCon  -- (-=>)
    ]

-- | Primitive 'TyCon's that are defined in, and exported from, GHC.Prim.
exposedPrimTyCons :: [TyCon]
exposedPrimTyCons
  = [ addrPrimTyCon
    , arrayPrimTyCon
    , byteArrayPrimTyCon
    , smallArrayPrimTyCon
    , charPrimTyCon
    , doublePrimTyCon
    , floatPrimTyCon
    , intPrimTyCon
    , int8PrimTyCon
    , int16PrimTyCon
    , int32PrimTyCon
    , int64PrimTyCon
    , bcoPrimTyCon
    , weakPrimTyCon
    , mutableArrayPrimTyCon
    , mutableByteArrayPrimTyCon
    , smallMutableArrayPrimTyCon
    , mVarPrimTyCon
    , ioPortPrimTyCon
    , tVarPrimTyCon
    , mutVarPrimTyCon
    , realWorldTyCon
    , stablePtrPrimTyCon
    , stableNamePrimTyCon
    , compactPrimTyCon
    , statePrimTyCon
    , proxyPrimTyCon
    , threadIdPrimTyCon
    , wordPrimTyCon
    , word8PrimTyCon
    , word16PrimTyCon
    , word32PrimTyCon
    , word64PrimTyCon
    , stackSnapshotPrimTyCon
    , promptTagPrimTyCon

    , fUNTyCon
    , tYPETyCon
    , cONSTRAINTTyCon

#include "primop-vector-tycons.hs-incl"
    ]

charPrimTyConName, intPrimTyConName, int8PrimTyConName, int16PrimTyConName, int32PrimTyConName, int64PrimTyConName,
  wordPrimTyConName, word32PrimTyConName, word8PrimTyConName, word16PrimTyConName, word64PrimTyConName,
  addrPrimTyConName, floatPrimTyConName, doublePrimTyConName,
  statePrimTyConName, proxyPrimTyConName, realWorldTyConName,
  arrayPrimTyConName, smallArrayPrimTyConName, byteArrayPrimTyConName,
  mutableArrayPrimTyConName, mutableByteArrayPrimTyConName,
  smallMutableArrayPrimTyConName, mutVarPrimTyConName, mVarPrimTyConName,
  ioPortPrimTyConName, tVarPrimTyConName, stablePtrPrimTyConName,
  stableNamePrimTyConName, compactPrimTyConName, bcoPrimTyConName,
  weakPrimTyConName, threadIdPrimTyConName,
  eqPrimTyConName, eqReprPrimTyConName, eqPhantPrimTyConName,
  stackSnapshotPrimTyConName, promptTagPrimTyConName :: Name
charPrimTyConName             = mkPrimTc (fsLit "Char#") charPrimTyConKey charPrimTyCon
intPrimTyConName              = mkPrimTc (fsLit "Int#") intPrimTyConKey  intPrimTyCon
int8PrimTyConName             = mkPrimTc (fsLit "Int8#") int8PrimTyConKey int8PrimTyCon
int16PrimTyConName            = mkPrimTc (fsLit "Int16#") int16PrimTyConKey int16PrimTyCon
int32PrimTyConName            = mkPrimTc (fsLit "Int32#") int32PrimTyConKey int32PrimTyCon
int64PrimTyConName            = mkPrimTc (fsLit "Int64#") int64PrimTyConKey int64PrimTyCon
wordPrimTyConName             = mkPrimTc (fsLit "Word#") wordPrimTyConKey wordPrimTyCon
word8PrimTyConName            = mkPrimTc (fsLit "Word8#") word8PrimTyConKey word8PrimTyCon
word16PrimTyConName           = mkPrimTc (fsLit "Word16#") word16PrimTyConKey word16PrimTyCon
word32PrimTyConName           = mkPrimTc (fsLit "Word32#") word32PrimTyConKey word32PrimTyCon
word64PrimTyConName           = mkPrimTc (fsLit "Word64#") word64PrimTyConKey word64PrimTyCon
addrPrimTyConName             = mkPrimTc (fsLit "Addr#") addrPrimTyConKey addrPrimTyCon
floatPrimTyConName            = mkPrimTc (fsLit "Float#") floatPrimTyConKey floatPrimTyCon
doublePrimTyConName           = mkPrimTc (fsLit "Double#") doublePrimTyConKey doublePrimTyCon
statePrimTyConName            = mkPrimTc (fsLit "State#") statePrimTyConKey statePrimTyCon
proxyPrimTyConName            = mkPrimTc (fsLit "Proxy#") proxyPrimTyConKey proxyPrimTyCon
eqPrimTyConName               = mkPrimTc (fsLit "~#") eqPrimTyConKey eqPrimTyCon
eqReprPrimTyConName           = mkBuiltInPrimTc (fsLit "~R#") eqReprPrimTyConKey eqReprPrimTyCon
eqPhantPrimTyConName          = mkBuiltInPrimTc (fsLit "~P#") eqPhantPrimTyConKey eqPhantPrimTyCon
realWorldTyConName            = mkPrimTc (fsLit "RealWorld") realWorldTyConKey realWorldTyCon
arrayPrimTyConName            = mkPrimTc (fsLit "Array#") arrayPrimTyConKey arrayPrimTyCon
byteArrayPrimTyConName        = mkPrimTc (fsLit "ByteArray#") byteArrayPrimTyConKey byteArrayPrimTyCon
smallArrayPrimTyConName       = mkPrimTc (fsLit "SmallArray#") smallArrayPrimTyConKey smallArrayPrimTyCon
mutableArrayPrimTyConName     = mkPrimTc (fsLit "MutableArray#") mutableArrayPrimTyConKey mutableArrayPrimTyCon
mutableByteArrayPrimTyConName = mkPrimTc (fsLit "MutableByteArray#") mutableByteArrayPrimTyConKey mutableByteArrayPrimTyCon
smallMutableArrayPrimTyConName= mkPrimTc (fsLit "SmallMutableArray#") smallMutableArrayPrimTyConKey smallMutableArrayPrimTyCon
mutVarPrimTyConName           = mkPrimTc (fsLit "MutVar#") mutVarPrimTyConKey mutVarPrimTyCon
ioPortPrimTyConName           = mkPrimTc (fsLit "IOPort#") ioPortPrimTyConKey ioPortPrimTyCon
mVarPrimTyConName             = mkPrimTc (fsLit "MVar#") mVarPrimTyConKey mVarPrimTyCon
tVarPrimTyConName             = mkPrimTc (fsLit "TVar#") tVarPrimTyConKey tVarPrimTyCon
stablePtrPrimTyConName        = mkPrimTc (fsLit "StablePtr#") stablePtrPrimTyConKey stablePtrPrimTyCon
stableNamePrimTyConName       = mkPrimTc (fsLit "StableName#") stableNamePrimTyConKey stableNamePrimTyCon
compactPrimTyConName          = mkPrimTc (fsLit "Compact#") compactPrimTyConKey compactPrimTyCon
stackSnapshotPrimTyConName    = mkPrimTc (fsLit "StackSnapshot#") stackSnapshotPrimTyConKey stackSnapshotPrimTyCon
bcoPrimTyConName              = mkPrimTc (fsLit "BCO") bcoPrimTyConKey bcoPrimTyCon
weakPrimTyConName             = mkPrimTc (fsLit "Weak#") weakPrimTyConKey weakPrimTyCon
threadIdPrimTyConName         = mkPrimTc (fsLit "ThreadId#") threadIdPrimTyConKey threadIdPrimTyCon
promptTagPrimTyConName        = mkPrimTc (fsLit "PromptTag#") promptTagPrimTyConKey promptTagPrimTyCon

{- *********************************************************************
*                                                                      *
                Type variables
*                                                                      *
********************************************************************* -}

{-
alphaTyVars is a list of type variables for use in templates:
        ["a", "b", ..., "z", "t1", "t2", ... ]
-}

mkTemplateKindVar :: Kind -> TyVar
mkTemplateKindVar = mkTyVar (mk_tv_name 0 "k")

mkTemplateKindVars :: [Kind] -> [TyVar]
-- k0  with unique (mkAlphaTyVarUnique 0)
-- k1  with unique (mkAlphaTyVarUnique 1)
-- ... etc
mkTemplateKindVars [kind] = [mkTemplateKindVar kind]
  -- Special case for one kind: just "k"
mkTemplateKindVars kinds
  = [ mkTyVar (mk_tv_name u ('k' : show u)) kind
    | (kind, u) <- kinds `zip` [0..] ]
mk_tv_name :: Int -> String -> Name
mk_tv_name u s = mkInternalName (mkAlphaTyVarUnique u)
                                (mkTyVarOccFS (mkFastString s))
                                noSrcSpan

mkTemplateTyVarsFrom :: Int -> [Kind] -> [TyVar]
-- a  with unique (mkAlphaTyVarUnique n)
-- b  with unique (mkAlphaTyVarUnique n+1)
-- ... etc
-- Typically called as
--   mkTemplateTyVarsFrom (length kv_bndrs) kinds
-- where kv_bndrs are the kind-level binders of a TyCon
mkTemplateTyVarsFrom n kinds
  = [ mkTyVar name kind
    | (kind, index) <- zip kinds [0..],
      let ch_ord = index + ord 'a'
          name_str | ch_ord <= ord 'z' = [chr ch_ord]
                   | otherwise         = 't':show index
          name = mk_tv_name (index + n) name_str
    ]

mkTemplateTyVars :: [Kind] -> [TyVar]
mkTemplateTyVars = mkTemplateTyVarsFrom 1

mkTemplateTyConBinders
    :: [Kind]                -- [k1, .., kn]   Kinds of kind-forall'd vars
    -> ([Kind] -> [Kind])    -- Arg is [kv1:k1, ..., kvn:kn]
                             --     same length as first arg
                             -- Result is anon arg kinds
    -> [TyConBinder]
mkTemplateTyConBinders kind_var_kinds mk_anon_arg_kinds
  = kv_bndrs ++ tv_bndrs
  where
    kv_bndrs   = mkTemplateKindTyConBinders kind_var_kinds
    anon_kinds = mk_anon_arg_kinds (mkTyVarTys (binderVars kv_bndrs))
    tv_bndrs   = mkTemplateAnonTyConBindersFrom (length kv_bndrs) anon_kinds

mkTemplateKiTyVars
    :: [Kind]                -- [k1, .., kn]   Kinds of kind-forall'd vars
    -> ([Kind] -> [Kind])    -- Arg is [kv1:k1, ..., kvn:kn]
                             --     same length as first arg
                             -- Result is anon arg kinds [ak1, .., akm]
    -> [TyVar]   -- [kv1:k1, ..., kvn:kn, av1:ak1, ..., avm:akm]
-- Example: if you want the tyvars for
--   forall (r::RuntimeRep) (a::TYPE r) (b::Type). blah
-- call mkTemplateKiTyVars [RuntimeRep] (\[r] -> [TYPE r, Type])
mkTemplateKiTyVars kind_var_kinds mk_arg_kinds
  = kv_bndrs ++ tv_bndrs
  where
    kv_bndrs   = mkTemplateKindVars kind_var_kinds
    anon_kinds = mk_arg_kinds (mkTyVarTys kv_bndrs)
    tv_bndrs   = mkTemplateTyVarsFrom (length kv_bndrs) anon_kinds

mkTemplateKiTyVar
    :: Kind                  -- [k1, .., kn]   Kind of kind-forall'd var
    -> (Kind -> [Kind])      -- Arg is kv1:k1
                             -- Result is anon arg kinds [ak1, .., akm]
    -> [TyVar]   -- [kv1:k1, ..., kvn:kn, av1:ak1, ..., avm:akm]
-- Example: if you want the tyvars for
--   forall (r::RuntimeRep) (a::TYPE r) (b::Type). blah
-- call mkTemplateKiTyVar RuntimeRep (\r -> [TYPE r, Type])
mkTemplateKiTyVar kind mk_arg_kinds
  = kv_bndr : tv_bndrs
  where
    kv_bndr    = mkTemplateKindVar kind
    anon_kinds = mk_arg_kinds (mkTyVarTy kv_bndr)
    tv_bndrs   = mkTemplateTyVarsFrom 1 anon_kinds

mkTemplateKindTyConBinders :: [Kind] -> [TyConBinder]
-- Makes named, Specified binders
mkTemplateKindTyConBinders kinds
  = [mkNamedTyConBinder Specified tv | tv <- mkTemplateKindVars kinds]

mkTemplateAnonTyConBinders :: [Kind] -> [TyConBinder]
mkTemplateAnonTyConBinders kinds
  = mkAnonTyConBinders (mkTemplateTyVars kinds)

mkTemplateAnonTyConBindersFrom :: Int -> [Kind] -> [TyConBinder]
mkTemplateAnonTyConBindersFrom n kinds
  = mkAnonTyConBinders (mkTemplateTyVarsFrom n kinds)

alphaTyVars :: [TyVar]
alphaTyVars = mkTemplateTyVars $ repeat liftedTypeKind

alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar :: TyVar
(alphaTyVar:betaTyVar:gammaTyVar:deltaTyVar:_) = alphaTyVars

alphaTyVarSpec, betaTyVarSpec, gammaTyVarSpec, deltaTyVarSpec :: TyVarBinder
(alphaTyVarSpec:betaTyVarSpec:gammaTyVarSpec:deltaTyVarSpec:_) = mkTyVarBinders Specified alphaTyVars

alphaConstraintTyVars :: [TyVar]
alphaConstraintTyVars = mkTemplateTyVars $ repeat constraintKind

alphaConstraintTyVar :: TyVar
(alphaConstraintTyVar:_) = alphaConstraintTyVars

alphaConstraintTy :: Type
alphaConstraintTy = mkTyVarTy alphaConstraintTyVar

alphaTys :: [Type]
alphaTys = mkTyVarTys alphaTyVars
alphaTy, betaTy, gammaTy, deltaTy :: Type
(alphaTy:betaTy:gammaTy:deltaTy:_) = alphaTys

alphaTyVarsUnliftedRep :: [TyVar]
alphaTyVarsUnliftedRep = mkTemplateTyVars $ repeat unliftedTypeKind

alphaTyVarUnliftedRep :: TyVar
(alphaTyVarUnliftedRep:_) = alphaTyVarsUnliftedRep

alphaTysUnliftedRep :: [Type]
alphaTysUnliftedRep = mkTyVarTys alphaTyVarsUnliftedRep
alphaTyUnliftedRep :: Type
(alphaTyUnliftedRep:_) = alphaTysUnliftedRep

runtimeRep1TyVar, runtimeRep2TyVar, runtimeRep3TyVar :: TyVar
(runtimeRep1TyVar : runtimeRep2TyVar : runtimeRep3TyVar : _)
  = drop 16 (mkTemplateTyVars (repeat runtimeRepTy))  -- selects 'q','r'

runtimeRep1TyVarInf, runtimeRep2TyVarInf :: TyVarBinder
runtimeRep1TyVarInf = mkTyVarBinder Inferred runtimeRep1TyVar
runtimeRep2TyVarInf = mkTyVarBinder Inferred runtimeRep2TyVar

runtimeRep1Ty, runtimeRep2Ty, runtimeRep3Ty :: RuntimeRepType
runtimeRep1Ty = mkTyVarTy runtimeRep1TyVar
runtimeRep2Ty = mkTyVarTy runtimeRep2TyVar
runtimeRep3Ty = mkTyVarTy runtimeRep3TyVar
openAlphaTyVar, openBetaTyVar, openGammaTyVar :: TyVar
-- alpha :: TYPE r1
-- beta  :: TYPE r2
-- gamma :: TYPE r3
[openAlphaTyVar,openBetaTyVar,openGammaTyVar]
  = mkTemplateTyVars [ mk_TYPE_app runtimeRep1Ty
                     , mk_TYPE_app runtimeRep2Ty
                     , mk_TYPE_app runtimeRep3Ty]

openAlphaTyVarSpec, openBetaTyVarSpec, openGammaTyVarSpec :: TyVarBinder
openAlphaTyVarSpec = mkTyVarBinder Specified openAlphaTyVar
openBetaTyVarSpec  = mkTyVarBinder Specified openBetaTyVar
openGammaTyVarSpec = mkTyVarBinder Specified openGammaTyVar

openAlphaTy, openBetaTy, openGammaTy :: Type
openAlphaTy = mkTyVarTy openAlphaTyVar
openBetaTy  = mkTyVarTy openBetaTyVar
openGammaTy = mkTyVarTy openGammaTyVar

levity1TyVar, levity2TyVar :: TyVar
(levity2TyVar : levity1TyVar : _) -- NB: levity2TyVar before levity1TyVar
  = drop 10 (mkTemplateTyVars (repeat levityTy)) -- selects 'k', 'l'
-- The ordering of levity2TyVar before levity1TyVar is chosen so that
-- the more common levity1TyVar uses the levity variable 'l'.

levity1TyVarInf, levity2TyVarInf :: TyVarBinder
levity1TyVarInf = mkTyVarBinder Inferred levity1TyVar
levity2TyVarInf = mkTyVarBinder Inferred levity2TyVar

levity1Ty, levity2Ty :: Type
levity1Ty = mkTyVarTy levity1TyVar
levity2Ty = mkTyVarTy levity2TyVar

levPolyAlphaTyVar, levPolyBetaTyVar :: TyVar
[levPolyAlphaTyVar, levPolyBetaTyVar] =
  mkTemplateTyVars
    [ mk_TYPE_app (mkTyConApp boxedRepDataConTyCon [levity1Ty])
    , mk_TYPE_app (mkTyConApp boxedRepDataConTyCon [levity2Ty])]
-- alpha :: TYPE ('BoxedRep l)
-- beta  :: TYPE ('BoxedRep k)

levPolyAlphaTyVarSpec, levPolyBetaTyVarSpec :: TyVarBinder
levPolyAlphaTyVarSpec = mkTyVarBinder Specified levPolyAlphaTyVar
levPolyBetaTyVarSpec  = mkTyVarBinder Specified levPolyBetaTyVar

levPolyAlphaTy, levPolyBetaTy :: Type
levPolyAlphaTy = mkTyVarTy levPolyAlphaTyVar
levPolyBetaTy  = mkTyVarTy levPolyBetaTyVar

multiplicityTyVar1, multiplicityTyVar2  :: TyVar
(multiplicityTyVar1 : multiplicityTyVar2 : _)
   = drop 13 (mkTemplateTyVars (repeat multiplicityTy))  -- selects 'n', 'm'


{-
************************************************************************
*                                                                      *
                FunTyCon
*                                                                      *
************************************************************************
-}

{- Note [Function type constructors and FunTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have four distinct function type constructors, and a type synonym

 FUN :: forall (m :: Multiplicity) ->
        forall {rep1 :: RuntimeRep} {rep2 :: RuntimeRep}.
        TYPE rep1 -> TYPE rep2 -> Type

 (=>)  :: forall {rep1 :: RuntimeRep} {rep2 :: RuntimeRep}.
          CONSTRAINT rep1 -> TYPE rep2 -> Type

 (==>) :: forall {rep1 :: RuntimeRep} {rep2 :: RuntimeRep}.
          CONSTRAINT rep1 -> CONSTRAINT rep2 -> Constraint

 (-=>) :: forall {rep1 :: RuntimeRep} {rep2 :: RuntimeRep}.
          TYPE rep1 -> CONSTRAINT rep2 -> Constraint

 type (->) = FUN Many

For efficiency, all four are always represented by
  FunTy { ft_af :: FunTyFlag, ft_mult :: Mult
        , ft_arg :: Type, ft_res :: Type }
rather than by using a TyConApp.

* The four TyCons FUN, (=>), (==>), (-=>) are all wired in.
  But (->) is just a regular synonym, with no special treatment;
  in particular it is not wired-in.

* The ft_af :: FunTyFlag distinguishes the four cases.
  See Note [FunTyFlag] in GHC.Types.Var.

* The ft_af field is redundant: it can always be gleaned from
  the kinds of ft_arg and ft_res.  See Note [FunTyFlag] in GHC.Types.Var.

* The ft_mult :: Mult field gives the first argument for FUN
  For the other three cases ft_mult is redundant; it is always Many.
  Note that of the four type constructors, only `FUN` takes a Multiplicity.

* Functions in GHC.Core.Type help to build and decompose `FunTy`.
  * funTyConAppTy_maybe
  * funTyFlagTyCon
  * tyConAppFun_maybe
  * splitFunTy_maybe
  Use them!
-}

funTyFlagTyCon :: FunTyFlag -> TyCon
-- `anonArgTyCon af` gets the TyCon that corresponds to the `FunTyFlag`
-- But be careful: fUNTyCon has a different kind to the others!
-- See Note [Function type constructors and FunTy]
funTyFlagTyCon FTF_T_T = fUNTyCon
funTyFlagTyCon FTF_T_C = tcArrowTyCon
funTyFlagTyCon FTF_C_T = ctArrowTyCon
funTyFlagTyCon FTF_C_C = ccArrowTyCon

isArrowTyCon :: TyCon -> Bool
-- We don't bother to look for plain (->), because this function
-- should only be used after unwrapping synonyms
isArrowTyCon tc
  = assertPpr (not (isTypeSynonymTyCon tc)) (ppr tc)
    getUnique tc `elem`
    [fUNTyConKey, ctArrowTyConKey, ccArrowTyConKey, tcArrowTyConKey]

fUNTyConName, ctArrowTyConName, ccArrowTyConName, tcArrowTyConName :: Name
fUNTyConName     = mkPrimTc        (fsLit "FUN") fUNTyConKey       fUNTyCon
ctArrowTyConName = mkBuiltInPrimTc (fsLit "=>")  ctArrowTyConKey ctArrowTyCon
ccArrowTyConName = mkBuiltInPrimTc (fsLit "==>") ccArrowTyConKey ccArrowTyCon
tcArrowTyConName = mkBuiltInPrimTc (fsLit "-=>") tcArrowTyConKey tcArrowTyCon

-- | The @FUN@ type constructor.
--
-- @
-- FUN :: forall (m :: Multiplicity) ->
--        forall {rep1 :: RuntimeRep} {rep2 :: RuntimeRep}.
--        TYPE rep1 -> TYPE rep2 -> Type
-- @
--
-- The runtime representations quantification is left inferred. This
-- means they cannot be specified with @-XTypeApplications@.
--
-- This is a deliberate choice to allow future extensions to the
-- function arrow.
fUNTyCon :: TyCon
fUNTyCon = mkPrimTyCon fUNTyConName tc_bndrs liftedTypeKind tc_roles
  where
    -- See also unrestrictedFunTyCon
    tc_bndrs = [ mkNamedTyConBinder Required multiplicityTyVar1
               , mkNamedTyConBinder Inferred runtimeRep1TyVar
               , mkNamedTyConBinder Inferred runtimeRep2TyVar ]
               ++ mkTemplateAnonTyConBinders [ mk_TYPE_app runtimeRep1Ty
                                             , mk_TYPE_app runtimeRep2Ty ]
    tc_roles = [Nominal, Nominal, Nominal, Representational, Representational]

-- (=>) :: forall {rep1 :: RuntimeRep} {rep2 :: RuntimeRep}.
--         CONSTRAINT rep1 -> TYPE rep2 -> Type
ctArrowTyCon :: TyCon
ctArrowTyCon = mkPrimTyCon ctArrowTyConName tc_bndrs liftedTypeKind tc_roles
  where
    -- See also unrestrictedFunTyCon
    tc_bndrs = [ mkNamedTyConBinder Inferred runtimeRep1TyVar
               , mkNamedTyConBinder Inferred runtimeRep2TyVar ]
               ++ mkTemplateAnonTyConBinders [ mk_CONSTRAINT_app runtimeRep1Ty
                                             , mk_TYPE_app       runtimeRep2Ty ]
    tc_roles = [Nominal, Nominal, Representational, Representational]

-- (==>) :: forall {rep1 :: RuntimeRep} {rep2 :: RuntimeRep}.
--          CONSTRAINT rep1 -> CONSTRAINT rep2 -> Constraint
ccArrowTyCon :: TyCon
ccArrowTyCon = mkPrimTyCon ccArrowTyConName tc_bndrs constraintKind tc_roles
  where
    -- See also unrestrictedFunTyCon
    tc_bndrs = [ mkNamedTyConBinder Inferred runtimeRep1TyVar
               , mkNamedTyConBinder Inferred runtimeRep2TyVar ]
               ++ mkTemplateAnonTyConBinders [ mk_CONSTRAINT_app runtimeRep1Ty
                                             , mk_CONSTRAINT_app runtimeRep2Ty ]
    tc_roles = [Nominal, Nominal, Representational, Representational]

-- (-=>) :: forall {rep1 :: RuntimeRep} {rep2 :: RuntimeRep}.
--          TYPE rep1 -> CONSTRAINT rep2 -> Constraint
tcArrowTyCon :: TyCon
tcArrowTyCon = mkPrimTyCon tcArrowTyConName tc_bndrs constraintKind tc_roles
  where
    -- See also unrestrictedFunTyCon
    tc_bndrs = [ mkNamedTyConBinder Inferred runtimeRep1TyVar
               , mkNamedTyConBinder Inferred runtimeRep2TyVar ]
               ++ mkTemplateAnonTyConBinders [ mk_TYPE_app       runtimeRep1Ty
                                             , mk_CONSTRAINT_app runtimeRep2Ty ]
    tc_roles = [Nominal, Nominal, Representational, Representational]

{-
************************************************************************
*                                                                      *
                Type and Constraint
*                                                                      *
************************************************************************

Note [TYPE and CONSTRAINT]  aka Note [Type vs Constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC distinguishes Type from Constraint throughout the compiler.
See GHC Proposal #518, and tickets #21623 and #11715.

All types that classify values have a kind of the form
  (TYPE rr) or (CONSTRAINT rr)
where the `RuntimeRep` parameter, rr, tells us how the value is represented
at runtime.  TYPE and CONSTRAINT are primitive type constructors.

See Note [RuntimeRep polymorphism] about the `rr` parameter.

There are a bunch of type synonyms and data types defined in the
library ghc-prim:GHC.Types.  All of them are also wired in to GHC, in
GHC.Builtin.Types

  type Constraint   = CONSTRAINT LiftedRep  :: Type

  type Type         = TYPE LiftedRep   :: Type
  type UnliftedType = TYPE UnliftedRep :: Type

  type LiftedRep    = BoxedRep Lifted   :: RuntimeRep
  type UnliftedRep  = BoxedRep Unlifted :: RuntimeRep

  data RuntimeRep     -- Defined in ghc-prim:GHC.Types
      = BoxedRep Levity
      | IntRep
      | FloatRep
      .. etc ..

  data Levity = Lifted | Unlifted

We abbreviate '*' specially (with -XStarIsType), as if we had this:
    type * = Type

So for example:
    Int        :: TYPE (BoxedRep Lifted)
    Array# Int :: TYPE (BoxedRep Unlifted)
    Int#       :: TYPE IntRep
    Float#     :: TYPE FloatRep
    Maybe      :: TYPE (BoxedRep Lifted) -> TYPE (BoxedRep Lifted)
    (# , #)    :: TYPE r1 -> TYPE r2 -> TYPE (TupleRep [r1, r2])

    Eq Int       :: CONSTRAINT (BoxedRep Lifted)
    IP "foo" Int :: CONSTRAINT (BoxedRep Lifted)
    a ~ b        :: CONSTRAINT (BoxedRep Lifted)
    a ~# b       :: CONSTRAINT (TupleRep [])

Constraints are mostly lifted, but unlifted ones are useful too.
Specifically  (a ~# b) :: CONSTRAINT (TupleRep [])

Wrinkles

(W1) Type and Constraint are considered distinct throughout GHC. But they
     are not /apart/: see Note [Type and Constraint are not apart]

(W2) We need two absent-error Ids, aBSENT_ERROR_ID for types of kind Type, and
     aBSENT_CONSTRAINT_ERROR_ID for types of kind Constraint.
     See Note [Type vs Constraint for error ids] in GHC.Core.Make.
     Ditto noInlineId vs noInlineConstraintId in GHC.Types.Id.Make;
     see Note [inlineId magic].

(W3) We need a TypeOrConstraint flag in LitRubbish.

(W4) In the CPR transformation, we can't unbox constructors with constraint
     arguments because unboxed tuples (# …, … #) currently only supports fields
     of type TYPE rr. See (CPR2) in Note [Which types are unboxed?] in
     GHC.Core.Opt.WorkWrap.Utils.

Note [Type and Constraint are not apart]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type and Constraint are not equal (eqType) but they are not /apart/
either. Reason (c.f. #7451):

* We want to allow newtype classes, where
    class C a where { op :: a -> a }

* The axiom for such a class will look like
    axiom axC a :: (C a :: Constraint) ~# (a->a :: Type)

* This axiom connects a type of kind Type with one of kind Constraint
  That is dangerous: kindCo (axC Int) :: Type ~N Constraint
  And /that/ is bad because we could have
     type family F a where
        F Type       = Int
        F Constraint = Bool
  So now we can prove Int ~N Bool, and all is lost.  We prevent this
  by saying that Type and Constraint are not Apart, which makes the
  above type family instances illegal.

So we ensure that Type and Constraint are not apart; or, more
precisely, that TYPE and CONSTRAINT are not apart.  This
non-apart-ness check is implemented in GHC.Core.Unify.unify_ty: look
for `maybeApart MARTypeVsConstraint`.

Note that, as before, nothing prevents writing instances like:

  instance C (Proxy @Type a) where ...

In particular, TYPE and CONSTRAINT (and the synonyms Type, Constraint
etc) are all allowed in instance heads. It's just that TYPE is not
apart from CONSTRAINT, which means that the above instance would
irretrievably overlap with:

  instance C (Proxy @Constraint a) where ...

Wrinkles

(W1) In GHC.Core.RoughMap.roughMatchTyConName we are careful to map
     TYPE and CONSTRAINT to the same rough-map key.  Reason:
     If we insert (F @Constraint tys) into a FamInstEnv, and look
     up (F @Type tys'), we /must/ ensure that the (C @Constraint tys)
     appears among the unifiables when we do the lookupRM' in
     GHC.Core.FamInstEnv.lookup_fam_inst_env'.  So for the RoughMap we
     simply pretend that they are the same type constructor.  If we
     don't, we'll treat them as fully apart, which is unsound.

(W2) We must extend this treatment to the different arrow types (see
     Note [Function type constructors and FunTy]): if we have
       FunCo (axC Int) <Int> :: (C Int => Int) ~ ((Int -> Int) -> Int),
     then we could extract an equality between (=>) and (->). We thus
     must ensure that (=>) and (->) (among the other arrow combinations)
     are not Apart. See the FunTy/FunTy case in GHC.Core.Unify.unify_ty.

(W3) Are (TYPE IntRep) and (CONSTRAINT WordRep) apart?  In truth yes,
     they are.  But it's easier to say that they are not apart, by
     reporting "maybeApart" (which is always safe), rather than
     recurse into the arguments (whose kinds may be utterly different)
     to look for apartness inside them.  Again this is in
     GHC.Core.Unify.unify_ty.

(W4) We give a different Typeable instance for Type than for Constraint.
     For type classes instances (unlike type family instances) it is not
     /unsound/ for Type and Constraint to treated as fully distinct; and
     for Typeable is desirable to give them different TypeReps.
     Certainly,
       - both Type and Constraint must /have/ a TypeRep, and
       - they had better not be the same (else eqTypeRep would give us
         a proof Type ~N Constraint, which we do not want
     So in GHC.Tc.Instance.Class.matchTypeable, Type and Constraint are
     treated as separate TyCons; i.e. given no special treatment.

Note [RuntimeRep polymorphism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking, you can't be polymorphic in `RuntimeRep`.  E.g
   f :: forall (rr::RuntimeRep) (a::TYPE rr). a -> [a]
   f = /\(rr::RuntimeRep) (a::rr) \(a::rr). ...
This is no good: we could not generate code for 'f', because the
calling convention for 'f' varies depending on whether the argument is
a a Int, Int#, or Float#.  (You could imagine generating specialised
code, one for each instantiation of 'rr', but we don't do that.)

Certain functions CAN be runtime-rep-polymorphic, because the code
generator never has to manipulate a value of type 'a :: TYPE rr'.

* error :: forall (rr::RuntimeRep) (a::TYPE rr). String -> a
  Code generator never has to manipulate the return value.

* unsafeCoerce#, defined in Desugar.mkUnsafeCoercePair:
  Always inlined to be a no-op
     unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                             (a :: TYPE r1) (b :: TYPE r2).
                             a -> b

* Unboxed tuples, and unboxed sums, defined in GHC.Builtin.Types
  Always inlined, and hence specialised to the call site
     (#,#) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                     (a :: TYPE r1) (b :: TYPE r2).
                     a -> b -> TYPE ('TupleRep '[r1, r2])
-}

----------------------
tYPETyCon :: TyCon
tYPETyCon = mkPrimTyCon tYPETyConName
                        (mkTemplateAnonTyConBinders [runtimeRepTy])
                        liftedTypeKind
                        [Nominal]

tYPETyConName :: Name
tYPETyConName = mkPrimTc (fsLit "TYPE") tYPETyConKey tYPETyCon

tYPEKind :: Type
tYPEKind = mkTyConTy tYPETyCon

----------------------
cONSTRAINTTyCon :: TyCon
cONSTRAINTTyCon = mkPrimTyCon cONSTRAINTTyConName
                              (mkTemplateAnonTyConBinders [runtimeRepTy])
                              liftedTypeKind
                              [Nominal]

cONSTRAINTTyConName :: Name
cONSTRAINTTyConName = mkPrimTc (fsLit "CONSTRAINT") cONSTRAINTTyConKey cONSTRAINTTyCon

cONSTRAINTKind :: Type
cONSTRAINTKind = mkTyConTy cONSTRAINTTyCon


{- *********************************************************************
*                                                                      *
       Basic primitive types (Char#, Int#, etc.)
*                                                                      *
********************************************************************* -}

charPrimTy :: Type
charPrimTy      = mkTyConTy charPrimTyCon
charPrimTyCon :: TyCon
charPrimTyCon   = pcPrimTyCon0 charPrimTyConName wordRepDataConTy

intPrimTy :: Type
intPrimTy       = mkTyConTy intPrimTyCon
intPrimTyCon :: TyCon
intPrimTyCon    = pcPrimTyCon0 intPrimTyConName intRepDataConTy

int8PrimTy :: Type
int8PrimTy     = mkTyConTy int8PrimTyCon
int8PrimTyCon :: TyCon
int8PrimTyCon  = pcPrimTyCon0 int8PrimTyConName int8RepDataConTy

int16PrimTy :: Type
int16PrimTy    = mkTyConTy int16PrimTyCon
int16PrimTyCon :: TyCon
int16PrimTyCon = pcPrimTyCon0 int16PrimTyConName int16RepDataConTy

int32PrimTy :: Type
int32PrimTy     = mkTyConTy int32PrimTyCon
int32PrimTyCon :: TyCon
int32PrimTyCon  = pcPrimTyCon0 int32PrimTyConName int32RepDataConTy

int64PrimTy :: Type
int64PrimTy     = mkTyConTy int64PrimTyCon
int64PrimTyCon :: TyCon
int64PrimTyCon  = pcPrimTyCon0 int64PrimTyConName int64RepDataConTy

wordPrimTy :: Type
wordPrimTy      = mkTyConTy wordPrimTyCon
wordPrimTyCon :: TyCon
wordPrimTyCon   = pcPrimTyCon0 wordPrimTyConName wordRepDataConTy

word8PrimTy :: Type
word8PrimTy     = mkTyConTy word8PrimTyCon
word8PrimTyCon :: TyCon
word8PrimTyCon  = pcPrimTyCon0 word8PrimTyConName word8RepDataConTy

word16PrimTy :: Type
word16PrimTy    = mkTyConTy word16PrimTyCon
word16PrimTyCon :: TyCon
word16PrimTyCon = pcPrimTyCon0 word16PrimTyConName word16RepDataConTy

word32PrimTy :: Type
word32PrimTy    = mkTyConTy word32PrimTyCon
word32PrimTyCon :: TyCon
word32PrimTyCon = pcPrimTyCon0 word32PrimTyConName word32RepDataConTy

word64PrimTy :: Type
word64PrimTy    = mkTyConTy word64PrimTyCon
word64PrimTyCon :: TyCon
word64PrimTyCon = pcPrimTyCon0 word64PrimTyConName word64RepDataConTy

addrPrimTy :: Type
addrPrimTy      = mkTyConTy addrPrimTyCon
addrPrimTyCon :: TyCon
addrPrimTyCon   = pcPrimTyCon0 addrPrimTyConName addrRepDataConTy

floatPrimTy     :: Type
floatPrimTy     = mkTyConTy floatPrimTyCon
floatPrimTyCon :: TyCon
floatPrimTyCon  = pcPrimTyCon0 floatPrimTyConName floatRepDataConTy

doublePrimTy :: Type
doublePrimTy    = mkTyConTy doublePrimTyCon
doublePrimTyCon :: TyCon
doublePrimTyCon = pcPrimTyCon0 doublePrimTyConName doubleRepDataConTy

{-
************************************************************************
*                                                                      *
   The @State#@ type (and @_RealWorld@ types)
*                                                                      *
************************************************************************

Note [The equality types story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC sports a veritable menagerie of equality types:

         Type or  Lifted?  Hetero?  Role      Built in         Defining module
         class?    L/U                        TyCon
-----------------------------------------------------------------------------------------
~#         T        U      hetero   nominal   eqPrimTyCon      GHC.Prim
~~         C        L      hetero   nominal   heqTyCon         GHC.Types
~          C        L      homo     nominal   eqTyCon          GHC.Types
:~:        T        L      homo     nominal   (not built-in)   Data.Type.Equality
:~~:       T        L      hetero   nominal   (not built-in)   Data.Type.Equality

~R#        T        U      hetero   repr      eqReprPrimTy     GHC.Prim
Coercible  C        L      homo     repr      coercibleTyCon   GHC.Types
Coercion   T        L      homo     repr      (not built-in)   Data.Type.Coercion
~P#        T        U      hetero   phantom   eqPhantPrimTyCon GHC.Prim

Recall that "hetero" means the equality can related types of different
kinds. Knowing that (t1 ~# t2) or (t1 ~R# t2) or even that (t1 ~P# t2)
also means that (k1 ~# k2), where (t1 :: k1) and (t2 :: k2).

To produce less confusion for end users, when not dumping and without
-fprint-equality-relations, each of these groups is printed as the bottommost
listed equality. That is, (~#) and (~~) are both rendered as (~) in
error messages, and (~R#) is rendered as Coercible.

Let's take these one at a time:

    --------------------------
    (~#) :: forall k1 k2. k1 -> k2 -> TYPE (TupleRep '[])
    --------------------------
This is The Type Of Equality in GHC. It classifies nominal coercions.
This type is used in the solver for recording equality constraints.
It responds "yes" to Type.isEqPred and classifies as an EqPred in
Type.classifyPredType.

All wanted constraints of this type are built with coercion holes.
(See Note [Coercion holes] in GHC.Core.TyCo.Rep.) But see also
Note [Deferred errors for coercion holes] in GHC.Tc.Errors to see how
equality constraints are deferred.

Within GHC, ~# is called eqPrimTyCon, and it is defined in GHC.Builtin.Types.Prim.


    --------------------------
    (~~) :: forall k1 k2. k1 -> k2 -> Constraint
    --------------------------
This is (almost) an ordinary class, defined as if by
  class a ~# b => a ~~ b
  instance a ~# b => a ~~ b
Here's what's unusual about it:

 * We can't actually declare it that way because we don't have syntax for ~#.
   And ~# isn't a constraint, so even if we could write it, it wouldn't kind
   check.

 * Users cannot write instances of it.

 * It is "naturally coherent". This means that the solver won't hesitate to
   solve a goal of type (a ~~ b) even if there is, say (Int ~~ c) in the
   context. (Normally, it waits to learn more, just in case the given
   influences what happens next.) See Note [Solving equality classes]
   in GHC.Tc.Solver.Dict

 * It always terminates. That is, in the UndecidableInstances checks, we
   don't worry if a (~~) constraint is too big, as we know that solving
   equality terminates.

On the other hand, this behaves just like any class w.r.t. eager superclass
unpacking in the solver. So a lifted equality given quickly becomes an unlifted
equality given. This is good, because the solver knows all about unlifted
equalities. There is some special-casing in GHC.Tc.Solver.Dict.matchClassInst to
pretend that there is an instance of this class, as we can't write the instance
in Haskell.

Within GHC, ~~ is called heqTyCon, and it is defined in GHC.Builtin.Types.


    --------------------------
    (~) :: forall k. k -> k -> Constraint
    --------------------------
This is /exactly/ like (~~), except with a homogeneous kind.
It is an almost-ordinary class defined as if by
  class a ~# b => (a :: k) ~ (b :: k)
  instance a ~# b => a ~ b

 * All the bullets for (~~) apply

 * In addition (~) is magical syntax, as ~ is a reserved symbol.
   It cannot be exported or imported.

 * The data constructor of the class is "Eq#", not ":C~"

Within GHC, ~ is called eqTyCon, and it is defined in GHC.Builtin.Types.

Historical note: prior to July 18 (~) was defined as a
  more-ordinary class with (~~) as a superclass.  But that made it
  special in different ways; and the extra superclass selections to
  get from (~) to (~#) via (~~) were tiresome.  Now it's defined
  uniformly with (~~) and Coercible; much nicer.)


    --------------------------
    (:~:) :: forall k. k -> k -> *
    (:~~:) :: forall k1 k2. k1 -> k2 -> *
    --------------------------
These are perfectly ordinary GADTs, wrapping (~) and (~~) resp.
They are not defined within GHC at all.


    --------------------------
    (~R#) :: forall k1 k2. k1 -> k2 -> TYPE (TupleRep '[])
    --------------------------
The is the representational analogue of ~#. This is the type of representational
equalities that the solver works on. All wanted constraints of this type are
built with coercion holes.

Within GHC, ~R# is called eqReprPrimTyCon, and it is defined in GHC.Builtin.Types.Prim.


    --------------------------
    Coercible :: forall k. k -> k -> Constraint
    --------------------------
This is quite like (~~) in the way it's defined and treated within GHC, but
it's homogeneous. Homogeneity helps with type inference (as GHC can solve one
kind from the other) and, in my (Richard's) estimation, will be more intuitive
for users.

An alternative design included HCoercible (like (~~)) and Coercible (like (~)).
One annoyance was that we want `coerce :: Coercible a b => a -> b`, and
we need the type of coerce to be fully wired-in. So the HCoercible/Coercible
split required that both types be fully wired-in. Instead of doing this,
I just got rid of HCoercible, as I'm not sure who would use it, anyway.

Within GHC, Coercible is called coercibleTyCon, and it is defined in
GHC.Builtin.Types.


    --------------------------
    Coercion :: forall k. k -> k -> *
    --------------------------
This is a perfectly ordinary GADT, wrapping Coercible. It is not defined
within GHC at all.


    --------------------------
    (~P#) :: forall k1 k2. k1 -> k2 -> TYPE (TupleRep '[])
    --------------------------
This is the phantom analogue of ~# and it is barely used at all.
(The solver has no idea about this one.) Here is the motivation:

    data Phant a = MkPhant
    type role Phant phantom

    Phant <Int, Bool>_P :: Phant Int ~P# Phant Bool

We just need to have something to put on that last line. You probably
don't need to worry about it.


Note [The State# TyCon]
~~~~~~~~~~~~~~~~~~~~~~~
State# is the primitive, unlifted type of states.  It has one type parameter,
thus
        State# RealWorld
or
        State# s

where s is a type variable. The only purpose of the type parameter is to
keep different state threads separate.  It is represented by nothing at all.

The type parameter to State# is intended to keep separate threads separate.
Even though this parameter is not used in the definition of State#, it is
given role Nominal to enforce its intended use.
-}

mkStatePrimTy :: Type -> Type
mkStatePrimTy ty = TyConApp statePrimTyCon [ty]

statePrimTyCon :: TyCon   -- See Note [The State# TyCon]
statePrimTyCon   = pcPrimTyCon statePrimTyConName [Nominal] zeroBitRepTy

{-
RealWorld is deeply magical.  It is *primitive*, but it is not
*unlifted* (hence ptrArg).  We never manipulate values of type
RealWorld; it's only used in the type system, to parameterise State#.
-}

realWorldTyCon :: TyCon
realWorldTyCon = mkPrimTyCon realWorldTyConName [] liftedTypeKind []
realWorldTy :: Type
realWorldTy          = mkTyConTy realWorldTyCon
realWorldStatePrimTy :: Type
realWorldStatePrimTy = mkStatePrimTy realWorldTy        -- State# RealWorld
realWorldMutableByteArrayPrimTy :: Type
realWorldMutableByteArrayPrimTy
  = mkMutableByteArrayPrimTy realWorldTy -- MutableByteArray# RealWorld

mkProxyPrimTy :: Type -> Type -> Type
mkProxyPrimTy k ty = TyConApp proxyPrimTyCon [k, ty]

proxyPrimTyCon :: TyCon
proxyPrimTyCon = mkPrimTyCon proxyPrimTyConName binders res_kind [Nominal,Phantom]
  where
     -- Kind: forall k. k -> TYPE (TupleRep '[])
     binders = mkTemplateTyConBinders [liftedTypeKind] id
     res_kind = unboxedTupleKind []


{- *********************************************************************
*                                                                      *
                Primitive equality constraints
    See Note [The equality types story]
*                                                                      *
********************************************************************* -}

eqPrimTyCon :: TyCon  -- The representation type for equality predicates
                      -- See Note [The equality types story]
eqPrimTyCon  = mkPrimTyCon eqPrimTyConName binders res_kind roles
  where
    -- Kind :: forall k1 k2. k1 -> k2 -> CONSTRAINT ZeroBitRep
    binders  = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] id
    res_kind = TyConApp cONSTRAINTTyCon [zeroBitRepTy]
    roles    = [Nominal, Nominal, Nominal, Nominal]

-- like eqPrimTyCon, but the type for *Representational* coercions
-- this should only ever appear as the type of a covar. Its role is
-- interpreted in coercionRole
eqReprPrimTyCon :: TyCon   -- See Note [The equality types story]
eqReprPrimTyCon = mkPrimTyCon eqReprPrimTyConName binders res_kind roles
  where
    -- Kind :: forall k1 k2. k1 -> k2 -> CONSTRAINT ZeroBitRep
    binders  = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] id
    res_kind = TyConApp cONSTRAINTTyCon [zeroBitRepTy]
    roles    = [Nominal, Nominal, Representational, Representational]

-- like eqPrimTyCon, but the type for *Phantom* coercions.
-- This is only used to make higher-order equalities. Nothing
-- should ever actually have this type!
eqPhantPrimTyCon :: TyCon
eqPhantPrimTyCon = mkPrimTyCon eqPhantPrimTyConName binders res_kind roles
  where
    -- Kind :: forall k1 k2. k1 -> k2 -> CONSTRAINT ZeroBitRep
    binders  = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] id
    res_kind = TyConApp cONSTRAINTTyCon [zeroBitRepTy]
    roles    = [Nominal, Nominal, Phantom, Phantom]

-- | Given a Role, what TyCon is the type of equality predicates at that role?
equalityTyCon :: Role -> TyCon
equalityTyCon Nominal          = eqPrimTyCon
equalityTyCon Representational = eqReprPrimTyCon
equalityTyCon Phantom          = eqPhantPrimTyCon

{- *********************************************************************
*                                                                      *
             The primitive array types
*                                                                      *
********************************************************************* -}

arrayPrimTyCon, mutableArrayPrimTyCon, mutableByteArrayPrimTyCon,
    byteArrayPrimTyCon,
    smallArrayPrimTyCon, smallMutableArrayPrimTyCon :: TyCon
arrayPrimTyCon             = pcPrimTyCon_LevPolyLastArg arrayPrimTyConName        [Representational]          unliftedRepTy
mutableArrayPrimTyCon      = pcPrimTyCon_LevPolyLastArg mutableArrayPrimTyConName [Nominal, Representational] unliftedRepTy
mutableByteArrayPrimTyCon  = pcPrimTyCon mutableByteArrayPrimTyConName  [Nominal] unliftedRepTy
byteArrayPrimTyCon         = pcPrimTyCon0 byteArrayPrimTyConName        unliftedRepTy
smallArrayPrimTyCon        = pcPrimTyCon_LevPolyLastArg smallArrayPrimTyConName        [Representational]          unliftedRepTy
smallMutableArrayPrimTyCon = pcPrimTyCon_LevPolyLastArg smallMutableArrayPrimTyConName [Nominal, Representational] unliftedRepTy

mkArrayPrimTy :: Type -> Type
mkArrayPrimTy elt           = TyConApp arrayPrimTyCon [getLevity elt, elt]
byteArrayPrimTy :: Type
byteArrayPrimTy             = mkTyConTy byteArrayPrimTyCon
mkSmallArrayPrimTy :: Type -> Type
mkSmallArrayPrimTy elt = TyConApp smallArrayPrimTyCon [getLevity elt, elt]
mkMutableArrayPrimTy :: Type -> Type -> Type
mkMutableArrayPrimTy s elt  = TyConApp mutableArrayPrimTyCon [getLevity elt, s, elt]
mkMutableByteArrayPrimTy :: Type -> Type
mkMutableByteArrayPrimTy s  = TyConApp mutableByteArrayPrimTyCon [s]
mkSmallMutableArrayPrimTy :: Type -> Type -> Type
mkSmallMutableArrayPrimTy s elt = TyConApp smallMutableArrayPrimTyCon [getLevity elt, s, elt]


{- *********************************************************************
*                                                                      *
                The mutable variable type
*                                                                      *
********************************************************************* -}

mutVarPrimTyCon :: TyCon
mutVarPrimTyCon = pcPrimTyCon_LevPolyLastArg mutVarPrimTyConName [Nominal, Representational] unliftedRepTy

mkMutVarPrimTy :: Type -> Type -> Type
mkMutVarPrimTy s elt        = TyConApp mutVarPrimTyCon [getLevity elt, s, elt]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-io-port-var]{The synchronizing I/O Port type}
*                                                                      *
************************************************************************
-}

ioPortPrimTyCon :: TyCon
ioPortPrimTyCon = pcPrimTyCon_LevPolyLastArg ioPortPrimTyConName [Nominal, Representational] unliftedRepTy

mkIOPortPrimTy :: Type -> Type -> Type
mkIOPortPrimTy s elt          = TyConApp ioPortPrimTyCon [getLevity elt, s, elt]

{-
************************************************************************
*                                                                      *
   The synchronizing variable type
\subsection[TysPrim-synch-var]{The synchronizing variable type}
*                                                                      *
************************************************************************
-}

mVarPrimTyCon :: TyCon
mVarPrimTyCon = pcPrimTyCon_LevPolyLastArg mVarPrimTyConName [Nominal, Representational] unliftedRepTy

mkMVarPrimTy :: Type -> Type -> Type
mkMVarPrimTy s elt          = TyConApp mVarPrimTyCon [getLevity elt, s, elt]

{-
************************************************************************
*                                                                      *
   The transactional variable type
*                                                                      *
************************************************************************
-}

tVarPrimTyCon :: TyCon
tVarPrimTyCon = pcPrimTyCon_LevPolyLastArg tVarPrimTyConName [Nominal, Representational] unliftedRepTy

mkTVarPrimTy :: Type -> Type -> Type
mkTVarPrimTy s elt = TyConApp tVarPrimTyCon [getLevity elt, s, elt]

{-
************************************************************************
*                                                                      *
   The stable-pointer type
*                                                                      *
************************************************************************
-}

stablePtrPrimTyCon :: TyCon
stablePtrPrimTyCon = pcPrimTyCon_LevPolyLastArg stablePtrPrimTyConName [Representational] addrRepDataConTy

mkStablePtrPrimTy :: Type -> Type
mkStablePtrPrimTy ty = TyConApp stablePtrPrimTyCon [getLevity ty, ty]

{-
************************************************************************
*                                                                      *
   The stable-name type
*                                                                      *
************************************************************************
-}

stableNamePrimTyCon :: TyCon
stableNamePrimTyCon = pcPrimTyCon_LevPolyLastArg stableNamePrimTyConName [Phantom] unliftedRepTy

mkStableNamePrimTy :: Type -> Type
mkStableNamePrimTy ty = TyConApp stableNamePrimTyCon [getLevity ty, ty]

{-
************************************************************************
*                                                                      *
   The Compact NFData (CNF) type
*                                                                      *
************************************************************************
-}

compactPrimTyCon :: TyCon
compactPrimTyCon = pcPrimTyCon0 compactPrimTyConName unliftedRepTy

compactPrimTy :: Type
compactPrimTy = mkTyConTy compactPrimTyCon

{-
************************************************************************
*                                                                      *
   The @StackSnapshot#@ type
*                                                                      *
************************************************************************
-}

stackSnapshotPrimTyCon :: TyCon
stackSnapshotPrimTyCon = pcPrimTyCon0 stackSnapshotPrimTyConName unliftedRepTy

stackSnapshotPrimTy :: Type
stackSnapshotPrimTy = mkTyConTy stackSnapshotPrimTyCon


{-
************************************************************************
*                                                                      *
   The ``bytecode object'' type
*                                                                      *
************************************************************************
-}

-- Unlike most other primitive types, BCO is lifted. This is because in
-- general a BCO may be a thunk for the reasons given in Note [Updatable CAF
-- BCOs] in GHCi.CreateBCO.
bcoPrimTy    :: Type
bcoPrimTy    = mkTyConTy bcoPrimTyCon
bcoPrimTyCon :: TyCon
bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName liftedRepTy

{-
************************************************************************
*                                                                      *
   The ``weak pointer'' type
*                                                                      *
************************************************************************
-}

weakPrimTyCon :: TyCon
weakPrimTyCon = pcPrimTyCon_LevPolyLastArg weakPrimTyConName [Representational] unliftedRepTy

mkWeakPrimTy :: Type -> Type
mkWeakPrimTy v = TyConApp weakPrimTyCon [getLevity v, v]

{-
************************************************************************
*                                                                      *
   The ``thread id'' type
*                                                                      *
************************************************************************

A thread id is represented by a pointer to the TSO itself, to ensure
that they are always unique and we can always find the TSO for a given
thread id.  However, this has the unfortunate consequence that a
ThreadId# for a given thread is treated as a root by the garbage
collector and can keep TSOs around for too long.

Hence the programmer API for thread manipulation uses a weak pointer
to the thread id internally.
-}

threadIdPrimTy :: Type
threadIdPrimTy    = mkTyConTy threadIdPrimTyCon
threadIdPrimTyCon :: TyCon
threadIdPrimTyCon = pcPrimTyCon0 threadIdPrimTyConName unliftedRepTy

{-
************************************************************************
*                                                                      *
   The ``prompt tag'' type
*                                                                      *
************************************************************************
-}

promptTagPrimTyCon :: TyCon
promptTagPrimTyCon = pcPrimTyCon promptTagPrimTyConName [Representational] unliftedRepTy

mkPromptTagPrimTy :: Type -> Type
mkPromptTagPrimTy v = TyConApp promptTagPrimTyCon [v]

{-
************************************************************************
*                                                                      *
\subsection{SIMD vector types}
*                                                                      *
************************************************************************
-}

#include "primop-vector-tys.hs-incl"
