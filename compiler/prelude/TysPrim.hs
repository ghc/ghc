{-
(c) The AQUA Project, Glasgow University, 1994-1998


\section[TysPrim]{Wired-in knowledge about primitive types}
-}

{-# LANGUAGE CPP #-}

-- | This module defines TyCons that can't be expressed in Haskell.
--   They are all, therefore, wired-in TyCons.  C.f module TysWiredIn
module TysPrim(
        mkPrimTyConName, -- For implicit parameters in TysWiredIn only

        mkTemplateKindVars, mkTemplateTyVars, mkTemplateTyVarsFrom,
        mkTemplateKiTyVars,

        mkTemplateTyConBinders, mkTemplateKindTyConBinders,
        mkTemplateAnonTyConBinders,

        alphaTyVars, alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar,
        alphaTys, alphaTy, betaTy, gammaTy, deltaTy,
        runtimeRep1TyVar, runtimeRep2TyVar, runtimeRep1Ty, runtimeRep2Ty,
        openAlphaTy, openBetaTy, openAlphaTyVar, openBetaTyVar,

        -- Kind constructors...
        tYPETyConName, unliftedTypeKindTyConName,

        -- Kinds
        tYPE,

        funTyCon, funTyConName,
        primTyCons,

        charPrimTyCon,          charPrimTy,
        intPrimTyCon,           intPrimTy,
        wordPrimTyCon,          wordPrimTy,
        addrPrimTyCon,          addrPrimTy,
        floatPrimTyCon,         floatPrimTy,
        doublePrimTyCon,        doublePrimTy,

        voidPrimTyCon,          voidPrimTy,
        statePrimTyCon,         mkStatePrimTy,
        realWorldTyCon,         realWorldTy, realWorldStatePrimTy,

        proxyPrimTyCon,         mkProxyPrimTy,

        arrayPrimTyCon, mkArrayPrimTy,
        byteArrayPrimTyCon,     byteArrayPrimTy,
        arrayArrayPrimTyCon, mkArrayArrayPrimTy,
        smallArrayPrimTyCon, mkSmallArrayPrimTy,
        mutableArrayPrimTyCon, mkMutableArrayPrimTy,
        mutableByteArrayPrimTyCon, mkMutableByteArrayPrimTy,
        mutableArrayArrayPrimTyCon, mkMutableArrayArrayPrimTy,
        smallMutableArrayPrimTyCon, mkSmallMutableArrayPrimTy,
        mutVarPrimTyCon, mkMutVarPrimTy,

        mVarPrimTyCon,                  mkMVarPrimTy,
        tVarPrimTyCon,                  mkTVarPrimTy,
        stablePtrPrimTyCon,             mkStablePtrPrimTy,
        stableNamePrimTyCon,            mkStableNamePrimTy,
        compactPrimTyCon,               compactPrimTy,
        bcoPrimTyCon,                   bcoPrimTy,
        weakPrimTyCon,                  mkWeakPrimTy,
        threadIdPrimTyCon,              threadIdPrimTy,

        int32PrimTyCon,         int32PrimTy,
        word32PrimTyCon,        word32PrimTy,

        int64PrimTyCon,         int64PrimTy,
        word64PrimTyCon,        word64PrimTy,

        eqPrimTyCon,            -- ty1 ~# ty2
        eqReprPrimTyCon,        -- ty1 ~R# ty2  (at role Representational)
        eqPhantPrimTyCon,       -- ty1 ~P# ty2  (at role Phantom)

        -- * SIMD
#include "primop-vector-tys-exports.hs-incl"
  ) where

#include "HsVersions.h"

import {-# SOURCE #-} TysWiredIn
  ( runtimeRepTy, liftedTypeKind
  , vecRepDataConTyCon, ptrRepUnliftedDataConTyCon
  , voidRepDataConTy, intRepDataConTy
  , wordRepDataConTy, int64RepDataConTy, word64RepDataConTy, addrRepDataConTy
  , floatRepDataConTy, doubleRepDataConTy
  , vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy
  , vec64DataConTy
  , int8ElemRepDataConTy, int16ElemRepDataConTy, int32ElemRepDataConTy
  , int64ElemRepDataConTy, word8ElemRepDataConTy, word16ElemRepDataConTy
  , word32ElemRepDataConTy, word64ElemRepDataConTy, floatElemRepDataConTy
  , doubleElemRepDataConTy )

import Var              ( TyVar, mkTyVar )
import Name
import TyCon
import SrcLoc
import Unique
import PrelNames
import FastString
import Outputable
import TyCoRep   -- Doesn't need special access, but this is easier to avoid
                 -- import loops which show up if you import Type instead

import Data.Char

{-
************************************************************************
*                                                                      *
\subsection{Primitive type constructors}
*                                                                      *
************************************************************************
-}

primTyCons :: [TyCon]
primTyCons
  = [ addrPrimTyCon
    , arrayPrimTyCon
    , byteArrayPrimTyCon
    , arrayArrayPrimTyCon
    , smallArrayPrimTyCon
    , charPrimTyCon
    , doublePrimTyCon
    , floatPrimTyCon
    , intPrimTyCon
    , int32PrimTyCon
    , int64PrimTyCon
    , bcoPrimTyCon
    , weakPrimTyCon
    , mutableArrayPrimTyCon
    , mutableByteArrayPrimTyCon
    , mutableArrayArrayPrimTyCon
    , smallMutableArrayPrimTyCon
    , mVarPrimTyCon
    , tVarPrimTyCon
    , mutVarPrimTyCon
    , realWorldTyCon
    , stablePtrPrimTyCon
    , stableNamePrimTyCon
    , compactPrimTyCon
    , statePrimTyCon
    , voidPrimTyCon
    , proxyPrimTyCon
    , threadIdPrimTyCon
    , wordPrimTyCon
    , word32PrimTyCon
    , word64PrimTyCon
    , eqPrimTyCon
    , eqReprPrimTyCon
    , eqPhantPrimTyCon

    , unliftedTypeKindTyCon
    , tYPETyCon

#include "primop-vector-tycons.hs-incl"
    ]

mkPrimTc :: FastString -> Unique -> TyCon -> Name
mkPrimTc fs unique tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS fs)
                  unique
                  (ATyCon tycon)        -- Relevant TyCon
                  UserSyntax

mkBuiltInPrimTc :: FastString -> Unique -> TyCon -> Name
mkBuiltInPrimTc fs unique tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS fs)
                  unique
                  (ATyCon tycon)        -- Relevant TyCon
                  BuiltInSyntax


charPrimTyConName, intPrimTyConName, int32PrimTyConName, int64PrimTyConName, wordPrimTyConName, word32PrimTyConName, word64PrimTyConName, addrPrimTyConName, floatPrimTyConName, doublePrimTyConName, statePrimTyConName, proxyPrimTyConName, realWorldTyConName, arrayPrimTyConName, arrayArrayPrimTyConName, smallArrayPrimTyConName, byteArrayPrimTyConName, mutableArrayPrimTyConName, mutableByteArrayPrimTyConName, mutableArrayArrayPrimTyConName, smallMutableArrayPrimTyConName, mutVarPrimTyConName, mVarPrimTyConName, tVarPrimTyConName, stablePtrPrimTyConName, stableNamePrimTyConName, compactPrimTyConName, bcoPrimTyConName, weakPrimTyConName, threadIdPrimTyConName, eqPrimTyConName, eqReprPrimTyConName, eqPhantPrimTyConName, voidPrimTyConName :: Name
charPrimTyConName             = mkPrimTc (fsLit "Char#") charPrimTyConKey charPrimTyCon
intPrimTyConName              = mkPrimTc (fsLit "Int#") intPrimTyConKey  intPrimTyCon
int32PrimTyConName            = mkPrimTc (fsLit "Int32#") int32PrimTyConKey int32PrimTyCon
int64PrimTyConName            = mkPrimTc (fsLit "Int64#") int64PrimTyConKey int64PrimTyCon
wordPrimTyConName             = mkPrimTc (fsLit "Word#") wordPrimTyConKey wordPrimTyCon
word32PrimTyConName           = mkPrimTc (fsLit "Word32#") word32PrimTyConKey word32PrimTyCon
word64PrimTyConName           = mkPrimTc (fsLit "Word64#") word64PrimTyConKey word64PrimTyCon
addrPrimTyConName             = mkPrimTc (fsLit "Addr#") addrPrimTyConKey addrPrimTyCon
floatPrimTyConName            = mkPrimTc (fsLit "Float#") floatPrimTyConKey floatPrimTyCon
doublePrimTyConName           = mkPrimTc (fsLit "Double#") doublePrimTyConKey doublePrimTyCon
statePrimTyConName            = mkPrimTc (fsLit "State#") statePrimTyConKey statePrimTyCon
voidPrimTyConName             = mkPrimTc (fsLit "Void#") voidPrimTyConKey voidPrimTyCon
proxyPrimTyConName            = mkPrimTc (fsLit "Proxy#") proxyPrimTyConKey proxyPrimTyCon
eqPrimTyConName               = mkPrimTc (fsLit "~#") eqPrimTyConKey eqPrimTyCon
eqReprPrimTyConName           = mkBuiltInPrimTc (fsLit "~R#") eqReprPrimTyConKey eqReprPrimTyCon
eqPhantPrimTyConName          = mkBuiltInPrimTc (fsLit "~P#") eqPhantPrimTyConKey eqPhantPrimTyCon
realWorldTyConName            = mkPrimTc (fsLit "RealWorld") realWorldTyConKey realWorldTyCon
arrayPrimTyConName            = mkPrimTc (fsLit "Array#") arrayPrimTyConKey arrayPrimTyCon
byteArrayPrimTyConName        = mkPrimTc (fsLit "ByteArray#") byteArrayPrimTyConKey byteArrayPrimTyCon
arrayArrayPrimTyConName           = mkPrimTc (fsLit "ArrayArray#") arrayArrayPrimTyConKey arrayArrayPrimTyCon
smallArrayPrimTyConName       = mkPrimTc (fsLit "SmallArray#") smallArrayPrimTyConKey smallArrayPrimTyCon
mutableArrayPrimTyConName     = mkPrimTc (fsLit "MutableArray#") mutableArrayPrimTyConKey mutableArrayPrimTyCon
mutableByteArrayPrimTyConName = mkPrimTc (fsLit "MutableByteArray#") mutableByteArrayPrimTyConKey mutableByteArrayPrimTyCon
mutableArrayArrayPrimTyConName= mkPrimTc (fsLit "MutableArrayArray#") mutableArrayArrayPrimTyConKey mutableArrayArrayPrimTyCon
smallMutableArrayPrimTyConName= mkPrimTc (fsLit "SmallMutableArray#") smallMutableArrayPrimTyConKey smallMutableArrayPrimTyCon
mutVarPrimTyConName           = mkPrimTc (fsLit "MutVar#") mutVarPrimTyConKey mutVarPrimTyCon
mVarPrimTyConName             = mkPrimTc (fsLit "MVar#") mVarPrimTyConKey mVarPrimTyCon
tVarPrimTyConName             = mkPrimTc (fsLit "TVar#") tVarPrimTyConKey tVarPrimTyCon
stablePtrPrimTyConName        = mkPrimTc (fsLit "StablePtr#") stablePtrPrimTyConKey stablePtrPrimTyCon
stableNamePrimTyConName       = mkPrimTc (fsLit "StableName#") stableNamePrimTyConKey stableNamePrimTyCon
compactPrimTyConName          = mkPrimTc (fsLit "Compact#") compactPrimTyConKey compactPrimTyCon
bcoPrimTyConName              = mkPrimTc (fsLit "BCO#") bcoPrimTyConKey bcoPrimTyCon
weakPrimTyConName             = mkPrimTc (fsLit "Weak#") weakPrimTyConKey weakPrimTyCon
threadIdPrimTyConName         = mkPrimTc (fsLit "ThreadId#") threadIdPrimTyConKey threadIdPrimTyCon

{-
************************************************************************
*                                                                      *
\subsection{Support code}
*                                                                      *
************************************************************************

alphaTyVars is a list of type variables for use in templates:
        ["a", "b", ..., "z", "t1", "t2", ... ]
-}

mkTemplateKindVars :: [Kind] -> [TyVar]
-- k0  with unique (mkAlphaTyVarUnique 0)
-- k1  with unique (mkAlphaTyVarUnique 1)
-- ... etc
mkTemplateKindVars kinds
  = [ mkTyVar name kind
    | (kind, u) <- kinds `zip` [0..]
    , let occ = mkTyVarOccFS (mkFastString ('k' : show u))
          name = mkInternalName (mkAlphaTyVarUnique u) occ noSrcSpan
    ]

mkTemplateTyVarsFrom :: Int -> [Kind] -> [TyVar]
-- a  with unique (mkAlphaTyVarUnique n)
-- b  with unique (mkAlphaTyVarUnique n+1)
-- ... etc
-- Typically called as
--   mkTemplateTyVarsFrom (legth kv_bndrs) kinds
-- where kv_bndrs are the kind-level binders of a TyCon
mkTemplateTyVarsFrom n kinds
  = [ mkTyVar name kind
    | (kind, index) <- zip kinds [0..],
      let ch_ord = index + ord 'a'
          name_str | ch_ord <= ord 'z' = [chr ch_ord]
                   | otherwise         = 't':show index
          uniq = mkAlphaTyVarUnique (index + n)
          name = mkInternalName uniq occ noSrcSpan
          occ  = mkTyVarOccFS (mkFastString name_str)
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
--   forall (r:RuntimeRep) (a:TYPE r) (b:*). blah
-- call mkTemplateKiTyVars [RuntimeRep] (\[r]. [TYPE r, *)
mkTemplateKiTyVars kind_var_kinds mk_arg_kinds
  = kv_bndrs ++ tv_bndrs
  where
    kv_bndrs   = mkTemplateKindVars kind_var_kinds
    anon_kinds = mk_arg_kinds (mkTyVarTys kv_bndrs)
    tv_bndrs   = mkTemplateTyVarsFrom (length kv_bndrs) anon_kinds

mkTemplateKindTyConBinders :: [Kind] -> [TyConBinder]
-- Makes named, Specified binders
mkTemplateKindTyConBinders kinds = [mkNamedTyConBinder Specified tv | tv <- mkTemplateKindVars kinds]

mkTemplateAnonTyConBinders :: [Kind] -> [TyConBinder]
mkTemplateAnonTyConBinders kinds = map mkAnonTyConBinder (mkTemplateTyVars kinds)

mkTemplateAnonTyConBindersFrom :: Int -> [Kind] -> [TyConBinder]
mkTemplateAnonTyConBindersFrom n kinds = map mkAnonTyConBinder (mkTemplateTyVarsFrom n kinds)

alphaTyVars :: [TyVar]
alphaTyVars = mkTemplateTyVars $ repeat liftedTypeKind

alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar :: TyVar
(alphaTyVar:betaTyVar:gammaTyVar:deltaTyVar:_) = alphaTyVars

alphaTys :: [Type]
alphaTys = mkTyVarTys alphaTyVars
alphaTy, betaTy, gammaTy, deltaTy :: Type
(alphaTy:betaTy:gammaTy:deltaTy:_) = alphaTys

runtimeRep1TyVar, runtimeRep2TyVar :: TyVar
(runtimeRep1TyVar : runtimeRep2TyVar : _)
  = drop 16 (mkTemplateTyVars (repeat runtimeRepTy))  -- selects 'q','r'

runtimeRep1Ty, runtimeRep2Ty :: Type
runtimeRep1Ty = mkTyVarTy runtimeRep1TyVar
runtimeRep2Ty = mkTyVarTy runtimeRep2TyVar

openAlphaTyVar, openBetaTyVar :: TyVar
[openAlphaTyVar,openBetaTyVar]
  = mkTemplateTyVars [tYPE runtimeRep1Ty, tYPE runtimeRep2Ty]

openAlphaTy, openBetaTy :: Type
openAlphaTy = mkTyVarTy openAlphaTyVar
openBetaTy  = mkTyVarTy openBetaTyVar

{-
************************************************************************
*                                                                      *
                FunTyCon
*                                                                      *
************************************************************************
-}

funTyConName :: Name
funTyConName = mkPrimTyConName (fsLit "(->)") funTyConKey funTyCon

funTyCon :: TyCon
funTyCon = mkFunTyCon funTyConName tc_bndrs tc_rep_nm
  where
    tc_bndrs = mkTemplateAnonTyConBinders [liftedTypeKind, liftedTypeKind]

        -- You might think that (->) should have type (?? -> ? -> *), and you'd be right
        -- But if we do that we get kind errors when saying
        --      instance Control.Arrow (->)
        -- because the expected kind is (*->*->*).  The trouble is that the
        -- expected/actual stuff in the unifier does not go contra-variant, whereas
        -- the kind sub-typing does.  Sigh.  It really only matters if you use (->) in
        -- a prefix way, thus:  (->) Int# Int#.  And this is unusual.
        -- because they are never in scope in the source

    tc_rep_nm = mkPrelTyConRepName funTyConName

{-
************************************************************************
*                                                                      *
                Kinds
*                                                                      *
************************************************************************

Note [TYPE and RuntimeRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~
All types that classify values have a kind of the form (TYPE rr), where

    data RuntimeRep     -- Defined in ghc-prim:GHC.Types
      = PtrRepLifted
      | PtrRepUnlifted
      | IntRep
      | FloatRep
      .. etc ..

    rr :: RuntimeRep

    TYPE :: RuntimeRep -> TYPE 'PtrRepLifted  -- Built in

So for example:
    Int        :: TYPE 'PtrRepLifted
    Array# Int :: TYPE 'PtrRepUnlifted
    Int#       :: TYPE 'IntRep
    Float#     :: TYPE 'FloatRep
    Maybe      :: TYPE 'PtrRepLifted -> TYPE 'PtrRepLifted

We abbreviate '*' specially:
    type * = TYPE 'PtrRepLifted

The 'rr' parameter tells us how the value is represented at runime.

Generally speaking, you can't be polymorphic in 'rr'.  E.g
   f :: forall (rr:RuntimeRep) (a:TYPE rr). a -> [a]
   f = /\(rr:RuntimeRep) (a:rr) \(a:rr). ...
This is no good: we could not generate code code for 'f', because the
calling convention for 'f' varies depending on whether the argument is
a a Int, Int#, or Float#.  (You could imagine generating specialised
code, one for each instantiation of 'rr', but we don't do that.)

Certain functions CAN be runtime-rep-polymorphic, because the code
generator never has to manipulate a value of type 'a :: TYPE rr'.

* error :: forall (rr:RuntimeRep) (a:TYPE rr). String -> a
  Code generator never has to manipulate the return value.

* unsafeCoerce#, defined in MkId.unsafeCoerceId:
  Always inlined to be a no-op
     unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                             (a :: TYPE r1) (b :: TYPE r2).
                             a -> b

* Unboxed tuples, and unboxed sums, defined in TysWiredIn
  Always inlined, and hence specialised to the call site
     (#,#) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                     (a :: TYPE r1) (b :: TYPE r2).
                     a -> b -> TYPE 'UnboxedTupleRep
     See Note [Unboxed tuple kinds]

Note [Unboxed tuple kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~
What kind does (# Int, Float# #) have?
The "right" answer would be
    TYPE ('UnboxedTupleRep [PtrRepLifted, FloatRep])
Currently we do not do this.  We just have
    (# Int, Float# #) :: TYPE 'UnboxedTupleRep
which does not tell us exactly how is is represented.

Note [PrimRep and kindPrimRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As part of its source code, in TyCon, GHC has
  data PrimRep = PtrRep | IntRep | FloatRep | ...etc...

Notice that
 * RuntimeRep is part of the syntax tree of the program being compiled
     (defined in a library: ghc-prim:GHC.Types)
 * PrimRep is part of GHC's source code.
     (defined in TyCon)

We need to get from one to the other; that is what kindPrimRep does.
Suppose we have a value
   (v :: t) where (t :: k)
Given this kind
    k = TyConApp "TYPE" [rep]
GHC needs to be able to figure out how 'v' is represented at runtime.
It expects 'rep' to be form
    TyConApp rr_dc args
where 'rr_dc' is a promoteed data constructor from RuntimeRep. So
now we need to go from 'dc' to the correponding PrimRep.  We store this
PrimRep in the promoted data constructor itself: see TyCon.promDcRepInfo.

-}

tYPETyCon, unliftedTypeKindTyCon :: TyCon
tYPETyConName, unliftedTypeKindTyConName :: Name

tYPETyCon = mkKindTyCon tYPETyConName
                        (mkTemplateAnonTyConBinders [runtimeRepTy])
                        liftedTypeKind
                        [Nominal]
                        (mkPrelTyConRepName tYPETyConName)

   -- See Note [TYPE and RuntimeRep]
   -- NB: unlifted is wired in because there is no way to parse it in
   -- Haskell. That's the only reason for wiring it in.
unliftedTypeKindTyCon = mkSynonymTyCon unliftedTypeKindTyConName
                          [] liftedTypeKind []
                          (tYPE (TyConApp ptrRepUnliftedDataConTyCon []))
                          True   -- no foralls
                          True   -- family free

--------------------------
-- ... and now their names

-- If you edit these, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
tYPETyConName             = mkPrimTyConName (fsLit "TYPE") tYPETyConKey tYPETyCon
unliftedTypeKindTyConName = mkPrimTyConName (fsLit "#") unliftedTypeKindTyConKey unliftedTypeKindTyCon

mkPrimTyConName :: FastString -> Unique -> TyCon -> Name
mkPrimTyConName = mkPrimTcName BuiltInSyntax
  -- All of the super kinds and kinds are defined in Prim,
  -- and use BuiltInSyntax, because they are never in scope in the source

mkPrimTcName :: BuiltInSyntax -> FastString -> Unique -> TyCon -> Name
mkPrimTcName built_in_syntax occ key tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS occ) key (ATyCon tycon) built_in_syntax

-----------------------------
-- | Given a RuntimeRep, applies TYPE to it.
-- see Note [TYPE and RuntimeRep]
tYPE :: Type -> Type
tYPE rr = TyConApp tYPETyCon [rr]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-basic]{Basic primitive types (@Char#@, @Int#@, etc.)}
*                                                                      *
************************************************************************
-}

-- only used herein
pcPrimTyCon :: Name -> [Role] -> PrimRep -> TyCon
pcPrimTyCon name roles rep
  = mkPrimTyCon name binders result_kind roles
  where
    binders     = mkTemplateAnonTyConBinders (map (const liftedTypeKind) roles)
    result_kind = tYPE rr

    rr = case rep of
      VoidRep       -> voidRepDataConTy
      PtrRep        -> TyConApp ptrRepUnliftedDataConTyCon []
      IntRep        -> intRepDataConTy
      WordRep       -> wordRepDataConTy
      Int64Rep      -> int64RepDataConTy
      Word64Rep     -> word64RepDataConTy
      AddrRep       -> addrRepDataConTy
      FloatRep      -> floatRepDataConTy
      DoubleRep     -> doubleRepDataConTy
      VecRep n elem -> TyConApp vecRepDataConTyCon [n', elem']
        where
          n' = case n of
            2  -> vec2DataConTy
            4  -> vec4DataConTy
            8  -> vec8DataConTy
            16 -> vec16DataConTy
            32 -> vec32DataConTy
            64 -> vec64DataConTy
            _  -> pprPanic "Disallowed VecCount" (ppr n)

          elem' = case elem of
            Int8ElemRep   -> int8ElemRepDataConTy
            Int16ElemRep  -> int16ElemRepDataConTy
            Int32ElemRep  -> int32ElemRepDataConTy
            Int64ElemRep  -> int64ElemRepDataConTy
            Word8ElemRep  -> word8ElemRepDataConTy
            Word16ElemRep -> word16ElemRepDataConTy
            Word32ElemRep -> word32ElemRepDataConTy
            Word64ElemRep -> word64ElemRepDataConTy
            FloatElemRep  -> floatElemRepDataConTy
            DoubleElemRep -> doubleElemRepDataConTy


pcPrimTyCon0 :: Name -> PrimRep -> TyCon
pcPrimTyCon0 name rep
  = pcPrimTyCon name [] rep

charPrimTy :: Type
charPrimTy      = mkTyConTy charPrimTyCon
charPrimTyCon :: TyCon
charPrimTyCon   = pcPrimTyCon0 charPrimTyConName WordRep

intPrimTy :: Type
intPrimTy       = mkTyConTy intPrimTyCon
intPrimTyCon :: TyCon
intPrimTyCon    = pcPrimTyCon0 intPrimTyConName IntRep

int32PrimTy :: Type
int32PrimTy     = mkTyConTy int32PrimTyCon
int32PrimTyCon :: TyCon
int32PrimTyCon  = pcPrimTyCon0 int32PrimTyConName IntRep

int64PrimTy :: Type
int64PrimTy     = mkTyConTy int64PrimTyCon
int64PrimTyCon :: TyCon
int64PrimTyCon  = pcPrimTyCon0 int64PrimTyConName Int64Rep

wordPrimTy :: Type
wordPrimTy      = mkTyConTy wordPrimTyCon
wordPrimTyCon :: TyCon
wordPrimTyCon   = pcPrimTyCon0 wordPrimTyConName WordRep

word32PrimTy :: Type
word32PrimTy    = mkTyConTy word32PrimTyCon
word32PrimTyCon :: TyCon
word32PrimTyCon = pcPrimTyCon0 word32PrimTyConName WordRep

word64PrimTy :: Type
word64PrimTy    = mkTyConTy word64PrimTyCon
word64PrimTyCon :: TyCon
word64PrimTyCon = pcPrimTyCon0 word64PrimTyConName Word64Rep

addrPrimTy :: Type
addrPrimTy      = mkTyConTy addrPrimTyCon
addrPrimTyCon :: TyCon
addrPrimTyCon   = pcPrimTyCon0 addrPrimTyConName AddrRep

floatPrimTy     :: Type
floatPrimTy     = mkTyConTy floatPrimTyCon
floatPrimTyCon :: TyCon
floatPrimTyCon  = pcPrimTyCon0 floatPrimTyConName FloatRep

doublePrimTy :: Type
doublePrimTy    = mkTyConTy doublePrimTyCon
doublePrimTyCon :: TyCon
doublePrimTyCon = pcPrimTyCon0 doublePrimTyConName DoubleRep

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-state]{The @State#@ type (and @_RealWorld@ types)}
*                                                                      *
************************************************************************

Note [The equality types story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC sports a veritable menagerie of equality types:

              Hetero?   Levity      Result       Role      Defining module
              ------------------------------------------------------------
  ~#          hetero    unlifted    #            nominal   GHC.Prim
  ~~          hetero    lifted      Constraint   nominal   GHC.Types
  ~           homo      lifted      Constraint   nominal   Data.Type.Equality
  :~:         homo      lifted      *            nominal   Data.Type.Equality

  ~R#         hetero    unlifted    #            repr      GHC.Prim
  Coercible   homo      lifted      Constraint   repr      GHC.Types
  Coercion    homo      lifted      *            repr      Data.Type.Coercion

  ~P#         hetero    unlifted                 phantom   GHC.Prim

Recall that "hetero" means the equality can related types of different
kinds. Knowing that (t1 ~# t2) or (t1 ~R# t2) or even that (t1 ~P# t2)
also means that (k1 ~# k2), where (t1 :: k1) and (t2 :: k2).

To produce less confusion for end users, when not dumping and without
-fprint-equality-relations, each of these groups is printed as the bottommost
listed equality. That is, (~#) and (~~) are both rendered as (~) in
error messages, and (~R#) is rendered as Coercible.

Let's take these one at a time:

    --------------------------
    (~#) :: forall k1 k2. k1 -> k2 -> #
    --------------------------
This is The Type Of Equality in GHC. It classifies nominal coercions.
This type is used in the solver for recording equality constraints.
It responds "yes" to Type.isEqPred and classifies as an EqPred in
Type.classifyPredType.

All wanted constraints of this type are built with coercion holes.
(See Note [Coercion holes] in TyCoRep.) But see also
Note [Deferred errors for coercion holes] in TcErrors to see how
equality constraints are deferred.

Within GHC, ~# is called eqPrimTyCon, and it is defined in TysPrim.


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
   influences what happens next.) This is quite like having
   IncoherentInstances enabled.

 * It always terminates. That is, in the UndecidableInstances checks, we
   don't worry if a (~~) constraint is too big, as we know that solving
   equality terminates.

On the other hand, this behaves just like any class w.r.t. eager superclass
unpacking in the solver. So a lifted equality given quickly becomes an unlifted
equality given. This is good, because the solver knows all about unlifted
equalities. There is some special-casing in TcInteract.matchClassInst to
pretend that there is an instance of this class, as we can't write the instance
in Haskell.

Within GHC, ~~ is called heqTyCon, and it is defined in TysWiredIn.


    --------------------------
    (~) :: forall k. k -> k -> Constraint
    --------------------------
This is defined in Data.Type.Equality:
  class a ~~ b => (a :: k) ~ (b :: k)
  instance a ~~ b => a ~ b
This is even more so an ordinary class than (~~), with the following exceptions:
 * Users cannot write instances of it.

 * It is "naturally coherent". (See (~~).)

 * (~) is magical syntax, as ~ is a reserved symbol. It cannot be exported
   or imported.

 * It always terminates.

Within GHC, ~ is called eqTyCon, and it is defined in PrelNames. Note that
it is *not* wired in.


    --------------------------
    (:~:) :: forall k. k -> k -> *
    --------------------------
This is a perfectly ordinary GADT, wrapping (~). It is not defined within
GHC at all.


    --------------------------
    (~R#) :: forall k1 k2. k1 -> k2 -> #
    --------------------------
The is the representational analogue of ~#. This is the type of representational
equalities that the solver works on. All wanted constraints of this type are
built with coercion holes.

Within GHC, ~R# is called eqReprPrimTyCon, and it is defined in TysPrim.


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
TysWiredIn.


    --------------------------
    Coercion :: forall k. k -> k -> *
    --------------------------
This is a perfectly ordinary GADT, wrapping Coercible. It is not defined
within GHC at all.


    --------------------------
    (~P#) :: forall k1 k2. k1 -> k2 -> #
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
statePrimTyCon   = pcPrimTyCon statePrimTyConName [Nominal] VoidRep

{-
RealWorld is deeply magical.  It is *primitive*, but it is not
*unlifted* (hence ptrArg).  We never manipulate values of type
RealWorld; it's only used in the type system, to parameterise State#.
-}

realWorldTyCon :: TyCon
realWorldTyCon = mkLiftedPrimTyCon realWorldTyConName [] liftedTypeKind []
realWorldTy :: Type
realWorldTy          = mkTyConTy realWorldTyCon
realWorldStatePrimTy :: Type
realWorldStatePrimTy = mkStatePrimTy realWorldTy        -- State# RealWorld

-- Note: the ``state-pairing'' types are not truly primitive,
-- so they are defined in \tr{TysWiredIn.hs}, not here.


voidPrimTy :: Type
voidPrimTy = TyConApp voidPrimTyCon []

voidPrimTyCon :: TyCon
voidPrimTyCon    = pcPrimTyCon voidPrimTyConName [] VoidRep

mkProxyPrimTy :: Type -> Type -> Type
mkProxyPrimTy k ty = TyConApp proxyPrimTyCon [k, ty]

proxyPrimTyCon :: TyCon
proxyPrimTyCon = mkPrimTyCon proxyPrimTyConName binders res_kind [Nominal,Nominal]
  where
     -- Kind: forall k. k -> Void#
     binders = mkTemplateTyConBinders [liftedTypeKind] (\ks-> ks)
     res_kind = tYPE voidRepDataConTy


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
    -- Kind :: forall k1 k2. k1 -> k2 -> Void#
    binders  = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] (\ks -> ks)
    res_kind = tYPE voidRepDataConTy
    roles    = [Nominal, Nominal, Nominal, Nominal]

-- like eqPrimTyCon, but the type for *Representational* coercions
-- this should only ever appear as the type of a covar. Its role is
-- interpreted in coercionRole
eqReprPrimTyCon :: TyCon   -- See Note [The equality types story]
eqReprPrimTyCon = mkPrimTyCon eqReprPrimTyConName binders res_kind roles
  where
    -- Kind :: forall k1 k2. k1 -> k2 -> Void#
    binders  = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] (\ks -> ks)
    res_kind = tYPE voidRepDataConTy
    roles    = [Nominal, Nominal, Representational, Representational]

-- like eqPrimTyCon, but the type for *Phantom* coercions.
-- This is only used to make higher-order equalities. Nothing
-- should ever actually have this type!
eqPhantPrimTyCon :: TyCon
eqPhantPrimTyCon = mkPrimTyCon eqPhantPrimTyConName binders res_kind roles
  where
    -- Kind :: forall k1 k2. k1 -> k2 -> Void#
    binders  = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] (\ks -> ks)
    res_kind = tYPE voidRepDataConTy
    roles    = [Nominal, Nominal, Phantom, Phantom]

{- *********************************************************************
*                                                                      *
             The primitive array types
*                                                                      *
********************************************************************* -}

arrayPrimTyCon, mutableArrayPrimTyCon, mutableByteArrayPrimTyCon,
    byteArrayPrimTyCon, arrayArrayPrimTyCon, mutableArrayArrayPrimTyCon,
    smallArrayPrimTyCon, smallMutableArrayPrimTyCon :: TyCon
arrayPrimTyCon             = pcPrimTyCon arrayPrimTyConName             [Representational] PtrRep
mutableArrayPrimTyCon      = pcPrimTyCon  mutableArrayPrimTyConName     [Nominal, Representational] PtrRep
mutableByteArrayPrimTyCon  = pcPrimTyCon mutableByteArrayPrimTyConName  [Nominal] PtrRep
byteArrayPrimTyCon         = pcPrimTyCon0 byteArrayPrimTyConName        PtrRep
arrayArrayPrimTyCon        = pcPrimTyCon0 arrayArrayPrimTyConName       PtrRep
mutableArrayArrayPrimTyCon = pcPrimTyCon mutableArrayArrayPrimTyConName [Nominal] PtrRep
smallArrayPrimTyCon        = pcPrimTyCon smallArrayPrimTyConName        [Representational] PtrRep
smallMutableArrayPrimTyCon = pcPrimTyCon smallMutableArrayPrimTyConName [Nominal, Representational] PtrRep

mkArrayPrimTy :: Type -> Type
mkArrayPrimTy elt           = TyConApp arrayPrimTyCon [elt]
byteArrayPrimTy :: Type
byteArrayPrimTy             = mkTyConTy byteArrayPrimTyCon
mkArrayArrayPrimTy :: Type
mkArrayArrayPrimTy = mkTyConTy arrayArrayPrimTyCon
mkSmallArrayPrimTy :: Type -> Type
mkSmallArrayPrimTy elt = TyConApp smallArrayPrimTyCon [elt]
mkMutableArrayPrimTy :: Type -> Type -> Type
mkMutableArrayPrimTy s elt  = TyConApp mutableArrayPrimTyCon [s, elt]
mkMutableByteArrayPrimTy :: Type -> Type
mkMutableByteArrayPrimTy s  = TyConApp mutableByteArrayPrimTyCon [s]
mkMutableArrayArrayPrimTy :: Type -> Type
mkMutableArrayArrayPrimTy s = TyConApp mutableArrayArrayPrimTyCon [s]
mkSmallMutableArrayPrimTy :: Type -> Type -> Type
mkSmallMutableArrayPrimTy s elt = TyConApp smallMutableArrayPrimTyCon [s, elt]


{- *********************************************************************
*                                                                      *
                The mutable variable type
*                                                                      *
********************************************************************* -}

mutVarPrimTyCon :: TyCon
mutVarPrimTyCon = pcPrimTyCon mutVarPrimTyConName [Nominal, Representational] PtrRep

mkMutVarPrimTy :: Type -> Type -> Type
mkMutVarPrimTy s elt        = TyConApp mutVarPrimTyCon [s, elt]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-synch-var]{The synchronizing variable type}
*                                                                      *
************************************************************************
-}

mVarPrimTyCon :: TyCon
mVarPrimTyCon = pcPrimTyCon mVarPrimTyConName [Nominal, Representational] PtrRep

mkMVarPrimTy :: Type -> Type -> Type
mkMVarPrimTy s elt          = TyConApp mVarPrimTyCon [s, elt]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-stm-var]{The transactional variable type}
*                                                                      *
************************************************************************
-}

tVarPrimTyCon :: TyCon
tVarPrimTyCon = pcPrimTyCon tVarPrimTyConName [Nominal, Representational] PtrRep

mkTVarPrimTy :: Type -> Type -> Type
mkTVarPrimTy s elt = TyConApp tVarPrimTyCon [s, elt]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-stable-ptrs]{The stable-pointer type}
*                                                                      *
************************************************************************
-}

stablePtrPrimTyCon :: TyCon
stablePtrPrimTyCon = pcPrimTyCon stablePtrPrimTyConName [Representational] AddrRep

mkStablePtrPrimTy :: Type -> Type
mkStablePtrPrimTy ty = TyConApp stablePtrPrimTyCon [ty]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-stable-names]{The stable-name type}
*                                                                      *
************************************************************************
-}

stableNamePrimTyCon :: TyCon
stableNamePrimTyCon = pcPrimTyCon stableNamePrimTyConName [Representational] PtrRep

mkStableNamePrimTy :: Type -> Type
mkStableNamePrimTy ty = TyConApp stableNamePrimTyCon [ty]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-compact-nfdata]{The Compact NFData (CNF) type}
*                                                                      *
************************************************************************
-}

compactPrimTyCon :: TyCon
compactPrimTyCon = pcPrimTyCon0 compactPrimTyConName PtrRep

compactPrimTy :: Type
compactPrimTy = mkTyConTy compactPrimTyCon

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-BCOs]{The ``bytecode object'' type}
*                                                                      *
************************************************************************
-}

bcoPrimTy    :: Type
bcoPrimTy    = mkTyConTy bcoPrimTyCon
bcoPrimTyCon :: TyCon
bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName PtrRep

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-Weak]{The ``weak pointer'' type}
*                                                                      *
************************************************************************
-}

weakPrimTyCon :: TyCon
weakPrimTyCon = pcPrimTyCon weakPrimTyConName [Representational] PtrRep

mkWeakPrimTy :: Type -> Type
mkWeakPrimTy v = TyConApp weakPrimTyCon [v]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-thread-ids]{The ``thread id'' type}
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
threadIdPrimTyCon = pcPrimTyCon0 threadIdPrimTyConName PtrRep

{-
************************************************************************
*                                                                      *
\subsection{SIMD vector types}
*                                                                      *
************************************************************************
-}

#include "primop-vector-tys.hs-incl"
