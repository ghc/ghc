{-
(c) The AQUA Project, Glasgow University, 1994-1998


\section[TysPrim]{Wired-in knowledge about primitive types}
-}

{-# LANGUAGE CPP #-}

-- | This module defines TyCons that can't be expressed in Haskell.
--   They are all, therefore, wired-in TyCons.  C.f module TysWiredIn
module TysPrim(
        mkPrimTyConName, -- For implicit parameters in TysWiredIn only

        mkTemplateTyVars,
        alphaTyVars, alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar,
        alphaTys, alphaTy, betaTy, gammaTy, deltaTy,
        levity1TyVar, levity2TyVar, levity1Ty, levity2Ty,
        openAlphaTy, openBetaTy, openAlphaTyVar, openBetaTyVar,
        kKiVar,

        -- Kind constructors...
        tYPETyCon, unliftedTypeKindTyCon, unliftedTypeKind,

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

        -- * Any
        anyTy, anyTyCon, anyTypeOfKind,

        -- * SIMD
#include "primop-vector-tys-exports.hs-incl"
  ) where

#include "HsVersions.h"

import {-# SOURCE #-} TysWiredIn ( levityTy, unliftedDataConTy, liftedTypeKind )

import Var              ( TyVar, KindVar, mkTyVar )
import Name
import TyCon
import SrcLoc
import Unique
import PrelNames
import FastString
import TyCoRep   -- doesn't need special access, but this is easier to avoid
                 -- import loops

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
    , statePrimTyCon
    , voidPrimTyCon
    , proxyPrimTyCon
    , threadIdPrimTyCon
    , wordPrimTyCon
    , word32PrimTyCon
    , word64PrimTyCon
    , anyTyCon
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


charPrimTyConName, intPrimTyConName, int32PrimTyConName, int64PrimTyConName, wordPrimTyConName, word32PrimTyConName, word64PrimTyConName, addrPrimTyConName, floatPrimTyConName, doublePrimTyConName, statePrimTyConName, proxyPrimTyConName, realWorldTyConName, arrayPrimTyConName, arrayArrayPrimTyConName, smallArrayPrimTyConName, byteArrayPrimTyConName, mutableArrayPrimTyConName, mutableByteArrayPrimTyConName, mutableArrayArrayPrimTyConName, smallMutableArrayPrimTyConName, mutVarPrimTyConName, mVarPrimTyConName, tVarPrimTyConName, stablePtrPrimTyConName, stableNamePrimTyConName, bcoPrimTyConName, weakPrimTyConName, threadIdPrimTyConName, eqPrimTyConName, eqReprPrimTyConName, eqPhantPrimTyConName, voidPrimTyConName :: Name
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

mkTemplateTyVars :: [Kind] -> [TyVar]
mkTemplateTyVars kinds =
  [ mkTyVar (mkInternalName (mkAlphaTyVarUnique u)
                            (mkTyVarOccFS (mkFastString name))
                            noSrcSpan) k
  | (k,u) <- zip kinds [2..],
    let name | c <= 'z'  = [c]
             | otherwise = 't':show u
          where c = chr (u-2 + ord 'a')
  ]

alphaTyVars :: [TyVar]
alphaTyVars = mkTemplateTyVars $ repeat liftedTypeKind

alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar :: TyVar
(alphaTyVar:betaTyVar:gammaTyVar:deltaTyVar:_) = alphaTyVars

alphaTys :: [Type]
alphaTys = mkTyVarTys alphaTyVars
alphaTy, betaTy, gammaTy, deltaTy :: Type
(alphaTy:betaTy:gammaTy:deltaTy:_) = alphaTys

levity1TyVar, levity2TyVar :: TyVar
(levity1TyVar : levity2TyVar : _)
  = drop 21 (mkTemplateTyVars (repeat levityTy))  -- selects 'v','w'

levity1Ty, levity2Ty :: Type
levity1Ty = mkTyVarTy levity1TyVar
levity2Ty = mkTyVarTy levity2TyVar

openAlphaTyVar, openBetaTyVar :: TyVar
[openAlphaTyVar,openBetaTyVar]
  = mkTemplateTyVars [tYPE levity1Ty, tYPE levity2Ty]

openAlphaTy, openBetaTy :: Type
openAlphaTy = mkTyVarTy openAlphaTyVar
openBetaTy  = mkTyVarTy openBetaTyVar

kKiVar :: KindVar
kKiVar = (mkTemplateTyVars $ repeat liftedTypeKind) !! 10
  -- the 10 selects the 11th letter in the alphabet: 'k'

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
funTyCon = mkFunTyCon funTyConName kind tc_rep_nm
  where
    kind = mkFunTys [liftedTypeKind, liftedTypeKind] liftedTypeKind
        -- You might think that (->) should have type (?? -> ? -> *), and you'd be right
        -- But if we do that we get kind errors when saying
        --      instance Control.Arrow (->)
        -- because the expected kind is (*->*->*).  The trouble is that the
        -- expected/actual stuff in the unifier does not go contra-variant, whereas
        -- the kind sub-typing does.  Sigh.  It really only matters if you use (->) in
        -- a prefix way, thus:  (->) Int# Int#.  And this is unusual.
        -- because they are never in scope in the source

    tc_rep_nm = mkSpecialTyConRepName (fsLit "tcFun") funTyConName

-- One step to remove subkinding.
-- (->) :: * -> * -> *
-- but we should have (and want) the following typing rule for fully applied arrows
--      Gamma |- tau   :: k1    k1 in {*, #}
--      Gamma |- sigma :: k2    k2 in {*, #, (#)}
--      -----------------------------------------
--      Gamma |- tau -> sigma :: *
-- Currently we have the following rule which achieves more or less the same effect
--      Gamma |- tau   :: ??
--      Gamma |- sigma :: ?
--      --------------------------
--      Gamma |- tau -> sigma :: *
-- In the end we don't want subkinding at all.

{-
************************************************************************
*                                                                      *
                Kinds
*                                                                      *
************************************************************************

Note [TYPE]
~~~~~~~~~~~
There are a few places where we wish to be able to deal interchangeably
with kind * and kind #. unsafeCoerce#, error, and (->) are some of these
places. The way we do this is to use levity polymorphism.

We have (levityTyCon, liftedDataCon, unliftedDataCon)

    data Levity = Lifted | Unlifted

and a magical constant (tYPETyCon)

    TYPE :: Levity -> TYPE Lifted

We then have synonyms (liftedTypeKindTyCon, unliftedTypeKindTyCon)

    type Type = TYPE Lifted
    type # = TYPE Unlifted

So, for example, we get

    unsafeCoerce# :: forall (v1 :: Levity) (v2 :: Levity)
                            (a :: TYPE v1) (b :: TYPE v2). a -> b

This replaces the old sub-kinding machinery. We call variables `a` and `b`
above "levity polymorphic".
-}

tYPETyCon, unliftedTypeKindTyCon :: TyCon
tYPETyConName, unliftedTypeKindTyConName :: Name

tYPETyCon = mkKindTyCon tYPETyConName
                        (ForAllTy (Anon levityTy) liftedTypeKind)
                        [Nominal]
                        (mkSpecialTyConRepName (fsLit "tcTYPE") tYPETyConName)

   -- See Note [TYPE]
   -- NB: unlifted is wired in because there is no way to parse it in
   -- Haskell. That's the only reason for wiring it in.
unliftedTypeKindTyCon = mkSynonymTyCon unliftedTypeKindTyConName
                                       liftedTypeKind
                                       [] []
                                       (tYPE unliftedDataConTy)

--------------------------
-- ... and now their names

-- If you edit these, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
tYPETyConName             = mkPrimTyConName (fsLit "TYPE") tYPETyConKey tYPETyCon
unliftedTypeKindTyConName = mkPrimTyConName (fsLit "#") unliftedTypeKindTyConKey unliftedTypeKindTyCon

unliftedTypeKind :: Kind
unliftedTypeKind = tYPE unliftedDataConTy

mkPrimTyConName :: FastString -> Unique -> TyCon -> Name
mkPrimTyConName = mkPrimTcName BuiltInSyntax
  -- All of the super kinds and kinds are defined in Prim,
  -- and use BuiltInSyntax, because they are never in scope in the source

mkPrimTcName :: BuiltInSyntax -> FastString -> Unique -> TyCon -> Name
mkPrimTcName built_in_syntax occ key tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS occ) key (ATyCon tycon) built_in_syntax

-----------------------------
-- | Given a Levity, applies TYPE to it. See Note [TYPE].
tYPE :: Type -> Type
tYPE lev = TyConApp tYPETyCon [lev]

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
  = mkPrimTyCon name kind roles rep
  where
    kind        = mkFunTys (map (const liftedTypeKind) roles) result_kind
    result_kind = unliftedTypeKind

pcPrimTyCon0 :: Name -> PrimRep -> TyCon
pcPrimTyCon0 name rep
  = mkPrimTyCon name result_kind [] rep
  where
    result_kind = unliftedTypeKind

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

Note [The ~# TyCon]
~~~~~~~~~~~~~~~~~~~~
There is a perfectly ordinary type constructor ~# that represents the type
of coercions (which, remember, are values).  For example
   Refl Int :: ~# * * Int Int

It is a kind-polymorphic type constructor like Any:
   Refl Maybe :: ~# (* -> *) (* -> *) Maybe Maybe

(~) only appears saturated. So we check that in CoreLint.

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

voidPrimTy :: Type
voidPrimTy = TyConApp voidPrimTyCon []

voidPrimTyCon :: TyCon
voidPrimTyCon    = pcPrimTyCon voidPrimTyConName [] VoidRep

mkProxyPrimTy :: Type -> Type -> Type
mkProxyPrimTy k ty = TyConApp proxyPrimTyCon [k, ty]

proxyPrimTyCon :: TyCon
proxyPrimTyCon = mkPrimTyCon proxyPrimTyConName kind [Nominal,Nominal] VoidRep
  where kind = ForAllTy (Named kv Invisible) $
               mkFunTy k unliftedTypeKind
        kv   = kKiVar
        k    = mkTyVarTy kv

eqPrimTyCon :: TyCon  -- The representation type for equality predicates
                      -- See Note [The ~# TyCon]
eqPrimTyCon  = mkPrimTyCon eqPrimTyConName kind roles VoidRep
  where kind = ForAllTy (Named kv1 Invisible) $
               ForAllTy (Named kv2 Invisible) $
               mkFunTys [k1, k2] unliftedTypeKind
        [kv1, kv2] = mkTemplateTyVars [liftedTypeKind, liftedTypeKind]
        k1 = mkTyVarTy kv1
        k2 = mkTyVarTy kv2
        roles = [Nominal, Nominal, Nominal, Nominal]

-- like eqPrimTyCon, but the type for *Representational* coercions
-- this should only ever appear as the type of a covar. Its role is
-- interpreted in coercionRole
eqReprPrimTyCon :: TyCon
eqReprPrimTyCon = mkPrimTyCon eqReprPrimTyConName kind
                              roles VoidRep
  where kind = ForAllTy (Named kv1 Invisible) $
               ForAllTy (Named kv2 Invisible) $
               mkFunTys [k1, k2] unliftedTypeKind
        [kv1, kv2]    = mkTemplateTyVars [liftedTypeKind, liftedTypeKind]
        k1            = mkTyVarTy kv1
        k2            = mkTyVarTy kv2
        roles         = [Nominal, Nominal, Representational, Representational]

-- like eqPrimTyCon, but the type for *Phantom* coercions.
-- This is only used to make higher-order equalities. Nothing
-- should ever actually have this type!
eqPhantPrimTyCon :: TyCon
eqPhantPrimTyCon = mkPrimTyCon eqPhantPrimTyConName kind
                               [Nominal, Nominal, Phantom, Phantom]
                               VoidRep
  where kind = ForAllTy (Named kv1 Invisible) $
               ForAllTy (Named kv2 Invisible) $
               mkFunTys [k1, k2] unliftedTypeKind
        [kv1, kv2]    = mkTemplateTyVars [liftedTypeKind, liftedTypeKind]
        k1            = mkTyVarTy kv1
        k2            = mkTyVarTy kv2

{-
RealWorld is deeply magical.  It is *primitive*, but it is not
*unlifted* (hence ptrArg).  We never manipulate values of type
RealWorld; it's only used in the type system, to parameterise State#.
-}

realWorldTyCon :: TyCon
realWorldTyCon = mkLiftedPrimTyCon realWorldTyConName liftedTypeKind [] PtrRep
realWorldTy :: Type
realWorldTy          = mkTyConTy realWorldTyCon
realWorldStatePrimTy :: Type
realWorldStatePrimTy = mkStatePrimTy realWorldTy        -- State# RealWorld

{-
Note: the ``state-pairing'' types are not truly primitive, so they are
defined in \tr{TysWiredIn.hs}, not here.

************************************************************************
*                                                                      *
\subsection[TysPrim-arrays]{The primitive array types}
*                                                                      *
************************************************************************
-}

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

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-mut-var]{The mutable variable type}
*                                                                      *
************************************************************************
-}

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
                Any
*                                                                      *
************************************************************************

Note [Any types]
~~~~~~~~~~~~~~~~
The type constructor Any of kind forall k. k has these properties:

  * It is defined in module GHC.Prim, and exported so that it is
    available to users.  For this reason it's treated like any other
    primitive type:
      - has a fixed unique, anyTyConKey,
      - lives in the global name cache

  * It is a *closed* type family, with no instances.  This means that
    if   ty :: '(k1, k2)  we add a given coercion
             g :: ty ~ (Fst ty, Snd ty)
    If Any was a *data* type, then we'd get inconsistency because 'ty'
    could be (Any '(k1,k2)) and then we'd have an equality with Any on
    one side and '(,) on the other. See also #9097.

  * It is lifted, and hence represented by a pointer

  * It is inhabited by at least one value, namely bottom

  * You can unsafely coerce any lifted type to Any, and back.

  * It does not claim to be a *data* type, and that's important for
    the code generator, because the code gen may *enter* a data value
    but never enters a function value.

  * It is used to instantiate otherwise un-constrained type variables
    For example         length Any []
    See Note [Strangely-kinded void TyCons]

Note [Strangely-kinded void TyCons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Trac #959 for more examples

When the type checker finds a type variable with no binding, which
means it can be instantiated with an arbitrary type, it usually
instantiates it to Void.  Eg.

        length []
===>
        length Any (Nil Any)

But in really obscure programs, the type variable might have a kind
other than *, so we need to invent a suitably-kinded type.

This commit uses
        Any for kind *
        Any(*->*) for kind *->*
        etc
-}

anyTyConName :: Name
anyTyConName = mkPrimTc (fsLit "Any") anyTyConKey anyTyCon

anyTy :: Type
anyTy = mkTyConTy anyTyCon

anyTyCon :: TyCon
anyTyCon = mkFamilyTyCon anyTyConName kind [kKiVar] Nothing
                         (ClosedSynFamilyTyCon Nothing)
                         Nothing
                         NotInjective
  where
    kind = ForAllTy (Named kKiVar Invisible) (mkTyVarTy kKiVar)

anyTypeOfKind :: Kind -> Type
anyTypeOfKind kind = TyConApp anyTyCon [kind]

{-
************************************************************************
*                                                                      *
\subsection{SIMD vector types}
*                                                                      *
************************************************************************
-}

#include "primop-vector-tys.hs-incl"
