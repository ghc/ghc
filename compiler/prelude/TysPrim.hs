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
        runtimeRep1TyVar, runtimeRep2TyVar, runtimeRep1Ty, runtimeRep2Ty,
        openAlphaTy, openBetaTy, openAlphaTyVar, openBetaTyVar,
        kKiVar,

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

import Var              ( TyVar, KindVar, mkTyVar )
import Name
import TyCon
import SrcLoc
import Unique
import PrelNames
import FastString
import Outputable
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
funTyCon = mkFunTyCon funTyConName (map Anon [liftedTypeKind, liftedTypeKind])
                                   tc_rep_nm
  where
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

Note [TYPE]
~~~~~~~~~~~
There are a few places where we wish to be able to deal interchangeably
with kind * and kind #. unsafeCoerce#, error, and (->) are some of these
places. The way we do this is to use runtime-representation polymorphism.

We have

    data RuntimeRep = PtrRepLifted | PtrRepUnlifted | ...

and a magical constant (tYPETyCon)

    TYPE :: RuntimeRep -> TYPE PtrRepLifted

We then have synonyms (liftedTypeKindTyCon, unliftedTypeKindTyCon)

    type * = TYPE PtrRepLifted
    type # = TYPE PtrRepUnlifted

The (...) in the definition for RuntimeRep includes possibilities for
the unboxed, unlifted representations, isomorphic to the PrimRep type
in TyCon. RuntimeRep is itself declared in GHC.Types.

An alternative design would be to have

  data RuntimeRep = PtrRep Levity | ...
  data Levity = Lifted | Unlifted

but this slowed down GHC because every time we looked at *, we had to
follow a bunch of pointers. When we have unpackable sums, we should
go back to the stratified representation. This would allow, for example:

    unsafeCoerce# :: forall (r1 :: RuntimeRep) (v2 :: Levity)
                            (a :: TYPE v1) (b :: TYPE v2). a -> b

TYPE replaces the old sub-kinding machinery. We call variables `a` and `b`
above "runtime-representation polymorphic".

-}

tYPETyCon, unliftedTypeKindTyCon :: TyCon
tYPETyConName, unliftedTypeKindTyConName :: Name

tYPETyCon = mkKindTyCon tYPETyConName
                        [Anon runtimeRepTy]
                        liftedTypeKind
                        [Nominal]
                        (mkPrelTyConRepName tYPETyConName)

   -- See Note [TYPE]
   -- NB: unlifted is wired in because there is no way to parse it in
   -- Haskell. That's the only reason for wiring it in.
unliftedTypeKindTyCon = mkSynonymTyCon unliftedTypeKindTyConName
                          [] liftedTypeKind
                          [] []
                          (tYPE (TyConApp ptrRepUnliftedDataConTyCon []))

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
-- | Given a RuntimeRep, applies TYPE to it. See Note [TYPE].
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
    binders     = map (const (Anon liftedTypeKind)) roles
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
  where binders  = [ Named kv Specified
                   , Anon k ]
        res_kind = tYPE voidRepDataConTy
        kv       = kKiVar
        k        = mkTyVarTy kv


{- *********************************************************************
*                                                                      *
                Primitive equality constraints
    See Note [The equality types story]
*                                                                      *
********************************************************************* -}

eqPrimTyCon :: TyCon  -- The representation type for equality predicates
                      -- See Note [The equality types story]
eqPrimTyCon  = mkPrimTyCon eqPrimTyConName binders res_kind roles
  where binders = [ Named kv1 Specified
                  , Named kv2 Specified
                  , Anon k1
                  , Anon k2 ]
        res_kind = tYPE voidRepDataConTy
        [kv1, kv2] = mkTemplateTyVars [liftedTypeKind, liftedTypeKind]
        k1 = mkTyVarTy kv1
        k2 = mkTyVarTy kv2
        roles = [Nominal, Nominal, Nominal, Nominal]

-- like eqPrimTyCon, but the type for *Representational* coercions
-- this should only ever appear as the type of a covar. Its role is
-- interpreted in coercionRole
eqReprPrimTyCon :: TyCon   -- See Note [The equality types story]
eqReprPrimTyCon = mkPrimTyCon eqReprPrimTyConName binders res_kind roles
  where binders = [ Named kv1 Specified
                  , Named kv2 Specified
                  , Anon k1
                  , Anon k2 ]
        res_kind = tYPE voidRepDataConTy
        [kv1, kv2]    = mkTemplateTyVars [liftedTypeKind, liftedTypeKind]
        k1            = mkTyVarTy kv1
        k2            = mkTyVarTy kv2
        roles         = [Nominal, Nominal, Representational, Representational]

-- like eqPrimTyCon, but the type for *Phantom* coercions.
-- This is only used to make higher-order equalities. Nothing
-- should ever actually have this type!
eqPhantPrimTyCon :: TyCon
eqPhantPrimTyCon = mkPrimTyCon eqPhantPrimTyConName binders res_kind
                               [Nominal, Nominal, Phantom, Phantom]
  where binders = [ Named kv1 Specified
                  , Named kv2 Specified
                  , Anon k1
                  , Anon k2 ]
        res_kind = tYPE voidRepDataConTy
        [kv1, kv2]    = mkTemplateTyVars [liftedTypeKind, liftedTypeKind]
        k1            = mkTyVarTy kv1
        k2            = mkTyVarTy kv2


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
anyTyCon = mkFamilyTyCon anyTyConName binders res_kind [kKiVar] Nothing
                         (ClosedSynFamilyTyCon Nothing)
                         Nothing
                         NotInjective
  where
    binders  = [Named kKiVar Specified]
    res_kind = mkTyVarTy kKiVar

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
