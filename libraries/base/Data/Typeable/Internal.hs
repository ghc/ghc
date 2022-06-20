{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE LinearTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Typeable.Internal
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- The representations of the types TyCon and TypeRep, and the
-- function mkTyCon which is used by derived instances of Typeable to
-- construct a TyCon.
--
-----------------------------------------------------------------------------

module Data.Typeable.Internal (
    -- * Typeable and kind polymorphism
    --
    -- #kind_instantiation

    -- * Miscellaneous
    Fingerprint(..),

    -- * Typeable class
    Typeable(..),
    withTypeable,

    -- * Module
    Module,  -- Abstract
    moduleName, modulePackage, rnfModule,

    -- * TyCon
    TyCon,   -- Abstract
    tyConPackage, tyConModule, tyConName, tyConKindArgs, tyConKindRep,
    tyConFingerprint,
    KindRep(.., KindRepTypeLit), TypeLitSort(..),
    rnfTyCon,

    -- * TypeRep
    TypeRep,
    pattern TypeRep,
    pattern App, pattern Con, pattern Con', pattern Fun,
    typeRep,
    typeOf,
    typeRepTyCon,
    typeRepFingerprint,
    rnfTypeRep,
    eqTypeRep,
    typeRepKind,
    splitApps,

    -- * SomeTypeRep
    SomeTypeRep(..),
    someTypeRep,
    someTypeRepTyCon,
    someTypeRepFingerprint,
    rnfSomeTypeRep,

    -- * Construction
    -- | These are for internal use only
    mkTrType, mkTrCon, mkTrApp, mkTrAppChecked, mkTrFun,
    mkTyCon, mkTyCon#,
    typeSymbolTypeRep, typeNatTypeRep, typeCharTypeRep,

    -- * For internal use
    trLiftedRep
  ) where

import GHC.Base
import qualified GHC.Arr as A
import Data.Type.Equality
import GHC.List ( splitAt, foldl', elem )
import GHC.Word
import GHC.Show
import GHC.TypeLits ( KnownChar, charVal', KnownSymbol, symbolVal'
                    , TypeError, ErrorMessage(..) )
import GHC.TypeNats ( KnownNat, Nat, natVal' )
import Unsafe.Coerce ( unsafeCoerce )

import GHC.Fingerprint.Type
import {-# SOURCE #-} GHC.Fingerprint
   -- loop: GHC.Fingerprint -> Foreign.Ptr -> Data.Typeable
   -- Better to break the loop here, because we want non-SOURCE imports
   -- of Data.Typeable as much as possible so we can optimise the derived
   -- instances.
-- import {-# SOURCE #-} Debug.Trace (trace)

#include "MachDeps.h"

{- *********************************************************************
*                                                                      *
                The TyCon type
*                                                                      *
********************************************************************* -}

modulePackage :: Module -> String
modulePackage (Module p _) = trNameString p

moduleName :: Module -> String
moduleName (Module _ m) = trNameString m

tyConPackage :: TyCon -> String
tyConPackage (TyCon _ _ m _ _ _) = modulePackage m

tyConModule :: TyCon -> String
tyConModule (TyCon _ _ m _ _ _) = moduleName m

tyConName :: TyCon -> String
tyConName (TyCon _ _ _ n _ _) = trNameString n

trNameString :: TrName -> String
trNameString (TrNameS s) = unpackCStringUtf8# s
trNameString (TrNameD s) = s

tyConFingerprint :: TyCon -> Fingerprint
tyConFingerprint (TyCon hi lo _ _ _ _)
  = Fingerprint (W64# hi) (W64# lo)

tyConKindArgs :: TyCon -> Int
tyConKindArgs (TyCon _ _ _ _ n _) = I# n

tyConKindRep :: TyCon -> KindRep
tyConKindRep (TyCon _ _ _ _ _ k) = k

-- | Helper to fully evaluate 'TyCon' for use as @NFData(rnf)@ implementation
--
-- @since 4.8.0.0
rnfModule :: Module -> ()
rnfModule (Module p m) = rnfTrName p `seq` rnfTrName m

rnfTrName :: TrName -> ()
rnfTrName (TrNameS _) = ()
rnfTrName (TrNameD n) = rnfString n

rnfKindRep :: KindRep -> ()
rnfKindRep (KindRepTyConApp tc args) = rnfTyCon tc `seq` rnfList rnfKindRep args
rnfKindRep (KindRepVar _)   = ()
rnfKindRep (KindRepApp a b) = rnfKindRep a `seq` rnfKindRep b
rnfKindRep (KindRepFun a b) = rnfKindRep a `seq` rnfKindRep b
rnfKindRep (KindRepTYPE rr) = rnfRuntimeRep rr
rnfKindRep (KindRepTypeLitS _ _) = ()
rnfKindRep (KindRepTypeLitD _ t) = rnfString t

rnfRuntimeRep :: RuntimeRep -> ()
rnfRuntimeRep (VecRep !_ !_) = ()
rnfRuntimeRep !_             = ()

rnfList :: (a -> ()) -> [a] -> ()
rnfList _     []     = ()
rnfList force (x:xs) = force x `seq` rnfList force xs

rnfString :: [Char] -> ()
rnfString = rnfList (`seq` ())

rnfTyCon :: TyCon -> ()
rnfTyCon (TyCon _ _ m n _ k) = rnfModule m `seq` rnfTrName n `seq` rnfKindRep k


{- *********************************************************************
*                                                                      *
                The TypeRep type
*                                                                      *
********************************************************************* -}

-- | A concrete representation of a (monomorphic) type.
-- 'TypeRep' supports reasonably efficient equality.
type TypeRep :: k -> Type
data TypeRep a where
    -- The TypeRep of Type. See Note [Kind caching], Wrinkle 2
    TrType :: TypeRep Type
    TrTyCon :: { -- See Note [TypeRep fingerprints]
                 trTyConFingerprint :: {-# UNPACK #-} !Fingerprint

                 -- The TypeRep represents the application of trTyCon
                 -- to the kind arguments trKindVars. So for
                 -- 'Just :: Bool -> Maybe Bool, the trTyCon will be
                 -- 'Just and the trKindVars will be [Bool].
               , trTyCon :: !TyCon
               , trKindVars :: [SomeTypeRep]
               , trTyConKind :: !(TypeRep k) }  -- See Note [Kind caching]
            -> TypeRep (a :: k)

    -- | Invariant: Saturated arrow types (e.g. things of the form @a -> b@)
    -- are represented with @'TrFun' a b@, not @TrApp (TrApp funTyCon a) b@.
    TrApp   :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
               { -- See Note [TypeRep fingerprints]
                 trAppFingerprint :: {-# UNPACK #-} !Fingerprint

                 -- The TypeRep represents the application of trAppFun
                 -- to trAppArg. For Maybe Int, the trAppFun will be Maybe
                 -- and the trAppArg will be Int.
               , trAppFun :: !(TypeRep (a :: k1 -> k2))
               , trAppArg :: !(TypeRep (b :: k1))
               , trAppKind :: !(TypeRep k2) }   -- See Note [Kind caching]
            -> TypeRep (a b)

    -- | @TrFun fpr m a b@ represents a function type @a # m -> b@. We use this for
    -- the sake of efficiency as functions are quite ubiquitous.
    TrFun   :: forall (m :: Multiplicity) (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                      (a :: TYPE r1) (b :: TYPE r2).
               { -- See Note [TypeRep fingerprints]
                 trFunFingerprint :: {-# UNPACK #-} !Fingerprint

                 -- The TypeRep represents a function from trFunArg to
                 -- trFunRes.
               , trFunMul :: !(TypeRep m)
               , trFunArg :: !(TypeRep a)
               , trFunRes :: !(TypeRep b) }
            -> TypeRep (FUN m a b)

-- | A 'TypeableInstance' wraps up a 'Typeable' instance for explicit
-- handling. For internal use: for defining 'TypeRep' pattern.
type TypeableInstance :: forall k. k -> Type
data TypeableInstance a where
 TypeableInstance :: Typeable a => TypeableInstance a

-- | Get a reified 'Typeable' instance from an explicit 'TypeRep'.
--
-- For internal use: for defining 'TypeRep' pattern.
typeableInstance :: forall {k :: Type} (a :: k). TypeRep a -> TypeableInstance a
typeableInstance rep = withTypeable rep TypeableInstance

-- | A explicitly bidirectional pattern synonym to construct a
-- concrete representation of a type.
--
-- As an __expression__: Constructs a singleton @TypeRep a@ given a
-- implicit 'Typeable a' constraint:
--
-- @
-- TypeRep @a :: Typeable a => TypeRep a
-- @
--
-- As a __pattern__: Matches on an explicit @TypeRep a@ witness bringing
-- an implicit @Typeable a@ constraint into scope.
--
-- @
-- f :: TypeRep a -> ..
-- f TypeRep = {- Typeable a in scope -}
-- @
--
-- @since 4.17.0.0
pattern TypeRep :: forall {k :: Type} (a :: k). () => Typeable @k a => TypeRep @k a
pattern TypeRep <- (typeableInstance -> TypeableInstance)
  where TypeRep = typeRep

{- Note [TypeRep fingerprints]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~
We store a Fingerprint of each TypeRep in its constructor. This allows
us to test whether two TypeReps are equal in constant time, rather than
having to walk their full structures.
-}

{- Note [Kind caching]
   ~~~~~~~~~~~~~~~~~~~
We cache the kind of the TypeRep in each TrTyCon and TrApp constructor.
This is necessary to ensure that typeRepKind (which is used, at least, in
deserialization and dynApply) is cheap. There are two reasons for this:

1. Calculating the kind of a nest of type applications, such as

  F X Y Z W   (App (App (App (App F X) Y) Z) W)

is linear in the depth, which is already a bit pricy. In deserialization,
we build up such a nest from the inside out, so without caching, that ends
up taking quadratic time, and calculating the KindRep of the constructor,
F, a linear number of times. See #14254.

2. Calculating the kind of a type constructor, in instantiateTypeRep,
requires building (allocating) a TypeRep for the kind "from scratch".
This can get pricy. When combined with point (1), we can end up with
a large amount of extra allocation deserializing very deep nests.
See #14337.

It is quite possible to speed up deserialization by structuring that process
very carefully. Unfortunately, that doesn't help dynApply or anything else
that may use typeRepKind. Since caching the kind isn't terribly expensive, it
seems better to just do that and solve all the potential problems at once.

There are two things we need to be careful about when caching kinds.

Wrinkle 1:

We want to do it eagerly. Suppose we have

  tf :: TypeRep (f :: j -> k)
  ta :: TypeRep (a :: j)

Then the cached kind of App tf ta should be eagerly evaluated to k, rather
than being stored as a thunk that will strip the (j ->) off of j -> k if
and when it is forced.

Wrinkle 2:

We need to be able to represent TypeRep Type. This is a bit tricky because
typeRepKind (typeRep @Type) = typeRep @Type, so if we actually cache the
typerep of the kind of Type, we will have a loop. One simple way to do this
is to make the cached kind fields lazy and allow TypeRep Type to be cyclical.

But we *do not* want TypeReps to have cyclical structure! Most importantly,
a cyclical structure cannot be stored in a compact region. Secondarily,
using :force in GHCi on a cyclical structure will lead to non-termination.

To avoid this trouble, we use a separate constructor for TypeRep Type.
mkTrApp is responsible for recognizing that TYPE is being applied to
'LiftedRep and produce trType; other functions must recognize that TrType
represents an application.
-}

-- Compare keys for equality

-- | @since 2.01
instance Eq (TypeRep a) where
  _ == _  = True
  {-# INLINABLE (==) #-}

instance TestEquality TypeRep where
  a `testEquality` b
    | Just HRefl <- eqTypeRep a b
    = Just Refl
    | otherwise
    = Nothing
  {-# INLINEABLE testEquality #-}

-- | @since 4.4.0.0
instance Ord (TypeRep a) where
  compare _ _ = EQ
  {-# INLINABLE compare #-}

-- | A non-indexed type representation.
data SomeTypeRep where
    SomeTypeRep :: forall k (a :: k). !(TypeRep a) %1 -> SomeTypeRep

instance Eq SomeTypeRep where
  SomeTypeRep a == SomeTypeRep b =
      case a `eqTypeRep` b of
          Just _  -> True
          Nothing -> False

instance Ord SomeTypeRep where
  SomeTypeRep a `compare` SomeTypeRep b =
    typeRepFingerprint a `compare` typeRepFingerprint b

-- | The function type constructor.
--
-- For instance,
--
-- @
-- typeRep \@(Int -> Char) === Fun (typeRep \@Int) (typeRep \@Char)
-- @
--
pattern Fun :: forall k (fun :: k). ()
            => forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                      (arg :: TYPE r1) (res :: TYPE r2).
               (k ~ Type, fun ~~ (arg -> res))
            => TypeRep arg
            -> TypeRep res
            -> TypeRep fun
pattern Fun arg res <- TrFun {trFunArg = arg, trFunRes = res, trFunMul = (eqTypeRep trMany -> Just HRefl)}
  where Fun arg res = mkTrFun trMany arg res

-- | Observe the 'Fingerprint' of a type representation
--
-- @since 4.8.0.0
typeRepFingerprint :: TypeRep a -> Fingerprint
typeRepFingerprint TrType = fpTYPELiftedRep
typeRepFingerprint (TrTyCon {trTyConFingerprint = fpr}) = fpr
typeRepFingerprint (TrApp {trAppFingerprint = fpr}) = fpr
typeRepFingerprint (TrFun {trFunFingerprint = fpr}) = fpr

-- For compiler use
mkTrType :: TypeRep Type
mkTrType = TrType

-- | Construct a representation for a type constructor
-- applied at a monomorphic kind.
--
-- Note that this is unsafe as it allows you to construct
-- ill-kinded types.
mkTrCon :: forall k (a :: k). TyCon -> [SomeTypeRep] -> TypeRep a
mkTrCon tc kind_vars = TrTyCon
    { trTyConFingerprint = fpr
    , trTyCon = tc
    , trKindVars = kind_vars
    , trTyConKind = kind }
  where
    fpr_tc  = tyConFingerprint tc
    fpr_kvs = map someTypeRepFingerprint kind_vars
    fpr     = fingerprintFingerprints (fpr_tc:fpr_kvs)
    kind    = unsafeCoerceRep $ tyConKind tc kind_vars

-- The fingerprint of Type. We don't store this in the TrType
-- constructor, so we need to build it here.
fpTYPELiftedRep :: Fingerprint
fpTYPELiftedRep = fingerprintFingerprints
      [ tyConFingerprint tyConTYPE
      , fingerprintFingerprints
        [ tyConFingerprint tyCon'BoxedRep
        , tyConFingerprint tyCon'Lifted
        ]
      ]
-- There is absolutely nothing to gain and everything to lose
-- by inlining the worker. The wrapper should inline anyway.
{-# NOINLINE fpTYPELiftedRep #-}

trTYPE :: TypeRep TYPE
trTYPE = typeRep

trLiftedRep :: TypeRep ('BoxedRep 'Lifted)
trLiftedRep = typeRep

trMany :: TypeRep 'Many
trMany = typeRep

-- | Construct a representation for a type application that is
-- NOT a saturated arrow type. This is not checked!

-- Note that this is known-key to the compiler, which uses it in desugar
-- 'Typeable' evidence.
mkTrApp :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
           TypeRep (a :: k1 -> k2)
        -> TypeRep (b :: k1)
        -> TypeRep (a b)
mkTrApp a b -- See Note [Kind caching], Wrinkle 2
  | Just HRefl <- a `eqTypeRep` trTYPE
  , Just HRefl <- b `eqTypeRep` trLiftedRep
  = TrType

  | TrFun {trFunRes = res_kind} <- typeRepKind a
  = TrApp
    { trAppFingerprint = fpr
    , trAppFun = a
    , trAppArg = b
    , trAppKind = res_kind }

  | otherwise = error ("Ill-kinded type application: "
                           ++ show (typeRepKind a))
  where
    fpr_a = typeRepFingerprint a
    fpr_b = typeRepFingerprint b
    fpr   = fingerprintFingerprints [fpr_a, fpr_b]

-- | Construct a representation for a type application that
-- may be a saturated arrow type. This is renamed to mkTrApp in
-- Type.Reflection.Unsafe
mkTrAppChecked :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
                  TypeRep (a :: k1 -> k2)
               -> TypeRep (b :: k1)
               -> TypeRep (a b)
mkTrAppChecked rep@(TrApp {trAppFun = p, trAppArg = x@TypeRep :: TypeRep x})
               (y :: TypeRep y)
  | TrTyCon {trTyCon=con} <- p
  , con == funTyCon  -- cheap check first
  , Just (IsTYPE TypeRep) <- isTYPE (typeRepKind x)
  , Just (IsTYPE (TypeRep :: TypeRep ry)) <- isTYPE (typeRepKind y)
  , Just HRefl <- typeRep @((->) x :: TYPE ry -> Type) `eqTypeRep` rep
  = mkTrFun trMany x y
mkTrAppChecked a b = mkTrApp a b

-- | A type application.
--
-- For instance,
--
-- @
-- typeRep \@(Maybe Int) === App (typeRep \@Maybe) (typeRep \@Int)
-- @
--
-- Note that this will also match a function type,
--
-- @
-- typeRep \@(Int# -> Char)
--   ===
-- App (App arrow (typeRep \@Int#)) (typeRep \@Char)
-- @
--
-- where @arrow :: TypeRep ((->) :: TYPE IntRep -> Type -> Type)@.
--
pattern App :: forall k2 (t :: k2). ()
            => forall k1 (a :: k1 -> k2) (b :: k1). (t ~ a b)
            => TypeRep a -> TypeRep b -> TypeRep t
pattern App f x <- (splitApp -> IsApp f x)
  where App f x = mkTrAppChecked f x

data AppOrCon (a :: k) where
    IsApp :: forall k k' (f :: k' -> k) (x :: k'). ()
          => TypeRep f %1 -> TypeRep x %1 -> AppOrCon (f x)
    -- See Note [Con evidence]
    IsCon :: NotApplication a => TyCon %1 -> [SomeTypeRep] %1 -> AppOrCon a

type family NotApplication (x :: k) :: Constraint where
  NotApplication (f a)
    = TypeError
      (     'Text "Cannot match this TypeRep with Con or Con': it is an application:"
      ':$$: 'Text "  " ':<>: 'ShowType (f a)
      )
  NotApplication _
    = ()

splitApp :: forall k (a :: k). ()
         => TypeRep a
         -> AppOrCon a
splitApp TrType = IsApp trTYPE trLiftedRep
splitApp (TrApp {trAppFun = f, trAppArg = x}) = IsApp f x
splitApp rep@(TrFun {trFunArg=a, trFunRes=b}) = IsApp (mkTrApp arr a) b
  where arr = bareArrow rep
splitApp (TrTyCon{trTyCon = con, trKindVars = kinds})
  = case unsafeCoerce Refl :: NotApplication a :~: (() :: Constraint) of
      Refl -> IsCon con kinds

-- | Use a 'TypeRep' as 'Typeable' evidence.
--
-- The 'TypeRep' pattern synonym brings a 'Typeable' constraint into
-- scope and can be used in place of 'withTypeable'.
--
-- @
-- f :: TypeRep a -> ..
-- f rep = withTypeable {- Typeable a in scope -}
--
-- f :: TypeRep a -> ..
-- f TypeRep = {- Typeable a in scope -}
-- @
withTypeable :: forall k (a :: k) rep (r :: TYPE rep). ()
             => TypeRep a -> (Typeable a => r) -> r
withTypeable rep k = withDict @(Typeable a) rep k

-- | Pattern match on a type constructor
pattern Con :: forall k (a :: k). ()
            => NotApplication a -- See Note [Con evidence]
            => TyCon -> TypeRep a
pattern Con con <- (splitApp -> IsCon con _)

-- | Pattern match on a type constructor including its instantiated kind
-- variables.
--
-- For instance,
--
-- @
-- App (Con' proxyTyCon ks) intRep = typeRep @(Proxy \@Int)
-- @
--
-- will bring into scope,
--
-- @
-- proxyTyCon :: TyCon
-- ks         == [someTypeRep @Type] :: [SomeTypeRep]
-- intRep     == typeRep @Int
-- @
--
pattern Con' :: forall k (a :: k). ()
             => NotApplication a -- See Note [Con evidence]
             => TyCon -> [SomeTypeRep] -> TypeRep a
pattern Con' con ks <- (splitApp -> IsCon con ks)

{-# COMPLETE App, Con  #-}
{-# COMPLETE App, Con' #-}

{- Note [Con evidence]
~~~~~~~~~~~~~~~~~~~~~~
Matching TypeRep t on Con or Con' fakes up evidence of NotApplication t.

Why should anyone care about the value of strange internal type family?
Well, almost nobody cares about it, but the pattern checker does!
For example, suppose we have TypeRep (f x) and we want to get
TypeRep f and TypeRep x. There is no chance that the Con constructor
will match, because (f x) is not a constructor, but without the
NotApplication evidence, omitting it will lead to an incomplete pattern
warning. With the evidence, the pattern checker will see that
Con wouldn't typecheck, so everything works out as it should.
-}

----------------- Observation ---------------------

-- | Observe the type constructor of a quantified type representation.
someTypeRepTyCon :: SomeTypeRep -> TyCon
someTypeRepTyCon (SomeTypeRep t) = typeRepTyCon t

-- | Observe the type constructor of a type representation
typeRepTyCon :: TypeRep a -> TyCon
typeRepTyCon TrType = tyConTYPE
typeRepTyCon (TrTyCon {trTyCon = tc}) = tc
typeRepTyCon (TrApp {trAppFun = a})   = typeRepTyCon a
typeRepTyCon (TrFun {})               = typeRepTyCon $ typeRep @(->)

-- | Type equality
--
-- @since 4.10
eqTypeRep :: forall k1 k2 (a :: k1) (b :: k2).
             TypeRep a -> TypeRep b -> Maybe (a :~~: b)
eqTypeRep a b
  | sameTypeRep a b = Just (unsafeCoerce HRefl)
  | otherwise       = Nothing
-- We want GHC to inline eqTypeRep to get rid of the Maybe
-- in the usual case that it is scrutinized immediately. We
-- split eqTypeRep into a worker and wrapper because otherwise
-- it's much larger than anything we'd want to inline.
{-# INLINABLE eqTypeRep #-}

sameTypeRep :: forall k1 k2 (a :: k1) (b :: k2).
               TypeRep a -> TypeRep b -> Bool
sameTypeRep a b = typeRepFingerprint a == typeRepFingerprint b

-------------------------------------------------------------
--
--      Computing kinds
--
-------------------------------------------------------------

-- | Observe the kind of a type.
typeRepKind :: TypeRep (a :: k) -> TypeRep k
typeRepKind TrType = TrType
typeRepKind (TrTyCon {trTyConKind = kind}) = kind
typeRepKind (TrApp {trAppKind = kind}) = kind
typeRepKind (TrFun {}) = typeRep @Type

tyConKind :: TyCon -> [SomeTypeRep] -> SomeTypeRep
tyConKind (TyCon _ _ _ _ nKindVars# kindRep) kindVars =
    let kindVarsArr :: A.Array KindBndr SomeTypeRep
        kindVarsArr = A.listArray (0, I# (nKindVars# -# 1#)) kindVars
    in instantiateKindRep kindVarsArr kindRep

instantiateKindRep :: A.Array KindBndr SomeTypeRep -> KindRep -> SomeTypeRep
instantiateKindRep vars = go
  where
    go :: KindRep -> SomeTypeRep
    go (KindRepTyConApp tc args)
      = let n_kind_args = tyConKindArgs tc
            (kind_args, ty_args) = splitAt n_kind_args args
            -- First instantiate tycon kind arguments
            tycon_app = SomeTypeRep $ mkTrCon tc (map go kind_args)
            -- Then apply remaining type arguments
            applyTy :: SomeTypeRep -> KindRep -> SomeTypeRep
            applyTy (SomeTypeRep acc) ty
              | SomeTypeRep ty' <- go ty
              = SomeTypeRep $ mkTrApp (unsafeCoerce acc) ty'
        in foldl' applyTy tycon_app ty_args
    go (KindRepVar var)
      = vars A.! var
    go (KindRepApp f a)
      = SomeTypeRep $ mkTrApp (unsafeCoerceRep $ go f) (unsafeCoerceRep $ go a)
    go (KindRepFun a b)
      = SomeTypeRep $ mkTrFun trMany (unsafeCoerceRep $ go a) (unsafeCoerceRep $ go b)
    go (KindRepTYPE (BoxedRep Lifted)) = SomeTypeRep TrType
    go (KindRepTYPE r) = unkindedTypeRep $ tYPE `kApp` runtimeRepTypeRep r
    go (KindRepTypeLitS sort s)
      = mkTypeLitFromString sort (unpackCStringUtf8# s)
    go (KindRepTypeLitD sort s)
      = mkTypeLitFromString sort s

    tYPE = kindedTypeRep @(RuntimeRep -> Type) @TYPE

unsafeCoerceRep :: SomeTypeRep -> TypeRep a
unsafeCoerceRep (SomeTypeRep r) = unsafeCoerce r

unkindedTypeRep :: SomeKindedTypeRep k -> SomeTypeRep
unkindedTypeRep (SomeKindedTypeRep x) = SomeTypeRep x

data SomeKindedTypeRep k where
    SomeKindedTypeRep :: forall k (a :: k). TypeRep a
                      %1 -> SomeKindedTypeRep k

kApp :: SomeKindedTypeRep (k -> k')
     -> SomeKindedTypeRep k
     -> SomeKindedTypeRep k'
kApp (SomeKindedTypeRep f) (SomeKindedTypeRep a) =
    SomeKindedTypeRep (mkTrApp f a)

kindedTypeRep :: forall k (a :: k). Typeable a => SomeKindedTypeRep k
kindedTypeRep = SomeKindedTypeRep (typeRep @a)

buildList :: forall k. Typeable k
          => [SomeKindedTypeRep k]
          -> SomeKindedTypeRep [k]
buildList = foldr cons nil
  where
    nil = kindedTypeRep @[k] @'[]
    cons x rest = SomeKindedTypeRep (typeRep @'(:)) `kApp` x `kApp` rest

runtimeRepTypeRep :: RuntimeRep -> SomeKindedTypeRep RuntimeRep
runtimeRepTypeRep r =
    case r of
      BoxedRep Lifted -> SomeKindedTypeRep trLiftedRep
      BoxedRep v  -> kindedTypeRep @_ @'BoxedRep
                     `kApp` levityTypeRep v
      VecRep c e  -> kindedTypeRep @_ @'VecRep
                     `kApp` vecCountTypeRep c
                     `kApp` vecElemTypeRep e
      TupleRep rs -> kindedTypeRep @_ @'TupleRep
                     `kApp` buildList (map runtimeRepTypeRep rs)
      SumRep rs   -> kindedTypeRep @_ @'SumRep
                     `kApp` buildList (map runtimeRepTypeRep rs)
      IntRep      -> rep @'IntRep
      Int8Rep     -> rep @'Int8Rep
      Int16Rep    -> rep @'Int16Rep
      Int32Rep    -> rep @'Int32Rep
      Int64Rep    -> rep @'Int64Rep
      WordRep     -> rep @'WordRep
      Word8Rep    -> rep @'Word8Rep
      Word16Rep   -> rep @'Word16Rep
      Word32Rep   -> rep @'Word32Rep
      Word64Rep   -> rep @'Word64Rep
      AddrRep     -> rep @'AddrRep
      FloatRep    -> rep @'FloatRep
      DoubleRep   -> rep @'DoubleRep
  where
    rep :: forall (a :: RuntimeRep). Typeable a => SomeKindedTypeRep RuntimeRep
    rep = kindedTypeRep @RuntimeRep @a

levityTypeRep :: Levity -> SomeKindedTypeRep Levity
levityTypeRep c =
    case c of
      Lifted   -> rep @'Lifted
      Unlifted -> rep @'Unlifted
  where
    rep :: forall (a :: Levity). Typeable a => SomeKindedTypeRep Levity
    rep = kindedTypeRep @Levity @a

vecCountTypeRep :: VecCount -> SomeKindedTypeRep VecCount
vecCountTypeRep c =
    case c of
      Vec2  -> rep @'Vec2
      Vec4  -> rep @'Vec4
      Vec8  -> rep @'Vec8
      Vec16 -> rep @'Vec16
      Vec32 -> rep @'Vec32
      Vec64 -> rep @'Vec64
  where
    rep :: forall (a :: VecCount). Typeable a => SomeKindedTypeRep VecCount
    rep = kindedTypeRep @VecCount @a

vecElemTypeRep :: VecElem -> SomeKindedTypeRep VecElem
vecElemTypeRep e =
    case e of
      Int8ElemRep     -> rep @'Int8ElemRep
      Int16ElemRep    -> rep @'Int16ElemRep
      Int32ElemRep    -> rep @'Int32ElemRep
      Int64ElemRep    -> rep @'Int64ElemRep
      Word8ElemRep    -> rep @'Word8ElemRep
      Word16ElemRep   -> rep @'Word16ElemRep
      Word32ElemRep   -> rep @'Word32ElemRep
      Word64ElemRep   -> rep @'Word64ElemRep
      FloatElemRep    -> rep @'FloatElemRep
      DoubleElemRep   -> rep @'DoubleElemRep
  where
    rep :: forall (a :: VecElem). Typeable a => SomeKindedTypeRep VecElem
    rep = kindedTypeRep @VecElem @a

bareArrow :: forall (m :: Multiplicity) (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                    (a :: TYPE r1) (b :: TYPE r2). ()
          => TypeRep (FUN m a b)
          -> TypeRep (FUN m :: TYPE r1 -> TYPE r2 -> Type)
bareArrow (TrFun _ m a b) =
    mkTrCon funTyCon [SomeTypeRep m, SomeTypeRep rep1, SomeTypeRep rep2]
  where
    rep1 = getRuntimeRep $ typeRepKind a :: TypeRep r1
    rep2 = getRuntimeRep $ typeRepKind b :: TypeRep r2
bareArrow _ = error "Data.Typeable.Internal.bareArrow: impossible"

data IsTYPE (a :: Type) where
    IsTYPE :: forall (r :: RuntimeRep). TypeRep r %1 -> IsTYPE (TYPE r)

-- | Is a type of the form @TYPE rep@?
isTYPE :: TypeRep (a :: Type) -> Maybe (IsTYPE a)
isTYPE TrType = Just (IsTYPE trLiftedRep)
isTYPE (TrApp {trAppFun=f, trAppArg=r})
  | Just HRefl <- f `eqTypeRep` typeRep @TYPE
  = Just (IsTYPE r)
isTYPE _ = Nothing

getRuntimeRep :: forall (r :: RuntimeRep). TypeRep (TYPE r) -> TypeRep r
getRuntimeRep TrType = trLiftedRep
getRuntimeRep (TrApp {trAppArg=r}) = r
getRuntimeRep _ = error "Data.Typeable.Internal.getRuntimeRep: impossible"


-------------------------------------------------------------
--
--      The Typeable class and friends
--
-------------------------------------------------------------

-- | The class 'Typeable' allows a concrete representation of a type to
-- be calculated.
class Typeable (a :: k) where
  typeRep# :: TypeRep a

typeRep :: Typeable a => TypeRep a
typeRep = typeRep#

typeOf :: Typeable a => a -> TypeRep a
typeOf _ = typeRep

-- | Takes a value of type @a@ and returns a concrete representation
-- of that type.
--
-- @since 4.7.0.0
someTypeRep :: forall proxy a. Typeable a => proxy a -> SomeTypeRep
someTypeRep _ = SomeTypeRep (typeRep :: TypeRep a)
{-# INLINE typeRep #-}

someTypeRepFingerprint :: SomeTypeRep -> Fingerprint
someTypeRepFingerprint (SomeTypeRep t) = typeRepFingerprint t

----------------- Showing TypeReps --------------------

-- This follows roughly the precedence structure described in Note [Precedence
-- in types].
instance Show (TypeRep (a :: k)) where
    showsPrec = showTypeable


showTypeable :: Int -> TypeRep (a :: k) -> ShowS
showTypeable _ TrType = showChar '*'
showTypeable _ rep
  | isListTyCon tc, [ty] <- tys =
    showChar '[' . shows ty . showChar ']'

    -- Take care only to render saturated tuple tycon applications
    -- with tuple notation (#14341).
  | isTupleTyCon tc,
    Just _ <- TrType `eqTypeRep` typeRepKind rep =
    showChar '(' . showArgs (showChar ',') tys . showChar ')'
  where (tc, tys) = splitApps rep
showTypeable _ (TrTyCon {trTyCon = tycon, trKindVars = []})
  = showTyCon tycon
showTypeable p (TrTyCon {trTyCon = tycon, trKindVars = args})
  = showParen (p > 9) $
    showTyCon tycon .
    showChar ' ' .
    showArgs (showChar ' ') args
showTypeable p (TrFun {trFunArg = x, trFunRes = r})
  = showParen (p > 8) $
    showsPrec 9 x . showString " -> " . showsPrec 8 r
showTypeable p (TrApp {trAppFun = f, trAppArg = x})
  = showParen (p > 9) $
    showsPrec 8 f .
    showChar ' ' .
    showsPrec 10 x

-- | @since 4.10.0.0
instance Show SomeTypeRep where
  showsPrec p (SomeTypeRep ty) = showsPrec p ty

splitApps :: TypeRep a -> (TyCon, [SomeTypeRep])
splitApps = go []
  where
    go :: [SomeTypeRep] -> TypeRep a -> (TyCon, [SomeTypeRep])
    go xs (TrTyCon {trTyCon = tc})
      = (tc, xs)
    go xs (TrApp {trAppFun = f, trAppArg = x})
      = go (SomeTypeRep x : xs) f
    go [] (TrFun {trFunArg = a, trFunRes = b, trFunMul = mul})
      | Just HRefl <- eqTypeRep trMany mul = (funTyCon, [SomeTypeRep a, SomeTypeRep b])
      | otherwise = errorWithoutStackTrace "Data.Typeable.Internal.splitApps: Only unrestricted functions are supported"
    go _  (TrFun {})
      = errorWithoutStackTrace "Data.Typeable.Internal.splitApps: Impossible 1"
    go [] TrType = (tyConTYPE, [SomeTypeRep trLiftedRep])
    go _ TrType
      = errorWithoutStackTrace "Data.Typeable.Internal.splitApps: Impossible 2"

-- This is incredibly shady! We don't really want to do this here; we
-- should really have the compiler reveal the TYPE TyCon directly
-- somehow. We need to construct this by hand because otherwise
-- we end up with horrible and somewhat mysterious loops trying to calculate
-- typeRep @TYPE. For the moment, we use the fact that we can get the proper
-- name of the ghc-prim package from the TyCon of LiftedRep (which we can
-- produce a TypeRep for without difficulty), and then just substitute in the
-- appropriate module and constructor names.
--
-- Prior to the introduction of BoxedRep, this was bad, but now it is
-- even worse! We have to construct several different TyCons by hand
-- so that we can build the fingerprint for TYPE ('BoxedRep 'LiftedRep).
-- If we call `typeRep @('BoxedRep 'LiftedRep)` while trying to compute
-- the fingerprint of `TYPE ('BoxedRep 'LiftedRep)`, we get a loop.
--
-- The ticket to find a better way to deal with this is
-- #14480.

tyConRuntimeRep :: TyCon
tyConRuntimeRep = mkTyCon ghcPrimPackage "GHC.Types" "RuntimeRep" 0
  (KindRepTYPE (BoxedRep Lifted))

tyConTYPE :: TyCon
tyConTYPE = mkTyCon ghcPrimPackage "GHC.Prim" "TYPE" 0
    (KindRepFun
      (KindRepTyConApp tyConRuntimeRep [])
      (KindRepTYPE (BoxedRep Lifted))
    )

tyConLevity :: TyCon
tyConLevity = mkTyCon ghcPrimPackage "GHC.Types" "Levity" 0
  (KindRepTYPE (BoxedRep Lifted))

tyCon'Lifted :: TyCon
tyCon'Lifted = mkTyCon ghcPrimPackage "GHC.Types" "'Lifted" 0
  (KindRepTyConApp tyConLevity [])

tyCon'BoxedRep :: TyCon
tyCon'BoxedRep = mkTyCon ghcPrimPackage "GHC.Types" "'BoxedRep" 0
  (KindRepFun (KindRepTyConApp tyConLevity []) (KindRepTyConApp tyConRuntimeRep []))

ghcPrimPackage :: String
ghcPrimPackage = tyConPackage (typeRepTyCon (typeRep @Bool))

funTyCon :: TyCon
funTyCon = typeRepTyCon (typeRep @(->))

isListTyCon :: TyCon -> Bool
isListTyCon tc = tc == typeRepTyCon (typeRep :: TypeRep [])

isTupleTyCon :: TyCon -> Bool
isTupleTyCon tc
  | ('(':',':_) <- tyConName tc = True
  | otherwise                   = False

-- This is only an approximation. We don't have the general
-- character-classification machinery here, so we just do our best.
-- This should work for promoted Haskell 98 data constructors and
-- for TypeOperators type constructors that begin with ASCII
-- characters, but it will miss Unicode operators.
--
-- If we wanted to catch Unicode as well, we ought to consider moving
-- GHC.Lexeme from ghc-boot-th to base. Then we could just say:
--
--   startsVarSym symb || startsConSym symb
--
-- But this is a fair deal of work just for one corner case, so I think I'll
-- leave it like this unless someone shouts.
isOperatorTyCon :: TyCon -> Bool
isOperatorTyCon tc
  | symb : _ <- tyConName tc
  , symb `elem` "!#$%&*+./<=>?@\\^|-~:" = True
  | otherwise                           = False

showTyCon :: TyCon -> ShowS
showTyCon tycon = showParen (isOperatorTyCon tycon) (shows tycon)

showArgs :: Show a => ShowS -> [a] -> ShowS
showArgs _   []     = id
showArgs _   [a]    = showsPrec 10 a
showArgs sep (a:as) = showsPrec 10 a . sep . showArgs sep as

-- | Helper to fully evaluate 'TypeRep' for use as @NFData(rnf)@ implementation
--
-- @since 4.8.0.0
rnfTypeRep :: TypeRep a -> ()
-- The TypeRep structure is almost entirely strict by definition. The
-- fingerprinting and strict kind caching ensure that everything
-- else is forced anyway. So we don't need to do anything special
-- to reduce to normal form.
rnfTypeRep !_ = ()

-- | Helper to fully evaluate 'SomeTypeRep' for use as @NFData(rnf)@
-- implementation
--
-- @since 4.10.0.0
rnfSomeTypeRep :: SomeTypeRep -> ()
rnfSomeTypeRep (SomeTypeRep r) = rnfTypeRep r

{- *********************************************************
*                                                          *
*       TyCon/TypeRep definitions for type literals        *
*              (Symbol and Nat)                            *
*                                                          *
********************************************************* -}

pattern KindRepTypeLit :: TypeLitSort -> String -> KindRep
pattern KindRepTypeLit sort t <- (getKindRepTypeLit -> Just (sort, t))
  where
    KindRepTypeLit sort t = KindRepTypeLitD sort t

{-# COMPLETE KindRepTyConApp, KindRepVar, KindRepApp, KindRepFun,
             KindRepTYPE, KindRepTypeLit #-}

getKindRepTypeLit :: KindRep -> Maybe (TypeLitSort, String)
getKindRepTypeLit (KindRepTypeLitS sort t) = Just (sort, unpackCStringUtf8# t)
getKindRepTypeLit (KindRepTypeLitD sort t) = Just (sort, t)
getKindRepTypeLit _                        = Nothing

-- | Exquisitely unsafe.
mkTyCon# :: Addr#       -- ^ package name
         -> Addr#       -- ^ module name
         -> Addr#       -- ^ the name of the type constructor
         -> Int#        -- ^ number of kind variables
         -> KindRep     -- ^ kind representation
         -> TyCon       -- ^ A unique 'TyCon' object
mkTyCon# pkg modl name n_kinds kind_rep
  | Fingerprint (W64# hi) (W64# lo) <- fingerprint
  = TyCon hi lo mod (TrNameS name) n_kinds kind_rep
  where
    mod = Module (TrNameS pkg) (TrNameS modl)
    fingerprint :: Fingerprint
    fingerprint = mkTyConFingerprint (unpackCStringUtf8# pkg)
                                     (unpackCStringUtf8# modl)
                                     (unpackCStringUtf8# name)

-- it is extremely important that this fingerprint computation
-- remains in sync with that in GHC.Tc.Instance.Typeable to ensure that type
-- equality is correct.

-- | Exquisitely unsafe.
mkTyCon :: String       -- ^ package name
        -> String       -- ^ module name
        -> String       -- ^ the name of the type constructor
        -> Int         -- ^ number of kind variables
        -> KindRep     -- ^ kind representation
        -> TyCon        -- ^ A unique 'TyCon' object
-- Used when the strings are dynamically allocated,
-- eg from binary deserialisation
mkTyCon pkg modl name (I# n_kinds) kind_rep
  | Fingerprint (W64# hi) (W64# lo) <- fingerprint
  = TyCon hi lo mod (TrNameD name) n_kinds kind_rep
  where
    mod = Module (TrNameD pkg) (TrNameD modl)
    fingerprint :: Fingerprint
    fingerprint = mkTyConFingerprint pkg modl name

-- This must match the computation done in GHC.Tc.Instance.Typeable.mkTyConRepTyConRHS.
mkTyConFingerprint :: String -- ^ package name
                   -> String -- ^ module name
                   -> String -- ^ tycon name
                   -> Fingerprint
mkTyConFingerprint pkg_name mod_name tycon_name =
        fingerprintFingerprints
        [ fingerprintString pkg_name
        , fingerprintString mod_name
        , fingerprintString tycon_name
        ]

mkTypeLitTyCon :: String -> TyCon -> TyCon
mkTypeLitTyCon name kind_tycon
  = mkTyCon "base" "GHC.TypeLits" name 0 kind
  where kind = KindRepTyConApp kind_tycon []

-- | Used to make `'Typeable' instance for things of kind Nat
typeNatTypeRep :: forall a. KnownNat a => TypeRep a
typeNatTypeRep = typeLitTypeRep (show (natVal' (proxy# @a))) tcNat

-- | Used to make `'Typeable' instance for things of kind Symbol
typeSymbolTypeRep :: forall a. KnownSymbol a => TypeRep a
typeSymbolTypeRep = typeLitTypeRep (show (symbolVal' (proxy# @a))) tcSymbol

-- | Used to make `'Typeable' instance for things of kind Char
typeCharTypeRep :: forall a. KnownChar a => TypeRep a
typeCharTypeRep = typeLitTypeRep (show (charVal' (proxy# @a))) tcChar

mkTypeLitFromString :: TypeLitSort -> String -> SomeTypeRep
mkTypeLitFromString TypeLitSymbol s =
    SomeTypeRep $ (typeLitTypeRep s tcSymbol :: TypeRep Symbol)
mkTypeLitFromString TypeLitNat s =
    SomeTypeRep $ (typeLitTypeRep s tcNat :: TypeRep Nat)
mkTypeLitFromString TypeLitChar s =
    SomeTypeRep $ (typeLitTypeRep s tcChar :: TypeRep Char)

tcSymbol :: TyCon
tcSymbol = typeRepTyCon (typeRep @Symbol)

tcNat :: TyCon
tcNat = typeRepTyCon (typeRep @Nat)

tcChar :: TyCon
tcChar = typeRepTyCon (typeRep @Char)

-- | An internal function, to make representations for type literals.
typeLitTypeRep :: forall k (a :: k). (Typeable k) =>
                  String -> TyCon -> TypeRep a
typeLitTypeRep nm kind_tycon = mkTrCon (mkTypeLitTyCon nm kind_tycon) []

-- | For compiler use.
mkTrFun :: forall (m :: Multiplicity) (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                  (a :: TYPE r1) (b :: TYPE r2).
           TypeRep m -> TypeRep a -> TypeRep b -> TypeRep ((FUN m a b) :: Type)
mkTrFun mul arg res = TrFun
    { trFunFingerprint = fpr
    , trFunMul = mul
    , trFunArg = arg
    , trFunRes = res }
  where fpr = fingerprintFingerprints [ typeRepFingerprint mul
                                      , typeRepFingerprint arg
                                      , typeRepFingerprint res]

{- $kind_instantiation

Consider a type like 'Data.Proxy.Proxy',

@
data Proxy :: forall k. k -> Type
@

One might think that one could decompose an instantiation of this type like
@Proxy Int@ into two applications,

@
'App' (App a b) c === typeRep @(Proxy Int)
@

where,

@
a = typeRep @Proxy
b = typeRep @Type
c = typeRep @Int
@

However, this isn't the case. Instead we can only decompose into an application
and a constructor,

@
'App' ('Con' proxyTyCon) (typeRep @Int) === typeRep @(Proxy Int)
@

The reason for this is that 'Typeable' can only represent /kind-monomorphic/
types. That is, we must saturate enough of @Proxy@\'s arguments to
fully determine its kind. In the particular case of @Proxy@ this means we must
instantiate the kind variable @k@ such that no @forall@-quantified variables
remain.

While it is not possible to decompose the 'Con' above into an application, it is
possible to observe the kind variable instantiations of the constructor with the
'Con\'' pattern,

@
'App' (Con' proxyTyCon kinds) _ === typeRep @(Proxy Int)
@

Here @kinds@ will be @[typeRep \@Type]@.

-}
