{-# LANGUAGE CPP #-}

#ifndef HASKELL98
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

# if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
# endif

# if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
# endif

# if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE DataKinds #-}
# endif
#endif

{-# OPTIONS_GHC -fno-warn-deprecations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Instances
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Backports orphan instances which are not provided by other modules in
-- @transformers-compat@.
----------------------------------------------------------------------------
module Control.Monad.Trans.Instances () where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(a,b,c) 1
#endif

#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(a,b,c) 1
#endif

import           Control.Applicative.Backwards (Backwards(..))
import           Control.Applicative.Lift (Lift(..))
import qualified Control.Monad.Fail as Fail (MonadFail(..))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Accum (AccumT(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Cont (ContT(..))
import           Control.Monad.Trans.Error (Error(..), ErrorT(..))
import           Control.Monad.Trans.Except (ExceptT(..))
import           Control.Monad.Trans.Identity (IdentityT(..))
import           Control.Monad.Trans.List (ListT(..), mapListT)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST(..))
import           Control.Monad.Trans.Reader (ReaderT(..))
import           Control.Monad.Trans.Select (SelectT(..))
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT(..))
import qualified Control.Monad.Trans.State.Strict as Strict (StateT(..))
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT(..))
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT(..))
import           Data.Functor.Classes
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Constant (Constant(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Functor.Product (Product(..))
import           Data.Functor.Reverse (Reverse(..))
import           Data.Functor.Sum (Sum(..))

import           Control.Applicative
import           Control.Arrow (Arrow((***)))
import           Control.Monad (MonadPlus(..), liftM)
import           Control.Monad.Fix (MonadFix(..))
import           Data.Bits
import           Data.Foldable (Foldable(..))
import           Data.Ix (Ix(..))
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid(..))
import           Data.String (IsString(fromString))
import           Data.Traversable (Traversable(..))
import           Foreign (Storable(..), castPtr)

#if MIN_VERSION_base(4,4,0)
import           Control.Monad.Zip (MonadZip(..))
#endif

#if MIN_VERSION_base(4,7,0)
import           Data.Proxy (Proxy(..))
#endif

#if MIN_VERSION_base(4,8,0)
import           Data.Bifunctor (Bifunctor(..))
#endif

#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semigroup (Semigroup(..))
#endif

#if MIN_VERSION_base(4,10,0)
import           Data.Bifoldable (Bifoldable(..))
import           Data.Bitraversable (Bitraversable(..))
#endif

#ifndef HASKELL98
import           Data.Data (Data)
import           Data.Typeable

# ifdef GENERIC_DERIVING
import           Generics.Deriving.Base
# elif __GLASGOW_HASKELL__ >= 702
import           GHC.Generics
# endif
#endif

#if !(MIN_VERSION_transformers(0,3,0))
-- Foldable/Traversable instances
instance (Foldable f) => Foldable (ErrorT e f) where
    foldMap f (ErrorT a) = foldMap (either (const mempty) f) a

instance (Traversable f) => Traversable (ErrorT e f) where
    traverse f (ErrorT a) =
        ErrorT <$> traverse (either (pure . Left) (fmap Right . f)) a

instance (Foldable f) => Foldable (IdentityT f) where
    foldMap f (IdentityT a) = foldMap f a

instance (Traversable f) => Traversable (IdentityT f) where
    traverse f (IdentityT a) = IdentityT <$> traverse f a

instance (Foldable f) => Foldable (ListT f) where
    foldMap f (ListT a) = foldMap (foldMap f) a

instance (Traversable f) => Traversable (ListT f) where
    traverse f (ListT a) = ListT <$> traverse (traverse f) a

instance (Foldable f) => Foldable (MaybeT f) where
    foldMap f (MaybeT a) = foldMap (foldMap f) a

instance (Traversable f) => Traversable (MaybeT f) where
    traverse f (MaybeT a) = MaybeT <$> traverse (traverse f) a

instance (Foldable f) => Foldable (Lazy.WriterT w f) where
    foldMap f = foldMap (f . fst) . Lazy.runWriterT

instance (Traversable f) => Traversable (Lazy.WriterT w f) where
    traverse f = fmap Lazy.WriterT . traverse f' . Lazy.runWriterT where
       f' (a, b) = fmap (\ c -> (c, b)) (f a)

instance (Foldable f) => Foldable (Strict.WriterT w f) where
    foldMap f = foldMap (f . fst) . Strict.runWriterT

instance (Traversable f) => Traversable (Strict.WriterT w f) where
    traverse f = fmap Strict.WriterT . traverse f' . Strict.runWriterT where
       f' (a, b) = fmap (\ c -> (c, b)) (f a)

-- MonadFix instances for IdentityT and MaybeT
instance (MonadFix m) => MonadFix (IdentityT m) where
    mfix f = IdentityT (mfix (runIdentityT . f))

instance (MonadFix m) => MonadFix (MaybeT m) where
    mfix f = MaybeT (mfix (runMaybeT . f . fromMaybe bomb))
      where bomb = error "mfix (MaybeT): inner computation returned Nothing"

# if !(MIN_VERSION_base(4,9,0))
-- Monad instances for Product
instance (Monad f, Monad g) => Monad (Product f g) where
    return x = Pair (return x) (return x)
    Pair m n >>= f = Pair (m >>= fstP . f) (n >>= sndP . f)
      where
        fstP (Pair a _) = a
        sndP (Pair _ b) = b

instance (MonadPlus f, MonadPlus g) => MonadPlus (Product f g) where
    mzero = Pair mzero mzero
    Pair x1 y1 `mplus` Pair x2 y2 = Pair (x1 `mplus` x2) (y1 `mplus` y2)

instance (MonadFix f, MonadFix g) => MonadFix (Product f g) where
    mfix f = Pair (mfix (fstP . f)) (mfix (sndP . f))
      where
        fstP (Pair a _) = a
        sndP (Pair _ b) = b
# endif
#endif

#if !(MIN_VERSION_transformers(0,4,0))
-- Alternative IO instance
# if !(MIN_VERSION_base(4,9,0))
-- The version bounds of transformers prior to 0.4.0.0 should prevent this
-- instance from being compiled on base-4.8.0.0 and later, but we'll put
-- a check here just to be safe.
instance Alternative IO where
    empty = mzero
    (<|>) = mplus
# endif
#endif

#if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,4,3))
-- transformers-0.4-specific Eq1, Ord1, Read1, and Show1 instances for Const
instance (Eq a) => Eq1 (Const a) where
    eq1 (Const x) (Const y) = x == y
instance (Ord a) => Ord1 (Const a) where
    compare1 (Const x) (Const y) = compare x y
instance (Read a) => Read1 (Const a) where
    readsPrec1 = readsData $ readsUnary "Const" Const
instance (Show a) => Show1 (Const a) where
    showsPrec1 d (Const x) = showsUnary "Const" d x
#endif

#if !(MIN_VERSION_transformers(0,5,0)) \
  || (MIN_VERSION_transformers(0,5,0) && !(MIN_VERSION_base(4,9,0)))
-- MonadFail instances
instance (Fail.MonadFail m) => Fail.MonadFail (ContT r m) where
    fail msg = ContT $ \ _ -> Fail.fail msg
    {-# INLINE fail #-}

instance (Monad m, Error e) => Fail.MonadFail (ErrorT e m) where
    fail msg = ErrorT $ return (Left (strMsg msg))

instance (Fail.MonadFail m) => Fail.MonadFail (IdentityT m) where
    fail msg = IdentityT $ Fail.fail msg
    {-# INLINE fail #-}

instance (Monad m) => Fail.MonadFail (ListT m) where
    fail _ = ListT $ return []
    {-# INLINE fail #-}

instance (Monad m) => Fail.MonadFail (MaybeT m) where
    fail _ = MaybeT (return Nothing)
    {-# INLINE fail #-}

instance (Fail.MonadFail m) => Fail.MonadFail (ReaderT r m) where
    fail msg = lift (Fail.fail msg)
    {-# INLINE fail #-}

instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (Lazy.RWST r w s m) where
    fail msg = Lazy.RWST $ \ _ _ -> Fail.fail msg
    {-# INLINE fail #-}

instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (Strict.RWST r w s m) where
    fail msg = Strict.RWST $ \ _ _ -> Fail.fail msg
    {-# INLINE fail #-}

instance (Fail.MonadFail m) => Fail.MonadFail (Lazy.StateT s m) where
    fail str = Lazy.StateT $ \ _ -> Fail.fail str
    {-# INLINE fail #-}

instance (Fail.MonadFail m) => Fail.MonadFail (Strict.StateT s m) where
    fail str = Strict.StateT $ \ _ -> Fail.fail str
    {-# INLINE fail #-}

instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (Lazy.WriterT w m) where
    fail msg = Lazy.WriterT $ Fail.fail msg
    {-# INLINE fail #-}

instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (Strict.WriterT w m) where
    fail msg = Strict.WriterT $ Fail.fail msg
    {-# INLINE fail #-}

# if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_base(4,9,0))
instance (Fail.MonadFail m) => Fail.MonadFail (ExceptT e m) where
    fail = ExceptT . Fail.fail
    {-# INLINE fail #-}
# endif

# if MIN_VERSION_transformers(0,5,3) && !(MIN_VERSION_base(4,9,0))
instance (Monoid w, Functor m, Fail.MonadFail m) => Fail.MonadFail (AccumT w m) where
    fail msg = AccumT $ const (Fail.fail msg)
    {-# INLINE fail #-}

instance (Fail.MonadFail m) => Fail.MonadFail (SelectT r m) where
    fail msg = lift (Fail.fail msg)
    {-# INLINE fail #-}
# endif
#endif

#if !(MIN_VERSION_transformers(0,5,0))
-- Monoid Constant instance
instance (Monoid a) => Monoid (Constant a b) where
    mempty = Constant mempty
    Constant x `mappend` Constant y = Constant (x `mappend` y)

-- MonadZip instances
# if MIN_VERSION_base(4,4,0)
instance (MonadZip m) => MonadZip (IdentityT m) where
    mzipWith f (IdentityT a) (IdentityT b) = IdentityT (mzipWith f a b)

instance (MonadZip m) => MonadZip (ListT m) where
    mzipWith f (ListT a) (ListT b) = ListT $ mzipWith (zipWith f) a b

instance (MonadZip m) => MonadZip (MaybeT m) where
    mzipWith f (MaybeT a) (MaybeT b) = MaybeT $ mzipWith (liftA2 f) a b

instance (MonadZip m) => MonadZip (ReaderT r m) where
    mzipWith f (ReaderT m) (ReaderT n) = ReaderT $ \ a ->
        mzipWith f (m a) (n a)

instance (Monoid w, MonadZip m) => MonadZip (Lazy.WriterT w m) where
    mzipWith f (Lazy.WriterT x) (Lazy.WriterT y) = Lazy.WriterT $
        mzipWith (\ ~(a, w) ~(b, w') -> (f a b, w `mappend` w')) x y

instance (Monoid w, MonadZip m) => MonadZip (Strict.WriterT w m) where
    mzipWith f (Strict.WriterT x) (Strict.WriterT y) = Strict.WriterT $
        mzipWith (\ (a, w) (b, w') -> (f a b, w `mappend` w')) x y

#  if !(MIN_VERSION_base(4,8,0))
instance MonadZip Identity where
    mzipWith f (Identity x) (Identity y) = Identity (f x y)
    munzip (Identity (a, b)) = (Identity a, Identity b)
#  endif

#  if !(MIN_VERSION_base(4,9,0))
instance (MonadZip f, MonadZip g) => MonadZip (Product f g) where
    mzipWith f (Pair x1 y1) (Pair x2 y2) = Pair (mzipWith f x1 x2) (mzipWith f y1 y2)
#  endif
# endif

# if MIN_VERSION_base(4,8,0)
-- Bifunctor Constant instance
instance Bifunctor Constant where
    first f (Constant x) = Constant (f x)
    second _ (Constant x) = Constant x
# else
-- Monoid Identity instance
instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend (Identity x) (Identity y) = Identity (mappend x y)
# endif

# ifndef HASKELL98
-- Typeable instances
#  if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
deriving instance Typeable Backwards
deriving instance Typeable Constant
deriving instance Typeable ContT
deriving instance Typeable ErrorT
deriving instance Typeable IdentityT
deriving instance Typeable Lift
deriving instance Typeable ListT
deriving instance Typeable MaybeT
deriving instance Typeable MonadTrans
deriving instance Typeable Lazy.RWST
deriving instance Typeable Strict.RWST
deriving instance Typeable ReaderT
deriving instance Typeable Reverse
deriving instance Typeable Lazy.StateT
deriving instance Typeable Strict.StateT

#   if !(MIN_VERSION_base(4,9,0))
deriving instance Typeable Compose
deriving instance Typeable MonadIO
deriving instance Typeable Product
#   endif
#  endif

-- Identity instances
#  if !(MIN_VERSION_base(4,8,0))
deriving instance Typeable1 Identity
deriving instance Data a => Data (Identity a)
#   if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable 'Identity
#   endif
#  endif

#  if !(MIN_VERSION_base(4,9,0))
#   if __GLASGOW_HASKELL__ >= 708
-- Data instances for Compose and Product
deriving instance (Data (f (g a)), Typeable f, Typeable g, Typeable a)
               => Data (Compose (f :: * -> *) (g :: * -> *) (a :: *))
deriving instance (Data (f a), Data (g a), Typeable f, Typeable g, Typeable a)
               => Data (Product (f :: * -> *) (g :: * -> *) (a :: *))

#    if MIN_VERSION_transformers(0,4,0)
-- Typeable/Data instances for Sum
-- These are also present in Data.Functor.Sum in transformers-compat, but only
-- these are reachable if using @transformers-0.4.0.0@
deriving instance Typeable Sum
deriving instance (Data (f a), Data (g a), Typeable f, Typeable g, Typeable a)
               => Data (Sum (f :: * -> *) (g :: * -> *) (a :: *))
#    endif
#   endif
#  endif
# endif
#endif

#if !(MIN_VERSION_transformers(0,5,1))
# if !(MIN_VERSION_base(4,8,0))
instance (Bounded a) => Bounded (Identity a) where
    minBound = Identity minBound
    maxBound = Identity maxBound

instance (Enum a) => Enum (Identity a) where
    succ (Identity x)     = Identity (succ x)
    pred (Identity x)     = Identity (pred x)
    toEnum i              = Identity (toEnum i)
    fromEnum (Identity x) = fromEnum x
    enumFrom (Identity x) = map Identity (enumFrom x)
    enumFromThen (Identity x) (Identity y) = map Identity (enumFromThen x y)
    enumFromTo   (Identity x) (Identity y) = map Identity (enumFromTo   x y)
    enumFromThenTo (Identity x) (Identity y) (Identity z) =
        map Identity (enumFromThenTo x y z)

instance (Ix a) => Ix (Identity a) where
    range     (Identity x, Identity y) = map Identity (range (x, y))
    index     (Identity x, Identity y) (Identity i) = index     (x, y) i
    inRange   (Identity x, Identity y) (Identity e) = inRange   (x, y) e
    rangeSize (Identity x, Identity y) = rangeSize (x, y)

instance (Storable a) => Storable (Identity a) where
    sizeOf    (Identity x)       = sizeOf x
    alignment (Identity x)       = alignment x
    peekElemOff p i              = fmap Identity (peekElemOff (castPtr p) i)
    pokeElemOff p i (Identity x) = pokeElemOff (castPtr p) i x
    peekByteOff p i              = fmap Identity (peekByteOff p i)
    pokeByteOff p i (Identity x) = pokeByteOff p i x
    peek p                       = fmap runIdentity (peek (castPtr p))
    poke p (Identity x)          = poke (castPtr p) x
# endif
#endif

#if !(MIN_VERSION_transformers(0,5,3))
# if !(MIN_VERSION_base(4,9,0))
#  if MIN_VERSION_base(4,7,0)
-- Data.Proxy
#   if defined(TRANSFORMERS_FOUR)
instance Eq1 Proxy where
    eq1 _ _ = True

instance Ord1 Proxy where
    compare1 _ _ = EQ

instance Show1 Proxy where
    showsPrec1 _ _ = showString "Proxy"

instance Read1 Proxy where
    readsPrec1 d =
        readParen (d > 10) (\r -> [(Proxy, s) | ("Proxy",s) <- lex r ])
#   elif MIN_VERSION_transformers(0,5,0)
instance Eq1 Proxy where
    liftEq _ _ _ = True

instance Ord1 Proxy where
    liftCompare _ _ _ = EQ

instance Show1 Proxy where
    liftShowsPrec _ _ _ _ = showString "Proxy"

instance Read1 Proxy where
    liftReadsPrec _ _ d =
        readParen (d > 10) (\r -> [(Proxy, s) | ("Proxy",s) <- lex r ])
#   endif
#  endif
# endif

# if !(MIN_VERSION_base(4,8,0))
-- Data.Functor.Identity
instance (Bits a) => Bits (Identity a) where
    Identity x .&. Identity y     = Identity (x .&. y)
    Identity x .|. Identity y     = Identity (x .|. y)
    xor (Identity x) (Identity y) = Identity (xor x y)
    complement   (Identity x)     = Identity (complement x)
    shift        (Identity x) i   = Identity (shift    x i)
    rotate       (Identity x) i   = Identity (rotate   x i)
    setBit       (Identity x) i   = Identity (setBit   x i)
    clearBit     (Identity x) i   = Identity (clearBit x i)
    shiftL       (Identity x) i   = Identity (shiftL   x i)
    shiftR       (Identity x) i   = Identity (shiftR   x i)
    rotateL      (Identity x) i   = Identity (rotateL  x i)
    rotateR      (Identity x) i   = Identity (rotateR  x i)
    testBit      (Identity x) i   = testBit x i
    bitSize      (Identity x)     = bitSize x
    isSigned     (Identity x)     = isSigned x
    bit i                         = Identity (bit i)
#  if MIN_VERSION_base(4,5,0)
    unsafeShiftL (Identity x) i   = Identity (unsafeShiftL x i)
    unsafeShiftR (Identity x) i   = Identity (unsafeShiftR x i)
    popCount     (Identity x)     = popCount x
#  endif
#  if MIN_VERSION_base(4,7,0)
    zeroBits                      = Identity zeroBits
    bitSizeMaybe (Identity x)     = bitSizeMaybe x
#  endif

#  if MIN_VERSION_base(4,7,0)
instance (FiniteBits a) => FiniteBits (Identity a) where
    finiteBitSize (Identity x) = finiteBitSize x
#  endif

instance (Floating a) => Floating (Identity a) where
    pi                                = Identity pi
    exp   (Identity x)                = Identity (exp x)
    log   (Identity x)                = Identity (log x)
    sqrt  (Identity x)                = Identity (sqrt x)
    sin   (Identity x)                = Identity (sin x)
    cos   (Identity x)                = Identity (cos x)
    tan   (Identity x)                = Identity (tan x)
    asin  (Identity x)                = Identity (asin x)
    acos  (Identity x)                = Identity (acos x)
    atan  (Identity x)                = Identity (atan x)
    sinh  (Identity x)                = Identity (sinh x)
    cosh  (Identity x)                = Identity (cosh x)
    tanh  (Identity x)                = Identity (tanh x)
    asinh (Identity x)                = Identity (asinh x)
    acosh (Identity x)                = Identity (acosh x)
    atanh (Identity x)                = Identity (atanh x)
    Identity x ** Identity y          = Identity (x ** y)
    logBase (Identity x) (Identity y) = Identity (logBase x y)

instance (Fractional a) => Fractional (Identity a) where
    Identity x / Identity y = Identity (x / y)
    recip (Identity x)      = Identity (recip x)
    fromRational r          = Identity (fromRational r)

instance (IsString a) => IsString (Identity a) where
    fromString s = Identity (fromString s)

instance (Integral a) => Integral (Identity a) where
    quot    (Identity x) (Identity y) = Identity (quot x y)
    rem     (Identity x) (Identity y) = Identity (rem  x y)
    div     (Identity x) (Identity y) = Identity (div  x y)
    mod     (Identity x) (Identity y) = Identity (mod  x y)
    quotRem (Identity x) (Identity y) = (Identity *** Identity) (quotRem x y)
    divMod  (Identity x) (Identity y) = (Identity *** Identity) (divMod  x y)
    toInteger (Identity x)            = toInteger x

instance (Num a) => Num (Identity a) where
    Identity x + Identity y = Identity (x + y)
    Identity x - Identity y = Identity (x - y)
    Identity x * Identity y = Identity (x * y)
    negate (Identity x)     = Identity (negate x)
    abs    (Identity x)     = Identity (abs    x)
    signum (Identity x)     = Identity (signum x)
    fromInteger n           = Identity (fromInteger n)

instance (Real a) => Real (Identity a) where
    toRational (Identity x) = toRational x

instance (RealFloat a) => RealFloat (Identity a) where
    floatRadix     (Identity x)     = floatRadix     x
    floatDigits    (Identity x)     = floatDigits    x
    floatRange     (Identity x)     = floatRange     x
    decodeFloat    (Identity x)     = decodeFloat    x
    exponent       (Identity x)     = exponent       x
    isNaN          (Identity x)     = isNaN          x
    isInfinite     (Identity x)     = isInfinite     x
    isDenormalized (Identity x)     = isDenormalized x
    isNegativeZero (Identity x)     = isNegativeZero x
    isIEEE         (Identity x)     = isIEEE         x
    significand    (Identity x)     = significand (Identity x)
    scaleFloat s   (Identity x)     = Identity (scaleFloat s x)
    encodeFloat m n                 = Identity (encodeFloat m n)
    atan2 (Identity x) (Identity y) = Identity (atan2 x y)

instance (RealFrac a) => RealFrac (Identity a) where
    properFraction (Identity x) = (id *** Identity) (properFraction x)
    truncate       (Identity x) = truncate x
    round          (Identity x) = round    x
    ceiling        (Identity x) = ceiling  x
    floor          (Identity x) = floor    x
# endif

# if MIN_VERSION_transformers(0,3,0)
-- Data.Functor.Reverse
instance (Monad m) => Monad (Reverse m) where
    return a = Reverse (return a)
    {-# INLINE return #-}
    m >>= f = Reverse (getReverse m >>= getReverse . f)
    {-# INLINE (>>=) #-}
    fail msg = Reverse (fail msg)
    {-# INLINE fail #-}

instance (Fail.MonadFail m) => Fail.MonadFail (Reverse m) where
    fail msg = Reverse (Fail.fail msg)
    {-# INLINE fail #-}

instance (MonadPlus m) => MonadPlus (Reverse m) where
    mzero = Reverse mzero
    {-# INLINE mzero #-}
    Reverse x `mplus` Reverse y = Reverse (x `mplus` y)
    {-# INLINE mplus #-}
# endif
#endif

#if !(MIN_VERSION_transformers(0,5,4))
# if MIN_VERSION_base(4,10,0)
instance Bifoldable Constant where
    bifoldMap f _ (Constant a) = f a
    {-# INLINE bifoldMap #-}

instance Bitraversable Constant where
    bitraverse f _ (Constant a) = Constant <$> f a
    {-# INLINE bitraverse #-}
# endif
#endif

#if !(MIN_VERSION_transformers(0,5,5))
# if MIN_VERSION_base(4,9,0)
instance (Semigroup.Semigroup a) => Semigroup.Semigroup (Constant a b) where
    Constant x <> Constant y = Constant (x Semigroup.<> y)
    {-# INLINE (<>) #-}
# endif

instance (MonadFix m) => MonadFix (ListT m) where
    mfix f = ListT $ mfix (runListT . f . head) >>= \ xs -> case xs of
        [] -> return []
        x:_ -> liftM (x:) (runListT (mfix (mapListT (liftM tail) . f)))
    {-# INLINE mfix #-}
#endif

-- Generic(1) instances
#ifndef HASKELL98
# if (!(MIN_VERSION_transformers(0,5,0)) && (__GLASGOW_HASKELL__ >= 702 || defined(GENERIC_DERIVING))) \
    || (MIN_VERSION_transformers(0,5,0)  &&  __GLASGOW_HASKELL__ < 702  && defined(GENERIC_DERIVING))

#  if !(MIN_VERSION_base(4,8,0))
instance Generic (Identity a) where
    type Rep (Identity a) = D1 MDIdentity (C1 MCIdentity (S1 MSIdentity (Rec0 a)))
    from (Identity x) = M1 (M1 (M1 (K1 x)))
    to (M1 (M1 (M1 (K1 x)))) = Identity x

instance Generic1 Identity where
    type Rep1 Identity = D1 MDIdentity (C1 MCIdentity (S1 MSIdentity Par1))
    from1 (Identity x) = M1 (M1 (M1 (Par1 x)))
    to1 (M1 (M1 (M1 x))) = Identity (unPar1 x)

data MDIdentity
data MCIdentity
data MSIdentity

instance Datatype MDIdentity where
  datatypeName _ = "Identity"
  moduleName _ = "Data.Functor.Identity"
#   if __GLASGOW_HASKELL__ >= 708
  isNewtype _ = True
#   endif

instance Constructor MCIdentity where
  conName _ = "Identity"
  conIsRecord _ = True

instance Selector MSIdentity where
  selName _ = "runIdentity"
#  endif

#  if !(MIN_VERSION_base(4,9,0))
-- Generic(1) instances for Compose
instance Generic (Compose f g a) where
    type Rep (Compose f g a) =
      D1 MDCompose
        (C1 MCCompose
          (S1 MSCompose (Rec0 (f (g a)))))
    from (Compose x) = M1 (M1 (M1 (K1 x)))
    to (M1 (M1 (M1 (K1 x)))) = Compose x

instance Functor f => Generic1 (Compose f g) where
    type Rep1 (Compose f g) =
      D1 MDCompose
        (C1 MCCompose
          (S1 MSCompose (f :.: Rec1 g)))
    from1 (Compose x) = M1 (M1 (M1 (Comp1 (fmap Rec1 x))))
    to1 (M1 (M1 (M1 x))) = Compose (fmap unRec1 (unComp1 x))

data MDCompose
data MCCompose
data MSCompose

instance Datatype MDCompose where
    datatypeName _ = "Compose"
    moduleName   _ = "Data.Functor.Compose"
#   if __GLASGOW_HASKELL__ >= 708
    isNewtype    _ = True
#   endif

instance Constructor MCCompose where
    conName     _ = "Compose"
    conIsRecord _ = True

instance Selector MSCompose where
    selName _ = "getCompose"

-- Generic(1) instances for Product
instance Generic (Product f g a) where
    type Rep (Product f g a) =
      D1 MDProduct
        (C1 MCPair
          (S1 NoSelector (Rec0 (f a)) :*: S1 NoSelector (Rec0 (g a))))
    from (Pair f g) = M1 (M1 (M1 (K1 f) :*: M1 (K1 g)))
    to (M1 (M1 (M1 (K1 f) :*: M1 (K1 g)))) = Pair f g

instance Generic1 (Product f g) where
    type Rep1 (Product f g) =
      D1 MDProduct
        (C1 MCPair
          (S1 NoSelector (Rec1 f) :*: S1 NoSelector (Rec1 g)))
    from1 (Pair f g) = M1 (M1 (M1 (Rec1 f) :*: M1 (Rec1 g)))
    to1 (M1 (M1 (M1 f :*: M1 g))) = Pair (unRec1 f) (unRec1 g)

data MDProduct
data MCPair

instance Datatype MDProduct where
    datatypeName _ = "Product"
    moduleName   _ = "Data.Functor.Product"

instance Constructor MCPair where
    conName _ = "Pair"

#   if MIN_VERSION_transformers(0,4,0)
-- Generic(1) instances for Sum
-- These are also present in Data.Functor.Sum in transformers-compat, but only
-- these are reachable if using @transformers-0.4.0.0@ or later
instance Generic (Sum f g a) where
    type Rep (Sum f g a) =
      D1 MDSum (C1 MCInL (S1 NoSelector (Rec0 (f a)))
            :+: C1 MCInR (S1 NoSelector (Rec0 (g a))))
    from (InL f) = M1 (L1 (M1 (M1 (K1 f))))
    from (InR g) = M1 (R1 (M1 (M1 (K1 g))))
    to (M1 (L1 (M1 (M1 (K1 f))))) = InL f
    to (M1 (R1 (M1 (M1 (K1 g))))) = InR g

instance Generic1 (Sum f g) where
    type Rep1 (Sum f g) =
      D1 MDSum (C1 MCInL (S1 NoSelector (Rec1 f))
            :+: C1 MCInR (S1 NoSelector (Rec1 g)))
    from1 (InL f) = M1 (L1 (M1 (M1 (Rec1 f))))
    from1 (InR g) = M1 (R1 (M1 (M1 (Rec1 g))))
    to1 (M1 (L1 (M1 (M1 f)))) = InL (unRec1 f)
    to1 (M1 (R1 (M1 (M1 g)))) = InR (unRec1 g)

data MDSum
data MCInL
data MCInR

instance Datatype MDSum where
    datatypeName _ = "Sum"
    moduleName   _ = "Data.Functor.Sum"

instance Constructor MCInL where
    conName _ = "InL"

instance Constructor MCInR where
    conName _ = "InR"
#   endif
#  endif

# endif
#endif
