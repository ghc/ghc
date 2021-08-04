{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif
-- manual generics instances are not safe
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_GHC -fno-warn-deprecations #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Tagged
-- Copyright  : 2009-2015 Edward Kmett
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------

module Data.Tagged
    (
    -- * Tagged values
      Tagged(..)
    , retag
    , untag
    , tagSelf
    , untagSelf
    , asTaggedTypeOf
    , witness
    -- * Conversion
    , proxy
    , unproxy
    , tagWith
    -- * Proxy methods GHC dropped
    , reproxy
    ) where

#if MIN_VERSION_base(4,8,0)
import Control.Applicative (liftA2)
#else
import Control.Applicative ((<$>), liftA2, Applicative(..))
import Data.Traversable (Traversable(..))
import Data.Monoid
#endif
import Data.Bits
import Data.Foldable (Foldable(..))
#ifdef MIN_VERSION_deepseq
import Control.DeepSeq (NFData(..))
#endif
#ifdef MIN_VERSION_transformers
import Data.Functor.Classes ( Eq1(..), Ord1(..), Read1(..), Show1(..)
# if !(MIN_VERSION_transformers(0,4,0)) || MIN_VERSION_transformers(0,5,0)
                            , Eq2(..), Ord2(..), Read2(..), Show2(..)
# endif
                            )
#endif
import Control.Monad (liftM)
#if MIN_VERSION_base(4,8,0)
import Data.Bifunctor
#endif
#if MIN_VERSION_base(4,10,0)
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))
#endif
#ifdef __GLASGOW_HASKELL__
import Data.Data
#endif
import Data.Ix (Ix(..))
#if __GLASGOW_HASKELL__ < 707
import Data.Proxy
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.String (IsString(..))
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif
#endif

-- | A @'Tagged' s b@ value is a value @b@ with an attached phantom type @s@.
-- This can be used in place of the more traditional but less safe idiom of
-- passing in an undefined value with the type, because unlike an @(s -> b)@,
-- a @'Tagged' s b@ can't try to use the argument @s@ as a real value.
--
-- Moreover, you don't have to rely on the compiler to inline away the extra
-- argument, because the newtype is \"free\"
--
-- 'Tagged' has kind @k -> * -> *@ if the compiler supports @PolyKinds@, therefore
-- there is an extra @k@ showing in the instance haddocks that may cause confusion.
newtype Tagged s b = Tagged { unTagged :: b } deriving
  ( Eq, Ord, Ix, Bounded
#if __GLASGOW_HASKELL__ >= 702
  , Generic
#if __GLASGOW_HASKELL__ >= 706
  , Generic1
#endif
#endif

#if __GLASGOW_HASKELL__ >= 707
  , Typeable
#endif

  )

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 707
instance Typeable2 Tagged where
  typeOf2 _ = mkTyConApp taggedTyCon []

taggedTyCon :: TyCon
#if __GLASGOW_HASKELL__ < 704
taggedTyCon = mkTyCon "Data.Tagged.Tagged"
#else
taggedTyCon = mkTyCon3 "tagged" "Data.Tagged" "Tagged"
#endif

#endif

instance (Data s, Data b) => Data (Tagged s b) where
  gfoldl f z (Tagged b) = z Tagged `f` b
  toConstr _ = taggedConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z Tagged)
    _ -> error "gunfold"
  dataTypeOf _ = taggedDataType
  dataCast1 f = gcast1 f
  dataCast2 f = gcast2 f

taggedConstr :: Constr
taggedConstr = mkConstr taggedDataType "Tagged" [] Prefix
{-# INLINE taggedConstr #-}

taggedDataType :: DataType
taggedDataType = mkDataType "Data.Tagged.Tagged" [taggedConstr]
{-# INLINE taggedDataType #-}
#endif

instance Show b => Show (Tagged s b) where
    showsPrec n (Tagged b) = showParen (n > 10) $
        showString "Tagged " .
        showsPrec 11 b

instance Read b => Read (Tagged s b) where
    readsPrec d = readParen (d > 10) $ \r ->
        [(Tagged a, t) | ("Tagged", s) <- lex r, (a, t) <- readsPrec 11 s]

#if MIN_VERSION_base(4,9,0)
instance Semigroup a => Semigroup (Tagged s a) where
    Tagged a <> Tagged b = Tagged (a <> b)
    stimes n (Tagged a)  = Tagged (stimes n a)

instance (Semigroup a, Monoid a) => Monoid (Tagged s a) where
    mempty = Tagged mempty
    mappend = (<>)
#else
instance Monoid a => Monoid (Tagged s a) where
    mempty = Tagged mempty
    mappend (Tagged a) (Tagged b) = Tagged (mappend a b)
#endif

instance Functor (Tagged s) where
    fmap f (Tagged x) = Tagged (f x)
    {-# INLINE fmap #-}

#if MIN_VERSION_base(4,8,0)
-- this instance is provided by the bifunctors package for GHC<7.9
instance Bifunctor Tagged where
    bimap _ g (Tagged b) = Tagged (g b)
    {-# INLINE bimap #-}
#endif

#if MIN_VERSION_base(4,10,0)
-- these instances are provided by the bifunctors package for GHC<8.1
instance Bifoldable Tagged where
    bifoldMap _ g (Tagged b) = g b
    {-# INLINE bifoldMap #-}

instance Bitraversable Tagged where
    bitraverse _ g (Tagged b) = Tagged <$> g b
    {-# INLINE bitraverse #-}
#endif

#ifdef MIN_VERSION_deepseq
instance NFData b => NFData (Tagged s b) where
    rnf (Tagged b) = rnf b
#endif

#ifdef MIN_VERSION_transformers
# if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,5,0))
instance Eq1 (Tagged s) where
    eq1 = (==)

instance Ord1 (Tagged s) where
    compare1 = compare

instance Read1 (Tagged s) where
    readsPrec1 = readsPrec

instance Show1 (Tagged s) where
    showsPrec1 = showsPrec
# else
instance Eq1 (Tagged s) where
    liftEq eq (Tagged a) (Tagged b) = eq a b

instance Ord1 (Tagged s) where
    liftCompare cmp (Tagged a) (Tagged b) = cmp a b

instance Read1 (Tagged s) where
    liftReadsPrec rp _ d = readParen (d > 10) $ \r ->
        [(Tagged a, t) | ("Tagged", s) <- lex r, (a, t) <- rp 11 s]

instance Show1 (Tagged s) where
    liftShowsPrec sp _ n (Tagged b) = showParen (n > 10) $
        showString "Tagged " .
        sp 11 b

instance Eq2 Tagged where
    liftEq2 _ eq (Tagged a) (Tagged b) = eq a b

instance Ord2 Tagged where
    liftCompare2 _ cmp (Tagged a) (Tagged b) = cmp a b

instance Read2 Tagged where
    liftReadsPrec2 _ _ rp _ d = readParen (d > 10) $ \r ->
        [(Tagged a, t) | ("Tagged", s) <- lex r, (a, t) <- rp 11 s]

instance Show2 Tagged where
    liftShowsPrec2 _ _ sp _ n (Tagged b) = showParen (n > 10) $
        showString "Tagged " .
        sp 11 b
# endif
#endif

instance Applicative (Tagged s) where
    pure = Tagged
    {-# INLINE pure #-}
    Tagged f <*> Tagged x = Tagged (f x)
    {-# INLINE (<*>) #-}
    _ *> n = n
    {-# INLINE (*>) #-}

instance Monad (Tagged s) where
    return = pure
    {-# INLINE return #-}
    Tagged m >>= k = k m
    {-# INLINE (>>=) #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}

instance Foldable (Tagged s) where
    foldMap f (Tagged x) = f x
    {-# INLINE foldMap #-}
    fold (Tagged x) = x
    {-# INLINE fold #-}
    foldr f z (Tagged x) = f x z
    {-# INLINE foldr #-}
    foldl f z (Tagged x) = f z x
    {-# INLINE foldl #-}
    foldl1 _ (Tagged x) = x
    {-# INLINE foldl1 #-}
    foldr1 _ (Tagged x) = x
    {-# INLINE foldr1 #-}

instance Traversable (Tagged s) where
    traverse f (Tagged x) = Tagged <$> f x
    {-# INLINE traverse #-}
    sequenceA (Tagged x) = Tagged <$> x
    {-# INLINE sequenceA #-}
    mapM f (Tagged x) = liftM Tagged (f x)
    {-# INLINE mapM #-}
    sequence (Tagged x) = liftM Tagged x
    {-# INLINE sequence #-}

instance Enum a => Enum (Tagged s a) where
    succ = fmap succ
    pred = fmap pred
    toEnum = Tagged . toEnum
    fromEnum (Tagged x) = fromEnum x
    enumFrom (Tagged x) = map Tagged (enumFrom x)
    enumFromThen (Tagged x) (Tagged y) = map Tagged (enumFromThen x y)
    enumFromTo (Tagged x) (Tagged y) = map Tagged (enumFromTo x y)
    enumFromThenTo (Tagged x) (Tagged y) (Tagged z) =
        map Tagged (enumFromThenTo x y z)

instance Num a => Num (Tagged s a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = Tagged . fromInteger

instance Real a => Real (Tagged s a) where
    toRational (Tagged x) = toRational x

instance Integral a => Integral (Tagged s a) where
    quot = liftA2 quot
    rem = liftA2 rem
    div = liftA2 div
    mod = liftA2 mod
    quotRem (Tagged x) (Tagged y) = (Tagged a, Tagged b) where
        (a, b) = quotRem x y
    divMod (Tagged x) (Tagged y) = (Tagged a, Tagged b) where
        (a, b) = divMod x y
    toInteger (Tagged x) = toInteger x

instance Fractional a => Fractional (Tagged s a) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = Tagged . fromRational

instance Floating a => Floating (Tagged s a) where
    pi = Tagged pi
    exp = fmap exp
    log = fmap log
    sqrt = fmap sqrt
    sin = fmap sin
    cos = fmap cos
    tan = fmap tan
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    tanh = fmap tanh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh
    (**) = liftA2 (**)
    logBase = liftA2 logBase

instance RealFrac a => RealFrac (Tagged s a) where
    properFraction (Tagged x) = (a, Tagged b) where
        (a, b) = properFraction x
    truncate (Tagged x) = truncate x
    round (Tagged x) = round x
    ceiling (Tagged x) = ceiling x
    floor (Tagged x) = floor x

instance RealFloat a => RealFloat (Tagged s a) where
    floatRadix (Tagged x) = floatRadix x
    floatDigits (Tagged x) = floatDigits x
    floatRange (Tagged x) = floatRange x
    decodeFloat (Tagged x) = decodeFloat x
    encodeFloat m n = Tagged (encodeFloat m n)
    exponent (Tagged x) = exponent x
    significand = fmap significand
    scaleFloat n = fmap (scaleFloat n)
    isNaN (Tagged x) = isNaN x
    isInfinite (Tagged x) = isInfinite x
    isDenormalized (Tagged x) = isDenormalized x
    isNegativeZero (Tagged x) = isNegativeZero x
    isIEEE (Tagged x) = isIEEE x
    atan2 = liftA2 atan2

instance Bits a => Bits (Tagged s a) where
    Tagged a .&. Tagged b = Tagged (a .&. b)
    Tagged a .|. Tagged b = Tagged (a .|. b)
    xor (Tagged a) (Tagged b) = Tagged (xor a b)
    complement (Tagged a) = Tagged (complement a)
    shift (Tagged a) i = Tagged (shift a i)
    shiftL (Tagged a) i = Tagged (shiftL a i)
    shiftR (Tagged a) i = Tagged (shiftR a i)
    rotate (Tagged a) i = Tagged (rotate a i)
    rotateL (Tagged a) i = Tagged (rotateL a i)
    rotateR (Tagged a) i = Tagged (rotateR a i)
    bit i = Tagged (bit i)
    setBit (Tagged a) i = Tagged (setBit a i)
    clearBit (Tagged a) i = Tagged (clearBit a i)
    complementBit (Tagged a) i = Tagged (complementBit a i)
    testBit (Tagged a) i = testBit a i
    isSigned (Tagged a) = isSigned a
    bitSize (Tagged a) = bitSize a -- deprecated, but still required :(
#if MIN_VERSION_base(4,5,0)
    unsafeShiftL (Tagged a) i = Tagged (unsafeShiftL a i)
    unsafeShiftR (Tagged a) i = Tagged (unsafeShiftR a i)
    popCount (Tagged a) = popCount a
#endif
#if MIN_VERSION_base(4,7,0)
    bitSizeMaybe (Tagged a) = bitSizeMaybe a
    zeroBits = Tagged zeroBits
#endif

#if MIN_VERSION_base(4,7,0)
instance FiniteBits a => FiniteBits (Tagged s a) where
    finiteBitSize (Tagged a) = finiteBitSize a
# if MIN_VERSION_base(4,8,0)
    countLeadingZeros (Tagged a) = countLeadingZeros a
    countTrailingZeros (Tagged a) = countTrailingZeros a
# endif
#endif

instance IsString a => IsString (Tagged s a) where
    fromString = Tagged . fromString

instance Storable a => Storable (Tagged s a) where
    sizeOf t = sizeOf a
      where
        Tagged a = Tagged undefined `asTypeOf` t
    alignment t = alignment a
      where
        Tagged a = Tagged undefined `asTypeOf` t
    peek ptr = Tagged <$> peek (castPtr ptr)
    poke ptr (Tagged a) = poke (castPtr ptr) a
    peekElemOff ptr i = Tagged <$> peekElemOff (castPtr ptr) i
    pokeElemOff ptr i (Tagged a) = pokeElemOff (castPtr ptr) i a
    peekByteOff ptr i = Tagged <$> peekByteOff (castPtr ptr) i
    pokeByteOff ptr i (Tagged a) = pokeByteOff (castPtr ptr) i a

-- | Some times you need to change the tag you have lying around.
-- Idiomatic usage is to make a new combinator for the relationship between the
-- tags that you want to enforce, and define that combinator using 'retag'.
--
-- @
-- data Succ n
-- retagSucc :: 'Tagged' n a -> 'Tagged' (Succ n) a
-- retagSucc = 'retag'
-- @
retag :: Tagged s b -> Tagged t b
retag = Tagged . unTagged
{-# INLINE retag #-}

-- | Alias for 'unTagged'
untag :: Tagged s b -> b
untag = unTagged

-- | Tag a value with its own type.
tagSelf :: a -> Tagged a a
tagSelf = Tagged
{-# INLINE tagSelf #-}

-- | 'asTaggedTypeOf' is a type-restricted version of 'const'. It is usually used as an infix operator, and its typing forces its first argument (which is usually overloaded) to have the same type as the tag of the second.
asTaggedTypeOf :: s -> tagged s b -> s
asTaggedTypeOf = const
{-# INLINE asTaggedTypeOf #-}

witness :: Tagged a b -> a -> b
witness (Tagged b) _ = b
{-# INLINE witness #-}

-- | 'untagSelf' is a type-restricted version of 'untag'.
untagSelf :: Tagged a a -> a
untagSelf (Tagged x) = x
{-# INLINE untagSelf #-}

-- | Convert from a 'Tagged' representation to a representation
-- based on a 'Proxy'.
proxy :: Tagged s a -> proxy s -> a
proxy (Tagged x) _ = x
{-# INLINE proxy #-}

-- | Convert from a representation based on a 'Proxy' to a 'Tagged'
-- representation.
unproxy :: (Proxy s -> a) -> Tagged s a
unproxy f = Tagged (f Proxy)
{-# INLINE unproxy #-}

-- | Another way to convert a proxy to a tag.
tagWith :: proxy s -> a -> Tagged s a
tagWith _ = Tagged
{-# INLINE tagWith #-}

-- | Some times you need to change the proxy you have lying around.
-- Idiomatic usage is to make a new combinator for the relationship
-- between the proxies that you want to enforce, and define that
-- combinator using 'reproxy'.
--
-- @
-- data Succ n
-- reproxySucc :: proxy n -> 'Proxy' (Succ n)
-- reproxySucc = 'reproxy'
-- @
reproxy :: proxy a -> Proxy b
reproxy _ = Proxy
