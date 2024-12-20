{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-|
GHC's @DataKinds@ language extension lifts data constructors, natural
numbers, and strings to the type level. This module provides the
primitives needed for working with type-level numbers (the 'Nat' kind),
strings (the 'Symbol' kind), and characters (the 'Char' kind). It also defines the 'TypeError' type
family, a feature that makes use of type-level strings to support user
defined type errors.

For now, this module is the API for working with type-level literals.
However, please note that it is a work in progress and is subject to change.
Once the design of the @DataKinds@ feature is more stable, this will be
considered only an internal GHC module, and the programmer interface for
working with type-level data will be defined in a separate library.

@since base-4.6.0.0
-}

module GHC.Internal.TypeLits
  ( -- * Kinds
    N.Natural, N.Nat, Symbol  -- Symbol is declared in GHC.Internal.Types

    -- * Linking type and value level
  , N.KnownNat(natSing), natVal, natVal'
  , KnownSymbol(symbolSing), symbolVal, symbolVal'
  , KnownChar(charSing), charVal, charVal'
  , N.SomeNat(..), SomeSymbol(..), SomeChar(..)
  , someNatVal, someSymbolVal, someCharVal
  , N.sameNat, sameSymbol, sameChar
  , N.decideNat, decideSymbol, decideChar
  , OrderingI(..)
  , N.cmpNat, cmpSymbol, cmpChar
    -- ** Singleton values
  , N.SNat (..)
  , SSymbol (UnsafeSSymbol)
      -- We export a pattern synonym instead of the real constructor:
      -- See Note [Preventing unsafe coercions for singleton types].
  , SChar (UnsafeSChar)
      -- We export a pattern synonym instead of the real constructor:
      -- See Note [Preventing unsafe coercions for singleton types].
  , pattern N.SNat, pattern SSymbol, pattern SChar
  , fromSNat, fromSSymbol, fromSChar
  , withSomeSNat, withSomeSSymbol, withSomeSChar
  , N.withKnownNat, withKnownSymbol, withKnownChar
  , N.unsafeWithSNatCo, unsafeWithSSymbolCo, unsafeWithSCharCo

    -- * Functions on type literals
  , type (N.<=), type (N.<=?), type (N.+), type (N.*), type (N.^), type (N.-)
  , type N.Div, type N.Mod, type N.Log2
  , AppendSymbol
  , N.CmpNat, CmpSymbol, CmpChar
  , ConsSymbol, UnconsSymbol
  , CharToNat, NatToChar

  -- * User-defined type errors
  , TypeError
  , ErrorMessage(..)

  ) where

import GHC.Internal.Base ( Bool(..), Eq(..), Functor(..), Ord(..), Ordering(..), String
                , (.), otherwise, withDict, Void, (++)
                , errorWithoutStackTrace)
import GHC.Internal.Types(Symbol, Char, TYPE, Coercible)
import GHC.Internal.TypeError(ErrorMessage(..), TypeError)
import GHC.Internal.Num(Integer, fromInteger)
import GHC.Internal.Show(Show(..), appPrec, appPrec1, showParen, showString)
import GHC.Internal.Read(Read(..))
import GHC.Internal.Real(toInteger)
import GHC.Internal.Prim(Proxy#)
import GHC.Internal.Data.Either (Either (..))
import GHC.Internal.Data.Maybe (Maybe(..))
import GHC.Internal.Data.Proxy (Proxy(..))
import GHC.Internal.Data.Type.Coercion (Coercion(..), TestCoercion(..))
import GHC.Internal.Data.Type.Equality((:~:)(Refl), TestEquality(..))
import GHC.Internal.Data.Type.Ord(OrderingI(..))
import GHC.Internal.Unsafe.Coerce(unsafeCoerce)

import GHC.Internal.TypeLits.Internal(CmpSymbol, CmpChar)
import qualified GHC.Internal.TypeNats as N

--------------------------------------------------------------------------------

-- | This class gives the string associated with a type-level symbol.
-- There are instances of the class for every concrete literal: "hello", etc.
--
-- @since base-4.7.0.0
class KnownSymbol (n :: Symbol) where
  symbolSing :: SSymbol n

-- | @since base-4.7.0.0
natVal :: forall n proxy. N.KnownNat n => proxy n -> Integer
natVal p = toInteger (N.natVal p)

-- | @since base-4.7.0.0
symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = case symbolSing :: SSymbol n of
                UnsafeSSymbol x -> x

-- | @since base-4.8.0.0
natVal' :: forall n. N.KnownNat n => Proxy# n -> Integer
natVal' p = toInteger (N.natVal' p)

-- | @since base-4.8.0.0
symbolVal' :: forall n. KnownSymbol n => Proxy# n -> String
symbolVal' _ = case symbolSing :: SSymbol n of
                UnsafeSSymbol x -> x


-- | This type represents unknown type-level symbols.
data SomeSymbol = forall n. KnownSymbol n => SomeSymbol (Proxy n)
                  -- ^ @since base-4.7.0.0

-- | @since base-4.16.0.0
class KnownChar (n :: Char) where
  charSing :: SChar n

charVal :: forall n proxy. KnownChar n => proxy n -> Char
charVal _ = case charSing :: SChar n of
                 UnsafeSChar x -> x

charVal' :: forall n. KnownChar n => Proxy# n -> Char
charVal' _ = case charSing :: SChar n of
                UnsafeSChar x -> x

data SomeChar = forall n. KnownChar n => SomeChar (Proxy n)

-- | Convert an integer into an unknown type-level natural.
--
-- @since base-4.7.0.0
someNatVal :: Integer -> Maybe N.SomeNat
someNatVal n
  | n >= 0        = Just (N.someNatVal (fromInteger n))
  | otherwise     = Nothing

-- | Convert a string into an unknown type-level symbol.
--
-- @since base-4.7.0.0
someSymbolVal :: String -> SomeSymbol
someSymbolVal s = withSomeSSymbol s (\(ss :: SSymbol s) ->
                  withKnownSymbol ss (SomeSymbol @s Proxy))

-- | @since base-4.7.0.0
instance Eq SomeSymbol where
  SomeSymbol x == SomeSymbol y = symbolVal x == symbolVal y

-- | @since base-4.7.0.0
instance Ord SomeSymbol where
  compare (SomeSymbol x) (SomeSymbol y) = compare (symbolVal x) (symbolVal y)

-- | @since base-4.7.0.0
instance Show SomeSymbol where
  showsPrec p (SomeSymbol x) = showsPrec p (symbolVal x)

-- | @since base-4.7.0.0
instance Read SomeSymbol where
  readsPrec p xs = [ (someSymbolVal a, ys) | (a,ys) <- readsPrec p xs ]


-- | Convert a character into an unknown type-level char.
--
-- @since base-4.16.0.0
someCharVal :: Char -> SomeChar
someCharVal c = withSomeSChar c (\(sc :: SChar c) ->
                withKnownChar sc (SomeChar @c Proxy))

instance Eq SomeChar where
  SomeChar x == SomeChar y = charVal x == charVal y

instance Ord SomeChar where
  compare (SomeChar x) (SomeChar y) = compare (charVal x) (charVal y)

instance Show SomeChar where
  showsPrec p (SomeChar x) = showsPrec p (charVal x)

instance Read SomeChar where
  readsPrec p xs = [ (someCharVal a, ys) | (a,ys) <- readsPrec p xs ]

--------------------------------------------------------------------------------

-- | Concatenation of type-level symbols.
--
-- @since base-4.10.0.0
type family AppendSymbol (m ::Symbol) (n :: Symbol) :: Symbol

-- Char-related type families

-- | Extending a type-level symbol with a type-level character
--
-- @since base-4.16.0.0
type family ConsSymbol (a :: Char) (b :: Symbol) :: Symbol

-- | This type family yields type-level `Just` storing the first character
-- of a symbol and its tail if it is defined and `Nothing` otherwise.
--
-- @since base-4.16.0.0
type family UnconsSymbol (a :: Symbol) :: Maybe (Char, Symbol)

-- | Convert a character to its Unicode code point (cf. `Data.Char.ord`)
--
-- @since base-4.16.0.0
type family CharToNat (c :: Char) :: N.Nat

-- | Convert a Unicode code point to a character (cf. `Data.Char.chr`)
--
-- @since base-4.16.0.0
type family NatToChar (n :: N.Nat) :: Char

--------------------------------------------------------------------------------

-- | We either get evidence that this function was instantiated with the
-- same type-level symbols, or 'Nothing'.
--
-- @since base-4.7.0.0
sameSymbol :: forall a b proxy1 proxy2.
              (KnownSymbol a, KnownSymbol b) =>
              proxy1 a -> proxy2 b -> Maybe (a :~: b)
sameSymbol _ _ = testEquality (symbolSing @a) (symbolSing @b)

-- | We either get evidence that this function was instantiated with the
-- same type-level symbols, or that the type-level symbols are distinct.
--
-- @since base-4.19.0.0
decideSymbol :: forall a b proxy1 proxy2.
              (KnownSymbol a, KnownSymbol b) =>
              proxy1 a -> proxy2 b -> Either (a :~: b -> Void) (a :~: b)
decideSymbol _ _ = decSymbol (symbolSing @a) (symbolSing @b)

-- Not exported: See [Not exported decNat, decSymbol and decChar]
decSymbol :: SSymbol a -> SSymbol b -> Either (a :~: b -> Void) (a :~: b)
decSymbol (UnsafeSSymbol x) (UnsafeSSymbol y)
  | x == y    = Right (unsafeCoerce Refl)
  | otherwise = Left (\Refl -> errorWithoutStackTrace ("decideSymbol: Impossible equality proof " ++ show x ++ " :~: " ++ show y))

-- | We either get evidence that this function was instantiated with the
-- same type-level characters, or 'Nothing'.
--
-- @since base-4.16.0.0
sameChar :: forall a b proxy1 proxy2.
            (KnownChar a, KnownChar b) =>
            proxy1 a -> proxy2 b -> Maybe (a :~: b)
sameChar _ _ = testEquality (charSing @a) (charSing @b)

-- | We either get evidence that this function was instantiated with the
-- same type-level characters, or that the type-level characters are distinct.
--
-- @since base-4.19.0.0
decideChar :: forall a b proxy1 proxy2.
            (KnownChar a, KnownChar b) =>
            proxy1 a -> proxy2 b -> Either (a :~: b -> Void) (a :~: b)
decideChar _ _ = decChar (charSing @a) (charSing @b)

-- Not exported: See [Not exported decNat, decSymbol and decChar]
decChar :: SChar a -> SChar b -> Either (a :~: b -> Void) (a :~: b)
decChar (UnsafeSChar x) (UnsafeSChar y)
  | x == y    = Right (unsafeCoerce Refl)
  | otherwise = Left (\Refl -> errorWithoutStackTrace ("decideChar: Impossible equality proof " ++ show x ++ " :~: " ++ show y))

-- | Like 'sameSymbol', but if the symbols aren't equal, this additionally
-- provides proof of LT or GT.
--
-- @since base-4.16.0.0
cmpSymbol :: forall a b proxy1 proxy2. (KnownSymbol a, KnownSymbol b)
          => proxy1 a -> proxy2 b -> OrderingI a b
cmpSymbol x y = case compare (symbolVal x) (symbolVal y) of
  EQ -> case unsafeCoerce (Refl, Refl) :: (CmpSymbol a b :~: 'EQ, a :~: b) of
    (Refl, Refl) -> EQI
  LT -> case unsafeCoerce Refl :: (CmpSymbol a b :~: 'LT) of
    Refl -> LTI
  GT -> case unsafeCoerce Refl :: (CmpSymbol a b :~: 'GT) of
    Refl -> GTI

-- | Like 'sameChar', but if the Chars aren't equal, this additionally
-- provides proof of LT or GT.
--
-- @since base-4.16.0.0
cmpChar :: forall a b proxy1 proxy2. (KnownChar a, KnownChar b)
        => proxy1 a -> proxy2 b -> OrderingI a b
cmpChar x y = case compare (charVal x) (charVal y) of
  EQ -> case unsafeCoerce (Refl, Refl) :: (CmpChar a b :~: 'EQ, a :~: b) of
    (Refl, Refl) -> EQI
  LT -> case unsafeCoerce Refl :: (CmpChar a b :~: 'LT) of
    Refl -> LTI
  GT -> case unsafeCoerce Refl :: (CmpChar a b :~: 'GT) of
    Refl -> GTI


--------------------------------------------------------------------------------
-- Singleton values

-- | Return the 'Integer' corresponding to @n@ in an @'SNat' n@ value.
-- The returned 'Integer' is always non-negative.
--
-- For a version of this function that returns a 'Natural' instead of an
-- 'Integer', see 'N.fromSNat' in "GHC.TypeNats".
--
-- @since base-4.18.0.0
fromSNat :: N.SNat n -> Integer
fromSNat sn = toInteger (N.fromSNat sn)

-- | Attempt to convert an 'Integer' into an @'SNat' n@ value, where @n@ is a
-- fresh type-level natural number. If the 'Integer' argument is non-negative,
-- invoke the continuation with @Just sn@, where @sn@ is the @'SNat' n@ value.
-- If the 'Integer' argument is negative, invoke the continuation with
-- 'Nothing'.
--
-- For a version of this function where the continuation uses @'SNat@ n@
-- instead of @'Maybe' ('SNat' n)@, see 'N.withSomeSNat' in "GHC.TypeNats".
--
-- @since base-4.18.0.0
withSomeSNat :: forall rep (r :: TYPE rep).
                Integer -> (forall n. Maybe (N.SNat n) -> r) -> r
withSomeSNat n k
  | n >= 0    = N.withSomeSNat (fromInteger n) (\sn -> k (Just sn))
  | otherwise = k Nothing

-- | A value-level witness for a type-level symbol. This is commonly referred
-- to as a /singleton/ type, as for each @s@, there is a single value that
-- inhabits the type @'SSymbol' s@ (aside from bottom).
--
-- The definition of 'SSymbol' is intentionally left abstract. To obtain an
-- 'SSymbol' value, use one of the following:
--
-- 1. The 'symbolSing' method of 'KnownSymbol'.
--
-- 2. The @SSymbol@ pattern synonym.
--
-- 3. The 'withSomeSSymbol' function, which creates an 'SSymbol' from a
--    'String'.
--
-- @since base-4.18.0.0
newtype SSymbol (s :: Symbol) = UnsafeSSymbol_ String
-- nominal role: See Note [Preventing unsafe coercions for singleton types]
-- in GHC.Internal.TypeNats
type role SSymbol nominal

-- | A explicitly bidirectional pattern synonym relating an 'SSymbol' to a
-- 'KnownSymbol' constraint.
--
-- As an __expression__: Constructs an explicit @'SSymbol' s@ value from an
-- implicit @'KnownSymbol' s@ constraint:
--
-- @
-- SSymbol @s :: 'KnownSymbol' s => 'SSymbol' s
-- @
--
-- As a __pattern__: Matches on an explicit @'SSymbol' s@ value bringing
-- an implicit @'KnownSymbol' s@ constraint into scope:
--
-- @
-- f :: 'SSymbol' s -> ..
-- f SSymbol = {- KnownSymbol s in scope -}
-- @
--
-- @since base-4.18.0.0
pattern SSymbol :: forall s. () => KnownSymbol s => SSymbol s
pattern SSymbol <- (knownSymbolInstance -> KnownSymbolInstance)
  where SSymbol = symbolSing
{-# COMPLETE SSymbol #-}

-- An internal data type that is only used for defining the SSymbol pattern
-- synonym.
data KnownSymbolInstance (s :: Symbol) where
  KnownSymbolInstance :: KnownSymbol s => KnownSymbolInstance s

-- An internal function that is only used for defining the SSymbol pattern
-- synonym.
knownSymbolInstance :: SSymbol s -> KnownSymbolInstance s
knownSymbolInstance ss = withKnownSymbol ss KnownSymbolInstance

-- | A pattern that can be used to manipulate the
-- 'String' that an @SSymbol s@ contains under the hood.
--
-- When using this pattern to construct an @SSymbol s@, the actual
-- @String@ being stored in the @SSymbol@ /must/ be equal to (the
-- contents of) @s@.  The compiler will not help you verify this,
-- hence the \'unsafe\' name.
pattern UnsafeSSymbol :: forall s. String -> SSymbol s
pattern UnsafeSSymbol guts = UnsafeSSymbol_ guts
{-# COMPLETE UnsafeSSymbol #-}

-- | 'unsafeWithSSymbolCo' allows uses of @coerce@ in its argument to see the
-- real representation of @SSymbol s@, without undermining the type-safety of
-- @coerce@ elsewhere in the module.
--
-- See also the documentation for 'UnsafeSSymbol'.
unsafeWithSSymbolCo
  :: forall r. ((forall s. Coercible (SSymbol s) String) => r) -> r
unsafeWithSSymbolCo v = v

-- | @since base-4.19.0.0
instance Eq (SSymbol s) where
  _ == _ = True

-- | @since base-4.19.0.0
instance Ord (SSymbol s) where
  compare _ _ = EQ

-- | @since base-4.18.0.0
instance Show (SSymbol s) where
  showsPrec p (UnsafeSSymbol s)
    = showParen (p > appPrec)
      ( showString "SSymbol @"
        . showsPrec appPrec1 s
      )

-- | @since base-4.18.0.0
instance TestEquality SSymbol where
  testEquality a b = case decSymbol a b of
    Right p -> Just p
    Left _  -> Nothing

-- | @since base-4.18.0.0
instance TestCoercion SSymbol where
  testCoercion x y = fmap (\Refl -> Coercion) (testEquality x y)

-- | Return the String corresponding to @s@ in an @'SSymbol' s@ value.
--
-- @since base-4.18.0.0
fromSSymbol :: SSymbol s -> String
fromSSymbol (UnsafeSSymbol s) = s

-- | Convert an explicit @'SSymbol' s@ value into an implicit @'KnownSymbol' s@
-- constraint.
--
-- @since base-4.18.0.0
withKnownSymbol :: forall s rep (r :: TYPE rep).
                   SSymbol s -> (KnownSymbol s => r) -> r
withKnownSymbol = withDict @(KnownSymbol s)
-- See Note [withDict] in "GHC.Tc.Instance.Class" in GHC

-- | Convert a 'String' into an @'SSymbol' s@ value, where @s@ is a fresh
-- type-level symbol.
--
-- @since base-4.18.0.0
withSomeSSymbol :: forall rep (r :: TYPE rep).
                   String -> (forall s. SSymbol s -> r) -> r
withSomeSSymbol s k = k (UnsafeSSymbol s)
{-# NOINLINE withSomeSSymbol #-}
-- For details see Note [NOINLINE withSomeSNat] in "GHC.TypeNats"
-- The issue described there applies to `withSomeSSymbol` as well.

-- | A value-level witness for a type-level character. This is commonly referred
-- to as a /singleton/ type, as for each @c@, there is a single value that
-- inhabits the type @'SChar' c@ (aside from bottom).
--
-- The definition of 'SChar' is intentionally left abstract. To obtain an
-- 'SChar' value, use one of the following:
--
-- 1. The 'charSing' method of 'KnownChar'.
--
-- 2. The @SChar@ pattern synonym.
--
-- 3. The 'withSomeSChar' function, which creates an 'SChar' from a 'Char'.
--
-- @since base-4.18.0.0
newtype SChar (s :: Char) = UnsafeSChar_ Char
-- nominal role: See Note [Preventing unsafe coercions for singleton types]
-- in GHC.Internal.TypeNats
type role SChar nominal

-- | A explicitly bidirectional pattern synonym relating an 'SChar' to a
-- 'KnownChar' constraint.
--
-- As an __expression__: Constructs an explicit @'SChar' c@ value from an
-- implicit @'KnownChar' c@ constraint:
--
-- @
-- SChar @c :: 'KnownChar' c => 'SChar' c
-- @
--
-- As a __pattern__: Matches on an explicit @'SChar' c@ value bringing
-- an implicit @'KnownChar' c@ constraint into scope:
--
-- @
-- f :: 'SChar' c -> ..
-- f SChar = {- KnownChar c in scope -}
-- @
--
-- @since base-4.18.0.0
pattern SChar :: forall c. () => KnownChar c => SChar c
pattern SChar <- (knownCharInstance -> KnownCharInstance)
  where SChar = charSing
{-# COMPLETE SChar #-}

-- An internal data type that is only used for defining the SChar pattern
-- synonym.
data KnownCharInstance (n :: Char) where
  KnownCharInstance :: KnownChar c => KnownCharInstance c

-- An internal function that is only used for defining the SChar pattern
-- synonym.
knownCharInstance :: SChar c -> KnownCharInstance c
knownCharInstance sc = withKnownChar sc KnownCharInstance

-- | A pattern that can be used to manipulate the
-- 'Char' that an @SChar c@ contains under the hood.
--
-- When using this pattern to construct an @SChar c@, the actual
-- @Char@ being stored in the @SChar c@ /must/ be equal to @c@.
-- The compiler will not help you verify this, hence the \'unsafe\' name.
pattern UnsafeSChar :: forall c. Char -> SChar c
pattern UnsafeSChar guts = UnsafeSChar_ guts
{-# COMPLETE UnsafeSChar #-}

-- | 'unsafeWithSCharCo' allows uses of @coerce@ in its argument to see the
-- real representation of @SChar c@, without undermining the type-safety of
-- @coerce@ elsewhere in the module.
--
-- See also the documentation for 'UnsafeSChar'.
unsafeWithSCharCo
  :: forall r. ((forall c. Coercible (SChar c) Char) => r) -> r
unsafeWithSCharCo v = v

-- | @since base-4.19.0.0
instance Eq (SChar c) where
  _ == _ = True

-- | @since base-4.19.0.0
instance Ord (SChar c) where
  compare _ _ = EQ

-- | @since base-4.18.0.0
instance Show (SChar c) where
  showsPrec p (UnsafeSChar c)
    = showParen (p > appPrec)
      ( showString "SChar @"
        . showsPrec appPrec1 c
      )

-- | @since base-4.18.0.0
instance TestEquality SChar where
  testEquality a b = case decChar a b of
    Right p -> Just p
    Left _  -> Nothing

-- | @since base-4.18.0.0
instance TestCoercion SChar where
  testCoercion x y = fmap (\Refl -> Coercion) (testEquality x y)

-- | Return the 'Char' corresponding to @c@ in an @'SChar' c@ value.
--
-- @since base-4.18.0.0
fromSChar :: SChar c -> Char
fromSChar (UnsafeSChar c) = c

-- | Convert an explicit @'SChar' c@ value into an implicit @'KnownChar' c@
-- constraint.
--
-- @since base-4.18.0.0
withKnownChar :: forall c rep (r :: TYPE rep).
                 SChar c -> (KnownChar c => r) -> r
withKnownChar = withDict @(KnownChar c)
-- See Note [withDict] in "GHC.Tc.Instance.Class" in GHC

-- | Convert a 'Char' into an @'SChar' c@ value, where @c@ is a fresh type-level
-- character.
--
-- @since base-4.18.0.0
withSomeSChar :: forall rep (r :: TYPE rep).
                 Char -> (forall c. SChar c -> r) -> r
withSomeSChar c k = k (UnsafeSChar c)
{-# NOINLINE withSomeSChar #-}
-- For details see Note [NOINLINE withSomeSNat] in "GHC.TypeNats"
-- The issue described there applies to `withSomeSChar` as well.
