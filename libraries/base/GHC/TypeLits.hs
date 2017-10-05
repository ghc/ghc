{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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
{-# LANGUAGE PolyKinds #-}

{-| This module is an internal GHC module.  It declares the constants used
in the implementation of type-level natural numbers.  The programmer interface
for working with type-level naturals should be defined in a separate library.

@since 4.6.0.0
-}

module GHC.TypeLits
  ( -- * Kinds
    Nat, Symbol  -- Both declared in GHC.Types in package ghc-prim

    -- * Linking type and value level
  , N.KnownNat, natVal, natVal'
  , KnownSymbol, symbolVal, symbolVal'
  , N.SomeNat(..), SomeSymbol(..)
  , someNatVal, someSymbolVal
  , N.sameNat, sameSymbol


    -- * Functions on type literals
  , type (N.<=), type (N.<=?), type (N.+), type (N.*), type (N.^), type (N.-)
  , type N.Div, type N.Mod, type N.Log2
  , AppendSymbol
  , N.CmpNat, CmpSymbol

  -- * User-defined type errors
  , TypeError
  , ErrorMessage(..)

  ) where

import GHC.Base(Eq(..), Ord(..), Ordering(..), otherwise)
import GHC.Types( Nat, Symbol )
import GHC.Num(Integer, fromInteger)
import GHC.Base(String)
import GHC.Show(Show(..))
import GHC.Read(Read(..))
import GHC.Real(toInteger)
import GHC.Prim(magicDict, Proxy#)
import Data.Maybe(Maybe(..))
import Data.Proxy (Proxy(..))
import Data.Type.Equality((:~:)(Refl))
import Unsafe.Coerce(unsafeCoerce)

import GHC.TypeNats (KnownNat)
import qualified GHC.TypeNats as N

--------------------------------------------------------------------------------

-- | This class gives the string associated with a type-level symbol.
-- There are instances of the class for every concrete literal: "hello", etc.
--
-- @since 4.7.0.0
class KnownSymbol (n :: Symbol) where
  symbolSing :: SSymbol n

-- | @since 4.7.0.0
natVal :: forall n proxy. KnownNat n => proxy n -> Integer
natVal p = toInteger (N.natVal p)

-- | @since 4.7.0.0
symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = case symbolSing :: SSymbol n of
                SSymbol x -> x

-- | @since 4.8.0.0
natVal' :: forall n. KnownNat n => Proxy# n -> Integer
natVal' p = toInteger (N.natVal' p)

-- | @since 4.8.0.0
symbolVal' :: forall n. KnownSymbol n => Proxy# n -> String
symbolVal' _ = case symbolSing :: SSymbol n of
                SSymbol x -> x


-- | This type represents unknown type-level symbols.
data SomeSymbol = forall n. KnownSymbol n => SomeSymbol (Proxy n)
                  -- ^ @since 4.7.0.0

-- | Convert an integer into an unknown type-level natural.
--
-- @since 4.7.0.0
someNatVal :: Integer -> Maybe N.SomeNat
someNatVal n
  | n >= 0        = Just (N.someNatVal (fromInteger n))
  | otherwise     = Nothing

-- | Convert a string into an unknown type-level symbol.
--
-- @since 4.7.0.0
someSymbolVal :: String -> SomeSymbol
someSymbolVal n   = withSSymbol SomeSymbol (SSymbol n) Proxy

-- | @since 4.7.0.0
instance Eq SomeSymbol where
  SomeSymbol x == SomeSymbol y = symbolVal x == symbolVal y

-- | @since 4.7.0.0
instance Ord SomeSymbol where
  compare (SomeSymbol x) (SomeSymbol y) = compare (symbolVal x) (symbolVal y)

-- | @since 4.7.0.0
instance Show SomeSymbol where
  showsPrec p (SomeSymbol x) = showsPrec p (symbolVal x)

-- | @since 4.7.0.0
instance Read SomeSymbol where
  readsPrec p xs = [ (someSymbolVal a, ys) | (a,ys) <- readsPrec p xs ]

--------------------------------------------------------------------------------

-- | Comparison of type-level symbols, as a function.
--
-- @since 4.7.0.0
type family CmpSymbol (m :: Symbol) (n :: Symbol) :: Ordering

-- | Concatenation of type-level symbols.
--
-- @since 4.10.0.0
type family AppendSymbol (m ::Symbol) (n :: Symbol) :: Symbol

-- | A description of a custom type error.
data {-kind-} ErrorMessage = Text Symbol
                             -- ^ Show the text as is.

                           | forall t. ShowType t
                             -- ^ Pretty print the type.
                             -- @ShowType :: k -> ErrorMessage@

                           | ErrorMessage :<>: ErrorMessage
                             -- ^ Put two pieces of error message next
                             -- to each other.

                           | ErrorMessage :$$: ErrorMessage
                             -- ^ Stack two pieces of error message on top
                             -- of each other.

infixl 5 :$$:
infixl 6 :<>:

-- | The type-level equivalent of 'error'.
--
-- The polymorphic kind of this type allows it to be used in several settings.
-- For instance, it can be used as a constraint, e.g. to provide a better error
-- message for a non-existent instance,
--
-- @
-- -- in a context
-- instance TypeError (Text "Cannot 'Show' functions." :$$:
--                     Text "Perhaps there is a missing argument?")
--       => Show (a -> b) where
--     showsPrec = error "unreachable"
-- @
--
-- It can also be placed on the right-hand side of a type-level function
-- to provide an error for an invalid case,
--
-- @
-- type family ByteSize x where
--    ByteSize Word16   = 2
--    ByteSize Word8    = 1
--    ByteSize a        = TypeError (Text "The type " :<>: ShowType a :<>:
--                                   Text " is not exportable.")
-- @
--
-- @since 4.9.0.0
type family TypeError (a :: ErrorMessage) :: b where


--------------------------------------------------------------------------------

-- | We either get evidence that this function was instantiated with the
-- same type-level symbols, or 'Nothing'.
--
-- @since 4.7.0.0
sameSymbol :: (KnownSymbol a, KnownSymbol b) =>
              Proxy a -> Proxy b -> Maybe (a :~: b)
sameSymbol x y
  | symbolVal x == symbolVal y  = Just (unsafeCoerce Refl)
  | otherwise                   = Nothing

--------------------------------------------------------------------------------
-- PRIVATE:

newtype SSymbol (s :: Symbol) = SSymbol String

data WrapS a b = WrapS (KnownSymbol a => Proxy a -> b)

-- See Note [magicDictId magic] in "basicType/MkId.hs"
withSSymbol :: (KnownSymbol a => Proxy a -> b)
            -> SSymbol a      -> Proxy a -> b
withSSymbol f x y = magicDict (WrapS f) x y
