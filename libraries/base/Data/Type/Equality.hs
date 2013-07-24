{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE PolyKinds          #-}

module Data.Type.Equality where

import Data.Data
import Data.Maybe
import GHC.Enum
import GHC.Show
import GHC.Read
import GHC.Base
import Control.Category

infix 4 :=:

-- | Propositional equality. If @a :=: b@ is inhabited by some terminating
-- value, then the type @a@ is the same as the type @b@. To use this equality
-- in practice, pattern-match on the @a :=: b@ to get out the @Refl@ constructor;
-- in the body of the pattern-match, the compiler knows that @a ~ b@.
data a :=: b where
  Refl :: a :=: a

-- with credit to Conal Elliott for 'ty', Erik Hesselink & Martijn van
-- Steenbergen for 'type-equality', Edward Kmett for 'eq', and Gabor Greif
-- for 'type-eq'

-- | Symmetry of equality
sym :: (a :=: b) -> (b :=: a)
sym Refl = Refl

-- | Transitivity of equality
trans :: (a :=: b) -> (b :=: c) -> (a :=: c)
trans Refl Refl = Refl

-- | Type-safe cast, using propositional equality
coerce :: (a :=: b) -> a -> b
coerce Refl x = x

-- | Lift equality into a unary type constructor
liftEq :: (a :=: b) -> (f a :=: f b)
liftEq Refl = Refl

-- | Lift equality into a binary type constructor
liftEq2 :: (a :=: a') -> (b :=: b') -> (f a b :=: f a' b')
liftEq2 Refl Refl = Refl

-- | Lift equality into a ternary type constructor
liftEq3 :: (a :=: a') -> (b :=: b') -> (c :=: c') -> (f a b c :=: f a' b' c')
liftEq3 Refl Refl Refl = Refl

-- | Lift equality into a quaternary type constructor
liftEq4 :: (a :=: a') -> (b :=: b') -> (c :=: c') -> (d :=: d')
        -> (f a b c d :=: f a' b' c' d')
liftEq4 Refl Refl Refl Refl = Refl

-- | Lower equality from a parameterized type into the parameters
lower :: (f a :=: f b) -> a :=: b
lower Refl = Refl

deriving instance Eq   (a :=: b)
deriving instance Show (a :=: b)
deriving instance Ord  (a :=: b)
deriving instance Typeable (:=:)
deriving instance (Typeable a, Data a) => Data (a :=: a)

instance Read (a :=: a) where
  readsPrec d = readParen (d > 10) (\r -> [(Refl, s) | ("Refl",s) <- lex r ])

instance Category (:=:) where
  id          = Refl
  Refl . Refl = Refl

instance Enum (a :=: a) where
  toEnum 0 = Refl
  toEnum _ = error "Data.Type.Equality.toEnum: bad argument"

  fromEnum Refl = 0

instance Bounded (a :=: a) where
  minBound = Refl
  maxBound = Refl

-- | This class contains types where you can learn the equality of two types
-- from information contained in /terms/. Typically, only singleton types should
-- inhabit this class.
class EqualityT f where
  -- | Conditionally prove the equality of @a@ and @b@.
  equalsT :: f a -> f b -> Maybe (a :=: b)

instance EqualityT ((:=:) a) where
  equalsT Refl Refl = Just Refl

