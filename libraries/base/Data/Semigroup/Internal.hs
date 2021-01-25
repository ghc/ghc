{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Auxiliary definitions for 'Semigroup'
--
-- This module provides some @newtype@ wrappers and helpers which are
-- reexported from the "Data.Semigroup" module or imported directly
-- by some other modules.
--
-- This module also provides internal definitions related to the
-- 'Semigroup' class some.
--
-- This module exists mostly to simplify or workaround import-graph
-- issues; there is also a .hs-boot file to allow "GHC.Base" and other
-- modules to import method default implementations for 'stimes'
--
-- @since 4.11.0.0
module Data.Semigroup.Internal where

import GHC.Base hiding (Any)
import GHC.Enum
import qualified GHC.List as List
import GHC.Num
import GHC.Read
import GHC.Show
import GHC.Generics
import GHC.Real

-- | This is a valid definition of 'stimes' for an idempotent 'Semigroup'.
--
-- When @x <> x = x@, this definition should be preferred, because it
-- works in \(\mathcal{O}(1)\) rather than \(\mathcal{O}(\log n)\).
stimesIdempotent :: Integral b => b -> a -> a
stimesIdempotent n x
  | n <= 0 = errorWithoutStackTrace "stimesIdempotent: positive multiplier expected"
  | otherwise = x

-- | This is a valid definition of 'stimes' for an idempotent 'Monoid'.
--
-- When @mappend x x = x@, this definition should be preferred, because it
-- works in \(\mathcal{O}(1)\) rather than \(\mathcal{O}(\log n)\)
stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesIdempotentMonoid n x = case compare n 0 of
  LT -> errorWithoutStackTrace "stimesIdempotentMonoid: negative multiplier"
  EQ -> mempty
  GT -> x

-- | This is a valid definition of 'stimes' for a 'Monoid'.
--
-- Unlike the default definition of 'stimes', it is defined for 0
-- and so it should be preferred where possible.
stimesMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesMonoid n x0 = case compare n 0 of
  LT -> errorWithoutStackTrace "stimesMonoid: negative multiplier"
  EQ -> mempty
  GT -> f x0 n
    where
      f x y
        | even y = f (x `mappend` x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x `mappend` x) (y `quot` 2) x               -- See Note [Half of y - 1]
      g x y z
        | even y = g (x `mappend` x) (y `quot` 2) z
        | y == 1 = x `mappend` z
        | otherwise = g (x `mappend` x) (y `quot` 2) (x `mappend` z) -- See Note [Half of y - 1]

-- this is used by the class definitionin GHC.Base;
-- it lives here to avoid cycles
stimesDefault :: (Integral b, Semigroup a) => b -> a -> a
stimesDefault y0 x0
  | y0 <= 0   = errorWithoutStackTrace "stimes: positive multiplier expected"
  | otherwise = f x0 y0
  where
    f x y
      | even y = f (x <> x) (y `quot` 2)
      | y == 1 = x
      | otherwise = g (x <> x) (y `quot` 2) x        -- See Note [Half of y - 1]
    g x y z
      | even y = g (x <> x) (y `quot` 2) z
      | y == 1 = x <> z
      | otherwise = g (x <> x) (y `quot` 2) (x <> z) -- See Note [Half of y - 1]

{- Note [Half of y - 1]
   ~~~~~~~~~~~~~~~~~~~~~
   Since y is guaranteed to be odd and positive here,
   half of y - 1 can be computed as y `quot` 2, optimising subtraction away.
-}

stimesMaybe :: (Integral b, Semigroup a) => b -> Maybe a -> Maybe a
stimesMaybe _ Nothing = Nothing
stimesMaybe n (Just a) = case compare n 0 of
    LT -> errorWithoutStackTrace "stimes: Maybe, negative multiplier"
    EQ -> Nothing
    GT -> Just (stimes n a)

stimesList  :: Integral b => b -> [a] -> [a]
stimesList n x
  | n < 0 = errorWithoutStackTrace "stimes: [], negative multiplier"
  | otherwise = rep n
  where
    rep 0 = []
    rep i = x ++ rep (i - 1)

-- | The dual of a 'Monoid', obtained by swapping the arguments of 'mappend'.
--
-- >>> getDual (mappend (Dual "Hello") (Dual "World"))
-- "WorldHello"
newtype Dual a = Dual { getDual :: a }
        deriving ( Eq       -- ^ @since 2.01
                 , Ord      -- ^ @since 2.01
                 , Read     -- ^ @since 2.01
                 , Show     -- ^ @since 2.01
                 , Bounded  -- ^ @since 2.01
                 , Generic  -- ^ @since 4.7.0.0
                 , Generic1 -- ^ @since 4.7.0.0
                 )

-- | @since 4.9.0.0
instance Semigroup a => Semigroup (Dual a) where
        Dual a <> Dual b = Dual (b <> a)
        stimes n (Dual a) = Dual (stimes n a)

-- | @since 2.01
instance Monoid a => Monoid (Dual a) where
        mempty = Dual mempty

-- | @since 4.8.0.0
instance Functor Dual where
    fmap     = coerce

-- | @since 4.8.0.0
instance Applicative Dual where
    pure     = Dual
    (<*>)    = coerce

-- | @since 4.8.0.0
instance Monad Dual where
    m >>= k  = k (getDual m)

-- | The monoid of endomorphisms under composition.
--
-- >>> let computation = Endo ("Hello, " ++) <> Endo (++ "!")
-- >>> appEndo computation "Haskell"
-- "Hello, Haskell!"
newtype Endo a = Endo { appEndo :: a -> a }
               deriving ( Generic -- ^ @since 4.7.0.0
                        )

-- | @since 4.9.0.0
instance Semigroup (Endo a) where
        (<>) = coerce ((.) :: (a -> a) -> (a -> a) -> (a -> a))
        stimes = stimesMonoid

-- | @since 2.01
instance Monoid (Endo a) where
        mempty = Endo id

-- | Boolean monoid under conjunction ('&&').
--
-- >>> getAll (All True <> mempty <> All False)
-- False
--
-- >>> getAll (mconcat (map (\x -> All (even x)) [2,4,6,7,8]))
-- False
newtype All = All { getAll :: Bool }
        deriving ( Eq      -- ^ @since 2.01
                 , Ord     -- ^ @since 2.01
                 , Read    -- ^ @since 2.01
                 , Show    -- ^ @since 2.01
                 , Bounded -- ^ @since 2.01
                 , Generic -- ^ @since 4.7.0.0
                 )

-- | @since 4.9.0.0
instance Semigroup All where
        (<>) = coerce (&&)
        stimes = stimesIdempotentMonoid

-- | @since 2.01
instance Monoid All where
        mempty = All True

-- | Boolean monoid under disjunction ('||').
--
-- >>> getAny (Any True <> mempty <> Any False)
-- True
--
-- >>> getAny (mconcat (map (\x -> Any (even x)) [2,4,6,7,8]))
-- True
newtype Any = Any { getAny :: Bool }
        deriving ( Eq      -- ^ @since 2.01
                 , Ord     -- ^ @since 2.01
                 , Read    -- ^ @since 2.01
                 , Show    -- ^ @since 2.01
                 , Bounded -- ^ @since 2.01
                 , Generic -- ^ @since 4.7.0.0
                 )

-- | @since 4.9.0.0
instance Semigroup Any where
        (<>) = coerce (||)
        stimes = stimesIdempotentMonoid

-- | @since 2.01
instance Monoid Any where
        mempty = Any False

-- | Monoid under addition.
--
-- >>> getSum (Sum 1 <> Sum 2 <> mempty)
-- 3
newtype Sum a = Sum { getSum :: a }
        deriving ( Eq       -- ^ @since 2.01
                 , Ord      -- ^ @since 2.01
                 , Read     -- ^ @since 2.01
                 , Show     -- ^ @since 2.01
                 , Bounded  -- ^ @since 2.01
                 , Generic  -- ^ @since 4.7.0.0
                 , Generic1 -- ^ @since 4.7.0.0
                 , Num      -- ^ @since 4.7.0.0
                 )

-- | @since 4.9.0.0
instance Num a => Semigroup (Sum a) where
        (<>) = coerce ((+) :: a -> a -> a)
        stimes n (Sum a) = Sum (fromIntegral n * a)

-- | @since 2.01
instance Num a => Monoid (Sum a) where
        mempty = Sum 0
        -- By default, we would get a lazy right fold. This forces the use of a strict
        -- left fold instead.
        mconcat = List.foldl' (<>) mempty
        {-# INLINE mconcat #-}

-- | @since 4.8.0.0
instance Functor Sum where
    fmap     = coerce

-- | @since 4.8.0.0
instance Applicative Sum where
    pure     = Sum
    (<*>)    = coerce

-- | @since 4.8.0.0
instance Monad Sum where
    m >>= k  = k (getSum m)

-- | Monoid under multiplication.
--
-- >>> getProduct (Product 3 <> Product 4 <> mempty)
-- 12
newtype Product a = Product { getProduct :: a }
        deriving ( Eq       -- ^ @since 2.01
                 , Ord      -- ^ @since 2.01
                 , Read     -- ^ @since 2.01
                 , Show     -- ^ @since 2.01
                 , Bounded  -- ^ @since 2.01
                 , Generic  -- ^ @since 4.7.0.0
                 , Generic1 -- ^ @since 4.7.0.0
                 , Num      -- ^ @since 4.7.0.0
                 )

-- | @since 4.9.0.0
instance Num a => Semigroup (Product a) where
        (<>) = coerce ((*) :: a -> a -> a)
        stimes n (Product a) = Product (a ^ n)


-- | @since 2.01
instance Num a => Monoid (Product a) where
        mempty = Product 1
        -- By default, we would get a lazy right fold. This forces the use of a strict
        -- left fold instead.
        mconcat = List.foldl' (<>) mempty
        {-# INLINE mconcat #-}

-- | @since 4.8.0.0
instance Functor Product where
    fmap     = coerce

-- | @since 4.8.0.0
instance Applicative Product where
    pure     = Product
    (<*>)    = coerce

-- | @since 4.8.0.0
instance Monad Product where
    m >>= k  = k (getProduct m)


-- | Monoid under '<|>'.
--
-- >>> getAlt (Alt (Just 12) <> Alt (Just 24))
-- Just 12
--
-- >>> getAlt $ Alt Nothing <> Alt (Just 24)
-- Just 24
--
-- @since 4.8.0.0
newtype Alt f a = Alt {getAlt :: f a}
  deriving ( Generic     -- ^ @since 4.8.0.0
           , Generic1    -- ^ @since 4.8.0.0
           , Read        -- ^ @since 4.8.0.0
           , Show        -- ^ @since 4.8.0.0
           , Eq          -- ^ @since 4.8.0.0
           , Ord         -- ^ @since 4.8.0.0
           , Num         -- ^ @since 4.8.0.0
           , Enum        -- ^ @since 4.8.0.0
           , Monad       -- ^ @since 4.8.0.0
           , MonadPlus   -- ^ @since 4.8.0.0
           , Applicative -- ^ @since 4.8.0.0
           , Alternative -- ^ @since 4.8.0.0
           , Functor     -- ^ @since 4.8.0.0
           )

-- | @since 4.9.0.0
instance Alternative f => Semigroup (Alt f a) where
    (<>) = coerce ((<|>) :: f a -> f a -> f a)
    stimes = stimesMonoid

-- | @since 4.8.0.0
instance Alternative f => Monoid (Alt f a) where
    mempty = Alt empty
