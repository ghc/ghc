{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
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
-- issues.
--
-- @since base-4.11.0.0
module GHC.Internal.Data.Semigroup.Internal where

import GHC.Internal.Base hiding (Any)
import GHC.Internal.Enum
import qualified GHC.Internal.List as List
import GHC.Internal.Num
import GHC.Internal.Read
import GHC.Internal.Show
import GHC.Internal.Generics
import GHC.Internal.Real

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
-- When @x <> x = x@, this definition should be preferred, because it
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

{- Note [Half of y - 1]
   ~~~~~~~~~~~~~~~~~~~~~
   Since y is guaranteed to be odd and positive here,
   half of y - 1 can be computed as y `quot` 2, optimising subtraction away.
-}

-- | The dual of a 'Monoid', obtained by swapping the arguments of '(<>)'.
--
-- > Dual a <> Dual b == Dual (b <> a)
--
-- ==== __Examples__
--
-- >>> Dual "Hello" <> Dual "World"
-- Dual {getDual = "WorldHello"}
--
-- >>> Dual (Dual "Hello") <> Dual (Dual "World")
-- Dual {getDual = Dual {getDual = "HelloWorld"}}
newtype Dual a = Dual { getDual :: a }
        deriving ( Eq       -- ^ @since base-2.01
                 , Ord      -- ^ @since base-2.01
                 , Read     -- ^ @since base-2.01
                 , Show     -- ^ @since base-2.01
                 , Bounded  -- ^ @since base-2.01
                 , Generic  -- ^ @since base-4.7.0.0
                 , Generic1 -- ^ @since base-4.7.0.0
                 )

-- | @since base-4.9.0.0
instance Semigroup a => Semigroup (Dual a) where
        Dual a <> Dual b = Dual (b <> a)
        stimes n (Dual a) = Dual (stimes n a)

-- | @since base-2.01
instance Monoid a => Monoid (Dual a) where
        mempty = Dual mempty

-- | @since base-4.8.0.0
instance Functor Dual where
    fmap     = coerce

-- | @since base-4.8.0.0
instance Applicative Dual where
    pure     = Dual
    (<*>)    = coerce

-- | @since base-4.8.0.0
instance Monad Dual where
    m >>= k  = k (getDual m)

-- | The monoid of endomorphisms under composition.
--
-- > Endo f <> Endo g == Endo (f . g)
--
-- ==== __Examples__
--
-- >>> let computation = Endo ("Hello, " ++) <> Endo (++ "!")
-- >>> appEndo computation "Haskell"
-- "Hello, Haskell!"
--
-- >>> let computation = Endo (*3) <> Endo (+1)
-- >>> appEndo computation 1
-- 6
newtype Endo a = Endo { appEndo :: a -> a }
               deriving ( Generic -- ^ @since base-4.7.0.0
                        )

-- | @since base-4.9.0.0
instance Semigroup (Endo a) where
    (<>) = coerce ((.) :: (a -> a) -> (a -> a) -> (a -> a))

    -- See Note [stimes Endo]
    stimes !n0 (Endo e) = Endo (\a0 ->
      -- We check separately for 0 and 1 per
      -- https://github.com/haskell/core-libraries-committee/issues/4#issuecomment-955605592
      -- We are explicitly strict in the number so strictness is calculated
      -- correctly even without specialization.
      case n0 of
        _ | n0 < 0 -> stimesEndoError
        0 -> a0
        1 -> e a0
        _ -> go n0 a0)
      where
        go !0 a = a
        go n a = e (go (n - 1) a)

{-# NOINLINE stimesEndoError #-}
-- There's no reason to put this gunk in the unfolding.
stimesEndoError :: a
stimesEndoError = errorWithoutStackTrace "stimes (for Endo): negative multiplier"

-- Note [stimes Endo]
-- ~~~~~~~~~~~~~~~~~~
--
-- We used to use
--
--   stimes = stimesMonoid
--
-- But this is pretty bad! The function it produces is represented in memory as
-- a balanced tree of compositions. To actually *apply* that function, it's
-- necessary to walk the tree. It's much better to just construct a function
-- that counts out applications.
--
-- Why do we break open the `Endo` construction rather than just using `mempty`
-- and `<>`? We want GHC to infer that `stimes` has an arity of 3. Currently,
-- it does so by default, but there has been some talk in the past of turning
-- on -fpedantic-bottoms, which would drop the arity to 2. Indeed, if we were
-- really careless, we could theoretically get GHC to build a *list* of
-- compositions, which would be awful.

-- | @since base-2.01
instance Monoid (Endo a) where
        mempty = Endo id

-- | Boolean monoid under conjunction '(&&)'.
--
-- > All x <> All y = All (x && y)
--
-- ==== __Examples__
--
-- >>> All True <> mempty <> All False)
-- All {getAll = False}
--
-- >>> mconcat (map (\x -> All (even x)) [2,4,6,7,8])
-- All {getAll = False}
--
-- >>> All True <> mempty
-- All {getAll = True}
newtype All = All { getAll :: Bool }
        deriving ( Eq      -- ^ @since base-2.01
                 , Ord     -- ^ @since base-2.01
                 , Read    -- ^ @since base-2.01
                 , Show    -- ^ @since base-2.01
                 , Bounded -- ^ @since base-2.01
                 , Generic -- ^ @since base-4.7.0.0
                 )

-- | @since base-4.9.0.0
instance Semigroup All where
        (<>) = coerce (&&)
        stimes = stimesIdempotentMonoid

-- | @since base-2.01
instance Monoid All where
        mempty = All True

-- | Boolean monoid under disjunction '(||)'.
--
-- > Any x <> Any y = Any (x || y)
--
-- ==== __Examples__
--
-- >>> Any True <> mempty <> Any False
-- Any {getAny = True}
--
-- >>> mconcat (map (\x -> Any (even x)) [2,4,6,7,8])
-- Any {getAny = True}
--
-- >>> Any False <> mempty
-- Any {getAny = False}
newtype Any = Any { getAny :: Bool }
        deriving ( Eq      -- ^ @since base-2.01
                 , Ord     -- ^ @since base-2.01
                 , Read    -- ^ @since base-2.01
                 , Show    -- ^ @since base-2.01
                 , Bounded -- ^ @since base-2.01
                 , Generic -- ^ @since base-4.7.0.0
                 )

-- | @since base-4.9.0.0
instance Semigroup Any where
        (<>) = coerce (||)
        stimes = stimesIdempotentMonoid

-- | @since base-2.01
instance Monoid Any where
        mempty = Any False

-- | Monoid under addition.
--
-- > Sum a <> Sum b = Sum (a + b)
--
-- ==== __Examples__
--
-- >>> Sum 1 <> Sum 2 <> mempty
-- Sum {getSum = 3}
--
-- >>> mconcat [ Sum n | n <- [3 .. 9]]
-- Sum {getSum = 42}
newtype Sum a = Sum { getSum :: a }
        deriving ( Eq       -- ^ @since base-2.01
                 , Ord      -- ^ @since base-2.01
                 , Read     -- ^ @since base-2.01
                 , Show     -- ^ @since base-2.01
                 , Bounded  -- ^ @since base-2.01
                 , Generic  -- ^ @since base-4.7.0.0
                 , Generic1 -- ^ @since base-4.7.0.0
                 , Num      -- ^ @since base-4.7.0.0
                 )

-- | @since base-4.9.0.0
instance Num a => Semigroup (Sum a) where
        (<>) = coerce ((+) :: a -> a -> a)
        stimes n (Sum a) = Sum (fromIntegral n * a)

-- | @since base-2.01
instance Num a => Monoid (Sum a) where
        mempty = Sum 0
        -- By default, we would get a lazy right fold. This forces the use of a strict
        -- left fold instead.
        mconcat = List.foldl' (<>) mempty
        {-# INLINE mconcat #-}

-- | @since base-4.8.0.0
instance Functor Sum where
    fmap     = coerce

-- | @since base-4.8.0.0
instance Applicative Sum where
    pure     = Sum
    (<*>)    = coerce

-- | @since base-4.8.0.0
instance Monad Sum where
    m >>= k  = k (getSum m)

-- | Monoid under multiplication.
--
-- > Product x <> Product y == Product (x * y)
--
-- ==== __Examples__
--
-- >>> Product 3 <> Product 4 <> mempty
-- Product {getProduct = 12}
--
-- >>> mconcat [ Product n | n <- [2 .. 10]]
-- Product {getProduct = 3628800}
newtype Product a = Product { getProduct :: a }
        deriving ( Eq       -- ^ @since base-2.01
                 , Ord      -- ^ @since base-2.01
                 , Read     -- ^ @since base-2.01
                 , Show     -- ^ @since base-2.01
                 , Bounded  -- ^ @since base-2.01
                 , Generic  -- ^ @since base-4.7.0.0
                 , Generic1 -- ^ @since base-4.7.0.0
                 , Num      -- ^ @since base-4.7.0.0
                 )

-- | @since base-4.9.0.0
instance Num a => Semigroup (Product a) where
        (<>) = coerce ((*) :: a -> a -> a)
        stimes n (Product a) = Product (a ^ n)


-- | @since base-2.01
instance Num a => Monoid (Product a) where
        mempty = Product 1
        -- By default, we would get a lazy right fold. This forces the use of a strict
        -- left fold instead.
        mconcat = List.foldl' (<>) mempty
        {-# INLINE mconcat #-}

-- | @since base-4.8.0.0
instance Functor Product where
    fmap     = coerce

-- | @since base-4.8.0.0
instance Applicative Product where
    pure     = Product
    (<*>)    = coerce

-- | @since base-4.8.0.0
instance Monad Product where
    m >>= k  = k (getProduct m)


-- | Monoid under '<|>'.
--
-- > Alt l <> Alt r == Alt (l <|> r)
--
-- ==== __Examples__
-- >>> Alt (Just 12) <> Alt (Just 24)
-- Alt {getAlt = Just 12}
--
-- >>> Alt Nothing <> Alt (Just 24)
-- Alt {getAlt = Just 24}
--
-- @since base-4.8.0.0
newtype Alt f a = Alt {getAlt :: f a}
  deriving ( Generic     -- ^ @since base-4.8.0.0
           , Generic1    -- ^ @since base-4.8.0.0
           , Read        -- ^ @since base-4.8.0.0
           , Show        -- ^ @since base-4.8.0.0
           , Eq          -- ^ @since base-4.8.0.0
           , Ord         -- ^ @since base-4.8.0.0
           , Num         -- ^ @since base-4.8.0.0
           , Enum        -- ^ @since base-4.8.0.0
           , Monad       -- ^ @since base-4.8.0.0
           , MonadPlus   -- ^ @since base-4.8.0.0
           , Applicative -- ^ @since base-4.8.0.0
           , Alternative -- ^ @since base-4.8.0.0
           , Functor     -- ^ @since base-4.8.0.0
           )

-- | @since base-4.9.0.0
instance Alternative f => Semigroup (Alt f a) where
    (<>) = coerce ((<|>) :: f a -> f a -> f a)
    stimes = stimesMonoid

-- | @since base-4.8.0.0
instance Alternative f => Monoid (Alt f a) where
    mempty = Alt empty
