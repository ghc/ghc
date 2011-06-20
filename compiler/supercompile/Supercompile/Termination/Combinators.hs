{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts #-}
module Supercompile.Termination.Combinators (
    -- | Termination tests
    TTest,
    alwaysT,
    Cofunctor(..),
    Finite, finiteT,
    WellOrdered, wellOrderedT,
    eitherT, pairT,
    Zippable(..), zippableT,
    HasDomain(..), equalDomainT,
    
    -- | Histories
    History(terminate), TermRes(..),
    mkHistory, isContinue
  ) where

import Supercompile.Utilities (Nat)

import Control.Arrow ((***))

import Data.Monoid (All(..))
import qualified Data.Foldable as Foldable

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S


-- Termination tests
-- ~~~~~~~~~~~~~~~~~

data TTest a = forall repr. WQO (a -> repr)                    -- Prepare item
                                (repr -> repr -> (Bool, Bool)) -- Embed two prepared items

-- -- | Tests whether two elements are embedding according to the given embedding operator.
-- (<|) :: TTest a -> a -> a -> Bool
-- (<|) (WQO prepare embed) x y = fst (embed (prepare x) (prepare y))


-- | Picks out type constructors that are contravariant functors, i.e. we have:
--
--   1. Identity: @cofmap id = id@
--
--   2. Composition: cofmap f . cofmap g = cofmap (g . f)
class Cofunctor f where
    cofmap :: (a -> b) -> f b -> f a

instance Cofunctor TTest where
    -- Trivially correct
    cofmap f (WQO prepare embed) = WQO (prepare . f) embed


-- | Trivial termination test: terminates immediately
alwaysT :: TTest a
alwaysT = WQO (\_ -> ()) (\() () -> (True, True))


-- | Picks out types @a@ that satisfy these two properties:
--
--  1. The @Eq@ instance provides a total equivalence relation (i.e. symmetric, transitive, reflexive)
--
--  2. There are only a finite number of elements of type @a@ that are distinguishable via the @Eq@ instance
class Eq a => Finite a where

instance Finite ()
instance Finite Int
instance Finite IS.IntSet
instance Finite v => Finite (IM.IntMap v)
instance (Finite k) => Finite (S.Set k)
instance (Finite k, Finite v) => Finite (M.Map k v)

-- | Embedding on finite types. Correct by the pigeonhole principle.
{-# INLINE finiteT #-}
finiteT :: Finite a => TTest a
finiteT = WQO id $ \x y -> if x == y then (True, True) else (False, False)


-- | Picks out types @a@ that satisfy these two properties:
--
--   1. The @Ord@ instance's (>=) defines a total order (i.e. total, antisymmetric, transitive)
--
--   2. There is no infinite descending chain @x_1 > x_2 > ...@ where each @x_i :: a@
class Ord a => WellOrdered a where

instance WellOrdered Int

-- | Embedding on well-orders. Correct because well-orders are strictly stronger than well-quasi-orders.
{-# INLINE wellOrderedT #-}
wellOrderedT :: WellOrdered a => TTest a
wellOrderedT = WQO id $ \x y -> case x `compare` y of LT -> (True, False); EQ -> (True, True); GT -> (False, True)


-- | Embedding on sums of things. Correct by appealing to partition of input into |Left|s and |Right|s.
{-# INLINE eitherT #-}
eitherT :: TTest a -> TTest b -> TTest (Either a b)
eitherT (WQO prepare_a embed_a) (WQO prepare_b embed_b) = WQO (either (Left . prepare_a) (Right . prepare_b)) go
  where go (Left a1)  (Left a2)  = a1 `embed_a` a2
        go (Right b1) (Right b2) = b1 `embed_b` b2
        go _          _          = (False, False)


-- | Embedding on pairs of things. Correct by a Ramsey argument.
{-# INLINE pairT #-}
pairT :: TTest a -> TTest b -> TTest (a, b)
pairT (WQO prepare_a embed_a) (WQO prepare_b embed_b) = WQO (prepare_a *** prepare_b) go
  where go (a1, b1) (a2, b2) = zipPair ((&&), (&&)) (a1 `embed_a` a2) (b1 `embed_b` b2)

zipPair :: (a -> b -> c, d -> e -> f)
        -> (a, d) -> (b, e) -> (c, f)
zipPair (f, g) (a, d) (b, e) = (f a b, g d e)

-- | Type class of zippable things. Instances should satisfy the laws:
--
-- Naturality:
--   > fmap (f *** g) (zip_ as bs) == zip_ (fmap f as) (fmap g bs)
--
-- Information preservation:
--   > fmap (const ()) as == fmap (const ()) bs
--   >  ==>
--   > fmap fst (zip_ ma mb) == ma
--   > fmap snd (zip_ ma mb) == mb
class Functor z => Zippable z where
    zip_ :: z a -> z b -> z (a, b)
    zip_ = zipWith_ (,)

    zipWith_ :: (a -> b -> c) -> z a -> z b -> z c
    zipWith_ f as bs = fmap (uncurry f) (zip_ as bs)

instance Zippable [] where
    zipWith_ = zipWith

instance Zippable IM.IntMap where
    zipWith_ = IM.intersectionWith

instance Ord k => Zippable (M.Map k) where
    zipWith_ = M.intersectionWith

-- | Embedding on things with exactly corresponding "shapes", derived from an embedding on the elements.
-- Correct (for finite "shapes") because it can be implemented by mapping the elements of the container to
-- a fixed length tuple and then iterating the 'product' lemma.
{-# INLINE zippableT #-}
zippableT :: (Finite (t ()), Zippable t, Foldable.Foldable t) => TTest a -> TTest (t a)
zippableT (WQO prepare embed) = WQO (fmap prepare) $ \xs ys -> (getAll *** getAll) $ Foldable.fold (zipWith_ (\x y -> (All *** All) (embed x y)) xs ys)


-- | Picks out types that we can extract a ``domain'' for.
class Functor f => HasDomain f where
    type Domain f :: *
    
    -- | Extract the domain of the object.
    --  > domain x == domain y ==> fmap (const ()) x == fmap (const ()) y
    domain :: f a -> Domain f

instance HasDomain [] where
    type Domain [] = Nat
    domain = length

instance HasDomain IM.IntMap where
    type Domain IM.IntMap = IS.IntSet
    domain = IM.keysSet

instance Ord k => HasDomain (M.Map k) where
    type Domain (M.Map k) = S.Set k
    domain = M.keysSet

-- | Convenience combinator allowing refining a chain of collections with varying domains into several subchains with uniform domains
{-# INLINE equalDomainT #-}
equalDomainT :: (HasDomain f, Finite (Domain f))
             => TTest (f a)
             -> TTest (f a)
equalDomainT wqo = cofmap (\x -> (domain x, x)) $ pairT finiteT wqo


-- Histories
-- ~~~~~~~~~

newtype History a = H { terminate :: a -> TermRes a }

data TermRes a = Stop a | Continue (History a)

isContinue :: TermRes a -> Bool
isContinue (Continue _) = True
isContinue _            = False

{-# INLINE mkHistory #-}
mkHistory :: forall a. TTest a -> History a
mkHistory (WQO (prepare :: a -> b) embed) = go_init []
  where
    -- Search the history starting with the earliest elements -- i.e. those towards the head of the list
    go_init abs = H $ \a -> go [] abs a (prepare a)
    
    go :: [(a, b)] -> [(a, b)] -> a -> b -> TermRes a
    go new_abs []           new_a new_b = Continue $ go_init (reverse ((new_a, new_b):new_abs))
    go new_abs ((a, b):abs) new_a new_b = case b `embed` new_b of
        (True, _)  -> Stop a
        (_, True)  -> go new_abs          abs new_a new_b
        (_, False) -> go ((a, b):new_abs) abs new_a new_b
