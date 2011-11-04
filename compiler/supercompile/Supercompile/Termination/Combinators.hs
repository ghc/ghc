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
    History(..),
    TermRes(..), isContinue,
    LinearHistory, mkLinearHistory,
    NodeKey, GraphicalHistory, generatedKey, mkGraphicalHistory
  ) where

import Supercompile.Utilities (Nat)

import Control.Arrow ((***))

import Data.Monoid (All(..))
import qualified Data.Foldable as Foldable
import Data.List (sortBy)
import Data.Ord (comparing)

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
-- > fmap (f *** g) (zip_ as bs) == zip_ (fmap f as) (fmap g bs)
--
-- Information preservation:
-- > fmap (const ()) as == fmap (const ()) bs
-- >  ==>
-- > fmap fst (zip_ ma mb) == ma
-- > fmap snd (zip_ ma mb) == mb
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

class History h where
    terminate :: h a -> a -> TermRes h a

data TermRes h a = Stop a | Continue (h a)

isContinue :: TermRes h a -> Bool
isContinue (Continue _) = True
isContinue _            = False


newtype LinearHistory a = LH { unLH :: a -> TermRes LinearHistory a }

instance History LinearHistory where
    terminate = unLH

{-# INLINE mkLinearHistory #-}
mkLinearHistory :: forall a. TTest a -> LinearHistory a
mkLinearHistory (WQO (prepare :: a -> b) embed) = go_init []
  where
    -- Search the history starting with the earliest elements -- i.e. those towards the head of the list
    go_init abs = LH $ \a -> go [] abs a (prepare a)
    
    go :: [(a, b)] -> [(a, b)] -> a -> b -> TermRes LinearHistory a
    go new_abs []           new_a new_b = Continue $ go_init (reverse ((new_a, new_b):new_abs))
    go new_abs ((a, b):abs) new_a new_b = case b `embed` new_b of
        (True, _)  -> Stop a
        (_, True)  -> go new_abs          abs new_a new_b
        (_, False) -> go ((a, b):new_abs) abs new_a new_b


data GraphicalHistory a = GH { unGH :: a -> TermRes GraphicalHistory a, generatedKey :: NodeKey }

instance History GraphicalHistory where
    terminate = unGH

{-# INLINE mkGraphicalHistory #-}
mkGraphicalHistory :: forall a. TTest a -> GraphicalHistory (NodeKey, a)
mkGraphicalHistory (WQO (prepare :: a -> b) embed) = go_init emptyTopologicalOrder [] 0
  where
    go_init topo abs generated_key = GH {
        unGH = \(key, a) -> let Just topo' = insertTopologicalOrder topo (key, generated_key + 1) in go topo' [] abs key (generated_key + 1) a (prepare a),
        generatedKey = generated_key
      }

    go topo new_abs []                  key key' new_a new_b = Continue $ go_init topo (reverse ((key, new_a, new_b):new_abs)) (key' + 1)
    go topo new_abs ((abkey, a, b):abs) key key' new_a new_b = case b `embed` new_b of
      (True, emb) | not emb
                  , Just topo' <- insertTopologicalOrder topo (key, key')
                  -> go topo' ((key, a, b):new_abs) abs key key' new_a new_b
                  | otherwise
                  -> Stop (abkey, a)
      (False, True)  -> go topo new_abs          abs key key' new_a new_b
      (False, False) -> go topo ((key, a, b):new_abs) abs key key' new_a new_b


type NodeKey = Int

data TopologicalOrder = TO {
    ord      :: IM.IntMap Int, -- ^ Maps NodeKey to node position in the topological order.
                               -- If there exists an edge (x, y) then IM.lookup x ord < IM.lookup y ord
    maxPos   :: Int,
    minPos   :: Int,
    outEdges :: IM.IntMap IS.IntSet, -- ^ Maps "from" NodeKey to all "to" nodes
    inEdges  :: IM.IntMap IS.IntSet  -- ^ Maps "to"   NodeKey to all "from" nodes
  }

emptyTopologicalOrder :: TopologicalOrder
emptyTopologicalOrder = TO { ord = IM.empty, maxPos = -1, minPos = 0, outEdges = IM.empty, inEdges = IM.empty }

-- | An algorithm for solving the dynamic topological order problem that performs well on
-- sparse graphs (which I expect to occur a lot) and is empirically (not asymptotically!) fast.
--
-- See <http://homepages.mcs.vuw.ac.nz/~djp/files/PK-JEA07.pdf>
insertTopologicalOrder :: TopologicalOrder -> (NodeKey, NodeKey) -> Maybe TopologicalOrder
insertTopologicalOrder (TO { ord = ord, maxPos = maxPos, minPos = minPos, outEdges = outEdges, inEdges = inEdges }) (x, y) = case (IM.lookup x ord, IM.lookup y ord) of
     -- Both nodes do not yet occur in the graph: arbitrarily insert them at the maximum of the ordering
    (Nothing,  Nothing) -> Just $ TO { ord = IM.insert y (maxPos + 2) $ IM.insert x (maxPos + 1) ord, maxPos = maxPos + 2, minPos = minPos,     outEdges = outEdges', inEdges = inEdges' }
    -- The "to" node does not yet exist in the graph: insert the "to" node at the maximum of the ordering
    (Just _,   Nothing) -> Just $ TO { ord = IM.insert y (maxPos + 1)                            ord, maxPos = maxPos + 1, minPos = minPos,     outEdges = outEdges', inEdges = inEdges' }
    -- The "from" node does not yet exist in the graph: insert the "from" node at the minimum of the ordering
    (Nothing,  Just _)  -> Just $ TO { ord =                            IM.insert x (minPos - 1) ord, maxPos = maxPos,     minPos = minPos - 1, outEdges = outEdges', inEdges = inEdges' }
    -- Both nodes already exist in the graph, so this might introduce a cycle (the hard case)
    (Just ub, Just lb)  -> case ub `compare` lb of
      -- Self-cycle: immediate failure
      EQ -> Nothing
      -- The nodes are already ordered in the existing order
      LT -> Just $ TO { ord = ord, maxPos = maxPos, minPos = minPos, outEdges = outEdges', inEdges = inEdges' }
      -- They appear to be unordered in the existing order, try to fix it
      GT -> dfs_f IM.empty [y] >>= \delta_f -> let delta_b = dfs_b IM.empty [x] in Just $ TO { ord = reorder delta_f delta_b, maxPos = maxPos, minPos = minPos, outEdges = outEdges', inEdges = inEdges' }
       where
        -- NB: for convenience the visited "set" actually maps the NodeKey to its position in the input order
        -- This is because the very next stage would need to look up this information, but we actually have it
        -- on hand in this previous stage.
        dfs_f visited []     = Just visited
        dfs_f visited (n:ns) = case n_ord `compare` ub of
            GT                      -> dfs_f visited ns
            _ | IM.member n visited -> dfs_f visited ns
            EQ -> Nothing -- Cycle detected!
            LT -> dfs_f (IM.insert n n_ord visited) (IS.foldr (:) ns (IM.findWithDefault IS.empty n outEdges))
          where n_ord = IM.findWithDefault (error "dfs_f: unknown node") n ord
        
        dfs_b visited []     = visited
        dfs_b visited (n:ns) = case lb `compare` n_ord of
            LT | not (IM.member n visited) -> dfs_b (IM.insert n n_ord visited) (IS.foldr (:) ns (IM.findWithDefault IS.empty n inEdges))
            _                              -> dfs_b visited ns
          where n_ord = IM.findWithDefault (error "dfs_b: unknown node") n ord
        
        sortDelta = sortBy (comparing snd) . IM.toList
        
        mergeDelta [] [] = []
        mergeDelta xs [] = xs
        mergeDelta [] ys = ys
        mergeDelta init_xs@((x, x_ord):xs) init_ys@((y, y_ord):ys)
          | x_ord < y_ord = (x, x_ord):mergeDelta xs init_ys
          | otherwise     = (y, y_ord):mergeDelta init_xs ys

        -- Reassign available positions in the ordering to affected vertices so that
        -- all of delta_b are to the left of delta_f
        reorder unsorted_delta_f unsorted_delta_b = foldr (uncurry IM.insert) ord (l `zip` r)
          where
            delta_f = sortDelta unsorted_delta_f
            delta_b = sortDelta unsorted_delta_b
            l = map fst (delta_b ++ delta_f)
            r = map snd (mergeDelta delta_b delta_f)
  where
    outEdges' = IM.insertWith IS.union x (IS.singleton y) outEdges
    inEdges'  = IM.insertWith IS.union y (IS.singleton x) inEdges
