{-# LANGUAGE RankNTypes, ScopedTypeVariables, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Schemes
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (local universal quantification)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell
-- See <http://www.cs.uu.nl/wiki/GenericProgramming/SYB>. The present module
-- provides frequently used generic traversal schemes.
--
-----------------------------------------------------------------------------

module Data.Generics.Schemes (

        everywhere,
        everywhere',
        everywhereBut,
        everywhereM,
        somewhere,
        everything,
        everythingBut,
        everythingWithContext,
        listify,
        something,
        synthesize,
        gsize,
        glength,
        gdepth,
        gcount,
        gnodecount,
        gtypecount,
        gfindtype

 ) where

------------------------------------------------------------------------------

#ifdef __HADDOCK__
import Prelude
#endif
import Data.Data
import Data.Generics.Aliases
import Control.Monad

-- | Apply a transformation everywhere in bottom-up manner

everywhere :: (forall a. Data a => a -> a)
           -> (forall a. Data a => a -> a)

-- Use gmapT to recurse into immediate subterms;
-- recall: gmapT preserves the outermost constructor;
-- post-process recursively transformed result via f
--
everywhere f = go
  where
    go :: forall a. Data a => a -> a
    go = f . gmapT go

-- | Apply a transformation everywhere in top-down manner
everywhere' :: (forall a. Data a => a -> a)
            -> (forall a. Data a => a -> a)

-- Arguments of (.) are flipped compared to everywhere
everywhere' f = go
  where
    go :: forall a. Data a => a -> a
    go = gmapT go . f


-- | Variation on everywhere with an extra stop condition
everywhereBut :: GenericQ Bool -> GenericT -> GenericT

-- Guarded to let traversal cease if predicate q holds for x
everywhereBut q f = go
  where
    go :: GenericT
    go x
      | q x       = x
      | otherwise = f (gmapT go x)


-- | Monadic variation on everywhere
everywhereM :: forall m. Monad m => GenericM m -> GenericM m

-- Bottom-up order is also reflected in order of do-actions
everywhereM f = go
  where
    go :: GenericM m
    go x = do
      x' <- gmapM go x
      f x'


-- | Apply a monadic transformation at least somewhere
somewhere :: forall m. MonadPlus m => GenericM m -> GenericM m

-- We try "f" in top-down manner, but descent into "x" when we fail
-- at the root of the term. The transformation fails if "f" fails
-- everywhere, say succeeds nowhere.
--
somewhere f = go
  where
    go :: GenericM m
    go x = f x `mplus` gmapMp go x


-- | Summarise all nodes in top-down, left-to-right order
everything :: forall r. (r -> r -> r) -> GenericQ r -> GenericQ r

-- Apply f to x to summarise top-level node;
-- use gmapQ to recurse into immediate subterms;
-- use ordinary foldl to reduce list of intermediate results
--
everything k f = go
  where
    go :: GenericQ r
    go x = foldl k (f x) (gmapQ go x)

-- | Variation of "everything" with an added stop condition
everythingBut :: forall r. (r -> r -> r) -> GenericQ (r, Bool) -> GenericQ r
everythingBut k f = go
  where
    go :: GenericQ r
    go x = let (v, stop) = f x
           in if stop
                then v
                else foldl k v (gmapQ go x)

-- | Summarise all nodes in top-down, left-to-right order, carrying some state
-- down the tree during the computation, but not left-to-right to siblings.
everythingWithContext :: forall s r. s -> (r -> r -> r) -> GenericQ (s -> (r, s)) -> GenericQ r
everythingWithContext s0 f q = go s0
  where
    go :: s -> GenericQ r
    go s x = foldl f r (gmapQ (go s') x)
      where (r, s') = q x s

-- | Get a list of all entities that meet a predicate
listify :: Typeable r => (r -> Bool) -> GenericQ [r]
listify p = everything (++) ([] `mkQ` (\x -> if p x then [x] else []))


-- | Look up a subterm by means of a maybe-typed filter
something :: GenericQ (Maybe u) -> GenericQ (Maybe u)

-- "something" can be defined in terms of "everything"
-- when a suitable "choice" operator is used for reduction
--
something = everything orElse


-- | Bottom-up synthesis of a data structure;
--   1st argument z is the initial element for the synthesis;
--   2nd argument o is for reduction of results from subterms;
--   3rd argument f updates the synthesised data according to the given term
--
synthesize :: forall s t. s  -> (t -> s -> s) -> GenericQ (s -> t) -> GenericQ t
synthesize z o f = go
  where
    go :: GenericQ t
    go x = f x (foldr o z (gmapQ go x))


-- | Compute size of an arbitrary data structure
gsize :: Data a => a -> Int
gsize t = 1 + sum (gmapQ gsize t)


-- | Count the number of immediate subterms of the given term
glength :: GenericQ Int
glength = length . gmapQ (const ())


-- | Determine depth of the given term
gdepth :: GenericQ Int
gdepth = (+) 1 . foldr max 0 . gmapQ gdepth


-- | Determine the number of all suitable nodes in a given term
gcount :: GenericQ Bool -> GenericQ Int
gcount p =  everything (+) (\x -> if p x then 1 else 0)


-- | Determine the number of all nodes in a given term
gnodecount :: GenericQ Int
gnodecount = gcount (const True)


-- | Determine the number of nodes of a given type in a given term
gtypecount :: Typeable a => a -> GenericQ Int
gtypecount (_::a) = gcount (False `mkQ` (\(_::a) -> True))


-- | Find (unambiguously) an immediate subterm of a given type
gfindtype :: (Data x, Typeable y) => x -> Maybe y
gfindtype = singleton
          . foldl unJust []
          . gmapQ (Nothing `mkQ` Just)
 where
  unJust l (Just x) = x:l
  unJust l Nothing  = l
  singleton [s] = Just s
  singleton _   = Nothing
