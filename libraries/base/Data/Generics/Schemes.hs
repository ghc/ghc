-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Schemes
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (local universal quantification)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>. The present module provides
-- frequently used generic traversal schemes.
--
-----------------------------------------------------------------------------

module Data.Generics.Schemes ( 

        everywhere,
        everywhere',
        everywhereBut,
        everywhereM,
        somewhere,
	everything,
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
import Data.Generics.Basics
import Data.Generics.Aliases
import Control.Monad


-- | Apply a transformation everywhere in bottom-up manner
everywhere :: (forall a. Data a => a -> a)
           -> (forall a. Data a => a -> a)

-- Use gmapT to recurse into immediate subterms;
-- recall: gmapT preserves the outermost constructor;
-- post-process recursively transformed result via f
-- 
everywhere f = f . gmapT (everywhere f)


-- | Apply a transformation everywhere in top-down manner
everywhere' :: (forall a. Data a => a -> a)
            -> (forall a. Data a => a -> a)

-- Arguments of (.) are flipped compared to everywhere
everywhere' f = gmapT (everywhere' f) . f


-- | Variation on everywhere with an extra stop condition
everywhereBut :: GenericQ Bool -> GenericT -> GenericT

-- Guarded to let traversal cease if predicate q holds for x
everywhereBut q f x
    | q x       = x
    | otherwise = f (gmapT (everywhereBut q f) x)


-- | Monadic variation on everywhere
everywhereM :: Monad m => GenericM m -> GenericM m

-- Bottom-up order is also reflected in order of do-actions
everywhereM f x = do x' <- gmapM (everywhereM f) x
                     f x'


-- | Apply a monadic transformation at least somewhere
somewhere :: MonadPlus m => GenericM m -> GenericM m

-- We try "f" in top-down manner, but descent into "x" when we fail
-- at the root of the term. The transformation fails if "f" fails
-- everywhere, say succeeds nowhere.
-- 
somewhere f x = f x `mplus` gmapMp (somewhere f) x


-- | Summarise all nodes in top-down, left-to-right order
everything :: (r -> r -> r) -> GenericQ r -> GenericQ r

-- Apply f to x to summarise top-level node;
-- use gmapQ to recurse into immediate subterms;
-- use ordinary foldl to reduce list of intermediate results
-- 
everything k f x 
  = foldl k (f x) (gmapQ (everything k f) x)


-- | Get a list of all entities that meet a predicate
listify :: Typeable r => (r -> Bool) -> GenericQ [r]
listify p
  = everything (++) ([] `mkQ` (\x -> if p x then [x] else []))


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
synthesize :: s  -> (s -> s -> s) -> GenericQ (s -> s) -> GenericQ s
synthesize z o f x = f x (foldr o z (gmapQ (synthesize z o f) x))


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
