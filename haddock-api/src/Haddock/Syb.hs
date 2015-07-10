{-# LANGUAGE Rank2Types #-}


module Haddock.Syb
    ( everything, everywhere
    , combine
    ) where


import Data.Data
import Control.Applicative


-- | Perform a query on each level of a tree.
--
-- This is stolen directly from SYB package and copied here to not introduce
-- additional dependencies.
everything :: (r -> r -> r) -> (forall a. Data a => a -> r)
           -> (forall a. Data a => a -> r)
everything k f x = foldl k (f x) (gmapQ (everything k f) x)

-- | Apply transformation on each level of a tree.
--
-- Just like 'everything', this is stolen from SYB package.
everywhere :: (forall a. Data a => a -> a) -> (forall a. Data a => a -> a)
everywhere f = f . gmapT (everywhere f)

-- | Combine two queries into one using alternative combinator.
combine :: Alternative f => (forall a. Data a => a -> f r)
                         -> (forall a. Data a => a -> f r)
                         -> (forall a. Data a => a -> f r)
combine f g x = f x <|> g x
