{-# LANGUAGE Rank2Types #-}


module Haddock.Syb
    ( everything, everythingWithState, everywhere
    , mkT
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


-- | Perform a query with state on each level of a tree.
--
-- This is the same as 'everything' but allows for stateful computations. In
-- SYB it is called @everythingWithContext@ but I find this name somewhat
-- nicer.
everythingWithState :: s -> (r -> r -> r)
                    -> (forall a. Data a => a -> s -> (r, s))
                    -> (forall a. Data a => a -> r)
everythingWithState s k f x =
    let (r, s') = f x s
    in foldl k r (gmapQ (everythingWithState s' k f) x)


-- | Apply transformation on each level of a tree.
--
-- Just like 'everything', this is stolen from SYB package.
everywhere :: (forall a. Data a => a -> a) -> (forall a. Data a => a -> a)
everywhere f = f . gmapT (everywhere f)

-- | Create generic transformation.
--
-- Another function stolen from SYB package.
mkT :: (Typeable a, Typeable b) => (b -> b) -> (a -> a)
mkT f = case cast f of
    Just f' -> f'
    Nothing -> id

-- | Combine two queries into one using alternative combinator.
combine :: Alternative f => (forall a. Data a => a -> f r)
                         -> (forall a. Data a => a -> f r)
                         -> (forall a. Data a => a -> f r)
combine f g x = f x <|> g x
