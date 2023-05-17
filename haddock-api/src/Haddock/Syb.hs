{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haddock.Syb
    ( everythingWithState
    , everywhereButType
    , mkT
    , mkQ
    , extQ
    ) where


import Data.Data
import Data.Maybe
import Data.Foldable

-- | Returns true if a == t.
-- requires AllowAmbiguousTypes
isType :: forall a b. (Typeable a, Typeable b) => b -> Bool
isType _ = isJust $ eqT @a @b

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
    in foldl' k r (gmapQ (everythingWithState s' k f) x)

-- | Variation on everywhere with an extra stop condition
-- Just like 'everything', this is stolen from SYB package.
everywhereBut :: (forall a. Data a => a -> Bool)
  -> (forall a. Data a => a -> a)
  -> (forall a. Data a => a -> a)
everywhereBut q f x
    | q x       = x
    | otherwise = f (gmapT (everywhereBut q f) x)

-- | Variation of "everywhere" that does not recurse into children of type t
-- requires AllowAmbiguousTypes
everywhereButType :: forall t . (Typeable t)
  => (forall a. Data a => a -> a)
  -> (forall a. Data a => a -> a)
everywhereButType = everywhereBut (isType @t)

-- | Create generic transformation.
--
-- Another function stolen from SYB package.
mkT :: (Typeable a, Typeable b) => (b -> b) -> (a -> a)
mkT f = case cast f of
    Just f' -> f'
    Nothing -> id

-- | Create generic query.
--
-- Another function stolen from SYB package.
mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
(r `mkQ` br) a = case cast a of
                        Just b  -> br b
                        Nothing -> r


-- | Extend a generic query by a type-specific case.
--
-- Another function stolen from SYB package.
extQ :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q
extQ f g a = maybe (f a) g (cast a)
