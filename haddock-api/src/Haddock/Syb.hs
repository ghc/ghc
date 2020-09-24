{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haddock.Syb
    ( everything, everythingButType, everythingWithState
    , everywhere, everywhereButType
    , mkT, mkQ, extQ
    , combine
    ) where


import Data.Data
import Control.Applicative
import Data.Maybe
import Data.Foldable

-- | Returns true if a == t.
-- requires AllowAmbiguousTypes
isType :: forall a b. (Typeable a, Typeable b) => b -> Bool
isType _ = isJust $ eqT @a @b

-- | Perform a query on each level of a tree.
--
-- This is stolen directly from SYB package and copied here to not introduce
-- additional dependencies.
everything :: (r -> r -> r)
           -> (forall a. Data a => a -> r)
           -> (forall a. Data a => a -> r)
everything k f x = foldl' k (f x) (gmapQ (everything k f) x)

-- | Variation of "everything" with an added stop condition
-- Just like 'everything', this is stolen from SYB package.
everythingBut :: (r -> r -> r)
              -> (forall a. Data a => a -> (r, Bool))
              -> (forall a. Data a => a -> r)
everythingBut k f x = let (v, stop) = f x
                      in if stop
                           then v
                           else foldl' k v (gmapQ (everythingBut k f) x)

-- | Variation of "everything" that does not recurse into children of type t
-- requires AllowAmbiguousTypes
everythingButType ::
  forall t r. (Typeable t)
  => (r -> r -> r)
  -> (forall a. Data a => a -> r)
  -> (forall a. Data a => a -> r)
everythingButType k f = everythingBut k $ (,) <$> f <*> isType @t

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

-- | Apply transformation on each level of a tree.
--
-- Just like 'everything', this is stolen from SYB package.
everywhere :: (forall a. Data a => a -> a) -> (forall a. Data a => a -> a)
everywhere f = f . gmapT (everywhere f)

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

-- | Combine two queries into one using alternative combinator.
combine :: Alternative f => (forall a. Data a => a -> f r)
                         -> (forall a. Data a => a -> f r)
                         -> (forall a. Data a => a -> f r)
combine f g x = f x <|> g x
