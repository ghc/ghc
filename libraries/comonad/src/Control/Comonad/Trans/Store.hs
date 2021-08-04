{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Store
-- Copyright   :  (C) 2008-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- The store comonad holds a constant value along with a modifiable /accessor/
-- function, which maps the /stored value/ to the /focus/.
--
-- This module defines the strict store (aka state-in-context/costate) comonad
-- transformer.
--
-- @stored value = (1, 5)@, @accessor = fst@, @resulting focus = 1@:
--
-- >>> :{
--  let
--    storeTuple :: Store (Int, Int) Int
--    storeTuple = store fst (1, 5)
-- :}
--
-- Add something to the focus:
--
-- >>> :{
--  let
--    addToFocus :: Int -> Store (Int, Int) Int -> Int
--    addToFocus x wa = x + extract wa
-- :}
--
-- >>> :{
--   let
--     added3 :: Store (Int, Int) Int
--     added3 = extend (addToFocus 3) storeTuple
-- :}
--
-- The focus of added3 is now @1 + 3 = 4@. However, this action changed only
-- the accessor function and therefore the focus but not the stored value:
--
-- >>> pos added3
-- (1,5)
--
-- >>> extract added3
-- 4
--
-- The strict store (state-in-context/costate) comonad transformer is subject
-- to the laws:
--
-- > x = seek (pos x) x
-- > y = pos (seek y x)
-- > seek y x = seek y (seek z x)
--
-- Thanks go to Russell O'Connor and Daniel Peebles for their help formulating
-- and proving the laws for this comonad transformer.
----------------------------------------------------------------------------
module Control.Comonad.Trans.Store
  (
  -- * The Store comonad
    Store, store, runStore
  -- * The Store comonad transformer
  , StoreT(..), runStoreT
  -- * Operations
  , pos
  , seek, seeks
  , peek, peeks
  , experiment
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Comonad
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class
import Data.Functor.Identity
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif

#ifdef __GLASGOW_HASKELL__
import Data.Typeable

-- $setup
-- >>> import Control.Comonad
-- >>> import Data.Tuple (swap)

#if __GLASGOW_HASKELL__ >= 707
deriving instance Typeable StoreT
#else
instance (Typeable s, Typeable1 w) => Typeable1 (StoreT s w) where
  typeOf1 dswa = mkTyConApp storeTTyCon [typeOf (s dswa), typeOf1 (w dswa)]
    where
      s :: StoreT s w a -> s
      s = undefined
      w :: StoreT s w a -> w a
      w = undefined

instance (Typeable s, Typeable1 w, Typeable a) => Typeable (StoreT s w a) where
  typeOf = typeOfDefault

storeTTyCon :: TyCon
#if __GLASGOW_HASKELL__ < 704
storeTTyCon = mkTyCon "Control.Comonad.Trans.Store.StoreT"
#else
storeTTyCon = mkTyCon3 "comonad-transformers" "Control.Comonad.Trans.Store" "StoreT"
#endif
{-# NOINLINE storeTTyCon #-}
#endif

#endif

type Store s = StoreT s Identity

-- | Create a Store using an accessor function and a stored value
store :: (s -> a) -> s -> Store s a
store f s = StoreT (Identity f) s

runStore :: Store s a -> (s -> a, s)
runStore (StoreT (Identity f) s) = (f, s)

data StoreT s w a = StoreT (w (s -> a)) s

runStoreT :: StoreT s w a -> (w (s -> a), s)
runStoreT (StoreT wf s) = (wf, s)

instance Functor w => Functor (StoreT s w) where
  fmap f (StoreT wf s) = StoreT (fmap (f .) wf) s

instance (ComonadApply w, Semigroup s) => ComonadApply (StoreT s w) where
  StoreT ff m <@> StoreT fa n = StoreT ((<*>) <$> ff <@> fa) (m <> n)

instance (Applicative w, Monoid s) => Applicative (StoreT s w) where
  pure a = StoreT (pure (const a)) mempty
  StoreT ff m <*> StoreT fa n = StoreT ((<*>) <$> ff <*> fa) (mappend m n)

instance Comonad w => Comonad (StoreT s w) where
  duplicate (StoreT wf s) = StoreT (extend StoreT wf) s
  extend f (StoreT wf s) = StoreT (extend (\wf' s' -> f (StoreT wf' s')) wf) s
  extract (StoreT wf s) = extract wf s

instance ComonadTrans (StoreT s) where
  lower (StoreT f s) = fmap ($ s) f

instance ComonadHoist (StoreT s) where
  cohoist l (StoreT f s) = StoreT (l f) s

-- | Read the stored value
--
-- >>> pos $ store fst (1,5)
-- (1,5)
--
pos :: StoreT s w a -> s
pos (StoreT _ s) = s

-- | Set the stored value
--
-- >>> pos . seek (3,7) $ store fst (1,5)
-- (3,7)
--
-- Seek satisfies the law
--
-- > seek s = peek s . duplicate
seek :: s -> StoreT s w a -> StoreT s w a
seek s ~(StoreT f _) = StoreT f s

-- | Modify the stored value
--
-- >>> pos . seeks swap $ store fst (1,5)
-- (5,1)
--
-- Seeks satisfies the law
--
-- > seeks f = peeks f . duplicate
seeks :: (s -> s) -> StoreT s w a -> StoreT s w a
seeks f ~(StoreT g s) = StoreT g (f s)

-- | Peek at what the current focus would be for a different stored value
--
-- Peek satisfies the law
--
-- > peek x . extend (peek y) = peek y
peek :: Comonad w => s -> StoreT s w a -> a
peek s (StoreT g _) = extract g s


-- | Peek at what the current focus would be if the stored value was
--   modified by some function
peeks :: Comonad w => (s -> s) -> StoreT s w a -> a
peeks f ~(StoreT g s) = extract g (f s)

-- | Applies a functor-valued function to the stored value, and then uses the
--   new accessor to read the resulting focus.
--
--   >>> let f x = if x > 0 then Just (x^2) else Nothing
--   >>> experiment f $ store (+1) 2
--   Just 5
--   >>> experiment f $ store (+1) (-2)
--   Nothing
experiment :: (Comonad w, Functor f) => (s -> f s) -> StoreT s w a -> f a
experiment f (StoreT wf s) = extract wf <$> f s
