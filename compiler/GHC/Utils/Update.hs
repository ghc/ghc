{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-stg-final -ddump-to-file #-}

-- | Updater that doesn't lose sharing
--
-- Example:
--
-- > data T = C0 Int | C1 T T | C2 T T T
-- > 
-- > foo :: T -> T
-- > foo = update go
-- >   where
-- >     go t = case t of
-- >       C0 x
-- >         | x == 0    -> build C0 (# Update (x * 999) #)
-- >         | otherwise -> NoUpdate t
-- >       C1 x y   -> rebuild t C1 (# go x, go y #)
-- >       C2 x y z -> rebuild t C2 (# go x, go y, go z #)

module GHC.Utils.Update
  ( Upd(Update,NoUpdate)
  , runUpd
  , bindUpd
  , update
  , updateList
  , updateListAccumR
  , updateListAccumL
  , Updater (..)
  )
where

import GHC.Prelude

import GHC.Exts (TYPE, RuntimeRep(..))
import Data.Kind

-- | Updater (unlifted)
-- Left value is NoUpdate; right value is Update
newtype Upd a = Upd (# a | a #)

{-# COMPLETE NoUpdate, Update #-}
pattern NoUpdate :: a -> Upd a
pattern NoUpdate a = Upd (# a | #)

pattern Update :: a -> Upd a
pattern Update a = Upd (# | a #)

runUpd :: Upd a -> a
runUpd (NoUpdate a) = a
runUpd (Update   a) = a

bindUpd :: Upd a -> (a -> Upd b) -> Upd b
bindUpd (NoUpdate a) f = f a
bindUpd (Update   a) f = case f a of
  NoUpdate b -> Update b
  Update   b -> Update b


update :: (a -> Upd a) -> a -> a
update upd_a a = runUpd (rebuild a id (# upd_a a #))

type Updater :: forall xs. TYPE ('TupleRep xs) -> Type -> Constraint
class Updater t r where
  type F t r :: Type

  -- | Rebuild a given value from its fields and a constructor function.
  --
  -- E.g. rebuild origt MkT (# Update x, NoUpdate y #)
  --
  -- Reuse the given value instead of building a new one if no field has been
  -- updated.
  rebuild :: r -> F t r -> t -> Upd r

  -- | Build a value from its fields.
  --
  -- As soon as `rebuild` finds an updated field, it uses `build` to avoid
  -- passing the value to rebuild for too long.
  build   :: F t r -> t -> Upd r

instance Updater (# Upd a #) r where
  type F (# Upd a #) r = a -> r
  build f (# ma #) = case ma of
    NoUpdate a -> Update (f a)
    Update   a -> Update (f a)

  rebuild def f (# ma #) = case ma of
    NoUpdate _ -> NoUpdate def
    Update   a -> Update   (f a)

instance Updater (# Upd a, Upd b #) r where
  type F (# Upd a, Upd b #) r = a -> b -> r
  build f (# a, b #) = build (f (runUpd a)) (# b #)

  rebuild def f (# ma, b #) = case ma of
    NoUpdate a -> rebuild def (f a) (# b #)
    Update   a -> build       (f a) (# b #)

instance Updater (# Upd a, Upd b, Upd c #) r where
  type F (# Upd a, Upd b, Upd c #) r = a -> b -> c -> r
  build f (# a, b, c #) = build (f (runUpd a)) (# b, c #)

  rebuild def f (# ma, b, c #) = case ma of
    NoUpdate a -> rebuild def (f a) (# b, c #)
    Update   a -> build       (f a) (# b, c #)

instance Updater (# Upd a, Upd b, Upd c, Upd d #) r where
  type F (# Upd a, Upd b, Upd c, Upd d #) r = a -> b -> c -> d -> r
  build f (# a, b, c, d #) = build (f (runUpd a)) (# b, c, d #)

  rebuild def f (# ma, b, c, d #) = case ma of
    NoUpdate a -> rebuild def (f a) (# b, c, d #)
    Update   a -> build       (f a) (# b, c, d #)

instance Updater (# Upd a, Upd b, Upd c, Upd d, Upd e #) r where
  type F (# Upd a, Upd b, Upd c, Upd d, Upd e #) r = a -> b -> c -> d -> e -> r
  build f (# a, b, c, d, e #) = build (f (runUpd a)) (# b, c, d, e #)

  rebuild def f (# ma, b, c, d, e #) = case ma of
    NoUpdate a -> rebuild def (f a) (# b, c, d, e #)
    Update   a -> build       (f a) (# b, c, d, e #)

-- | Update a list. Share the non-updated trail as much as possible.
{-# INLINE updateList #-}
updateList :: forall a. (a -> Upd a) -> [a] -> Upd [a]
updateList = go
  where
    go :: (a -> Upd a) -> [a] -> Upd [a]
    go _ []       = NoUpdate []
    go f t@(a:as) = rebuild t (:) (# f a, go f as #)

-- | Update a list with an accumulator. Share the non-updated trail as much as
-- possible.
{-# INLINE updateListAccumR #-}
updateListAccumR :: forall a env. (env -> a -> (# env, Upd a #)) -> env -> [a] -> (# env, Upd [a] #)
updateListAccumR = go
  where
    go :: (env -> a -> (# env, Upd a #)) -> env -> [a] -> (# env, Upd [a] #)
    go _ env []       = (# env, NoUpdate [] #)
    go f env t@(a:as) = (# env'', rebuild t (:) (# a', as' #) #)
      where
        !(# !env', !as' #) = go f env as
        !(# !env'', !a' #) = f env' a

-- | Update a list with an accumulator. Share the non-updated trail as much as
-- possible.
{-# INLINE updateListAccumL #-}
updateListAccumL :: forall a env. (env -> a -> (# env, Upd a #)) -> env -> [a] -> (# env, Upd [a] #)
updateListAccumL = go
  where
    go :: (env -> a -> (# env, Upd a #)) -> env -> [a] -> (# env, Upd [a] #)
    go _ env []       = (# env, NoUpdate [] #)
    go f env t@(a:as) = (# env'', rebuild t (:) (# a', as' #) #)
      where
        !(# !env'', !as' #) = go f env' as
        !(# !env', !a' #)   = f env a
