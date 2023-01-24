{-# OPTIONS_GHC -Wno-missing-methods #-}

module T22715_2a where

newtype IdentityT m a = IdentityT (m a) deriving Functor
newtype IdT m a = IdT {runIdT :: m a} deriving Functor

class Functor m => SillyA m where
  unused :: m a -> m a

class SillyA m => SillyB m where
  unused2 :: m a -> m a

instance SillyA m => SillyA (IdentityT m) where
instance SillyB m => SillyB (IdentityT m) where

instance SillyA m => SillyA (IdT m) where
instance SillyB m => SillyB (IdT m) where

instance SillyA IO where
instance SillyB IO where

class Functor m => Special m
instance Functor m => Special (IdT m)

type Input m = IdentityT (IdentityT m)

class (Special m, SillyB m) => CommandMonad m
instance SillyB m => CommandMonad (IdT (Input m))
