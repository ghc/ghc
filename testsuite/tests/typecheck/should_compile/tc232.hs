{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- This one fixed the constraint solver (Lint error)
-- See #1494

module ShouldCompile where

import Control.Monad.State

newtype L m r = L (StateT Int m r)

instance Functor (L m) where
  fmap = undefined

instance Applicative (L m) where
  pure  = undefined
  (<*>) = undefined

instance Monad m => Monad (L m) where
  (>>=)  = undefined
  return = undefined

zork :: (Monad m) => a -> L m ()
zork = undefined

mumble e = do { modify id; zork e }

