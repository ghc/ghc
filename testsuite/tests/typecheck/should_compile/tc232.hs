
-- This one foxed the constraint solver (Lint error)
-- See Trac #1494

module ShouldCompile where

import Control.Monad.State

newtype L m r = L (StateT Int m r)

instance Monad m => Monad (L m) where
  (>>=)  = undefined
  return = undefined

zork :: (Monad m) => a -> L m ()
zork = undefined

mumble e = do { modify id; zork e }

