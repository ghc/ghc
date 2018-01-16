{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, NamedWildCards #-}
module Meltdown where

import Control.Applicative

data NukeMonad a b c

instance Functor (NukeMonad a b) where
  fmap = undefined

instance Applicative (NukeMonad a b) where
  pure = undefined
  (<*>) = undefined

instance Monad (NukeMonad a b) where
  return = undefined
  (>>=) = undefined


isMeltdown :: NukeMonad param1 param2 Bool
isMeltdown = undefined

unlessMeltdown :: _nm () ->  _nm ()
unlessMeltdown c = do m <- isMeltdown
                      if m then return () else c
