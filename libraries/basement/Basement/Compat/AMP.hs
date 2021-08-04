{-# LANGUAGE CPP             #-}
{-# LANGUAGE ConstraintKinds #-}
-- a compat module for ghc < 7.10 to handle the AMP change smoothly
module Basement.Compat.AMP
    ( AMPMonad
    ) where

import Basement.Compat.Base

{-# DEPRECATED AMPMonad "use Monad" #-}
type AMPMonad m = Monad m
