{-# LANGUAGE MagicHash #-}

module KeepAliveWrapper where

import GHC.Exts ( State#, RealWorld, keepAlive# )

keepAliveWrapper :: v -> State# RealWorld -> (State# (RealWorld) -> p) -> p
keepAliveWrapper a1 a2 a3 = keepAlive# a1 a2 a3
