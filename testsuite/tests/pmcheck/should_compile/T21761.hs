{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

module T21761 where

data Void

idV :: Void -> Void
idV v = v

idV' :: Void -> Void
idV' v = case v of w -> w

bangIdV :: Void -> Void
bangIdV !v = v

bangIdV' :: Void -> Void
bangIdV' v = case v of !w -> w
