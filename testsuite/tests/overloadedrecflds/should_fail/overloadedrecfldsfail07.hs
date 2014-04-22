{-# LANGUAGE OverloadedRecordFields #-}
{-# OPTIONS_GHC -fwarn-unused-binds -Werror #-}

module Main (main, T(MkT)) where

data S = MkS { foo :: Int }
data T = MkT { foo :: Int }

-- This should count as a use of S(foo) but not T(foo), but the DefUse
-- machinery is not currently accurate enough to spot this
main = print (foo (MkS 3))
