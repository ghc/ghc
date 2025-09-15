{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}
{- This test makes sure that if two expressions with conflicting types are
   CSEd then appropriate things happen. -}
module CSETest where

minimal :: a ⊸ a
minimal x = x

maximal :: a -> a
maximal x = x
