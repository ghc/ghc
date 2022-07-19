{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, PatternSynonyms #-}
module BundledPatterns2 (Vec((:>), Empty), RTree(..)) where

import GHC.TypeLits

import BundledPatterns

pattern Empty :: Vec 0 a
pattern Empty <- Nil
