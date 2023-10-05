{-# LANGUAGE PatternSynonyms #-}
module BundleExport(P(.., A), Q(B)) where

data P = P

data Q = Q

pattern A = P
pattern B = Q
