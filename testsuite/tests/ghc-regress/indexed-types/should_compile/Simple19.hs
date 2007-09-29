{-# LANGUAGE TypeFamilies, LiberalTypeSynonyms #-}
                           -- ^ crucial for exercising the code paths to be
                           --   tested here

module ShouldCompile where

type family Element c :: *

f :: Element x
f = undefined
