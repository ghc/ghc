{-# LANGUAGE TypeFamilies, LiberalTypeSynonyms #-}
                           -- ^ crucial for exercising the code paths to be
                           --   tested here

module ShouldCompile where

import Data.Kind (Type)

type family Element c :: Type

f :: x -> Element x
f x = undefined
