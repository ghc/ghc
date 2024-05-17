{-# LANGUAGE Haskell2010 #-}
module Deprecated where

-- | Docs for something deprecated
deprecated :: Int
deprecated = 1

{-# DEPRECATED deprecated "Don't use this" #-}
