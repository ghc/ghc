{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module IgnoreExports (Foo, foo) where

-- | documentation for Foo
data Foo = Bar -- ^ Documentation for Bar

-- | documentation for foo
foo :: Int
foo = 23

-- | documentation for bar
bar :: Int
bar = 23
