{-# OPTIONS -fglasgow-exts #-}

module ShouldFail where

newtype (f <.> g) a = Compose (f (g a))
