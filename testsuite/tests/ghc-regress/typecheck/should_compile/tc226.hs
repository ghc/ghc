{-# OPTIONS_GHC -funbox-strict-fields -fglasgow-exts #-}

-- The combination of unboxing and a recursive newtype crashed GHC 6.6.1
-- Trac #1255

module Foo where

newtype Bar = Bar Bar -- Recursive

data Gah = Gah { baaz  :: !Bar }


