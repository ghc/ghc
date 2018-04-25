{-# OPTIONS_GHC -O -funbox-strict-fields #-}

-- The combination of unboxing and a recursive newtype crashed GHC 6.6.1
-- Trac #1255
-- Use -O to force the unboxing to happen

module Foo where

newtype Bar = Bar Bar -- Recursive

data Gah = Gah { baaz  :: !Bar }


