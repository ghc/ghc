{-# LANGUAGE TemplateHaskell #-}

module T6005a where

-- The point here is that we don't need to generate the
-- derived code inside the bracket; doing so is troublesome
-- and it should never be type incorrect, so it's also a
-- waste of effort.

$( [d|
   data Nat = Zero | Succ Nat deriving( Show )
   |] )

foo :: String
foo = show (Succ Zero)
