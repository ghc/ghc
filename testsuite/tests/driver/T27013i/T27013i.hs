{-# LANGUAGE NoImplicitPrelude #-}

-- Check for -Wunused-packages interaction with the implicit GHC.Essentials import.
module T27013i where

myId :: forall a. a -> a
myId x = x
