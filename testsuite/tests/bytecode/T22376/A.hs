module A where
import B

foo :: String
foo = f "bc"
{-# NOINLINE foo #-}
