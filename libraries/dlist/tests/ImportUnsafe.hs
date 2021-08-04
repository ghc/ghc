{- ORMOLU_DISABLE -}
{-# LANGUAGE CPP #-}

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#else
#error "Your GHC does not support Safe Haskell. That's okay!"
#endif

-----------------------------------------------------------------------------

{-|

This module is declared @Safe@ but imports a module declared @Unsafe@.
Therefore, any attempt to compile this module should fail.

We use @#error@ above just to make it fail for older versions of GHC that did
not support Safe Haskell.

Run this test in the top-level directory with the following command:

> ! ghc tests/ImportUnsafe.hs

-}
{- ORMOLU_ENABLE -}

module ImportUnsafe (main) where

-----------------------------------------------------------------------------

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
import Data.DList.Unsafe ()
#endif

main :: IO ()
main = putStrLn "You should not see this message because this module should fail to compile."
