{-# LANGUAGE CPP #-}

module FullGHCVersion where

main :: IO ()
#if defined(__GHC_FULL_VERSION__)
main = putStrLn "__GHC_FULL_VERSION__ is well-defined!"
#else
main = putStrLn "__GHC_FULL_VERSION__ is not defined!"
#endif
