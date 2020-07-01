{-# LANGUAGE CPP #-}

module Main where

main :: IO ()
#if defined(__GLASGOW_HASKELL_FULL_VERSION__)
main = putStrLn "__GLASGOW_HASKELL_FULL_VERSION__ is well-defined!"
#else
main = putStrLn "__GLASGOW_HASKELL_FULL_VERSION__ is not defined!"
#endif
