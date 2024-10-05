-- `-mavx` should imply `-msse4.2`.
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/24989
import Data.Bits

{-# NOINLINE foo #-}
foo :: Int -> Int
foo x = 1 + popCount x

main :: IO ()
main = print (foo 42)
