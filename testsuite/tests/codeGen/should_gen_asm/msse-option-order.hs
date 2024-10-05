-- `-msse2 -msse4.2` and `-msse4.2 -msse2` should have the same effect.
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/24989#note_587510
import Data.Bits

{-# NOINLINE foo #-}
foo :: Int -> Int
foo x = 1 + popCount x

main :: IO ()
main = print (foo 42)
