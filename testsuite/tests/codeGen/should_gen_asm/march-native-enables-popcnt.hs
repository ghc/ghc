-- `-march=native` enables the host's CPU features. On a host with SSE4.2
-- (gated in all.T via have_cpu_feature) this makes popCount compile to a
-- `popcnt` instruction rather than the SSE2-baseline software fallback.
import Data.Bits

{-# NOINLINE foo #-}
foo :: Int -> Int
foo x = 1 + popCount x

main :: IO ()
main = print (foo 42)
