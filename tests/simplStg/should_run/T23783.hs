module Main where
import T23783a
import GHC.Conc

expensive :: Int -> Int
{-# OPAQUE expensive #-}
expensive x = x

{-# OPAQUE f #-}
f xs = let ys = expensive xs
           h zs = let t = wombat t ys in ys `seq` (zs, t, ys)
        in h

main :: IO ()
main = do
  setAllocationCounter 100000
  enableAllocationLimit
  case f 0 () of (_, t, _) -> seqT 16 t `seq` pure ()
