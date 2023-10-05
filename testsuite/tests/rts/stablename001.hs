import System.Mem.StableName
import System.Mem

-- Test that we get the same StableName even after a GC.  This is easy
-- to get wrong, by not following indirections properly.

main = do
  let x = [1..10]
  seq x (return ())
  n1 <- makeStableName x
  performGC
  n2 <- makeStableName x
  print (n1 == n2)
