-- Test from [ 973063 ] DiffArray deadlock
-- Fixed in rev. 1.9 of libraries/base/Data/Array/Diff.hs
import Data.Array.Diff
main = print (a // [((a ! 0, 1))] ! 0)
  where a = array (0,0) [(0,0)] :: DiffArray Int Int
