-- The direct implementation of drop and dropWhile operates in O(1) space.
-- This regression test asserts that potential fusion rules for dropWhile/drop
-- maintain that property for the fused pipelines in dropWhile2 and drop2 (which
-- are marked NOINLINE for that purpose).
-- #23021 was opened because we had fusion rules in place that did not maintain
-- this property.

dropWhile2 :: Int -> [Int] -> [Int]
dropWhile2 n = dropWhile (< n) . dropWhile (< n)
{-# NOINLINE dropWhile2 #-}

drop2 :: Int -> [Int] -> [Int]
drop2 n = drop n . drop n
{-# NOINLINE drop2 #-}

main :: IO ()
main = do
  let xs = [0..9999999]
  print $ last $ dropWhile2 0 xs
  print $ last $ dropWhile2 1 xs
  print $ last $ dropWhile2 2 xs
  print $ last $ dropWhile2 3 xs
  print $ last $ dropWhile2 4 xs
  print $ last $ dropWhile2 5 xs
  print $ last $ drop2 0 xs
  print $ last $ drop2 1 xs
  print $ last $ drop2 2 xs
  print $ last $ drop2 3 xs
  print $ last $ drop2 4 xs
  print $ last $ drop2 5 xs
