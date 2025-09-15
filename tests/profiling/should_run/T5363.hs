import Data.Array.Unboxed

main = do
  let l1 = [1..10] :: [Int]
  let l2 = [ map (i+) l1 | i <- [1..500000] ]
  let l3 = map (\l -> listArray (1,length l) l) l2 :: [UArray Int Int]
  print $ accumulate l3 0

accumulate [] rv = rv
accumulate (h:t) rv =
  let nextRv = (rv + sum (elems h)) in
  accumulate t $! nextRv
