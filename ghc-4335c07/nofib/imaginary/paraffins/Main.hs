{-
 - Id Example Program
 - Ensnaffled by SLPJ from MIT via
 - RPaul <rpaul@juicy-juice.lcs.mit.edu> 93/08/26.
 - Original author: Steve Heller
 -}

module Main (main) where
import Data.Array
import System.Environment

-- Generation of radicals

data Radical = H | C Radical Radical Radical

three_partitions :: Int -> [(Int,Int,Int)]
three_partitions m =
  [ (i,j,k) | i <- [0..(div m 3)], j <- [i..(div (m-i) 2)], k <- [m - (i+j)]]

remainders [] = []
remainders (r:rs) = (r:rs) : (remainders rs)

radical_generator :: Int -> Array Int [Radical]
radical_generator n =
  radicals
 where 
  radicals =
    array (0,n) ((0,[H]) : [(j,rads_of_size_n radicals j) | j <- [1..n]])

rads_of_size_n :: Array Int [Radical] -> Int -> [Radical]
rads_of_size_n radicals n =
  [ (C ri rj rk)
  | (i,j,k)  <- (three_partitions (n-1)),
    (ri:ris) <- (remainders (radicals!i)),
    (rj:rjs) <- (remainders (if (i==j) then (ri:ris) else radicals!j)),
    rk       <- (if (j==k) then (rj:rjs) else radicals!k)]

-- Generation of paraffins.

data Paraffin = BCP Radical Radical | CCP Radical Radical Radical Radical

bcp_generator :: Array Int [Radical] -> Int -> [Paraffin]
bcp_generator radicals n =
  if (odd n) then []
  else
    [ (BCP r1 r2) | (r1:r1s) <- (remainders (radicals!(div n 2))),
                    r2       <- (r1:r1s) ]
    
four_partitions :: Int -> [(Int,Int,Int,Int)]
four_partitions m =
  [ (i,j,k,l)
  | i <- [0..(div m 4)],
    j <- [i..(div (m-i) 3)],
    k <- [(max j (ceiling ((fromIntegral m)/(fromInteger 2)) - i - j))..(div (m-i-j) 2)],
    l <- [(m - (i+j+k))]]

ccp_generator :: Array Int [Radical] -> Int -> [Paraffin]
ccp_generator radicals n =
  [ (CCP ri rj rk rl)
  | (i,j,k,l) <- (four_partitions (n-1)),
    (ri:ris)  <- (remainders (radicals!i)),
    (rj:rjs)  <- (remainders (if (i==j) then (ri:ris) else radicals!j)),
    (rk:rks)  <- (remainders (if (j==k) then (rj:rjs) else radicals!k)),
    rl        <- (if (k==l) then (rk:rks) else radicals!l)]

bcp_until :: Int -> [Int]
bcp_until n =
  [length(bcp_generator radicals j) | j <- [1..n]]
 where
  radicals = radical_generator (div n 2)

ccp_until :: Int -> [Int]
ccp_until n =
  [length(ccp_generator radicals j) | j <- [1..n]]
 where
  radicals = radical_generator (div n 2)

paraffins_until :: Int -> [Int]
paraffins_until n =
  [length (bcp_generator radicals j) + length (ccp_generator radicals j)
   | j <- [1..n]]
 where
  radicals = radical_generator (div n 2)

main = do
  [arg] <- getArgs
  let num = read arg
  print [length (rads!i) | rads <- [(radical_generator num)], i <- [0..num]]
  print (bcp_until num)
  print (ccp_until num)
  print (paraffins_until num)
