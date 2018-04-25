{-# OPTIONS -cpp #-}
{- 
    warshall*.hs

Finding Shortest Paths in a Graph using the 
Warshall algorithm and a ring topology.

author: Rita Loogen, Jost Berthold
        Philipps-Universität Marburg

based on a  concurrent Clean program in \cite{CleanBook}

Version for recursive ring skeleton and GpH-style ring,
to use in IFL08 paper on Parallel-Haskell-on-multicore.

------------------------------------------------- -}

module Main(main) where
import System.Environment
import Data.List

-- import Eden

-- import EdenRing(badRing',ring,ringRec)
-- import EdiRing(edirings,ediringnames)

-- import PipeRings

#ifdef EAGER
import StrategiesEager(parList)
#else
import Control.Parallel.Strategies
#endif

--rings     = {- [seqring,badRing',ring,ringRec] ++ edirings ++ piperings 
--             ++ -} [gphRing] -- ring size parameter == total no. of sparks

-- ringnames = {- ["seq. ring","Eden badRing'","Eden ring","Eden ringRec"]
--	    ++ ediringnames ++ piperingNames ++ -} ["GpH ring"]

-- gph ring, simply by sparking all ring process outputs
-- (granularity control by ring size)
gphRing np splitIn combOut ringF input 
    =  combOut (outs `using` parList rdeepseq)
    where ringIns      = splitIn np input
	  (outs,rComm) = foldl' fld ([],rComm) ringIns
--	  fld :: ([o],[r]) -> i -> ([o],[r])
          fld (outs,rIn) inp = let (out,rOut) = ringF (inp,rIn)
                               in (out:outs, rOut)


-- reference: sequential version
seqring np splitIn combOut ringF input 
    = combOut outs
    where ringIns      = splitIn np input
	  (outs,rComm) = foldl' fld ([],rComm) ringIns
--	  fld :: ([o],[r]) -> i -> ([o],[r])
          fld (outs,rIn) inp = let (out,rOut) = ringF (inp,rIn)
                               in (out:outs, rOut)

type Matrix a = [[a]]
dim :: Matrix a -> Int
dim = length

-- warshallRing' :: Int -> Matrix Int -> Matrix Int
-- warshallRing' _ mat = warshallRing (mat, dim mat) 
-- 
-- -- ring size = matrix dimension
-- warshallRing :: (Matrix Int,Int) -> Matrix Int
-- warshallRing (mat,n) = ring (length mat) split concat rf (mat,n)
--   where 
--      split n (mat,_) = zip (splitIntoN n mat) [1..n]
--      rf  (([row],k), inpleft)  = ([sol],outp) 
--              where (sol,outp) =  ring_iterate (length row) k 1 row inpleft

-- ring size = parameter np
wr2 :: Int -> Matrix Int -> Matrix Int
--wr2 v _ mat | (v == length ringnames - 1) 
--                -- treat as a special case, want many more sparks!
--                = wr_ gphRing (length mat) mat
wr2 np mat = wr_ gphRing np mat

-- wr_ :: ring skel type -> Int -> Matrix Int -> Matrix Int
wr_ ring np mat = ring np split concat ringf (mat,0) -- ring type issue, 0 is dummy parameter
    where split :: Int -> (Matrix Int, Int) -> [ (Matrix Int,    Int      )]
      --       ring size  all input    [ (some rows ,no. of 1st row)]
	  split n (mat,_) = let inputrows = splitIntoN n mat -- [[r1..rk],[r(k+1)..r(2k)]..[r(i*k)..r(dim mat)]]
			    in zip inputrows  (scanl (+) 1 (map length inputrows)) -- should be "(init (scanl (+)...)"
	  ringf :: ((Matrix Int, Int), [[Int]] )  -> ( Matrix Int  ,   [[Int]])
      --           ((some rows,start),more rows)  -> ( result rows , rows for ring)
	  ringf ((rows,startrow), fromLeft) = create_procs (length $ head rows) startrow rows fromLeft -- 

-- sequential version from Clean book
warshall :: Int -> Matrix Int -> Matrix Int
warshall  _ mat = solution 
    where (solution, output) = create_procs (length mat) 1 mat output

create_procs :: Int -> Int -> Matrix Int -> [[Int]] -> ([[Int]],[[Int]])
create_procs size k [rowN] inputleft = ([rowNsolution], output)
    where (rowNsolution, output) = ring_iterate size k 1 rowN inputleft
create_procs size k (rowk:restmat) inputleft = (rowksolution:restsolutions, outputN)
    where (rowksolution, outputk) = ring_iterate size k 1 rowk inputleft
	  (restsolutions, outputN) = create_procs size (k+1) restmat outputk

ring_iterate :: Int -> Int -> Int -> [Int] -> [[Int]] -> ([Int],[[Int]])
ring_iterate size k i rowk rows 
    | i > size =  (rowk, []) --iterations_finished
    | i == k   =  (solution, rowk:restoutput) --  start_sending_this_row
    | otherwise = (solution, rowi:restoutput)
    where rowi:xs = rows
	  (solution, restoutput) = rnf nextrowk `seq` 
				   ring_iterate size k (i+1) nextrowk xs
	  nextrowk | i == k = rowk -- no update for own row 
		   | otherwise = updaterow rowk rowi distki
	  distki  = rowk!!(i-1)

--updaterow :: 
updaterow [] rowi distij = [] 
updaterow (distjk:restrowj) (distik:restrowi) distji
	= min distjk (distji + distik):updaterow restrowj restrowi distji

-------------------------------------------------------------------------------

usage :: String
usage = "Usage:\n" ++
	"\t #> warshall <size> <ring size: default noPe> <..rest is ignored..>\n"

main =  do [size,noPe] <- fmap (map read) getArgs
           let res = wr2 noPe (m1 size)
           -- print res 
           rnf res `seq` putStrLn "done"

test6 :: Matrix Int
test6 = [[ 0, 100, 100, 13, 100, 100], [100, 0, 100, 100, 4,9],
         [11, 100, 0, 100, 100, 100], [100, 3, 100, 0, 100, 7],
         [15, 5, 100, 1, 0, 100], [11, 100, 100, 14, 100, 0]]
{-
Adjacency Matrix                         Shortest Paths:

         - - To - - 
   [[  0 , 100, 100,  13, 100, 100],        [[ 0,16,100,13,20,20],
 F  [100 ,   0, 100, 100,   4,   9],         [19, 0,100, 5, 4, 9],
 r  [ 11 , 100,   0, 100, 100, 100],         [11,27,  0,24,31,31],
 o  [100 ,   3, 100,   0, 100,   7],         [18, 3,100, 0, 7, 7],
 m  [ 15 ,   5, 100,   1,   0, 100],         [15, 4,100, 1, 0, 8],
    [ 11 , 100, 100,  14, 100,   0]]         [11,17,100,14,21, 0]]
-}
test3 :: [[Int]]
test3 = [[0,1,100],[1,0,1],[100,1,0]]   

m1 size = replicate size [1..size]
m2 size = listToListList size [1..size*size]
mA size = if size <= 4000 then m1 size else listToListList size (concat (take 20 (repeat [1..(size*size `div` 20)])))
mB size = if size <= 4000 then m1 size else listToListList size (concat (take 20 (repeat [0,2.. ((size*size) `div` 20)-2])))
listToListList c m 
 | length m <= c = [m]
 | otherwise = c1 : listToListList c resto
  where (c1,resto) = splitAt c m

---------------------
splitIntoN :: Int -> [a] -> [[a]]
splitIntoN n xs = takeIter parts xs
  where l = length xs
        parts = zipWith (+) ((replicate (l `mod` n) 1) ++ repeat 0)
                            (replicate n (l `div` n))
takeIter :: [Int] -> [a] -> [[a]]
takeIter [] [] = []
takeIter [] _  = error "elements left over"
takeIter (t:ts) xs = hs : takeIter ts rest
    where (hs,rest) = splitAt t xs

unshuffle :: Int -> [a] -> [[a]]
unshuffle n xs = [takeEach n (drop i xs) | i <- [0..n-1]]
 where takeEach n [] = []
       takeEach n (x:xs) = x : takeEach n (drop (n-1) xs)
-- inverse to unshuffle
shuffle :: [[a]] -> [a]
shuffle = concat . transpose
