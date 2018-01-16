{-# OPTIONS -cpp #-}
{-# LANGUAGE BangPatterns #-}

{- Matrix multiplication using a torus (gentleman algorithm) -- FR10 --    -}

{-
RL/JB ParCo2005: eliminate result communication (Maybe-Type)

JB PhD2008: adapt for simple PhD skeleton tests

JB MSR07/2008: modified to use all available toroid skeletons.

JB MSR07/2008: derived a straight-forward GpH program using identical
               helpers and strategies

JB optimised prodEscalar

JB for ghc-6.9: replaced Control.Parallel.Strategies by a workaround
   (reexporting what should work)

-}
        
module Main(main) where

import System.Environment
import Data.List hiding (foldl', foldl1')

import ListAux
import Control.DeepSeq

-- replaced by StratWorkaround, excluding what does not work with
-- ghc-6.9
#ifdef WORKAROUND
import StratWorkaround
-- workaround hacks needed for ghc-6.9:
parList :: Strategy a -> Strategy [a]
parList = parListStrict -- as name suggests: it is strict!
parListChunk :: Int -> Strategy a -> Strategy [a]
parListChunk c strat l = let subLs = splitAtN c l
                             evaluateMe = (map (seqList strat) subLs)::[()]
                         in parListStrict rnf evaluateMe

#else
import Control.Parallel.Strategies 
#endif

-----------  matrix strategies here:
strats :: [ Int -> Strategy Matrix {- == Int -> [[Int]] -> Done -} ]
strats = [ undefined, -- do not use it!
            lineStrat, blockStrat, columnStrat]
names    = ["sequential",
            "linewise", "blockwise", "columnwise"] 

lineStrat c          = parListChunk c rdeepseq -- OK?
columnStrat c matrix = parListChunk c rdeepseq (transpose matrix) -- bad ?
blockStrat c  matrix -- best?
    = let blocks = concat (splitIntoClusters numB matrix) -- result splitted
                                                    -- in numB * numB blocks
          numB  = round (sqrt (fromIntegral (length matrix) / fromIntegral c))
                  -- approx. same num/granularity of sparks as in others...
      in fmap concat $ parList rdeepseq blocks

undef _ _ = error "undefined strategy"

-------------------------------------

type Vector = [Int]
type Matrix = [Vector]

-- main computation, different versions:
mult :: Int -> Matrix -> Matrix -> Int -> [[Maybe Matrix]]
mult 0 m1 m2 _ = 
#ifdef OUTPUT
        [[Just $ multMatricesTr m1 (transpose m2)]]
#else
        rnf (multMatricesTr m1 (transpose m2)) `seq` [[Nothing]]
#endif
mult v m1 m2 c = results
 where results ::  [[Maybe Matrix]]
#ifdef OUTPUT
       results = [[Just computed]]
#else
       results = (rnf computed `seq` [[Nothing]])
#endif
       computed = multMatricesTr m1 m2Tr `using` (strats'!!v) c
       strats' = strats ++ repeat undef
       m2Tr = transpose m2

prMM' :: (Matrix,Matrix) -> Matrix
prMM' (c,mt) = [[prVV f c | c <- mt]|f <-c]
prVV :: Vector -> Vector -> Int
prVV f c = sum (zipWith (*) f c)
 
shiftRight c [] = []
shiftRight c (xs:xss) = (xs2++xs1):shiftRight (c-1) xss
 where (xs1,xs2) = splitAt c xs

shiftDown c xss = transpose (shiftRight c (transpose xss))

join2 :: Matrix -> Matrix -> Matrix
join2 xs ys = zipWith (++) xs ys
join :: [Matrix] -> Matrix
join xss = foldr join2 (repeat []) xss
       
splitIntoClusters :: Int -> Matrix -> [[Matrix]]
splitIntoClusters c m | c < 1 = splitIntoClusters 1 m
splitIntoClusters c m1 = mss
  where bh  = kPartition (length m1) c
        bhsplit [] [] = []
        bhsplit [] _  = error "some elements left over"
	bhsplit (t:ts) xs = hs : (bhsplit ts rest)
	  	  where (hs,rest) = splitAt t xs   
        ms  = bhsplit bh m1 -- blocks of rows 
        mss = map (colsplit bh) ms
        colsplit [] _  = []
        colsplit (t:ts) rs
         | head rs == [] = []
         | otherwise = (cab:colsplit ts resto)
          where  (cab,resto) = unzip (map (splitAt t) rs)
        
--        mss = map (repartir (length m1 `div` c)) ms
--        repartir c xs
--         | head xs == [] = []
--         | otherwise = (cab:repartir c resto)
--          where  (cab,resto) = unzip (map (splitAt c) xs)

-- helper for splitIntoClusters (formerly bresenham)
kPartition :: Int -> Int -> [Int]
kPartition n k = zipWith (+) ((replicate (n `mod` k) 1) ++ repeat 0)
                             (replicate k (n `div` k))

          
mult' :: Int -> Int -> ((Matrix,Matrix),[Matrix],[Matrix]) -> (Maybe Matrix,[Matrix],[Matrix])
mult' nc nr ((sm1,sm2),sm1s,sm2s) 
#ifdef OUTPUT
    =  (Just result,toRight,toDown)
#else
    =  (rnf result `seq` Nothing ,toRight,toDown)
#endif
  where toRight  = take (nc-1) (sm1:sm1s)
        toDown   = take (nr-1) (sm2':sm2s)
        sm2'     = transpose sm2
        sms      = zipWith multMatricesTr (sm1:sm1s) (sm2':sm2s)
        result = foldl1' addMatrices sms  -- foldr1: not enough demand??

        
addMatrices :: Matrix -> Matrix -> Matrix
addMatrices m1 m2 = zipWith addVectors m1 m2
  where addVectors :: Vector -> Vector -> Vector
        addVectors v1 v2 = zipWith (+) v1 v2

-- Assumes the second matrix has already been transposed        
multMatricesTr :: Matrix -> Matrix -> Matrix
multMatricesTr m1 m2 = [[prodEscalar2 row col | col <- m2] | row <- m1]

-- JB 2008: a lot faster, directly consuming lists, and tail-recursive (optimised with -O2)
prodEscalar2JB :: Vector -> Vector -> Int
prodEscalar2JB v1 v2 = addProd v1 v2 0
    where addProd :: Vector -> Vector -> Int -> Int
          addProd (v:vs) (w:ws) acc = addProd vs ws (acc + v*w)
          addProd [] [] n = n
          addProd _  _  _ = error "addProd: length does not match"

-- JB 2008: identical when using ghc-6.8.3, avoids bug in ghc-HEAD. Version suggested by SM
prodEscalar2 :: Vector -> Vector -> Int
prodEscalar2 v1 v2 = addProd v1 v2 0
addProd :: Vector -> Vector -> Int -> Int
addProd (v:vs) (w:ws) !acc = addProd vs ws (acc + v*w)
addProd _ _ !n = n


prodEscalar :: Vector -> Vector -> Int
prodEscalar v1 v2 = sum (zipWith (*) v1 v2)


------- foldl, strict in head element
foldl1' :: NFData a => (a->a->a) -> [a] -> a
foldl1' f (x:xs) = foldl' f x xs

foldl'           ::  NFData a => (a -> b -> a) -> a -> [b] -> a
foldl' f a []     = a
foldl' f a (x:xs) = -- whnf, not enough( (foldl' f) $! (f a x)) xs
		    let first = f a x 
		    in rnf first `seq` foldl' f first xs


usage :: String -> String
usage name = "Cannon's algorithm: Usage:\n\t "++
	     name ++ " <matrix size> <version> <blocksPerRow> \n" ++
             "Version selects from " ++ show (zip [0..] names)

main = do 
       args <- getArgs
       let l = length args
       if l == 0 then do n <- getProgName
                         putStrLn (usage n)
                         putStrLn "\n *** defaults: size 100, seq. computation ***"
                 else return () --putStrLn "Cannon's algorithm"

       let    size  = if null args then 100 else read (head args)
	      opt   = if length args < 2 then 0 else read (args!!1)
	      chunk = if length args < 3 then 1 
                                         else read (args!!2)
	      a = "Matrices of size " ++ show size ++ 
                  " with skeleton " ++ ((names++repeat "UNDEF")!!opt) ++
                  " using chunk parameter " ++ show chunk ++ "\n"
	      res = mult opt (mA size) (mB size) chunk
	      b = multMatricesTr (mA size) (transpose (mB size))
       -- putStrLn a
#ifdef OUTPUT
       putStrLn "Output wanted, checking result for correctness..."
       let computed = map (map fromJust) res
           computed' = concat (map join computed)
       printMat computed'
       if (b == computed')  
                 then putStrLn "Correct!"
                 else do putStrLn "WRONG RESULT! Should be"
                         printMat b
#else
       -- putStrLn "No Output, matrix stays distributed."
       putStrLn (show res)
#endif        


         
m1 size = replicate size [1..size]
m2 size = listToListList size [1..size*size]
mA size = if size <= 4000 then m1 size else listToListList size (concat (take 20 (repeat [1..(size*size `div` 20)])))
mB size = if size <= 4000 then m1 size else listToListList size (concat (take 20 (repeat [0,2.. ((size*size) `div` 20)-2])))
listToListList c m 
 | length m <= c = [m]
 | otherwise = c1 : listToListList c resto
  where (c1,resto) = splitAt c m


printMat :: Matrix -> IO ()
printMat m = putStrLn ("Matrix: " ++ (show (length (head m))) 
                       ++ " x " ++ (show $ length m) ++ "\n" 
                       ++ (showMat m))

-- instance Show a => Show (Matrix a) where
showMat m_ = "<<" ++ unlines (map (concatMap (\ x -> show x ++ " ")) m_) ++ ">>"

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "fromJust"
