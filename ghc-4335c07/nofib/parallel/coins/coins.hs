{-# LANGUAGE BangPatterns #-}

import Data.List
import System.Environment
import Control.Parallel
import Control.Parallel.Strategies
import Control.Applicative

-- Rough results, GHC 6.13: (val=777)
--   V1 (SDM):             2.2s
--   V2 (SDM):             2.7s
--   V3 (SDM, parallel):   1.0s on 7 cores
--   V4 (original):        got bored waiting
--   V5 (HWL assoc):       5.2s
--   V6 (SDM, Int result): 0.9s
--   V7 (SDM, parallel):   0.2s on 7 cores

-----------------------------------------------------------------------------
-- Version 1: returns results as a list of list of coins

payL :: Int -> [(Int,Int)] -> [Int] -> [[Int]]
payL 0   coins     acc = [acc]
payL _   []        acc = []
payL val ((c,q):coins) acc
  | c > val   = payL val coins acc
  | otherwise = left ++ right
  where
    left  = payL (val - c) coins' (c:acc)
    right = payL val coins acc

    coins' | q == 1    = coins
           | otherwise = (c,q-1) : coins

-----------------------------------------------------------------------------
-- Version 2: uses a custom AList type to avoid repeated appends

-- The idea here is that by avoiding the append we might be able to
-- parallelise this more easily by just forcing evaluation to WHNF at
-- each level.  I haven't parallelised this version yet, though (V5
-- below is much easier) --SDM

data AList a = ANil | ASing a | Append (AList a) (AList a)

lenA :: AList a -> Int
lenA ANil          = 0
lenA (ASing _)     = 1
lenA (Append l r)  = lenA l + lenA r

append ANil r = r
append l ANil = l -- **
append l r    = Append l r

   -- making append less strict (omit ** above) can make the algorithm
   -- faster in sequential mode, because it runs in constant space.
   -- However, ** helps parallelism.

payA :: Int -> [(Int,Int)] -> [Int] -> AList [Int]
payA 0   coins     acc = ASing acc
payA _   []        acc = ANil
payA val ((c,q):coins) acc
  | c > val   = payA val coins acc
  | otherwise = append left right -- strict in l, maybe strict in r
  where
    left  = payA (val - c) coins' (c:acc)
    right = payA val coins acc
    coins' | q == 1    = coins
           | otherwise = (c,q-1) : coins

-----------------------------------------------------------------------------
-- Version 3: parallel version of V2

payA_par :: Int -> Int -> [(Int,Int)] -> [Int] -> AList [Int]
payA_par 0     val coins acc = payA val coins acc
payA_par _     0   coins acc = ASing acc
payA_par _     _   []    acc = ANil
payA_par depth val ((c,q):coins) acc
  | c > val    = payA_par depth val coins acc
  | otherwise  = res
                
  where
    res = runEval $ pure append <*> rpar left <*> rseq right

    left  = payA_par (if q == 1 then (depth-1) else depth) (val - c) coins' (c:acc)
    right = payA_par (depth-1) val coins acc

    coins' | q == 1    = coins
           | otherwise = (c,q-1) : coins

-----------------------------------------------------------------------------
-- Version 4: original list-of-list version (very slow)

pay :: Int -> Int -> [Int] -> [Int] -> [[Int]]
pay _   0 coins accum   = [accum]
pay _   val [] _        = []
pay pri val coins accum = 
    res		
    where -- 
          coins'  = dropWhile (>val) coins
          coin_vals = nub coins'      
          res = concat ( map
                           ( \ c -> let 
                                      new_coins = 
                                          ((dropWhile (>c) coins')\\[c])
                                    in 			   
                                      pay (pri-1)
				          (val-c) 
                                          new_coins
     	                                  (c:accum)
                           )
                           coin_vals )


-----------------------------------------------------------------------------
-- Version 5: assoc-list version (by HWL?)

-- assoc-list-based version; still multiple list traversals
pay1 :: Int -> Int -> [(Int,Int)] -> [(Int,Int)] -> [[(Int,Int)]]
pay1 _   0 coins accum   = [accum]
pay1 _   val [] _        = []
pay1 pri val coins accum = res
    where --
          coins'  = dropWhile ((>val) . fst) coins
          res = concat ( 
                         map
                           ( \ (c,q) -> let 
                                          -- several traversals                                        
                                          new_coins = 
                                            filter (not . (==0) . snd) $
                                             map (\ x'@(c',q') -> if c==c' then (c',q'-1) else x') $
                                              dropWhile ((>c) . fst) $
                                               coins'
                                          new_accum = 
                                            map (\ x'@(c',q') -> if c==c' then (c',q'+1) else x') accum
                                        in 			   
                                      	  pay1 (pri-1)
				      	      (val-c) 
                                      	      new_coins
     	                              	      new_accum
                           )
                           coins' )

-----------------------------------------------------------------------------
-- Version 6: just return the number of results, not the results themselves

payN :: Int -> [(Int,Int)] -> Int
payN 0   coins     = 1
payN _   []        = 0
payN val ((c,q):coins)
  | c > val   = payN val coins
  | otherwise = left + right
  where
    left  = payN (val - c) coins'
    right = payN val coins

    coins' | q == 1    = coins
           | otherwise = (c,q-1) : coins

-----------------------------------------------------------------------------
-- Version 7: parallel version of payN

payN_par :: Int -> Int -> [(Int,Int)] -> Int
payN_par 0     val coins  = payN val coins
payN_par _     0   coins  = 1
payN_par _     _   []     = 0
payN_par depth val ((c,q):coins)
  | c > val    = payN_par depth val coins
  | otherwise  = res
                
  where
    res = right `par` left `pseq` left + right

    left  = payN_par (if q == 1 then (depth-1) else depth) (val - c) coins'
    right = payN_par (depth-1) val coins

    coins' | q == 1    = coins
           | otherwise = (c,q-1) : coins

-----------------------------------------------------------------------------
-- driver

main = do
         let vals = [250, 100, 25, 10, 5, 1]   
         -- let quants = [1, 3, 2, 5, 7, 12]		   -- small setup
         -- let quants = [5, 8, 8, 9, 12, 17]           -- std setup
         let quants = [55, 88, 88, 99, 122, 177]  -- large setup

         let coins  = concat (zipWith replicate quants vals)
             coins1 = zip vals quants

         [n, arg] <- fmap (fmap read) getArgs

         case n of
           -- sequential, list of results
           1 -> print $ length $ payL arg coins1 []
           -- sequential, append-list of results
           2 -> print $ lenA   $ payA arg coins1 []
           -- parallel, append-list of results
           3 -> print $ lenA   $ payA_par 3 arg coins1 []

           4 -> print $ length (pay 0 arg coins [])
           5 -> print $ length (pay1 0 arg coins1 (map (\(c,q) -> (c,0)) coins1))
           6 -> print $ payN arg coins1
           7 -> print $ payN_par 4 arg coins1
