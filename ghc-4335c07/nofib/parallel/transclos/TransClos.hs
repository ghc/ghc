-- Time-stamp: <2010-11-03 11:40:43 simonmar>
-- Versions of computing a transitive closure to a given relation.
-- Based on this lecture (in German, my apologies):
--  http://www2.tcs.ifi.lmu.de/lehre/SS09/Fun/AFP_04.pdf
-- Exercises 'Blatt 4' corresponding to AFP_04.tex:
--  http://www2.tcs.ifi.lmu.de/lehre/SS09/Fun/Exc04.pdf
-----------------------------------------------------------------------------

module TransClos where

-- import BinTree
import Data.List as List
import qualified Data.Set
-- import CircPrgs(nub2)

-- P-11

-- sieve of Erathostenes; generates a lot of intermediate lists
sieve :: [Integer]
sieve = sieve' [2..]
        where sieve' (x:xs) = x:(sieve' . filter (\n -> n `mod` x /= 0) $ xs)

-- sum (take 5555 sieve)
-- 143086552
-- (14.00 secs, 940991964 bytes)
-- ca. 940MB total

-- circular program computing all prime numbers (AFP_04)
-- is circular, but generates some intermediate lists when checking a candidate
primes1 :: [Integer]
primes1 = 2:[ n | n <- [3,5..], all (\ p -> n `mod` p /= 0) . takeWhile (\p -> p^2 <= n) $ primes1 ]

-- sum (take 5555 primes1)                                                                                                    
-- 143086552
-- (0.63 secs, 57357568 bytes)
-- ca. 57MB total

-- circular, without generating intermediate lists
primes2 :: [Integer]
primes2 = 2:(filter (not . multiple primes2) [3,5..])
          where multiple (x:xs) n  | x*x > n = False
                                   | n `mod` x == 0 = True
                                   | otherwise = multiple xs n
-- sum (take 5555 primes2)                                                                                                    
-- 143086552
-- (0.56 secs, 22138868 bytes)
-- ca. 22MB total

-----------------------------------------------------------------------------
-- P-12

-- naive, non-circular version
transcl' :: (Eq a) => (a -> [a]) -> [a] -> [a]
transcl' r xs = if xs==xs'
                 then xs
                 else transcl' r xs'
                where xs' = foldl union xs (map r xs)

-- transcl' (r1 444) [1]
-- (5.65 secs, 1135677448 bytes)
-- ca 1.1GB

-- simple circular version
-- the basic idea is shown by this simplified prg using a 1-to-1 relation only
-- the list comp picks an elem from earlier in the *result* list, feeds it through
-- the relation and adds it to the result list if it's not there already
-- of course, we must make sure that elem doesn't search further in the list than the current elem!
transcl_simp :: (Eq a) => (a -> a) -> [a] -> [a]
transcl_simp r xs = zs
                    where zs = xs ++ [ x' | (n,x) <- zip [1..] zs, let x' = r x, not (x' `elem` take n zs) ] 
                    -- possibly restrict the initial segment being searched, to increase parallelism: ^^^ (take (n `div` 2) zs)) ]

-- version that does not check for duplicates! -- , not (x' `elem` (take n zs)) ]
transcl_dup :: (Eq a) => (a -> a) -> [a] -> [a]
transcl_dup r xs = zs
                   where zs = xs ++ [ x' | (n,x) <- zip [1..] zs, let x' = r x ]

-- main parallel version:
-- producing a list-of-list improves parallelism, since the position of an element 
-- does not depend on all the previous elements
transcl_nested :: (Eq a) => (a -> [a]) -> [a] -> [[a]]  {- [a] -}
transcl_nested r xs = {- (nub . concat) -}  zss
                    where -- zss :: [[a]]
                          zss = xs:(build 1 zss)
	                  -- build :: Int -> [[a]] -> [[a]]
                          build j []       = []
                          build j (xs:xss) = zss' ++ build (j+length zss') xss
                                             where zss' = [ filter (not . (`elem` (concat (take j zss)))) xs' | x <- xs, let xs' = r x ] 
                                             -- where zss' = [ filter (not . or . (map (`elem` (take j zss)))) xs' | x <- xs, let xs' = r x ] 

-- main circular version (seq)
transcl :: (Eq a) => (a -> [a]) -> [a] -> [a]
transcl r xs = xs'
               where
                     xs' = xs ++ build 0 (length xs) 
                     -- m and n is the interval that is used to generate new elements
                     build m n = if List.null ys'  
                                  then []
                                  else ys' ++ build n (n + length ys')
                                 where ys' = filter (not . (`elem` (take (n-1) xs'))) $ foldl union [] [ ys | y <- take (n-m) (drop m xs'), let ys = r y ] 

-- transcl (r1 444) [1]
-- (0.02 secs, 3367572 bytes)
-- ca 3.4MB
-- transcl (r1 666) [1]
-- (0.03 secs, 6617576 bytes)
-- ca 6.6MB

-- circular version, using sets rather than lists
transcl_set :: (Ord a, Eq a) => (a -> Data.Set.Set a) -> Data.Set.Set a -> Data.Set.Set a
transcl_set r xs = foldl Data.Set.union Data.Set.empty xs'
                   where
                     xs' = [xs] ++ build xs 1
                     -- build :: (Ord a, Eq a) => Data.Set.Set a -> Int -> [Data.Set.Set a]
                     build s n = if Data.Set.null ys'  
                                   then []
                                   else [ys'] ++ build ys' (n+1)
                                 where ys' = Data.Set.filter (is_new ys0) $
                                              foldl Data.Set.union Data.Set.empty [ ys | y <- Data.Set.toList s, let ys = r y ] 
                                       ys0 = take n xs'

                                       is_new ([]) y                              = True
                                       is_new (xs:xss) y | y `Data.Set.member` xs = False
                                                         | otherwise              = is_new xss y
                                       	    

-- transcl_set (r1_set 444) (Data.Set.fromList [1])
-- (0.07 secs, 3884380 bytes)
-- ca 3.8MB

-- this version tracks the interval which generated a list element
-- t1 :: (Eq a) => (a -> [a]) -> [a] -> [a]
transcl_dbg r xs = xs'
                   where
                     xs' = [ (x,0,0) | x <- xs ] ++ build 0 (length xs) 
                     build m n = if List.null ys'
                                  then []
                                  else ys' ++ build n (n + length ys')
                                 where ys' = filter (not . (`elem` (take (n-1) xs'))) $ foldl union [] [ ys | (y,_,_) <- take (n-m) (drop m xs'), let ys = [ (y,m,n) | y <- r y ] ]

r1 b n | n<b       = [n+1]  
       | otherwise = []

r1_set b n = Data.Set.fromList (r1 b n)

r2 b n | n<b       = [ m | m <- [(n-1),(n-2)..1] , even m ]  -- n R m iff m is an even number less than n
       | otherwise = []

r2_set b n = Data.Set.fromList (r2 b n)

