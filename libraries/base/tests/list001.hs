{-# LANGUAGE CPP #-}
module Main where

import Data.List
import Control.Exception
#if __GLASGOW_HASKELL__ < 705
import Prelude hiding (catch)
#endif

-- This module briefly tests all the functions in PrelList and a few
-- from List.

-- ToDo: test strictness properties.

main = do

  -- head
  print (head [1,2,3,4], head "a")
  catch (print (head [] :: String)) (\(ErrorCall _) -> putStr "head []\n")

  -- tail
  print (tail [1,2,3,4], tail "a")
  catch (print (tail [] :: String)) (\(ErrorCall _) -> putStr "tail []\n")

  -- init
  print (init [1,2,3,4], init "a")
  catch (print (init [] :: String)) (\(ErrorCall _) -> putStr "init []\n")

  -- last
  print (last [1,2,3,4], last "a")
  catch (print (last [] :: String)) (\(ErrorCall _) -> putStr "last []\n")

  -- null
  print [null [], null "abc"]

  -- length
  print (length [1..10])

  -- foldl
  print (foldl  (+) 1 [1..10])

  -- foldl1
  print (foldl1 (+) [1..10])
  catch (print (foldl1 (+) [] :: Int)) (\(ErrorCall _) -> putStr "foldl1 []\n")

  -- scanl
  print (scanl  (+) 1 [1..10])

  -- scanl1
  print (scanl1 (+) [1..10])
  print (scanl1 (+) [] :: [Int])

  -- foldr1
  print (foldr1 (+) [1..10])
  catch (print (foldr1 (+) [] :: Int)) (\(ErrorCall _) -> putStr "foldr1 []\n")

  -- scanr
  print (scanr  (+) 1 [1..10])

  -- scanr1
  print (scanr1 (+) [1..10])
  print (scanr1 (+) [] :: [Int])

  -- iterate
  print (take 10 (cycle (take 4 (iterate (+1) 1))))

  -- take
  print (take 4 (repeat "x"), take 0 (repeat "x"), take 5 [1..4])
  catch (print (take (-1) [1..10])) (\(ErrorCall _) -> putStr "take (-1)\n")

  -- replicate
  print [replicate 2 "abc", replicate 0 "abc", replicate 3 []]

  -- drop
  print [drop 5 [1..10], drop 0 [1..10], drop 5 [1..4]]
  catch (print (drop (-1) [1..10])) (\(ErrorCall _) -> putStr "drop (-1)\n")

  -- splitAt
  print [splitAt 5 [1..10], splitAt 5 [1..4]]
  catch (print (splitAt (-1) [1..10])) (\(ErrorCall _) -> putStr "splitAt (-1)\n")

  -- scan
  print (span (<5) [1..10])

  -- break
  print (break (<5) [1..10])

  -- reverse
  print [reverse [1..10], reverse []]

  -- and
  print [and [], and [True], and [False]]

  -- or
  print [or [], or [True], or [False]]

  -- elem
  print [elem 5 [1..10], elem 0 [1..10], elem 1 []]

  -- notElem
  print [notElem 5 [1..10], notElem 0 [1..10], notElem 1 []]

  -- lookkup
  print (lookup 4 (zip [1..10] (reverse [1..10])))

  -- sum
  print [sum [1..10], sum []]

  -- product
  print [product [1..10], product []]

  -- maximum
  print (maximum [1..10])
  catch (print (maximum [] :: Int)) (\(ErrorCall _) -> putStr "maximum []\n")

  -- minimum
  print (minimum [1..10])
  catch (print (minimum [] :: Int)) (\(ErrorCall _) -> putStr "minimum []\n")

  -- concatMap
  print (concatMap (:[]) [(1::Int)..10])

  -- zip
  print [zip [1] [2], zip [1] [], zip [] [2], zip [1..5] [2..6]]

  -- zip3
  print (zip3 [1,2] [3,4] [5,6])

  -- zipWith
  print [zipWith (+) [1,2] [3,4], zipWith (+) [1] [], zipWith (+) [] []]

  -- unzip
  print [unzip [(1,2),(3,4)], unzip []]

  -- unzip3
  print [unzip3 [(1,2,3),(3,4,5)], unzip3 []]

  -- unlines
  print (unlines (lines "a\nb\nc\n"), lines "", unlines [])

  -- words
  print (unwords (words "a b c d"),   words "", unwords [])

  -- deleteBy
  print [deleteBy (==) 1 [0,1,1,2,3,4], 
	 deleteBy (==) (error "deleteBy") []]

  -- delete
  print [delete 1 [0,1,1,2,3,4], 
	 delete (error "delete") []]
  
  -- (\\)
  print [ [0,1,1,2,3,4] \\ [3,2,1],
          [1,2,3,4] \\ [],
	  [] \\ [error "\\\\"] ]
