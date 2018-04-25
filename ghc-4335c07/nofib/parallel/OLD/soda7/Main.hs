------------------------------------------------------------------
-- Time-stamp: <2009-05-07 13:48:09 simonmar>
--
-- Searching in a grid of words for hidden words oriented in any of
-- the 8 possible directions.
--
-- Version 9
-- Hans-Wolfgang Loidl
-- search parallel lines in parallel
-- Using Strategies to define parallelism.
-- Tunable grid size and number of words to search.
-- Haskell98 version.
-- 
------------------------------------------------------------------

module Main(main) where

-- @menu
-- * Imports::			
-- * Datatypes::			
-- * Main fct::			
-- * Aux fcts::			
-- @end menu

-- @node Imports, Datatypes
-- @section Imports

import Control.Parallel
import Control.Parallel.Strategies
import Data.List          ( transpose )
import System.Environment ( getArgs )
import Data.Char
import System.Random
import Control.Exception

-- @node Datatypes, Main fct, Imports
-- @section Datatypes

data	DIRS = RIGHT | DOWN | DOWNLEFT | UPLEFT
	     | LEFT  | UP   | UPRIGHT  | DOWNRIGHT 
	     deriving (Show,Eq)

instance NFData DIRS

-- @node Main fct, Aux fcts, Datatypes
-- @section Main fct

main = do 
        [n] <- fmap (fmap read) getArgs
        grid <- mk_grid n           -- build a grid of the given size
        evaluate (rnf grid)
        let 
    	 r    = grid
    	 d    = transpose grid
    	 dl   = diagonals grid
    	 ul   = diagonals (reverse grid)

         locate word grid = myfilter (any (contains word)) grid

         find :: String -> (String, [DIRS])
         find word = (word, dirs)
                     where
                       dirs = forw ++ back
                       forw = locate word
                              [(r,RIGHT), (d,DOWN), (dl,DOWNLEFT), (ul,UPLEFT)]
                       back = locate drow
                              [(r,LEFT), (d,UP), (dl,UPRIGHT), (ul,DOWNRIGHT)]
                       drow = reverse word

         -- res = map find hidden -- all matches as (word, [dirs])
         res = map (length . snd . find ) hidden -- only count no. of matches
               `using` myParList
               {-
               `using` \ r ->(rnf d   >||
                       	      rnf dl  >||
                       	      rnf ul)  >|
                       	      parList rnf r
               -}
         res_str = show (sum res) ++ " matches"

         {- if returning all matches do:
         res_str = show $
                   foldr (\ (word,dirs) str -> 
                            ((word ++ " -> " ++ (show dirs) ++ "\n") ++ str)) 
	                 [] 
                         res
         -}

       	putStrLn res_str -- "done"

myParList [] = ()
myParList (x:xs) = x `par` myParList xs
        
-- @node Aux fcts,  , Main fct
-- @section Aux fcts

-- plain seq fct on a slightly weird list
myfilter f [] = []
myfilter f ((g,d):gds) = -- rest `par`
		          (if cond then d : rest
		     	           else rest)
		     	  where cond = f g
			        rest = myfilter f gds

diagonals [r] = map (:[]) r
diagonals (r:rs) = zipinit r ([]:diagonals rs)

zipinit [] ys = ys
zipinit (x:xs) (y:ys) = (x : y) : zipinit xs ys

contains xs ys = any (prefix xs) (suffixes ys)

suffixes [] = []
suffixes xs = xs : suffixes (tail xs) 

prefix [] ys = True
prefix xs [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

{-
grid =
   [['Y', 'I', 'O', 'M', 'R', 'E', 'S', 'K', 'S', 'T'],
    ['A', 'E', 'H', 'Y', 'G', 'E', 'H', 'E', 'D', 'W'],
    ['Z', 'F', 'I', 'A', 'C', 'N', 'I', 'T', 'I', 'A'],
    ['N', 'T', 'O', 'C', 'O', 'M', 'V', 'O', 'O', 'R'],
    ['E', 'R', 'D', 'L', 'O', 'C', 'E', 'N', 'S', 'M'],
    ['Z', 'O', 'U', 'R', 'P', 'S', 'R', 'N', 'D', 'A'],
    ['O', 'Y', 'A', 'S', 'M', 'O', 'Y', 'E', 'D', 'L'],
    ['R', 'N', 'D', 'E', 'N', 'L', 'O', 'A', 'I', 'T'],
    ['F', 'I', 'W', 'I', 'N', 'T', 'E', 'R', 'R', 'C'],
    ['F', 'E', 'Z', 'E', 'E', 'R', 'F', 'T', 'F', 'I'],
    ['I', 'I', 'D', 'T', 'P', 'H', 'U', 'B', 'R', 'L'],
    ['C', 'N', 'O', 'H', 'S', 'G', 'E', 'I', 'O', 'N'],
    ['E', 'G', 'M', 'O', 'P', 'S', 'T', 'A', 'S', 'O'],
    ['T', 'G', 'F', 'F', 'C', 'I', 'S', 'H', 'T', 'H'],
    ['O', 'T', 'B', 'C', 'S', 'S', 'N', 'O', 'W', 'I']]
-}

hidden =
  ["COSY", "SOFT", "WINTER", "SHIVER", "FROZEN", "SNOW",
   "WARM", "HEAT", "COLD",   "FREEZE", "FROST",  "ICE" ]

mk_grid :: Int -> IO [[Char]]
mk_grid size = do
                 let
                   g = mkStdGen 1701 -- deterministic input via fixed seed val
	           cs :: [Char]
                   cs = filter isUpper (randoms g)
                   grid = mk_grid' size size cs []
                   mk_grid' 0 _ _ res = res
                   mk_grid' m n l res = mk_grid' (m-1) n l (l1:res)
                                        where (l1, l2) = splitAt n l
	         return grid

-- show_grid :: [[Char]] -> String
-- show_grid xss = foldl "" (\ s xs -> s ++ (foldl "" (\ x c -> (x ++ " " ++ [c]) xs)) ++ "\n") xss
