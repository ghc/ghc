{- Andy Gill, Oct 99 

Here is a generic cryptarithm solver, written in Haskell. It does
use a State Monad library, which is based on the work documented in 
"Functional Programming with Overloading and Higher-Order Polymorphism",
Mark P. Jones, Advanced School of Functional Programming, 1995.

This can solve the puzzle in about 3 seconds on my laptop.
On key optimization is captured by the line
     guard (topN `mod` 10 == botN)
in the function solve. It prunes searches than simply
can not ever reach valid results.
-}

module Main where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.List
import Data.Maybe

--    newtype DigitState = DigitState (Digits -> [(a,Digits))])
-- which some might recognize as the list-of-successes parsing monad.

type DigitState a = StateT Digits [] a

-- Our digits state
-- * First we have the remaining digit to allocate.
-- * Second, we have the mapping from Char to Digit,
--   for the chars that have been mapped so far.

data Digits = Digits {
		digits :: [Int],
		digitEnv :: [(Char,Int)] 
	} deriving Show

initState = Digits {
		digits = [0..9],
		digitEnv = []
		}

-- permute adds a mapping from a char to each of the
-- remaining allocable digits.
-- This is used in the context of the list-of-successes
-- monad, so it actually returns all possible mappings.

permute :: Char -> DigitState Int
permute c =
     do st <- get
	let xs = digits st
	(i,is) <- lift [ (x,xs \\ [x]) | x <-  xs]
	put (st { digits = is,
		  digitEnv = (c,i):digitEnv st })
	return i

-- select attempt first checks to see if a mapping
-- from a specific char to digit already has been
-- mapped. If so, use the mapping, otherwise
-- add a new mapping.

select :: Char -> DigitState Int
select c = 
     do st <- get
	case lookup c (digitEnv st) of
	  Just r -> return r
	  Nothing -> permute c

-- solve takes a list of list of (backwards) letters,
-- and a list of (backwards) letters, and tries
-- to map the letter to digits, such that
-- the sum of the first list of letters (mapped to digits)
-- is equal to the sum of the second list of letters,
-- again mapped to digits.
--
-- So a possible mapping for A+B=C might be
-- solve ["A","B"] "C" 0
-- 	  => A -> 1, B -> 2, C -> 3

solve :: [[Char]] -> [Char] -> Int -> DigitState ()
solve tops (bot:bots) carry =
  do topN <- (case tops of
		   [] -> return carry
		   (top:_) -> 
		     do topNS <- mapM select top
	     	        return (sum topNS + carry))
     botN <- select bot
     guard (topN `mod` 10 == botN)	-- key optimization
     solve (rest tops) bots (topN `div` 10)
  where
     rest []     = []
     rest (x:xs) = xs
solve [] [] 0 = return ()
solve _  _  _ = mzero

-- Puzzle provides a cleaner interface into solve.
-- The strings are in the order *we* write them.

puzzle :: [[Char]] -> [Char] -> String
puzzle top bot = 
	     if length (nub (concat top ++ bot)) > 10 
	     then error "can not map more than 10 chars"
	else if topVal /= botVal 
	     then error ("Internal Error")
	else unlines [ [c] ++ " => " ++ show i |
			(c,i) <- digitEnv answer
		   ]
   where
	solution = solve (transpose (map reverse top)) 
			 (reverse bot)
			 0
	answer  = case (execStateT solution initState) of
		     (a:_) -> a
		     [] -> error "can not find a solution"
	env    = digitEnv answer
	look c = fromJust (lookup c env)
	topVal = sum [expand xs | xs <- top] 
	botVal = expand bot
	expand = foldl (\ a b -> a * 10 + look b) 0
			
main = putStr (
	puzzle	["THIRTY",
		 "TWELVE",
		 "TWELVE",
		 "TWELVE",
		 "TWELVE",
		 "TWELVE"]
		 "NINETY")
