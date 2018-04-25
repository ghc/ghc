-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module Output (outGamteb) where

import	GamtebType
import	Consts
import	Utils
import	InitTable
import Data.Array

outGamteb :: Int -> [Stat] -> [Result] -> [Char]
outGamteb nPart stats results =
	   "Number of particles " ++ show nPart ++ "\n"
	++ outXsectTbl
	++ outResultsRaw results
	++ outStats stats
	++ outResults results


-- output the result of particle transformations
-- outResults :: [Result] -> [Char]
outResults results =
	"\nScatter, Escape, Transit tables:\n" ++ show resArray
	where
	    resArray = accumArray (+) 0 ((1, 1), (numExit, numLev)) results


-- print statistics
-- outStats :: [Stat] -> [Char]
outStats stats =
	showStats titles statList
	where
	    statArray = accumArray (+) 0 (1, numStat) stats
	    statList = elems statArray
	    titles = ["Number of escapes: ",
		      "Number of transits: ",
		      "Number of scatters: ",
		      "Number of energy kills: ",
		      "Number of weight kills: ",
		      "Number of roulettes: ",
		      "Number of splits: ",
		      "Number of collisions: ",
		      "Number of noncollisions: ",
		      "Number of roulettes kills: ",
		      "Weight of roulette kills: ",
		      "Weight of roulette gains: "]
	    showStats [] [] = ""
	    showStats (t:ts) (s:ss) = t ++ show s ++ "\n" ++ showStats ts ss


-- output result list
outResultsRaw :: [Result] -> [Char]
outResultsRaw [] = []
outResultsRaw (((i, t), w):rs) =
	   "Result: index " ++ show i
	++ "  type " ++ show t
	++ "  weight " ++ show w ++ "\n"
	++ outResultsRaw rs


-- print cross section tables of constant data
outXsectTbl :: [Char]
outXsectTbl =
           "\nEnergy table:\n" ++ show (ergs!1)
        ++ "\nCompton table:\n" ++ show (xComp!1)
        ++ "\nPair table:\n" ++ show (xPair!1)
        ++ "\nPhoto table:\n" ++ show (xPhot!1) ++ "\n"
