{- 
Time-stamp: <2010-11-03 09:26:39 simonmar>
$Id: testLinSolv.hs,v 1.6.2.5 2002/06/21 02:39:07 hwloidl Exp $

Main module for testing linSolv.hs, the solution of a system of 
linear equations by using the modular method.                             -}

-- @node Top, Modular Arithmetic, (dir), (dir)
-- @top A Linear System Solver

-- @node Test wrapper for LinSolv,  , Top, Top
-- @chapter Test wrapper for LinSolv

module Main where

--Other  modules:
-- @menu
-- * Modular Arithmetic::	
-- * ADT Matrix::		
-- * LinSolv top level function::  
-- @end menu

-- ----------------------------------------------------------------------------
-- @node Imports, Functions for I/O, Test wrapper for LinSolv, Test wrapper for LinSolv
-- @section Imports
--
-- Using modular method for linear system solving
-- ----------------------------------------------------------------------------

import System.Environment(getArgs)

-- @include ModArithm.hs
import ModArithm

-- @include Matrix-list.hs
import Matrix (SqMatrix, Vector, 
               vector,
	       lolSqMatrix,listSqMatrix,maxElemVec)

#if defined(STRATEGIES)
import Control.Parallel
import Control.Parallel.Strategies
#endif
import Control.DeepSeq

-- @include LinSolv-par.hs
import LinSolvS (linSolv)

-- fixed input data
import Inputs8

-- get_input :: (Integral a, NFData a) =>
--              Int -> [(Int, SqMatrix a, Vector a)] -> (SqMatrix a, Vector a)
get_input n = case (lookup (fromIntegral n) all_inputs) of
                Just x -> x
                Nothing -> error $ "Unknown input " ++ (show n) ++ ". Possible inputs are " ++ (show (map fst all_inputs))
              
-- ----------------------------------------------------------------------------
-- @node Functions for I/O, Main fct, Imports, Test wrapper for LinSolv
-- @section Functions for I/O
--
-- Functions for I/O
-- ----------------------------------------------------------------------------

-- Compute one number out of result; JUST FOR TESTING
-- This demands the computation of the full result and is fast to print
-- i.e. not much IO overhead in timing.

--compact :: (Integral a) => (Vector a, a, a, a) -> a
{-
compact (x', a, b, _) = if a == 0 
	               then if b == 0 then x
		    		      else 1 +  x
	               else if b == 1 then  x
				      else 1 + x
	             where -- bonzo x = map (\ y -> if y==0 then 0 else 1) x
			   x = jaH x'
-}

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- @node Main fct,  , Functions for I/O, Test wrapper for LinSolv
-- @section Main fct

main = do
        args <- getArgs
        let 
         v = read (args!!0) :: Int -- version (i.e. strategy to use)
         n = read (args!!1) :: Int -- input
	 -- Pick input based on command line argument
         (a, b) = get_input n
         x = linSolv v a b
#ifdef TEST
	putStrLn ( case determinant a of
			0 -> error "determinant 0, no solution"
			x -> "determinant: " ++ show x)
#endif
# if defined(OUTPUT)
	putStr ("Testing linSolv with various matrices:\n" ++
		"\nSolving a*x=b for a = " ++
		show a ++
		"b = " ++ 
		show b ++ 
		"\nSolution: " ++
		show x ++
	        "\n")
# else
        putStrLn (x `deepseq` "done" )
#endif /* OUTPUT */
        -- putStr ( show (compact x) ++ "done")
