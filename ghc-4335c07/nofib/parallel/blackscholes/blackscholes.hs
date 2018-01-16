{-# LANGUAGE RecordWildCards,CPP  #-}
-- Intel Concurrent Collections for Haskell
-- Copyright (c) 2010, Intel Corporation.
--
-- This program is free software; you can redistribute it and/or modify it
-- under the terms and conditions of the GNU Lesser General Public License,
-- version 2.1, as published by the Free Software Foundation.
--
-- This program is distributed in the hope it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
-- more details.
--
-- You should have received a copy of the GNU Lesser General Public License along with
-- this program; if not, write to the Free Software Foundation, Inc., 
-- 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.

-- Ported from CnC/C++ program by Ryan Newton

-- Description
-- ===========

-- The Black-Scholes equation is a differential equation that describes how,
-- under a certain set of assumptions, the value of an option changes as the 
-- price of the underlying asset changes.

-- The formula for a put option is similar. The cumulative normal distribution 
-- function, CND(x), gives the probability that normally distributed random
-- variable will have a value less than x. There is no closed form expression for
-- this function, and as such it must be evaluated numerically. The other 
-- parameters are as follows: S underlying asset's current price, 
-- X the strike price, T time to the expiration date, r risk-less rate of return,
-- and v stock's volatility.

-- Blackscholes has a single step collection called (compute). It is executed once
-- for each tag in the tag collection <tags>. This tag collection is produced
-- by the environment.

-- (compute) inputs [data] items. Each [data] item contains a pointer to an 
-- array of ParameterSet objects. For each ParameterSet object the Black-Scholes equation 
-- is solved, and results are stored in the emitted items [price].

-- To reduce the influence of the serial part each calculation is repeated for NUM_RUNS times.

-- There are no constraints on the parallelism in this example. An instance of
-- (compute) does not rely on a data item from another instance and it does not
-- relay on a control tag from another instance. All the tags are produced by
-- the environment before the program begins execution. All steps can be executed
-- in parallel if there are enough processors.


-- Usage
-- =====

-- The command line is:

-- blackscholes b n
--     b  : positive integer for the size of blocks
--     n  : positive integer for the number of options
    
-- e.g.
-- blackscholes 100000 100 4

import Control.Monad hiding (join)
--import Data.Array

import Data.Array.Unboxed
import System.Environment

import Control.Parallel.Strategies
import Control.DeepSeq
import Future

-- #define fptype float
-- #define ERR_CHK
num_runs = 100

type FpType = Float


-- -- This scheme is for the original data tuples in the input:
-- data OptionData = OptionData {
-- 	s       :: FpType,              -- spot price
-- 	strike  :: FpType,		-- strike price
-- 	r       :: FpType ,	        -- risk-free interest rate
-- 	divq    :: FpType ,	        -- dividend rate
-- 	v       :: FpType ,		-- volatility
-- 	t       :: FpType ,		-- time to maturity or option expiration in years 
-- 				       	--     (1yr = 1.0, 6mos = 0.5, 3mos = 0.25, ..., etc)  
-- 	optionType :: String,	-- Option type.  "P"=PUT, "C"=CALL
-- 	divs :: FpType,		-- dividend vals (not used in this test)
-- 	dGrefval :: FpType	-- DerivaGem Reference Value
-- } deriving Show

-- -- These are the blocked/chunked inputs to our kernel extracted from
-- -- the original tuples:
-- data ParameterSet =  ParameterSet {
-- 	dat :: OptionData,
-- 	sptprice   :: UArray Int FpType,
-- 	strikep    :: UArray Int FpType,
-- 	rate       :: UArray Int FpType,
-- 	volatility :: UArray Int FpType ,
-- 	otime      :: UArray Int FpType,
-- 	otype      :: UArray Int Bool,
-- 	granularity :: Int 
-- } deriving Show


-- This tuple contains the inputs for one invocation of our kernel
data ParameterSet =  ParameterSet {
	sptprice   :: FpType,
	strike     :: FpType,
	rate       :: FpType,
	volatility :: FpType ,
	otime      :: FpType,
	otype      :: Bool
} deriving Show


--data_init :: Array Int Int
--data_init :: Array Int OptionData
data_init :: Array Int ParameterSet

-- This defines some hard coded data as a big constant array:
#include "blackscholes_data.hs-inc"
size_init = let (s,e) = bounds data_init in e - s + 1

inv_sqrt_2xPI = 0.39894228040143270286

cndf :: FpType -> FpType
cndf inputX = if sign then 1.0 - xLocal else xLocal
  where 
    sign = inputX < 0.0
    inputX' = if sign then -inputX else inputX
    
    -- Compute NPrimeX term common to both four & six decimal accuracy calcs
    xNPrimeofX = inv_sqrt_2xPI * exp(-0.5 * inputX * inputX);

    xK2 = 1.0 / (0.2316419 * inputX + 1.0);    
    xK2_2 = xK2   * xK2; -- Need all powers of xK2 from ^1 to ^5:
    xK2_3 = xK2_2 * xK2;
    xK2_4 = xK2_3 * xK2;
    xK2_5 = xK2_4 * xK2;
    
    xLocal   = 1.0 - xLocal_1 * xNPrimeofX;
    xLocal_1 = xK2   *   0.319381530  + xLocal_2;
    xLocal_2 = xK2_2 * (-0.356563782) + xLocal_3 + xLocal_3' + xLocal_3'';
    xLocal_3   = xK2_3 * 1.781477937;
    xLocal_3'  = xK2_4 * (-1.821255978);
    xLocal_3'' = xK2_5 * 1.330274429;


blkSchlsEqEuroNoDiv :: FpType -> FpType -> FpType -> FpType -> FpType -> Bool -> Float -> FpType
blkSchlsEqEuroNoDiv sptprice strike rate volatility time otype timet =
   if not otype
   then (sptprice * nofXd1) - (futureValueX * nofXd2)
   else let negNofXd1 = 1.0 - nofXd1
	    negNofXd2 = 1.0 - nofXd2
	in (futureValueX * negNofXd2) - (sptprice * negNofXd1)
 where 
   logValues  = log( sptprice / strike )                
   xPowerTerm = 0.5 * volatility * volatility
   xDen = volatility * sqrt(time)
   xD1  = (((rate + xPowerTerm) * time) + logValues) / xDen
   xD2  = xD1 -  xDen

   nofXd1 = cndf xD1 
   nofXd2 = cndf xD1    
   futureValueX = strike *  exp ( -(rate) * (time) )

executeStep :: Int -> Int -> UArray Int FpType
executeStep t granularity = 
--       stepPutStr$ "  Executing "++ show granularity ++ " iterations starting at "++ show t ++ "\n"
--       let ls = map (\ j ->        
       listArray (0, granularity-1) $
		        Prelude.map (\i -> 
--                              let OptionData { .. } = data_init ! (t+i `mod` size_init)
                              let ParameterSet { .. } = data_init ! ((t+i) `mod` size_init)
			      in blkSchlsEqEuroNoDiv sptprice strike rate volatility otime otype 0)
		            [0 .. granularity-1]
--                     )
--	         [0 .. num_runs-1]
                 

blackscholes :: Int -> Int -> Eval Float
blackscholes numOptions granularity = 
   do 
      fs <- forM [0, granularity .. numOptions-1] $ \t ->
              fork (return (executeStep t granularity))

      foldM (\ acc f -> 
		 do x <- join f
		    return (acc + (x ! 0)))
	        0 fs

main = do args <- getArgs 
          let (numOptions, granularity) =
               case args of 
--  	         []      -> (10, 5)
  	         []      -> (10000, 1000)
  	         [b]     -> (10, read b)
	         [b,n] -> (read n, read b)

          if granularity > numOptions
	   then error "Granularity must be bigger than numOptions!!"
	   else return ()

	  putStrLn$ "Running blackscholes, numOptions "++ show numOptions ++ " and block size " ++ show granularity
          let result = runEval $ blackscholes numOptions granularity
--          putStrLn$ "Final result, here's one price: "++ show ((result !! 0) ! 0)
          --putStrLn$ "Final result, here's one price: "++ show (result UB.! 0)
	  putStrLn$ "Final result: "++ show result
	  return result



