{-# OPTIONS_GHC -XFlexibleInstances -XBangPatterns #-}
-- Time-stamp: <2010-11-03 12:01:15 simonmar>
--
-- Test wrapper for (parallel) transitive closure computation.
-- The main parallel version is: TRANSCL_NESTED
-- Other versions are: 
--  TRANSCL ... seq, circular implementation over lists
--  TRANSCL_SET ... seq, circular implementation over sets
-----------------------------------------------------------------------------

module Main where

import System.Environment(getArgs)
import Data.List
#if defined(STRATEGIES)
import Control.Parallel
import Control.DeepSeq
import Control.Parallel.Strategies
#else
import GHC.Conc -- hiding (pseq,par)
#endif
--import Random
import Control.Monad
import TransClos
import qualified Data.Set


{-
evalKlustered :: Kluster c => Int -> Strategy (c (c a)) -> Strategy (c a)
evalKlustered n strat xs = return (dekluster (kluster n xs `using` strat))

parChunkN :: (Kluster c, Traversable c) => Int -> Int -> Strategy a -> Strategy (c a)
parChunkN n m strat = evalKlustered m (evalDepthN n (evalTraversable (rpar `dot` evalTraversable strat)))
-}

#define TRANS_CLOS 1

main = do
         args <- getArgs
#ifndef TRANS_CLOS
         when (length args < 4) $
           error "Usage: Main <version> <list len> <block size> <nfib input>"
         let [v,n,z,m] = (map read args) :: [Int]
         {- test parBuffer -}
         let (strat, str) = case v of
                 1 -> (parList rnf, "parList rnf: expect "++(show n)++" converted sparks")
                 2 -> (parListChunk z rnf, "parListChunk:  expect "++(show (n `div` z))++" converted sparks")
                 3 -> (parListChunk_ z rnf, "parListChunk_:  expect "++(show (n `div` z))++" converted sparks")
                 4 -> (parListChunkS z rnf, "parListChunkS:  expect "++(show (n
`div` z))++" converted sparks")
                 5 -> (parBuffer' z rnf, "parBuffer': expect "++(show n)++" converted sparks, at most "++(show z)++" running at the same time")
                 6 -> (parBuffer z rnf, "parBuffer: expect "++(show n)++" converted sparks, at most "++(show z)++" running at the same time")
                 7 -> (parBuffer_ z rnf, "parBuffer_: expect "++(show n)++" converted sparks, at most "++(show z)++" running at the same time")
                 8 -> (evalBuffer_ z (rpar `dot` rnf), "evalBuffer_: expect "++(show n)++" converted sparks, at most "++(show z)++" running at the same time")
                 9 -> (parBufferChunk_ 2 z rnf , "parBufferChunk_: chunksize 2; expect "++(show (n `div` 2))++" converted sparks, at most "++(show z)++" running at the same time")
                 10 -> (evalBufferChunk 2 z (rpar `dot` seqList rnf) , "parBufferChunk: chunksize 2; expect "++(show (n `div` 2))++" converted sparks, at most "++(show z)++" running at the same time")
                 _ -> error "Unknown version"
         let res = map nfib (take n (repeat m)) `using` strat
         putStrLn ("Computing: map nfib (take n (repeat m)) for n = "++(show n)++" and m = "++(show m))
         putStrLn ("Version: "++str)
         putStrLn ("Res: "++(show res))
#else
         when (length args < 5) $
           error "Usage: Main <version> <buffer size> <chunk size> <value> <delay>"
         let [v,n,z,m,d] = (map read args) :: [Int]
--         g <- newStdGen
         let seeds = [1..10] -- take 10 $ randomRs (0::Int,m) g
         let rel_one = \ n -> nfib d `pseq` n+1
         let rel_list = \ n -> nfib ((d-1) `min` (n `max` d)) `pseq` [n+1..n+11]
         let rel_set = \n -> nfib d `pseq` Data.Set.fromList [n+1..n+11]
#if defined(TRANSCL)
         let zs  = {- take n $ -} transcl      rel_list seeds  -- list-based with 1-to-n rel
#elif defined(TRANSCL_NESTED)
         let zs  = {- take n $ -} transcl_nested rel_list seeds  -- list-based with 1-to-n rel; main PARALLEL version
#elif defined(TRANSCL_SET)
         let zs  = Data.Set.toList $ {- take n $ -} transcl_set  rel_set (Data.Set.fromList seeds)   -- set-based  with 1-to-n rel
#else
	 let zs  = {- take n $ -} transcl      rel_list seeds -- default: seq, circular, with a 1-to-n list-based relation
	 -- unused verions
         -- let zs  = {- take n $ -} transcl_dup  rel_one seeds   -- no elim of duplicates; good parallelism but stupid
         -- let zs  = {- take n $ -} transcl_simp rel_one seeds       -- list-based with 1-to-1 rel
#endif
         let (strat, str) = case v of
                 {- temp out of order
                 1 -> (\ _ -> parListN n rnf (drop (length seeds) zs), "parListN with n = "++(show n))
                 2 -> (\ _ -> parListChunkK z rnf (drop (length seeds) zs), "parListChunkK with z = "++(show z))
                 3 -> (\ _ -> parListChunkN z n rnf (drop (length seeds) zs), "parListChunkN with blocksize z = "++(show z)++" and length n = "++(show n))
                 -}
                 4 -> (\ _ -> error "parBuffer'  ", "parBuffer with buffer size "++(show n))
		 -- 5 -> (\ _ -> parBufferLChunk n z (ins rnf) (drop (length seeds) zs), "parBufferLChunk with buffer size "++(show n)++" chunk size size "++(show z))
		 -- 6 -> (\ _ -> parBufferQChunk n z (ins rnf) (drop (length seeds) zs), "parBufferQChunk with buffer size "++(show n)++" chunk size size "++(show z))
		 -- 7 -> (\ _ -> parBufferAChunk n z (ins rnf) (drop (length seeds) zs), "parBufferAChunk with buffer size "++(show n)++" chunk size size "++(show z))
                 -- 10 -> (\ _ -> parBufferChunk_ z n rnf (drop (length seeds) zs), "parBufferChunk with buffer size "++(show n)++" chunk size size "++(show z))
                 -- 11 -> (\ _ -> evalBufferChunk z n (rpar `dot` seqList rnf) (drop (length seeds) zs), "evalBufferChunk with buffer size "++(show n)++" chunk size size "++(show z))
                 -- 12 -> (\ _ -> parBufferLSliceChunk n z z (rpar `dot` seqList (ins rnf)) (drop (length seeds) zs), "parBufferLSliceChunk with buffer size "++(show n)++" stride "++(show z)++" chunk size "++(show z))
                 -- 13 -> (\ _ -> parBufferQSliceChunk n z z (rpar `dot` seqList (ins rnf)) (drop (length seeds) zs), "parBufferQSliceChunk with buffer size "++(show n)++" stride "++(show z)++" chunk size "++(show z))
                 -- 14 -> (\ _ -> parBufferASliceChunk n z z (rpar `dot` seqList (ins rnf)) (drop (length seeds) zs), "parBufferASliceChunk with buffer size "++(show n)++" stride "++(show z)++" chunk size "++(show z))
                 -- 13 -> (\ b -> parBuffer_ z (drop (length seeds) zs) >> return b, "parBuffer_ with buffer size "++(show z))
                 v' -> error $ "Unknown version "++(show v')
#if defined(TRANSCL)
         let res = m `elem` zs  -- NO: parallelism is hopeless on this one:  `using` strat)
#elif defined(TRANSCL_NESTED)
         let res = if (v==4)  -- special case for parBuffer (not of strategy type!)
                     then m `elem` (nub (concat (runEval $ do let (first, rest) = splitAt (length seeds) zs  
                                                              rest' <- parBuffer n rdeepseq rest
                                                              return (first ++ rest') ))) -- main PARALLEL version
                     else m `elem` (nub (concat (zs `using` strat))) -- main PARALLEL version
#elif defined(TRANSCL_SET)
         let res = m `elem` zs  -- default: seq, circular, with a 1-to-n list-based relation
#else
         let res = m `elem` zs  -- default: seq, circular, with a 1-to-n list-based relation
#endif
         putStrLn ("Searching for value "++(show m)++" in transitive closure of relation \\ n -> [n+1..n+11] with seeds "++(show seeds))
         putStrLn ("Version: "++str)
         putStrLn ("Res: "++(show res))
#endif


nfib :: Int -> Int
nfib 0 = 1
nfib 1 = 1
nfib n = nfib (n-1) + nfib (n-2) + 1

