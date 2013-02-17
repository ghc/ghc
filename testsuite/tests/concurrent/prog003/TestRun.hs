
{-# LANGUAGE UndecidableInstances, PatternSignatures, FlexibleInstances, MultiParamTypeClasses #-}

module TestRun (
   run_testdata  -- FilePath -> FilePath -> IO ()
) where

import TestData
import TestDataParser
import Collection

import Control.Monad
import GHC.Conc
import Text.Printf
import Data.List
import Data.IORef
import Control.Concurrent
--import Control.Concurrent.STM
import System.Environment
import Data.Time
import System.Mem
import System.Random
import Control.Exception

-- the contenders
import qualified CASList as CAS
import qualified ImmList as IMM
--import qualified CASusingSTMList as CASusingSTM
import qualified MVarListLockCoupling as MLC
--import qualified MVarusingSTM as MLCusingSTM
--import qualified LazyList2 as Lazy
import qualified IOList as I
--import qualified STMList as S



instance (Eq e, Show e) => Col (CAS.ListHandle e) e where
   newCol = CAS.newList
   insertCol c e = do CAS.addToTail c e
                      return ()
   deleteCol = CAS.delete
   findCol = CAS.find
   printCol = CAS.printList
   cntCol = CAS.cntList

instance (Eq e, Show e) => Col (IMM.ListHandle e) e where
   newCol = IMM.newList
   insertCol c e = do IMM.addToTail c e
                      return ()
   deleteCol = IMM.delete
   findCol = IMM.find
   printCol = IMM.printList
   cntCol = IMM.cntList

--instance (Eq e, Show e) => Col (CASusingSTM.ListHandle e) e where
--   newCol = CASusingSTM.newList
--   insertCol c e = do CASusingSTM.addToTail c e
--                      return ()
--   deleteCol = CASusingSTM.delete
--   findCol = CASusingSTM.find
--   printCol = CASusingSTM.printList
--   cntCol = CASusingSTM.cntList


--instance (Eq e, Show e) => Col (Lazy.ListHandle e) e where
--   newCol = Lazy.newList
--   insertCol c e = do  Lazy.addToTail c e
--                       return ()
--   deleteCol =  Lazy.delete
--   findCol =  Lazy.find
--   printCol =  Lazy.printList
--   cntCol =  Lazy.cntList


instance (Eq e, Show e) => Col (I.ListHandle e) e where
   newCol = I.newList
   insertCol c e = do I.addToTail c e
                      return ()
   deleteCol = I.delete
   findCol = I.find
   printCol = I.printList
   cntCol = I.cntList

--instance (Eq e, Show e) => Col (MLCusingSTM.ListHandle e) e where
--   newCol = MLCusingSTM.newList
--   insertCol c e = do MLCusingSTM.addToTail c e
--                      return ()
--   deleteCol = MLCusingSTM.delete
--   findCol = MLCusingSTM.find
--   printCol = MLCusingSTM.printList
--   cntCol = MLCusingSTM.cntList


instance (Eq e, Show e) => Col (MLC.ListHandle e) e where
   newCol = MLC.newList
   insertCol c e = do MLC.addToTail c e
                      return ()
   deleteCol = MLC.delete
   findCol = MLC.find
   printCol = MLC.printList
   cntCol = MLC.cntList

--instance (Eq e, Show e) => Col (S.ListHandle e) e where
--   newCol = S.newList
--   insertCol c e = do S.addToTail c e
--                      return ()
--   deleteCol = S.delete
--   findCol = S.find
--   printCol = S.printList
--   cntCol = S.cntList

-- Auxiliary functions

createList :: Col c e => [e] -> IO c
createList n = 
  do nl <- newCol
     mapM (insertCol nl) n
     return nl

executeTasks :: Col c e => c -> [Op e] -> IO ()
executeTasks lh ops =
 do mapM (\ task -> 
          case task of
             Find x -> do { findCol lh x; return () }
             Insert x -> do { insertCol lh x; return () }
             Delete x -> do { deleteCol lh x; return () })
         ops
    return ()

appendIORef :: IORef [a] -> a -> IO ()
appendIORef ref a = do
  { as <- readIORef ref
  ; writeIORef ref (a:as) }
    
showComma :: [String] -> String
showComma (s:ss) = "," ++ s ++ (showComma ss)
showComma [] = ""
  
-- Main interface
    
run_testdata :: FilePath -> String -> IO ()
run_testdata testdata_fname mode = do
    { tc <- parse_testdata testdata_fname
    ; putStrLn "Test Initiated: "
    ; let init_elems = t_init_list tc
          works      = t_tasks tc
    ; medians  <- newIORef []
    ; highlows <- newIORef []
    ; runtests mode init_elems works (medians,highlows)
    }
    where
        runtests m elems works refs =
          case m of
             "CAS"   -> do nl :: CAS.ListHandle Int <- createList elems 
                           runtest m works nl refs
             "IMM"   -> do nl :: IMM.ListHandle Int <- createList elems 
                           runtest m works nl refs
--             "CASusingSTM"   -> do nl :: CASusingSTM.ListHandle Int <- createList elems 
--                                   runtest m works nl refs
--             "LAZY" -> do nl :: Lazy.ListHandle Int <- createList elems
--                          runtest m works nl refs
             "MLC" -> do nl :: MLC.ListHandle Int <- createList elems 
                         runtest m works nl refs
--             "MLCusingSTM" -> do nl :: MLCusingSTM.ListHandle Int <- createList elems 
--                                 runtest m works nl refs
             "IO" -> do nl  :: I.ListHandle Int <- createList elems
                        runtest m works nl refs
--             "STM" -> do nl  :: S.ListHandle Int <- createList elems
--                         runtest m works nl refs
             trash -> fail $ "Oi! No such concurrency mode: " ++ trash 
           
   
        runtest m works nl (medians,highlows) = do
          { putStrLn $ "Test Started: " ++ m
          ; performGC 
--          ; wait <- atomically (newTVar 0)
          ; wait <- newEmptyMVar
          ; start <- getCurrentTime
          ; zipWithM (\n work -> forkOn n (do { executeTasks nl work
                                              ; putMVar wait () }))
                                              --atomically(do counter <- readTVar wait
--                                                              writeTVar wait (counter+1)) }))
            [0..] works
          ; replicateM_ (length works) (takeMVar wait)
--          ; atomically ( do { counter <- readTVar wait
--                            ; if counter < length works then retry 
--                              else return () } )
          ; fin <- getCurrentTime
          ; let result = diffUTCTime fin start
          ; -- printf "time: %.2fs\n" (realToFrac result :: Double)
          ; return () }

        output_fname = "out"

        write_output mod ms mref hlref = do
          { if mod == 1 
            then do { writeFile output_fname ""
                    ; output_header output_fname ms
                    ; appendFile output_fname ",,"
                    ; output_header output_fname ms
                    ; appendFile output_fname "\n1Core" }
            else appendFile output_fname ("\n" ++ (show mod) ++ "Cores") 
          ; medians  <- readIORef mref
          ; highlows <- readIORef hlref
          ; let mstr = reverse $ map show medians
                hlstr = reverse $ highlows
          ; appendFile output_fname (filter (/='s') (showComma mstr))
          ; appendFile output_fname ",," 
          ; appendFile output_fname (filter (/='s') (showComma hlstr)) }
          
        output_header output_fname (m:ms) = do
          { appendFile output_fname ("," ++ m)
          ; output_header output_fname ms }
        output_header output_fname [] = return ()
          
        high_low results =
          let fst = head results
              lst = head $ drop ((length results) - 1) results
          in (fst,lst)
