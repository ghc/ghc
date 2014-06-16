
> {-# LANGUAGE UndecidableInstances, PatternSignatures, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

> module Main where

> import Data.IORef
> import Control.Concurrent
> --import Control.Concurrent.STM
> import System.Environment
> import Data.Time

> import System.Mem
> import Data.List

> import Collection
> import RefInterface

> import TestData
> import TestRun

-- the contenders (we can run stand-alone for a fixed test case mainPar)

> import qualified CASList as CAS
>-- import qualified CASusingSTMList as CASusingSTM
> import qualified MVarListLockCoupling as MLC
>-- import qualified MVarusingSTM as MLCusingSTM
>-- import qualified LazyList2 as Lazy
> import qualified IOList as I
>-- import qualified STMList as S
> import qualified ImmList as IMM


create List

> createList :: Col c e => [e] -> IO c
> createList n = 
>   do nl <- newCol
>      mapM (insertCol nl) n
>      return nl



> createTasks :: [a] -> [Op a]
> createTasks xs = task 1 xs
>   where
>     insCnt = 5   -- every 5th op is insert
>     delCnt = 9   -- ever 9th op is delete
>     task _ [] = []
>     task cnt (x:xs) 
>       | (cnt `mod` insCnt) == 0 = (Insert x) : task (cnt+1) xs
>       | (cnt `mod` delCnt) == 0 = (Delete x) : task (cnt+1) xs
>       | otherwise             = (Find x) : task (cnt+1) xs



mainly finds, some deletes which will be inserted again

> specificTask1 :: [a] -> [Op a]
> specificTask1 xs = task 1 xs []
>   where
>     delCnt = 6   -- every 6th op is delete
>     insCnt = 50   -- after 5 deletes we'll insert them again
>     task _ [] _ = []
>     task cnt (x:xs) deletes 
>       | length deletes == insCnt =  map Insert deletes ++ task (cnt+1) (x:xs) []
>       | (cnt `mod` delCnt) == 0 = (Delete x) : task (cnt+1) xs (x:deletes)
>       | otherwise             = (Find x) : task (cnt+1) xs deletes

> executeTasks :: Col c e => c -> [Op e] -> IO ()
> executeTasks lh ops =
>  do mapM (\ task -> 
>           case task of
>              Find x -> do { findCol lh x; return () }
>              Insert x -> do { insertCol lh x; return () }
>              Delete x -> do { deleteCol lh x; return () })
>          ops
>     return ()

  put number into threads buckets

> distribution :: [Int] -> Int -> [[Int]]
> distribution no threads =
>   let init = map (\ _ -> []) [1..threads]
>       go :: [Int] -> Int -> [[Int]] -> [[Int]]
>       go [] _ acc = acc
>       go (x:xs) cnt acc = 
>          let idx = cnt `mod` threads
>              acc' = take idx acc ++ [x : (acc !! idx)] ++ drop (idx+1) acc
>          in go xs (cnt+1) acc'
>  
>   in go no 1 init           


runnable version

 main = mainPar

parallel version

> type RUN = CAS.ListHandle Int

> main :: IO ()
> main = 
>  do args <- getArgs
>     case args of
>      (mode:"-t":in_fname:rest) -> run_testdata in_fname mode
>--      [mode, t, l] -> 
>--        do let len = read l :: Int
>--           let threads = read t :: Int
>--           let run nl = mainPar nl threads len
>--           case mode of
>--            "CAS"   -> do nl :: CAS.ListHandle Int <- createList [0..len]
>--                          run nl
>--            "CASusingSTM"   -> do nl :: CASusingSTM.ListHandle Int <- createList [0..len]
>--                                  run nl
>--            "LAZY" -> do nl :: Lazy.ListHandle Int <- createList [0..len]
>--                         run nl
>--            "MLC" -> do nl :: MLC.ListHandle Int <- createList [0..len] 
>--                        run nl
>--            "MLCusingSTM" -> do nl :: MLCusingSTM.ListHandle Int <- createList [0..len] 
>--                                run nl
>--            "IO" -> do nl  :: I.ListHandle Int <- createList [0..len]
>--                       run nl
>--            "STM" -> do nl  :: S.ListHandle Int <- createList [0..len]
>--                        run nl
>--            "IMM" -> do nl  :: IMM.ListHandle Int <- createList [0..len]
>--                        run nl
        


 mainPar :: Col c Int => c -> Int -> Int -> IO ()
 mainPar nl threads len =
  do let numbers = [1..len] ++ (reverse [1..len]) ++ [1..len] ++ (reverse [1..len])
                   ++ [1..len] ++ (reverse [1..len]) ++ [1..len] ++ (reverse [1..len])
     let ds = distribution numbers threads
     let ts = ds
     wait <- atomically (newTVar 0)
     putStrLn "Start"
     start <- getCurrentTime
     mapM (\ t -> forkIO (do executeTasks nl (specificTask1 t)
                             atomically(do counter <- readTVar wait
                                           writeTVar wait (counter+1))))
          ts
     atomically ( do counter <- readTVar wait
                     if counter < threads then retry 
                      else return () )
     fin <- getCurrentTime
     putStrLn "Done"
     putStrLn $ "Time: " ++ show (diffUTCTime fin start)



 mainPar2 nl len =
   do cnt <- atomically (newTVar 0)
      printCol nl
      mapM (\ e -> forkIO ( do insertCol nl e
                               atomically(do i <- readTVar cnt
                                             writeTVar cnt (i+1))))
           [6..len]

      atomically ( do i <- readTVar cnt
                      if i <= len-6 then retry 
                       else return () )

      printCol nl
      n <- cntCol nl
      putStrLn $ "Overall: " ++ show n


sequential version
  
> mainSeq nl len = 
>  do let threads = 4
>     let numbers = [1..len] ++ (reverse [1..len]) ++ [1..len] ++ (reverse [1..len])
>                   ++ [1..len] ++ (reverse [1..len]) ++ [1..len] ++ (reverse [1..len])
>     let [d1,d2,d3,d4] = distribution numbers threads
>     let t1 = d1++d2++d3++d4
>     let t2 = d2++d3++d4++d1
>     let t3 = d3++d4++d1++d2
>     let t4 = d4++d1++d2++d3
>     putStrLn "Start"
>     start <- getCurrentTime
>     mapM (\ t -> executeTasks nl (specificTask1 t)) [t1,t2,t3,t4]
>     fin <- getCurrentTime
>     putStrLn "Done"
>     putStrLn $ "Time: " ++ show (diffUTCTime fin start)


just testing

> mainTest nl len =
>  do 
>     printCol nl   

      r <- deleteCol nl 3
      putStrLn ("Result : " ++ show r)
      find nl 10

>     insertCol nl 11

>     deleteCol nl 3
>     findCol nl 11


>     mapM (\x -> forkIO (insertCol nl x)) [12..50]

>     threadDelay 1000000

> {-
>     putStrLn "Start"
>     executeTasks nl $ createTasks [1..len]
>     find nl (len+1) -- we try to find a non-existant element
>                     -- this way, in the LazyList case, we will physically delete all (logically deleted) elements 
> -}
>     putStrLn "End"
>     printCol nl

