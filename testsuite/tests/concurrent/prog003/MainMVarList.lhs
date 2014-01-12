
> module Main where

> import Data.IORef
> import Control.Concurrent
> --import Control.Concurrent.STM
> import System.Environment
> import Data.Time


> import MVarList


printing 

> printList :: Show a => ListHandle a -> IO ()
> printList (ListHandle {headList = ptrPtr}) =
>  do startptr <- (
>          do ptr <- readIORef ptrPtr
>             Head {next = startptr} <- readMVar ptr
>             return startptr)
>     printListHelp startptr


> printListHelp :: Show a => MVar (List a) -> IO ()
> printListHelp curNodePtr =
>   do { curNode <- readMVar curNodePtr
>      ; case curNode of
>          Null -> putStr "Nil"
>          Node {val = curval, next = curnext} ->
>             do { putStr (show curval  ++ " -> ")
>                ;  printListHelp curnext }
>          DelNode {val = curval, next = curnext} ->
>             do { putStr (show curval  ++ "DEAD -> ")
>                ;  printListHelp curnext }
>      } 

> cntList :: Show a => ListHandle a -> IO Int
> cntList (ListHandle {headList = ptrPtr}) =
>  do startptr <- (
>          do ptr <- readIORef ptrPtr
>             Head {next = startptr} <- readMVar ptr
>             return startptr)
>     cntListHelp startptr 0


> cntListHelp :: Show a => MVar (List a) -> Int -> IO Int
> cntListHelp curNodePtr i =
>   do { curNode <- readMVar curNodePtr
>      ; case curNode of
>          Null -> return i
>          Node {val = curval, next = curnext} -> 
>                cntListHelp curnext (i+1)
>          DelNode {val = curval, next = curnext} ->
>                cntListHelp curnext (i+1)
>      } 


create List

> createList :: Int -> IO (ListHandle Int)
> createList n =
>   do nl <- newList
>      mapM (addToTail nl) [1..n]
>      return nl


> data Op a = Find a | Insert a | Delete a deriving Show
        

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
>     delCnt = 5   -- every 6th op is delete
>     insCnt = 50   -- after 5 deletes we'll insert them again
>     task _ [] _ = []
>     task cnt (x:xs) deletes 
>       | length deletes == insCnt =  map Insert deletes ++ task (cnt+1) (x:xs) []
>       | (cnt `mod` delCnt) == 0 = (Delete x) : task (cnt+1) xs (x:deletes)
>       | otherwise             = (Find x) : task (cnt+1) xs deletes

> executeTasks :: Eq a => ListHandle a -> [Op a] -> IO ()
> executeTasks lh ops =
>  do mapM (\ task -> 
>           case task of
>              Find x -> do { find lh x; return () }
>              Insert x -> do { addToTail lh x; return () }
>              Delete x -> do { delete lh x; return () })
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

> insert :: Eq a => ListHandle a -> a -> IO ()
> insert = addToTail


runnable version

> main = mainPar

parallel version

> mainPar :: IO ()
> mainPar =
>  do let len = 3000
>     let threads = 4
>     nl <- createList len
>     let numbers = [1..len] ++ (reverse [1..len]) ++ [1..len] ++ (reverse [1..len])
>                   ++ [1..len] ++ (reverse [1..len]) ++ [1..len] ++ (reverse [1..len])
>     let [d1,d2,d3,d4] = distribution numbers threads
>     let t1 = d1++d2++d3++d4
>     let t2 = d2++d3++d4++d1
>     let t3 = d3++d4++d1++d2
>     let t4 = d4++d1++d2++d3
>     wait <- atomically (newTVar 0)
>     putStrLn "Start"
>     start <- getCurrentTime
>     mapM (\ t -> forkIO (do executeTasks nl (specificTask1 t)
>                             atomically(do counter <- readTVar wait
>                                           writeTVar wait (counter+1))))
>          [t1,t2,t3,t4]
>     atomically ( do counter <- readTVar wait
>                     if counter < 4 then retry 
>                      else return () )
>     fin <- getCurrentTime
>     putStrLn "Done"
>     putStrLn $ "Time: " ++ show (diffUTCTime fin start)


> mainPar2 :: IO ()
> mainPar2 =
>   do nl <- createList 5
>      let len = 5 + 200
>      cnt <- atomically (newTVar 0)
>      printList nl
>      mapM (\ e -> forkIO ( do insert nl e
>                               atomically(do i <- readTVar cnt
>                                             writeTVar cnt (i+1))))
>           [6..len]

>      atomically ( do i <- readTVar cnt
>                      if i <= len-6 then retry 
>                       else return () )

>      printList nl
>      n <- cntList nl
>      putStrLn $ "Overall: " ++ show n


sequential version 
  
> mainSeq :: IO ()
> mainSeq = 
>  do let len = 3000
>     let threads = 4
>     nl <- createList len
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

> mainTest2 :: IO ()
> mainTest2 =
>  do let len = 10
>     nl <- createList len
>     printList nl
>     addToTail nl 1
>     printList nl   

> mainTest :: IO ()
> mainTest =
>  do let len = 10
>     nl <- createList len
>     printList nl   

      r <- delete nl 3
      putStrLn ("Result : " ++ show r)
      find nl 10

>     insert nl 11

>     delete nl 3
>     find nl 11


>     mapM (\x -> forkIO (insert nl x)) [12..50]

>     threadDelay 1000000

> {-
>     putStrLn "Start"
>     executeTasks nl $ createTasks [1..len]
>     find nl (len+1) -- we try to find a non-existant element
>                     -- this way, in the LazyList case, we will physically delete all (logically deleted) elements 
> -}
>     putStrLn "End"
>     printList nl
