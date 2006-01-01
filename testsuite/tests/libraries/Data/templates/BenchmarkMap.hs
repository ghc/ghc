{-

: MAPMODULE    :   MAPK      : KEY : EL  :
------------------------------------------
: Data.Map     :   M.Map Int : Int : Int : 
: Data.IntMap  :   M.IntMap  : Int : Int :

-}


{-
TODO
*  Have both ordered and random testData
-}

import System.Time
import System.CPUTime
import qualified MAPMODULE as M

import System.Environment
import System.Mem
import Data.List(foldl', transpose)

type MAP = MAPK EL

testData :: [KEY]
testData = [1..500000]

testPairs = zip testData testData



lookups :: MAP -> [KEY] -> IO ()
lookups table [] = return ()
lookups table (x:xs) = lk `seq` lookups table xs
    where lk = M.lookup x table
          lk :: Maybe EL

main = do
  mapM_ (\a -> a `seq` return ()) testData

  testMany ["Insertion", "Lookup", "Deletion"] $ do
     t1 <- getCPUTime
     let table = foldl' (\t a -> M.insert a a t) M.empty testData
     table `seq` return ()
     t1' <- getCPUTime
     performGC
     t2 <- getCPUTime
     lookups table testData
     t2' <- getCPUTime
     performGC
     t3 <- getCPUTime
     let table' = foldl' (\t a -> M.delete a t) table testData in table' `seq` return ()
     t3' <- getCPUTime
     return [t1'-t1, t2'-t2, t3'-t3]


numberOfTests = 10

testMany :: [String] -> IO [Integer] -> IO ()
testMany names io = do
  ts <- sequence [performGC >> io | _ <- [1..numberOfTests]]
  let averages = map (`div` numberOfTests) $ map sum $ transpose ts 
  putStrLn fileName
  mapM_ showTime $ zip names averages

showTime (name, time::Integer) = do
  putStrLn $ name ++ " time " ++ show (fromIntegral time/(10^^12))


