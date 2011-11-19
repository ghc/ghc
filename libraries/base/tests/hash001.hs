import Data.HashTable
import qualified Data.HashTable as HT

test :: Int -> IO ()
test n = do ht <- new (==) hashInt
            sequence_ [ insert ht key 0 | key <- [0..n]]
            sequence_ [ insert ht key 1 | key <- [0..n]]
          
            let check key = do (Just val) <- HT.lookup ht key
                               if val==1 then return () else putStrLn $ show key 
          
            sequence_ [ check key | key <- [0..n]]

main = test 2048
