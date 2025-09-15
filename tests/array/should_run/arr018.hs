-- test for #1131
import Control.Monad.ST
import Data.Array.ST
import Data.Array
import System.Mem

tickle :: Int
tickle = runST (do {
     x <- newArray_ (0,100) ;
     (readArray :: STUArray s Int Int -> Int -> ST s Int) x 3
   })

main :: IO ()
main = do print $ length (replicate 100000 'a')
          performGC
          print tickle
