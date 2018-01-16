import Control.Monad
import qualified HashTable as H
import System.Environment

main = do
  [size] <- fmap (fmap read) getArgs
  m <- H.new (==) H.hashInt
  forM_ [1..size] $ \n -> H.insert m n n
  v <- H.lookup m 100
  print v
