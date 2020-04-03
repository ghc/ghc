import Control.Monad
import GHC.Compact
import qualified Data.Map as Map

main = do
  let m = Map.fromList [(x,show x) | x <- [1..(10000::Int)]]
  c <- compactWithSharing m
  print =<< compactSize c
  c <- foldM (\c _ -> do c <- compactWithSharing (getCompact c)
                         print =<< compactSize c
                         return c) c [1..10]
  print (length (show (getCompact c)))
  print =<< compactSize c
