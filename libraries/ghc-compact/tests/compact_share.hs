import Data.Char
import GHC.Compact
import qualified Data.Map as Map

main = do
  let m1 = Map.fromList [(x,show x) | x <- ['a'..chr 10000]]
      m2 = Map.fromList [(x,y) | x <- ['a'..chr 10000],
                                 Just y <- [Map.lookup x m1]]
  c <- compact (m1,m2)
  print (length (show (getCompact c)))
  print =<< compactSize c
  c <- compactWithSharing (m1,m2)
  print (length (show (getCompact c)))
  print =<< compactSize c
