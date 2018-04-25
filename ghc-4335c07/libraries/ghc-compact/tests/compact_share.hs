import GHC.Compact
import qualified Data.Map as Map

main = do
  let m1 = Map.fromList [(x,show x) | x <- [1..(10000::Integer)]]
      m2 = Map.fromList [(x,y) | x <- [1..(10000::Integer)],
                                 Just y <- [Map.lookup x m1]]
  c <- compact (m1,m2)
  print (length (show (getCompact c)))
  print =<< compactSize c
  c <- compactWithSharing (m1,m2)
  print (length (show (getCompact c)))
  print =<< compactSize c
