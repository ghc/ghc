import GHC.Compact
import qualified Data.Map as Map

main = do
  let m = Map.fromList [(x,show x) | x <- [1..(10000::Integer)]]
  c <- compactWithSharing m
  print (length (show (getCompact c)))
  c <- compact m
  print (length (show (getCompact c)))
