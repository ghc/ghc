import qualified Data.ByteString.Char8 as B
import Data.Compact
import Data.Compact.Internal
import qualified Data.Map as Map

main = do
  c <- compact (Map.fromList [(B.pack (show x), x) | x <- [1..(10000::Int)]])
  print (getCompact c)
