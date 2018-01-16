import Control.Exception
import GHC.Compact
import qualified Data.Map as Map
import System.Exit

main = do
  c <- compactWithSharing (cycle "abc") -- magic!
  print (length (show (take 100 (getCompact c))))
  print =<< compactSize c
