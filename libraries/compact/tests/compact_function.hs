import Control.DeepSeq
import Control.Exception
import Data.Compact

data HiddenFunction = HiddenFunction (Int -> Int)

instance NFData HiddenFunction where
  rnf x = x `seq` () -- ignore the function inside

main = compact (HiddenFunction (+1))
