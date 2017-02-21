import Control.Exception
import Data.Compact

data HiddenFunction = HiddenFunction (Int -> Int)

main = compact (HiddenFunction (+1))
