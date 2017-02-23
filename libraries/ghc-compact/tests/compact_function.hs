import Control.Exception
import GHC.Compact

data HiddenFunction = HiddenFunction (Int -> Int)

main = compact (HiddenFunction (+1))
