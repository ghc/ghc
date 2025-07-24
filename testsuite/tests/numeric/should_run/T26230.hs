import Data.Bits
import GHC.Num.Natural

main = do
  print $ naturalAndNot ((2 ^ 4) .|. (2 ^ 3)) (2 ^ 3)
  print $ naturalAndNot ((2 ^ 129) .|. (2 ^ 65)) (2 ^ 65)
  print $ naturalAndNot ((2 ^ 4) .|. (2 ^ 3)) ((2 ^ 65) .|. (2 ^ 3))
  print $ naturalAndNot ((2 ^ 65) .|. (2 ^ 3)) (2 ^ 3)
