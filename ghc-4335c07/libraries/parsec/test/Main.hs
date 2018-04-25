
import Test.Framework

import Bugs ( bugs )

main :: IO ()
main = do
  defaultMain
    [ testGroup "Bugs" bugs
    ]