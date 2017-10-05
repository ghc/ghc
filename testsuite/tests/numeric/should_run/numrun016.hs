-- test for trac #8532

import Data.Complex

main :: IO ()
main = do
    print $ acosh ((-1)::Complex Double)
    print $ acosh ((-1)::Complex Float)
