
module DfltProb1 where

import Control.Monad.ST

traverse :: a -> ST s [a]
traverse = undefined

-- WORKS with signature test :: Num a => [a]
test = runST (traverse 1)

main = print test




