
{-# LANGUAGE Arrows #-}

import Control.Arrow
import qualified Control.Category as Cat

test :: Int -> Int
test = proc x -> do
    let neg = x < 0
    case x of
        x | neg -> returnA -< 0           -- GHC panics
        --x | x < 0 -> returnA -< 0       -- GHC doesn't panic
        _       -> returnA -< 10

main = do
    print $ test (-1)
    print $ test 1
