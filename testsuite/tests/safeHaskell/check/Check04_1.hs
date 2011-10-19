{-# LANGUAGE Unsafe #-}
module Check04_1 ( main' ) where

import safe Check04_B

main' = do
    let n = mainM 1
    print $ n

