{-# LANGUAGE Unsafe #-}
module Check03 where

import Check03_B

mainN = do
    let n = mainM 1
    print $ n

