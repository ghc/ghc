{-# LANGUAGE Unsafe #-}
module Check02 ( main' ) where

import safe Check02_B

main' = do
    let n = mainM 1
    print $ n

