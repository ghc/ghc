{-# OPTIONS_GHC -fpackage-trust #-}
{-# LANGUAGE Unsafe #-}
module Check01 ( main' ) where

import safe Check01_B

main' = do
    let n = mainM 1
    print $ n

