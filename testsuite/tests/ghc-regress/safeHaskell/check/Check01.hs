{-# LANGUAGE SafeImports #-}
module Check01 ( main' ) where

import safe CheckB

main' = do
    let n = mainM 1
    print $ n

