{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Exts

foreign import prim "foo" foo :: Int# -> Int#

main = do

    let f x = case x of I# x' -> case foo x' of x -> print (I# x)
    mapM_ f [1..7]