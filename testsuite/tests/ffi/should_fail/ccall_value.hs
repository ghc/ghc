
{-# LANGUAGE CApiFFI #-}

module Main (main) where

import Foreign.C

main :: IO ()
main = print i

foreign import ccall "ccall_value_c.h value i" i :: CInt

