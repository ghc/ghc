
{-# LANGUAGE CApiFFI #-}

module Main (main) where

import Foreign.C

main :: IO ()
main = do print i
          print j

foreign import capi "capi_value_c.h value i" i :: CInt
foreign import capi "capi_value_c.h value j" j :: CInt

