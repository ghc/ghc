{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -optc-O3 #-}

module T14452 where
foreign import capi unsafe "stdlib.h exit" c_exit :: Int -> IO ()
