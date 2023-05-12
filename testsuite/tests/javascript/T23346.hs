{-# LANGUAGE CPP #-}

module Main where

main :: IO ()
main = print (correct_host && correct_arch)

#ifdef ghcjs_HOST_OS
correct_host = True
#else
correct_host = False 
#endif

#ifdef javascript_HOST_ARCH
correct_arch = True
#else
correct_arch = False 
#endif
