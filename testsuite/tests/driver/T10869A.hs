{-# LANGUAGE CPP #-}
module T10869A (writeMsg) where

writeMsg :: IO ()
#if defined(__GLASGOW_HASKELL__)
writeMsg = putStrLn "Hello HSPP File"
#endif
