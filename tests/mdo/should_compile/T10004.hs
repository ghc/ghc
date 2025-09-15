{-# LANGUAGE RecursiveDo #-}
module T10004 where

bar :: IO ()
bar = do rec {}
         return ()
