{-# OPTIONS_GHC -Wunused-do-bind -fdefer-out-of-scope-variables #-}
module T17697 where

main :: IO ()
main = do
    threadDelay 1
    return ()
