module Main (main) where

main = seq (error "hello world!" :: Int) (return ())
