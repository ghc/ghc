{-# LANGUAGE QuasiQuotes #-}
module Main where

main :: IO ()
main = p undefined
  where
    p = \parse -> case () of
                    [parse||] -> return ()
                    _ -> return ()
