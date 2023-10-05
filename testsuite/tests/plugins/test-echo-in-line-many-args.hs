{-# OPTIONS -fplugin Echo -fplugin-opt Echo:A #-}
{-# OPTIONS -fplugin Echo -fplugin-opt Echo:B #-}

module Main where

foo :: IO a
foo = undefined

bar :: IO a
bar = undefined

main :: IO ()
main = return ()
