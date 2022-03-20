{-# OPTIONS -fplugin-opt Echo2:B #-}

{-# OPTIONS -fplugin Echo1 #-}
{-# OPTIONS -fplugin-opt Echo1:A #-}

{-# OPTIONS -fplugin Echo2 #-}

module Main where

foo :: IO a
foo = undefined

bar :: IO a
bar = undefined

main :: IO ()
main = return ()
