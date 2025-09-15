{-# OPTIONS -fplugin=Hooks.MetaPlugin #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

main :: IO ()
main = print $( [|1|] )
