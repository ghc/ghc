{-# OPTIONS -fplugin=Hooks.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

main :: IO ()
main = print $( [|1|] )
