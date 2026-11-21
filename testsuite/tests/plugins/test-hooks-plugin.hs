{-# OPTIONS -fplugin=Hooks.MetaPlugin #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

main :: $([t| IO () |])
main = print $( [|1|] )
