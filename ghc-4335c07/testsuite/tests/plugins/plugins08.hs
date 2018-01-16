-- Tests a plugin added with TH.addCorePlugin
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Simple.DataStructures
import Language.Haskell.TH.Syntax

do addCorePlugin "Simple.Plugin"
   return []

{-# ANN theMessage (ReplaceWith "Right") #-}
{-# NOINLINE theMessage #-}
theMessage = "Wrong"

main = do
    putStrLn "Program Started"
    putStrLn theMessage
    putStrLn "Program Ended"
