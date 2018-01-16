-- Intended to test that the plugins have basic functionality --
--  * Can modify the program
--  * Get to see command line options
module Main where

import Simple.DataStructures

{-# ANN theMessage (ReplaceWith "Right") #-}
{-# NOINLINE theMessage #-}
theMessage = "Wrong"

main = do
    putStrLn "Program Started"
    putStrLn theMessage
    putStrLn "Program Ended"