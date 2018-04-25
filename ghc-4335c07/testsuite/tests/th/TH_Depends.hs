
{-# LANGUAGE TemplateHaskell #-}

module Main where

import TH_Depends_External (loadStringFromFile)

main :: IO ()
main = putStrLn $loadStringFromFile
