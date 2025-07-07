
{-# LANGUAGE TemplateHaskell #-}

module Main where

import TH_Depends_External (checkDirectoryContent)

main :: IO ()
main = putStrLn $checkDirectoryContent
