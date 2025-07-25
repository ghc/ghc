
{-# LANGUAGE TemplateHaskell #-}

module Main where

import TH_Depends_Dir_External (checkDirectoryContent)

main :: IO ()
main = do 
  print $checkDirectoryContent
