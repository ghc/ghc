{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Types

-- splices in "string :: String"
$(Types.genCode)

main :: IO ()
main = putStrLn string
