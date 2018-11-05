{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Bug where

import Language.Haskell.TH

-- Warnings should be preserved through recover
main :: IO ()
main = putStrLn $(recover (stringE "splice failed")
                          [| let x = "a" in let x = "b" in x |])
