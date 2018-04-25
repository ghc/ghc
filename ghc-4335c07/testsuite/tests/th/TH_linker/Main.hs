{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH

main :: IO ()
main = putStrLn $(return $ LitE $ StringL "hello")
