-- Copyright (C) 2015, Luke Iannini

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where
import Printf ( pr )

foreign import ccall "talkToCxx" talkToCxx :: IO ()

main :: IO ()
main = do
  putStrLn ( $(pr "Hello From Template Haskell!") )
  talkToCxx
