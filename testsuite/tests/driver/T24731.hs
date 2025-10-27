{-# LANGUAGE TemplateHaskell #-}
module T24731 where

foo :: Int
foo = $([|10|])            
