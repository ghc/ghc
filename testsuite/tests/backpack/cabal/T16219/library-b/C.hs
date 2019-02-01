{-# language TemplateHaskell #-}
module C where

import Data.Char (toUpper)

import A.Sig
import B

veryExclaimA :: A -> String
veryExclaimA = fmap toUpper . exclaimA
