{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import LexicalLetProvider
import LexicalLinearRule

main :: IO ()
main = print (readFromRule True (linearPayload True), readFromRule False (linearPayload False))
