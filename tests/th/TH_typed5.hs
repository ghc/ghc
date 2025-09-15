{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Ppr

main = do
    putStrLn =<< fmap pprint (typedSpliceE $ typedBracketE [| 'z' |])
    putStrLn =<< fmap pprint (typedSpliceE $ appE [| id |] (typedBracketE [| 'z' |]))
