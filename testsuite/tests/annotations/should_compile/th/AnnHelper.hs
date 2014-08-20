module AnnHelper where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

traverseModuleAnnotations :: Q [String]
traverseModuleAnnotations = do
  ModuleInfo children <- reifyModule =<< thisModule
  go children [] []
  where
    go []     _visited acc = return acc
    go (x:xs) visited  acc | x `elem` visited = go xs visited acc
                           | otherwise = do
                             ModuleInfo newMods <- reifyModule x
                             newAnns <- reifyAnnotations $ AnnLookupModule x
                             go (newMods ++ xs) (x:visited) (newAnns ++ acc)
