{-# OPTIONS_GHC -O2 #-}

-- Trac #1988: this one killed GHC 6.8.2
-- 		at least with -O2

module ShouldCompile where

newtype CFTree = CFTree (String, [CFTree])

prCFTree :: CFTree -> String
prCFTree (CFTree (_,trees)) = concatMap ps trees
    where ps t@(CFTree (_,[])) = prCFTree t
