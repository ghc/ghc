{-# LANGUAGE TemplateHaskell #-}

module THBug1 where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data HCons a b = HCons a b
data HNil = HNil

mhlt :: [Q Type] -> Q Type
mhlt xss = [t| $(foldThing xss)|]
  where
    foldThing (x:xs) = [t| HCons $x $(foldThing xs)|]
    foldThing []     = [t| HNil |]

mhlt1 :: [Int] -> Q Exp
mhlt1 xss = [| $(foldThing1 xss) |]
  where
    foldThing1 (x:xs) = [| x : $(foldThing1 xs)|]
    foldThing1 [] = [| [] |]
