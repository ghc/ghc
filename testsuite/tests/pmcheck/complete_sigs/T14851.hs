{-# OPTIONS_GHC -Woverlapping-patterns #-}

{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Bug where

import Type.Reflection

pattern X arg <- (checkFun -> arg)

checkFun :: TypeRep fun -> a
checkFun = undefined

f x = case (x, True) of
          (X _, _) -> 5
          _        -> 6

g x = case x of
          (X _) -> 5
          _     -> 6

