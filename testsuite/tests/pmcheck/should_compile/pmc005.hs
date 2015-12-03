{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}
{-# LANGUAGE GADTs #-}

module PMC005 where

data T a where
  TList :: T [a]
  TBool :: T Bool

foo :: T c -> T c -> ()
foo TList _     = ()
foo _     TList = ()
