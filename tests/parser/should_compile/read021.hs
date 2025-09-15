-- !!! Empty export list

module ShouldCompile() where

instance Show (a->b) where
  show f = "<<function>>"
