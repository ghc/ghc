
-- See Trac #1370
-- THis made GHC 6.6 diverge!

module ShouldCompile where

newtype T = V T deriving(Eq,Ord)
