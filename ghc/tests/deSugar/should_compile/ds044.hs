-- !!! Use of empty record patterns for constructors
-- !!! that don't have any labelled fields. According
-- !!! to the report, this isn't illegal.
module ShouldCompile where

data F = F Int Int
       | G

isF F{} = True
isF _   = False
