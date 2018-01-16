-- !!! can't name a constructor in an export list
module ShouldFail ( C ) where
data T = C
