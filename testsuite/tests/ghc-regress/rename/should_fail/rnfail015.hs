module ShouldFail where

-- !!! duplicate constructors in datatype
-- (bug report from Alex Ferguson, c. 2.06)

data Token
     =  TokNewline
     |  TokLiteral
     |  TokCount
     |  TokCheck
     |  TokIs
     |  TokDeref
     |  TokFind
     |  TokLiteral		-- Duplicated!
     |  TokThe

      deriving Show

main = print TokCount

