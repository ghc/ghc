module ShouldFail where

-- !!! do must end in an expression
main = do x <- return ()
