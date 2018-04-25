-- Test Trac #2114 (error message)

module ShouldFail where

data T = forall a. MkT a

