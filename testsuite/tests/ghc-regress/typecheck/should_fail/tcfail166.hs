-- Without -fscoped-type-variables, this should fail

module ShouldFail where

destroy :: (forall a. (a -> Maybe (b,a)) -> a -> c) -> [b] -> c
destroy = error "urk"
