module ShouldFail where

-- !!! ambiguous constraint in the context of an instance declaration
class Bar a
instance Bar a => Bar Bool

