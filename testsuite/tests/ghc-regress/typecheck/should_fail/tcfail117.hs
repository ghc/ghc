module ShouldFail where

-- Without -fglasgow-exts both of these should fail

newtype N1 = N1 Int deriving ( Enum )
data    N2 = N2 Int deriving ( Enum )
