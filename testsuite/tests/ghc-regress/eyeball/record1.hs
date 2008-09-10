-- Check that the record selector for maskMB unfolds in the body of f
-- At one stage it didn't because the implicit unfolding looked too big

-- Trac #2581

module ShouldCompile where
import Data.Array.Base

data MBloom s a = MB {
     shiftMB :: {-# UNPACK #-} !Int
    , maskMB :: {-# UNPACK #-} !Int
    , bitArrayMB :: {-# UNPACK #-} !(STUArray s Int Int)
    }

f a b c = case maskMB (MB a b c) of 
	    3 -> True
	    _ -> False
