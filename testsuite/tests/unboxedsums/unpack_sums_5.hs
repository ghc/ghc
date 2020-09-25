module UnpackSumsFive where
-- Check that failure to unpack is warned about.

data SMaybeT = NoT | JustT {-# UNPACK #-} !T
  deriving Show

data T = MkT {-# UNPACK #-} !SMaybeT
  deriving Show

t :: T
t = MkT (JustT (MkT (JustT (MkT NoT))))
