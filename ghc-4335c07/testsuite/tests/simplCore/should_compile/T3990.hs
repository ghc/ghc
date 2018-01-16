{-# LANGUAGE TypeFamilies #-}
module T3990 where

data family Complex a
data instance Complex Double = CD {-# UNPACK #-} !Double
                                  {-# UNPACK #-} !Double

data T = T {-# UNPACK #-} !(Complex Double)
-- This shouuld actually get unpacked!

test_case :: T
test_case = T (CD 1 1)
