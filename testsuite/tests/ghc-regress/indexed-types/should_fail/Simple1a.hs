{-# OPTIONS -findexed-types #-}

data family T1 a :: * -> *
data instance T1 Int           = T1_1   -- must fail: too few args
