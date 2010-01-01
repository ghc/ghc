{-# LANGUAGE TypeFamilies #-}

data family T1 a :: * -> *
data instance T1 Int Bool Char = T1_3   -- must fail: too many args
