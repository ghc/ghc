{-# LANGUAGE TypeFamilies #-}
module B (t, k, useG) where

import C

t :: T
t = MkT 1

k :: F T
k = 41

useG :: Int -> Int
useG x = g x
{-# INLINE useG #-}
