{-# LANGUAGE TypeFamilies #-}

module ShouldFail where

type family T a
type instance T (a, [b])     = T (b, b)        -- var occurs more often
type instance T (a, Maybe b) = T (a, Maybe b)  -- not smaller
type instance T (a, IO [b])  = T (a, T b)      -- nested tyfam application
