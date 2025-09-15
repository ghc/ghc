{-# LANGUAGE LinearTypes #-}

module LinearAsPat where

shouldFail :: Bool %1 -> Bool
shouldFail x@True = x
