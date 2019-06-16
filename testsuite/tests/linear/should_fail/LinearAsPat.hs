{-# LANGUAGE LinearTypes #-}

module LinearAsPat where

shouldFail :: Bool ->. Bool
shouldFail x@True = x
