{-# LANGUAGE LinearTypes #-}

module LinearRecordUpdate where

data R = R { x :: Int, y :: Bool }

shouldFail :: R ->. R
shouldFail r = r { y = False }
