module T11458 where

optIntArg f = (f Nothing, f (Just True))

optIntArg2 f = (f (Just True), f Nothing)
