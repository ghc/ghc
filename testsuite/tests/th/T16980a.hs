module T16980a where

default (Integer)  -- just to be really explicit

x = 5   -- this should be an Integer

$(return [])

y :: Int
y = x   -- this should be a type error; types cannot communicate across splices
