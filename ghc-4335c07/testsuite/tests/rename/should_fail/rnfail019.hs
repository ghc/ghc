module ShouldFail where

-- !!! Section with with a bad precedence

f x y = (x:y:)

-- GHC 4.04 (as released) let this by, but it's a precedence error.
