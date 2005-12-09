-- Killed a test compiler, so I thought it was worth including

module ShouldFail where

f :: a -> [a]
f x = g x
    where
      g y = if y  then [] else [y]
