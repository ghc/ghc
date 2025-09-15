{-# LANGUAGE LinearTypes #-}

module LinearListComprehension where

-- Probably nobody actually cares if monad comprehension realised that it can be
-- linear in the first statement. But it can, so we might as well.

guard :: a %1 -> (a %1 -> Bool) %1 -> [Int]
guard x g = [ y | g x, y <- [0,1] ]

-- This isn't correct syntax, but a singleton list comprehension would
-- presumably work too
-- last :: a %1 -> [a]
-- last x = [ x | ]
