-- This one sent 5.03 into an infinite loop, because it
-- gazed too deeply into the functional type of PP

module ShouldCompile where

newtype PP = PP (Int -> PP)

foo = PP undefined
