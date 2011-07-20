-- Trac #1402
-- Broke the specialiser

module ShouldCompile where

newtype Gen a = MkGen{ unGen :: Int -> a }

choose :: Eq a => a -> Gen a
choose n = MkGen (\r -> n)

oneof = choose (1::Int)
