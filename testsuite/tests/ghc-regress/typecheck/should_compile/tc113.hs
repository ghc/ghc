-- !!! Monomorphism restriction

module ShouldCompile where

foo :: Eq a => a -> b -> b
foo x y = y

-- Expect test2 :: forall b. b->b
-- despite the monomorphism restriction
poly = foo (3::Int)

-- Check that test2 is polymorphic
test = (poly True, poly 'c')
