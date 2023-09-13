{-# OPTIONS_GHC -O -fworker-wrapper-cbv -dcore-lint -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- It is very tricky to tickle this bug in 9.6/9.8!
-- (It came up in a complicated program due to Mikolaj.)
--
-- We need a join point, with only dictionary arguments
-- whose RHS is just another join-point application, which
-- can be eta-reduced.
--
-- The -fworker-wrapper-cbv makes a wrapper whose RHS looks eta-reducible.

module T23922a where

f :: forall a. Eq a => [a] -> Bool
f x = let {-# NOINLINE j #-}
          j :: Eq [a] => Bool
          j = x==x
      in j
