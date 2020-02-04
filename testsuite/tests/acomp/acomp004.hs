{-# LANGUAGE ApplicativeComprehensions #-}
{-# OPTIONS_GHC -ddump-types #-}
module Test where

-- This is a do expression that typechecks with only an Applicative constraint
test1 :: Applicative f => (Int -> f Int) -> f Int
test1 f = [x + y | x <- f 3, y <- f 4]

-- Test we can also infer the Applicative version of the type
test2 f = [x + y | x <- f 3, y <- f 4]

-- Test we can also infer the Functor version of the type
test2a f = [x + 1 | x <- f 3]
