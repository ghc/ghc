
{-# OPTIONS_GHC -Wall #-}

module Foo where

-- We should complain that the first r shadows the second one, and give
-- the right locations for the two of them. (trac #2137)

z :: a
z = r
    where
        _a = 'a'
        _f r = r
        _b = 'b'
        r = undefined
        _c = 'c'

