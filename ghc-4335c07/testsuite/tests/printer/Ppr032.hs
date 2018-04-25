{-# LANGUAGE PatternSynonyms #-}

module Sigs where

-- TypeSig
f :: Num a => a -> a
f = undefined

pattern Single :: () => (Show a) => a -> [a]
pattern Single x = [x]

g :: (Show a) => [a] -> a
g (Single x) = x

-- Fixities

infixr  6 +++
infixr  7 ***,///

(+++) :: Int -> Int -> Int
a +++ b = a + 2*b

(***) :: Int -> Int -> Int
a *** b = a - 4*b

(///) :: Int -> Int -> Int
a /// b = 2*a - 3*b

-- Inline signatures

{-# Inline g #-}
{-# INLINE [~34] f #-}

-- Specialise signature

-- Multiple sigs
x,y,z :: Int
x = 0
y = 0
z = 0
