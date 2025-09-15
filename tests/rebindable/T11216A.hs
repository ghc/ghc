{-# LANGUAGE RebindableSyntax #-}

module Bug where

data Maybe a = Just a | Nothing

foo :: [Maybe a] -> [a]
foo xs = [ x | Just x <- xs ]
