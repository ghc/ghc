
{-# LANGUAGE TypeOperators #-}

module B where

infixl 9 :-

type a :- b = (a,b)
