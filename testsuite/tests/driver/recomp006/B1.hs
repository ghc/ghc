
{-# LANGUAGE TypeOperators #-}

module B where

infixr 9 :-

type a :- b = (a,b)
