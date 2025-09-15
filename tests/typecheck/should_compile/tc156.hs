{-# LANGUAGE TypeOperators #-}

-- Test infix type constructors

module ShouldCompile where

infixl 4 :*:
infixl 3 :+:

data a :*: b = a :*: b
data a :+: b = a :+: b

data T a b = T (a `b` Int)

type Foo a b = a `T` b

f :: Int :*: Bool :+: Char
f = (3 :*: True) :+: 'c'
