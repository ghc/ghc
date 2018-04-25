{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

module Main where

class (There a ~ b, BackAgain b ~ a) => Foo a b where
     type There a
     type BackAgain b
     there :: a -> b
     back :: b -> a
     tickle :: b -> b

instance Foo Char Int where
     type There Char = Int
     type BackAgain Int = Char
     there = fromEnum
     back = toEnum
     tickle = (+1)

test :: (Foo a b) => a -> a
test x = back (tickle (there x))

main :: IO ()
main = print $ test 'F'

{-

[G] Foo a b
[G] There a ~ b
[G] Back b ~ a

[W] Foo a beta      -- from 'there'
[W] Foo alpha beta  -- from tickle
[W] Foo a beta      -- from back

[D] There a ~ beta
[D] Back beta ~ a

[D] There alpha ~ beta
[D] Back beta ~ alpha


-- Hence beta = b
--       alpha = a



[W] Foo a (There t_a1jL)
[W] Foo t_a1jL (There t_a1jL)
[W] Back (There t_a1jL) ~ t_a1jL

[D] There a ~ There t_a1jL
     hence There t_a1jL ~ b
[D] Back (There t_a1jL) ~ a
[D] There t_a1jL ~ There t_a1jL
[D] Back (There t_a1jL) ~ t_a1jL
-}
