{-# LANGUAGE ImplicitParams #-}

-- !!! Another implicit parameter test, from Alastair Reid

module ShouldCompile where

import Data.Maybe

type Env = ([(String,Int)],Int)

ident1 :: (?env :: Env) => String -> Int
ident1 x = y
 where
  env = ?env
  y   = fromJust (lookup x (fst env))

ident2 :: (?env :: Env) => String -> Int
ident2 x = y
 where
  y   = fromJust (lookup x (fst ?env))


-- Two more tests from Jeff Lewis
x () = y where y = ?wibble

f () = ?wibble :: Int
g () = f ()
