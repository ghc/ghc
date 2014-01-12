module WarnMinimalFail2 where

global :: Int
global = 0

class Foo a where
  local :: a
  {-# MINIMAL global #-}
