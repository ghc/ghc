module WarnMinimalFail1 where

global :: Int
global = 0
{-# MINIMAL global #-} -- invalid, should only be used inside a class declaration
