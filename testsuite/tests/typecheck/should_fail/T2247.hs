{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}

module T2247 where

class FD a b | a -> b
instance CFD a b => FD a b

class {- FD a b => -} CFD a b
instance CFD Bool Char
instance CFD Bool Bool

f' :: FD Bool Bool => Bool
f' = True

g' :: FD Bool Char => Bool
g' = False

x = f'
