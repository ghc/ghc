{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FunctionalDependencies #-}

module T6134 where

class C a b | a -> b 

f :: C Int b => Int -> Int
f = undefined
