{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}
module T14745 where

class C a b c | a -> b c

instance C Int Bool Char

f :: (C Int b c) => a -> c
f = undefined
