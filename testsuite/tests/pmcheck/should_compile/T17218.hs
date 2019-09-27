{-# LANGUAGE PatternSynonyms #-}

module Lib where

data T = A | B | C

pattern P = B
{-# COMPLETE A, P #-}

f :: T -> ()
f A = ()
