{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternSynonyms #-}
module Completesig11 where

data T = A | B | C
{-# COMPLETE A,B #-}
{-# COMPLETE A,C #-}

pattern BS :: T
pattern BS = B
{-# COMPLETE A,BS #-}

m1  :: T -> ()
m1 A = ()
