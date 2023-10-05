{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
module Simple where

pattern Foo :: ()
pattern Foo = ()

a :: () -> ()
a Foo = ()

data A = B | C | D

{-# COMPLETE Foo #-}
{-# COMPLETE B,C #-}
{-# COMPLETE B #-}

b :: A -> A
b B = B
b C = C
