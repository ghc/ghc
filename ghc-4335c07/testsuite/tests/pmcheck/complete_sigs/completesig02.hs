{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
module Empty where

pattern Foo :: ()
pattern Foo = ()

a :: () -> ()
a Foo = ()
