{-# LANGUAGE TemplateHaskell #-}

-- Tickles a GHC 6.4 buglet

module ShouldCompile where

class Foo a where
    foo :: a -> a
    foo = id

-- Splice an instance decl that uses the default method
$( [d| instance Foo () where |] )



