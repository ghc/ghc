
-- We should get an error message, including a location, for these flags:
{-# OPTIONS_GHC -this-flag-does-not-exist -nor-does-this-one #-}

module Foo where

foo :: ()
foo = ()

