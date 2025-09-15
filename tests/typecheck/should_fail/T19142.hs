{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE ExplicitForAll
           , TypeApplications
           , KindSignatures
           , ScopedTypeVariables
#-}

module T19142 where

-- Both these examples gave Lint errors
-- NB: the -fdefer-type-errors at the top!!

-- Example 1
f (x :: Maybe Maybe) = x

-- Example 2
foo :: forall (f :: * -> *) . String
foo = ""

g x = foo @Int
