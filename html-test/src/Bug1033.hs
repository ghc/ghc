{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Bug1033 where

import GHC.Generics

data Foo = Foo

-- | This does some generic foos.
deriving instance Generic Foo
