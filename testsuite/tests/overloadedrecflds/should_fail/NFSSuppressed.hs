{-# LANGUAGE NoFieldSelectors #-}

module NFSSuppressed where

import Prelude

data Foo = Foo { foo :: Int }

x = foo
