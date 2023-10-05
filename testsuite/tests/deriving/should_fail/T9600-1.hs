{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative

newtype Foo a = Foo (a -> a) deriving Applicative
