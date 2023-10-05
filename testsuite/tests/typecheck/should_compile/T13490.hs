{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module T13490 where

import Data.Typeable

type family Foo a

data C a

foo :: (Typeable (C z), z ~ Foo zp) => C zp
foo = undefined
