{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module T20465 where

import Data.Kind
import Data.Proxy

class Cls (a :: (Type -> Constraint) -> Type)
instance Cls a
instance Cls Proxy
foo :: Cls Proxy => Int
foo = 42
bar :: Int
bar = foo
