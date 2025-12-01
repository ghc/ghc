{-# LANGUAGE TypeFamilies #-}

module T25901_sub_d where

import Data.Kind (Type)
import T25901_sub_d_helper

-- This is OK
f :: C a b => a -> b -> ()
f = (#)

-- This should fail because (#) in the type namespace is not exported by T25901_sub_d_helper
type F :: Type -> Type -> Type
type F a b = a # b