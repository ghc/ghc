module T25901_sub_c where

import Data.Kind (Type)
import T25901_sub_c_helper

-- This is OK
type F :: Type -> Type -> Type
type F a b = a # b

-- This should fail because (#) in the data namespace is not exported by T25901_sub_c_helper
f :: C a b => a -> b -> ()
f = (#)