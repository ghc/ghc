{-# LANGUAGE TemplateHaskell #-}

module T25901_sub_w1 where

import Data.Kind (Type)
import T25901_sub_w1_helper

-- This is OK
type F' :: Type -> Type -> Type
type F' a b = F a b

-- This is OK
f', g', (#.) :: C a b => a -> b -> ()
f' = f
g' = g
(#.) = (#)

$(return [])  -- ensure we check the term-level definitions above

-- This should fail because 'G' is not exported by T25901_sub_w1_helper
type G' :: Type -> Type -> Type
type G' a b = G a b

-- This should fail because (#) in the type namespace is not exported by T25901_sub_w1_helper
type (#.) :: Type -> Type -> Type
type a #. b = a # b
