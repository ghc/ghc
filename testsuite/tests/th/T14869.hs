{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T14869 where

import Data.Kind
import GHC.Exts
import Language.Haskell.TH (pprint, reify, stringE)

type MyConstraint = Constraint
type MyLiftedRep  = LiftedRep

type family Foo1 :: Type
type family Foo2 :: Constraint
type family Foo3 :: MyConstraint
type family Foo4 :: TYPE MyLiftedRep

$(pure [])

foo1, foo2, foo3 :: String
foo1 = $(reify ''Foo1 >>= stringE . pprint)
foo2 = $(reify ''Foo2 >>= stringE . pprint)
foo3 = $(reify ''Foo3 >>= stringE . pprint)
foo4 = $(reify ''Foo4 >>= stringE . pprint)
