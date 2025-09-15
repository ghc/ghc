{-# Language ConstraintKinds          #-}
{-# Language DataKinds                #-}
{-# Language GADTs                    #-}
{-# Language PolyKinds                #-}
{-# Language RankNTypes               #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeFamilies             #-}
{-# Language TypeOperators            #-}
module T18828 where

import Data.Kind

type Cat :: Type -> Type
type Cat ob = ob -> ob -> Type

type Dict :: Constraint -> Type
data Dict cls where
 Dict :: cls => Dict cls

type    (:-) :: Cat Constraint
newtype cls1 :- cls2 where
 Sub :: (cls1 => Dict cls2) -> (cls1 :- cls2)

type ObjectSyn :: Cat ob -> Type
type ObjectSyn (cat :: ob -> ob -> Type) = ob

type
  ObjectFam :: Cat ob -> Type
type family
  ObjectFam cat where
  ObjectFam @ob cat = ob
