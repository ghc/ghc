{-# Language DataKinds                #-}
{-# Language PolyKinds                #-}
{-# Language RankNTypes               #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeApplications         #-}
module T17963 where

import GHC.Types (Constraint, Type, TYPE, RuntimeRep(..))

type Cat :: forall (rep :: RuntimeRep). TYPE rep -> Type
type Cat ob = ob -> ob -> Type

type  Category' :: forall rep (ob :: TYPE rep). Cat @rep ob -> Constraint
class Category' (cat :: Cat @rep ob) where
 id' :: forall a. cat a a
