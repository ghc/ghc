{-# LANGUAGE TypeFamilies, DataKinds, TemplateHaskell #-}

-- Based on singleton-gadts
module T12088sg1 where

import Data.Kind (Type)

type Promote :: Type -> Type
type family Promote k

type PromoteX :: k -> Promote k
type family PromoteX a

type Demote :: Type -> Type
type family Demote k

type DemoteX :: k -> Demote k
type family DemoteX a

$(return [])

type instance Demote  [a] = [Demote a]
type instance Promote [a] = [Promote a]

type instance DemoteX '[]    = '[]
type instance DemoteX (x:xs) = DemoteX x : DemoteX xs

type instance PromoteX '[]    = '[]
type instance PromoteX (x:xs) = PromoteX x : PromoteX xs

