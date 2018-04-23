{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T14441 where

import Data.Kind

type family Demote (k :: Type) :: Type
type family DemoteX (a :: k) :: Demote k

data Prox (a :: k) = P

type instance Demote (Prox (a :: k)) = Prox (DemoteX a)
$(return [])
type instance DemoteX P = P
