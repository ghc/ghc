{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}

module T11348 where

import Data.Kind
import Data.Proxy

type family TrivialFamily t :: Type
type instance TrivialFamily (t :: Type) = Bool

data R where
    R :: Proxy Bool -> R

type ProblemType t = 'R ('Proxy :: Proxy (TrivialFamily t))
