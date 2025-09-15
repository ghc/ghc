{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module T20521 where

import Data.Kind

type XFam :: forall k l -> k -> l
type family XFam k l x where
  forall k (x :: k). XFam k k x = x

type C2T c = XFam Constraint Type c
type T2C t = XFam Type Constraint t

bad :: C2T (T2C Float) -> Float
bad x = x
