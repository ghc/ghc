{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Bug where

import Data.Type.Equality

data S (a :: Either x y)

mkRefl :: n :~: j
mkRefl = Refl

right :: forall x y (r :: Either x y).
         S r -> ()
right no =
  case mkRefl @x @y of
    Refl -> no + _
