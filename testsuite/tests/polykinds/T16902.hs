{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Bug where

import Data.Kind
import Data.Type.Equality

data F (a :: k) :: (a ~~ k) => Type where
  MkF :: F a
