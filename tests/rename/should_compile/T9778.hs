{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module T9778 where

import Data.Kind

data T = A | B

data G a where
  C :: G A

data D = MkD Type Type

type S = Int `MkD` Bool
