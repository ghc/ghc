{-# LANGUAGE PolyKinds, GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module T11811 where

import Data.Kind

type Test :: x -> x -> x -> Type
data Test a b c
  where K :: Test Int Bool Double
