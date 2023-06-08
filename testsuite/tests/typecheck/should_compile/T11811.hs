{-# LANGUAGE PolyKinds, GADTs, CUSKs #-}

module T11811 where

import Data.Kind

-- This test is specifically about CUSKs, so we do not use
-- a standalone kind signature here.
data Test (a :: x) (b :: x) :: x -> Type
  where K :: Test Int Bool Double
