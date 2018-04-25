{-# LANGUAGE TypeInType, GADTs #-}

module T11811 where

import Data.Kind

data Test (a :: x) (b :: x) :: x -> *
  where K :: Test Int Bool Double
