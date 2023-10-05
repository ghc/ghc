{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wpartial-fields #-}
module T16411 where

import Data.Type.Equality

data T1 z where
  MkT1a :: { rec1 :: () } -> T1 Int
  MkT1b :: (z ~ Bool) => T1 z

data T2 z where
  MkT2a :: { rec2 :: () } -> T2 Int
  MkT2b :: (z ~~ Bool) => T2 z
