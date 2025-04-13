{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module T12088mm2 where

import Data.Kind
import T12088mm2_helper

data Q

type instance Open Q = Bool

data instance F Q r where
  F0 :: F Q 'True

