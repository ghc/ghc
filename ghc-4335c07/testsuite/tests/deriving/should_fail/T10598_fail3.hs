{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}
module T10598_fail3 where

import GHC.Generics

data T = MkT Int deriving anyclass Generic
