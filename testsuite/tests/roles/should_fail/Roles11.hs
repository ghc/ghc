{-# LANGUAGE GADTs, RoleAnnotations #-}

module Roles11 where

type role T2 representational
data T2 a where
  K2 :: T2 Int

