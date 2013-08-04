{-# LANGUAGE GADTs, RoleAnnotations #-}

module Roles11 where

data T2 a@R where
  K2 :: T2 Int

