{-# LANGUAGE RoleAnnotations, GADTs #-}

module Roles8 where

data T1 a@P = K1 a
