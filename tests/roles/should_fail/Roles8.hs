{-# LANGUAGE RoleAnnotations, GADTs #-}

module Roles8 where

data T1 a = K1 a

type role T1 nominal
type role T1 nominal

data T2 b = MkT2

type role T2 representational
type role T2 phantom