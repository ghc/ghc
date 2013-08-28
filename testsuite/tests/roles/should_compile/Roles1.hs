{-# LANGUAGE RoleAnnotations, PolyKinds #-}

module Roles1 where

data T1 a = K1 a
data T2 a = K2 a
data T3 (a :: k) = K3
data T4 (a :: * -> *) b = K4 (a b)

data T5 a = K5 a
data T6 a = K6
data T7 a b = K7 b

type role T1 nominal
type role T2 representational
type role T3 phantom
type role T4 nominal _
type role T5 _