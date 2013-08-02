{-# LANGUAGE RoleAnnotations, PolyKinds #-}

module Roles1 where

data T1 a@N = K1 a
data T2 a@R = K2 a
data T3 (a :: k)@P = K3
data T4 (a :: * -> *)@N b = K4 (a b)

data T5 a = K5 a
data T6 a = K6
data T7 a b = K7 b

