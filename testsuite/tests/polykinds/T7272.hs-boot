{-# LANGUAGE PolyKinds, RoleAnnotations #-}

module T7272 where

type role C phantom
class C (a :: k)
