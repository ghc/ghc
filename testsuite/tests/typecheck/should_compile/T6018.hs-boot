{-# LANGUAGE TypeFamilies, PolyKinds #-}

module T6018 where

-- this declaration uses different type variables than the one in the source
-- file but it should be accepted nevertheless
type family F d e f = (r :: k) | r -> d e f
