{-# LANGUAGE TypeFamilies #-}

module T6018failclosed12 where

-- This exposed a subtle bug in the implementation during development. After
-- unifying the RHS of (1) and (2) the LHS substitution was done only in (2)
-- which made it look like an overlapped equation. This is not the case and this
-- definition should be rejected. The first two equations are here to make sure
-- that the internal implementation does list indexing corrcectly (this is a bit
-- tricky because the list is kept in reverse order).
type family F a b  = r | r -> a b where
  F Float  IO      = Float
  F Bool   IO      = Bool
  F a      IO      = IO a   -- (1)
  F Char   b       = b Int  -- (2)
