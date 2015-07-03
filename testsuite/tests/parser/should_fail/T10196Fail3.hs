module T10196Fail3 where

-- Modifier letters are not allowed in the middle of an identifier.
-- And this should not be lexed as 2 separate identifiers either.
xᵦx :: Int
xᵦx = 1
