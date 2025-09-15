module T10181 where

-- GHC 8.0 and previous wrongly eta-reduced this to
--   t = t
-- but GHC 8.2 does not; some kind of consequence of
-- better simplification in the early stages.

t a = t a
