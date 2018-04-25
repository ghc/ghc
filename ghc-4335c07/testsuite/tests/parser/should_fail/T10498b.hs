module T10498b where

-- ghc-7.10 would show the unhelpful error message:
--
-- T10498b.hs:7:5: parse error in if statement: naked if statement

f = if module then True else False
