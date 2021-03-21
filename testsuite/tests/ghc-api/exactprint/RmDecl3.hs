module RmDecl3 where

-- Remove last declaration from a where clause, where should disappear too
ff y = y + zz
  where
    zz = 1

foo = 3
-- EOF
