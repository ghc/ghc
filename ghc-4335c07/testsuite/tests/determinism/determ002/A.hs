module A where

-- This is a repro for the issue where to fingerprint a record, field labels
-- were pulled from FastStringEnv in the order of Uniques which are known
-- to have arbitrary order - see Note [Unique Determinism] in Unique.

data B = C
  { e :: ()
  , d :: ()
  }
