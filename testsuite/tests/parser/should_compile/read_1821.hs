
-- Trac #1821

module Par where

f x = x
  where
-- ######### x86_64 machine code:
    g y = y
    h y = y
