{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module Bug where

-- regression test for #504:
-- the pragma start and end sequences can both start in column 1
-- without parse error

{-# RULES
  "foo" foo 1 = 1
#-}
foo 1 = 1
