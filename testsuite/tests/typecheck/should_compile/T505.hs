{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module Bug where

foo 1 = 2
bar 0 = 1

-- regression test for #505:
-- the following rule should not case a panic

{-# RULES
  "foo/bar" foo bar = foobar
 #-}

foobar = 2
