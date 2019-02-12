{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module T15957_Fail where

data P = P { x :: Int, y :: Int }

f1 P{..} = 1 + 3 -- nothing bound is used
f2 P{x, ..} = x + 3 -- y bound but not used
f3 P{x, y, ..} = x + y -- no bindings left, i.e. no new useful bindings introduced

g2 P{x=a, ..} = a + 3
g3 P{x=a, y=b, ..} = a + b
g4 P{x=0, y=0,..} = 0
g4 _ = 0


