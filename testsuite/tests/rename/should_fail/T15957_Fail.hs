{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module T15957_Fail where

data P = P { x :: Int, y :: Int }

f1 P{..} = 1 + 3 -- nothing bound is used
f2 P{x, ..} = x + 3 -- y bound but not used
f3 P{x, y, ..} = x + y -- no bindings left, i.e. no new useful bindings introduced


