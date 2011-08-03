{-# LANGUAGE TypeFamilies #-}
module T5306 where

import T5306b(F(FInt))   -- succeeds
import T5306a(F(FBool))  -- failed (before fix)
