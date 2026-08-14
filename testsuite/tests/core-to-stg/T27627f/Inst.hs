module Inst where

import Callee

-- Deliberately not in Callee or Caller: see the comment in Caller.hs
instance TC W where tcDummy _ = 7
