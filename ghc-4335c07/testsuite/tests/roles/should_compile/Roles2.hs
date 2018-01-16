module Roles2 where

import GHC.Ptr

-- these *must* have certain roles, or things break strangely
-- see TcForeign

data T1 a = K1 (IO a)
data T2 a = K2 (FunPtr a)
