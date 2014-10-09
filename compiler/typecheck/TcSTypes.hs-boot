module TcSTypes (TcS) where

import {-# SOURCE #-} TcRnTypes (TcM)

data TcSEnv
newtype TcS a = TcS { unTcS :: TcSEnv -> TcM a }

