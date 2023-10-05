{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
module DerivingViaFail2 where

class C a
data A = A
deriving via Maybe instance C A
