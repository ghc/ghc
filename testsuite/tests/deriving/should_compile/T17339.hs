{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module T17339 where

class Cls a b
data A1
data A2
data B1
data B2

instance Cls A1 B1
instance Cls A2 B1

deriving anyclass instance Cls A1 B2
deriving anyclass instance Cls A2 B2
