{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module T7704 where

import Data.Typeable

data A1 = A1 deriving Typeable
data A2 = A2
deriving instance Typeable A2

data B1 a = B1 deriving Typeable
data B2 a = B2
deriving instance Typeable B2

data C1 a = C1 deriving Typeable
data C2 a = C2
deriving instance Typeable C2

data D1 f = D1 (f (D1 f)) deriving Typeable
data D2 f = D2 (f (D2 f))
deriving instance Typeable D2

data E1 (a :: k) f = E1 (f Int) deriving Typeable
data E2 (a :: k) f = E2 (f Int)
deriving instance Typeable E2
