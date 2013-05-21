{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE PolyKinds          #-}

module AutoDeriveTypeable where

import Data.Typeable


data A = A

data B = B deriving Typeable

data C = C
deriving instance Typeable C

test = [ typeRep [A], typeRep [B], typeRep [C]
       , typeRep (Proxy :: Proxy 'B), typeRep (Proxy :: Proxy 'C)]
