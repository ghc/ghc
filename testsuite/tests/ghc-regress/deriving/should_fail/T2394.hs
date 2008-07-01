{-# OPTIONS_GHC -XDeriveDataTypeable -XStandaloneDeriving #-}

-- Test Trac #2394

module Foo where

import Data.Generics(Data)

deriving instance (Data a,Data b) => Data (a->b)
