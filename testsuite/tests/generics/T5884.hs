{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

module T5884 where

import GHC.Generics

import Data.Complex

deriving instance Generic (Complex v)
