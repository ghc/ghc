{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

module T10487 where

import GHC.Generics

import qualified T10487_M as M

data Name = Name

deriving instance Generic Name
deriving instance Generic M.Name
