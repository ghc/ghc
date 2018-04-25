{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

module T5884 where

import GHC.Generics
import T5884Other

deriving instance Generic (Pair a)
