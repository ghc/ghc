{-# OPTIONS_GHC -XStandaloneDeriving #-}

module ShouldFail where

import System.IO( Handle )


-- T is a synonym
type T = Int
deriving instance Eq T

-- Handle is abstract
deriving instance Eq Handle
