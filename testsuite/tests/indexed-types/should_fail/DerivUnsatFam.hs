{-# LANGUAGE TypeFamilies, StandaloneDeriving #-}

-- Crashed 6.12

module T1769 where

data family T a
deriving instance Functor T
