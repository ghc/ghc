{-# LANGUAGE DeriveRepresentable #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE GADTs               #-}

module ShouldFail2 where

import GHC.Generics

-- We do not support GADTs
data Term a where
  Int :: Term Int

deriving instance Representable0 (Term a)
