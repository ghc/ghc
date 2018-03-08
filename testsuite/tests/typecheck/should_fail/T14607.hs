{-# OPTIONS_GHC -fdefer-type-errors #-}
    -- This line is crucial to the bug

{-# Language GADTs #-}
{-# Language InstanceSigs #-}
{-# Language KindSignatures #-}
{-# Language TypeFamilies #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}

module T14607 where

import Data.Kind

data LamCons :: Type -> Type -> () -> Type where
  C :: LamCons a a '()

class Mk a where
  mk :: LamCons a a '()

instance Mk a where
  mk :: LamCons a '()
  mk = mk
