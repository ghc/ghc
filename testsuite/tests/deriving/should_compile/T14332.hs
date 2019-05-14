{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module T14332 where

import Data.Kind

class C a b

data D a = D
  deriving ( forall a. C a
           , Show :: Type -> Constraint
           )
