{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module T21010A ( WrapMono, Constrained(..), withMonoCoercible ) where
import T21010B ( WrapMono(..), withMonoCoercible )

import Data.Kind ( Type, Constraint )

class Constrained (f :: Type -> Type) where
  type Dom f (a :: Type) :: Constraint

instance Constrained (WrapMono mono) where
  type Dom (WrapMono mono) b = b ~ mono
