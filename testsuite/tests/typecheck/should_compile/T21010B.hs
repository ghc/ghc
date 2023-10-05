{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module T21010B where
import Data.Coerce ( Coercible)
import Data.Kind   ( Constraint, Type )

newtype WrapFunctor f (a :: Type) = WrapFunctor {runFunctor :: f a}

type role WrapMono representational phantom
newtype WrapMono mono b = WrapMono mono

withMonoCoercible
  :: (Coercible (WrapMono mono other) mono => r)
  -> r
withMonoCoercible = \x -> x
