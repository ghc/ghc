{-# LANGUAGE NoScopedTypeVariables, ExplicitForAll #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeApplications #-}

module T16635a where

data Unit = U
data P a = MkP

-- ScopedTypeVariables are disabled.
-- Fails because because @a is not in scope.
type F = (Just @a :: forall a. a -> Maybe a) U
