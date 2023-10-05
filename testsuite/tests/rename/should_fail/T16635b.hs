{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeApplications #-}

module T16635b where

data Unit = U
data P a = MkP

-- OK.
f =      (Just @a :: forall a. a -> Maybe a) U

-- Fails because we cannot generalize to (/\a. Just @a)
--            but NOT because @a is not in scope.
type F = (Just @a :: forall a. a -> Maybe a) U
