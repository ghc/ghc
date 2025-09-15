{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module ExplicitSpecificityA2 where

class C a where

-- D :: forall {k}. k -> *
data D a where
  K :: D a

-- While the type of D abstracts over an implicit (inferred) variable `k`,
-- this instance should not be rejected for implicitly including an inferred
-- type variable, as it is not user written.
instance C (D a) where


