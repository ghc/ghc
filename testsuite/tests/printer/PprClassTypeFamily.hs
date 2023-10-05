{-# LANGUAGE UndecidableInstances #-}
module Servant.Auth.Server.Internal.Class where

-- | @IsAuth a ctx v@ indicates that @a@ is an auth type that expects all
-- elements of @ctx@ to be the in the Context and whose authentication check
-- returns an @AuthCheck v@.
class IsAuth a v  where
  type family AuthArgs a :: [*]
  runAuth :: proxy a -> proxy v -> Unapp (AuthArgs a) (AuthCheck v)
