{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
module T11947 where

theFloatDigits :: forall a. RealFloat a => Int
-- The type is ambiguous, despite potential defaulting
theFloatDigits = floatDigits (undefined @_ @a)
