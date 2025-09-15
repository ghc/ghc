{-# LANGUAGE TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
module T11947 where

theFloatDigits :: forall a. RealFloat a => Int
-- The type is ambiguous
theFloatDigits = floatDigits (undefined @_ @a)

foo :: IO ()
foo = print (theFloatDigits @Double, theFloatDigits @Float)
-- But the applications are not
