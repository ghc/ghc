{-# LANGUAGE NoImplicitPrelude #-}

module M where

-- NB: this module carefully avoids pulling in anything from GHC.Essentials
-- (as we have a stub empty GHC.Essentials in this test):
--
--   - no datatypes (would pull in Typeable)
--   - the type signature (avoids defaulting kicking in)

myId :: forall a. a -> a
myId x = x
