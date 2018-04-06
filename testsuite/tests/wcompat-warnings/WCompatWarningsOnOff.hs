-- Test purpose:
-- Ensure that -Wno-compat disables a previously set -Wcompat

{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wno-compat #-}

module WCompatWarningsOnOff where

import qualified Data.Semigroup as Semi

monadFail :: Monad m => m a
monadFail = do
    Just _ <- undefined
    undefined

(<>) = undefined -- Semigroup warnings

-- -fwarn-noncanonical-monoid-instances
newtype S = S Int

instance Semi.Semigroup S where
  (<>) = mappend

instance Monoid S where
  S a `mappend` S b = S (a+b)
  mempty = S 0
