-- Test purpose:
-- Ensure that using -Wno-compat does not switch on warnings

-- {-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wno-compat #-}

module WCompatWarningsOff where

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
