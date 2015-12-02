-- Test purpose:
-- Ensure that -Wno-compat disables a previously set -Wcompat

{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wno-compat #-}

module WCompatWarningsOnOff where

monadFail :: Monad m => m a
monadFail = do
    Just _ <- undefined
    undefined

(<>) = undefined -- Semigroup warnings
