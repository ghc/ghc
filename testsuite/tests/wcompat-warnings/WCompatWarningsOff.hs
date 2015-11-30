-- Test purpose:
-- Ensure that using -Wno-compat does not switch on warnings

-- {-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wno-compat #-}

module WCompatWarningsOff where

monadFail :: Monad m => m a
monadFail = do
    Just _ <- undefined
    undefined

(<>) = undefined -- Semigroup warnings
