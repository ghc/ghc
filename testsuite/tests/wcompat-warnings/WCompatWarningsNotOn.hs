-- Test purpose:
-- Ensure that not using -Wcompat does not enable its warnings

-- {-# OPTIONS_GHC -Wcompat #-}
-- {-# OPTIONS_GHC -Wno-compat #-}

module WCompatWarningsNotOn where

monadFail :: Monad m => m a
monadFail = do
    Just _ <- undefined
    undefined

(<>) = undefined -- Semigroup warnings
