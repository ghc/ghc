-- Test purpose:
-- Ensure that -Wcompat switches on the right warnings

{-# OPTIONS_GHC -Wcompat #-}
-- {-# OPTIONS_GHC -Wno-compat #-}

module WCompatWarningsOn where

monadFail :: Monad m => m a
monadFail = do
    Just _ <- undefined
    undefined

(<>) = undefined -- Semigroup warnings
