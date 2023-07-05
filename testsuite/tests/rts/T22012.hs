-- Ensure that C11 atomics, which may be implemented as function calls on ARMv8
-- (c.f. `-moutline-atomics`) work in GHCi.
--
-- See #22012.
--
-- See Note [ARM outline atomics and the RTS linker] in m4/fp_armv8_outline_atomics.m4.

{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C.Types

foreign import ccall unsafe "test" main :: IO ()
