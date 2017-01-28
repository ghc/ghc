{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | @since 4.7.0.0
module GHC.Profiling where

import GHC.Base

-- | Stop attributing ticks to cost centres. Allocations will still be
-- attributed.
--
-- @since 4.7.0.0
foreign import ccall stopProfTimer :: IO ()

-- | Start attributing ticks to cost centres. This is called by the RTS on
-- startup.
--
-- @since 4.7.0.0
foreign import ccall startProfTimer :: IO ()
