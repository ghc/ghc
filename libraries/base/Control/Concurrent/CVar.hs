-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.CVar
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: CVar.hs,v 1.2 2002/04/24 16:31:37 simonmar Exp $
--
-- Channel variables are one-element channels.
--
-----------------------------------------------------------------------------

module Control.Concurrent.CVar
	( -- abstract
	  CVar
	, newCVar	-- :: IO (CVar a)
	, writeCVar	-- :: CVar a -> a -> IO ()
	, readCVar	-- :: CVar a -> IO a
	) where

import Prelude

import Control.Concurrent.MVar

-- @MVars@ provide the basic mechanisms for synchronising access to a
-- shared resource. @CVars@, or channel variables, provide an abstraction
-- that guarantee that the producer is not allowed to run riot, but
-- enforces the interleaved access to the channel variable,i.e., a
-- producer is forced to wait up for a consumer to remove the previous
-- value before it can deposit a new one in the @CVar@.

data CVar a
 = CVar (MVar a)     -- prod -> cons
        (MVar ())    -- cons -> prod

newCVar :: IO (CVar a)
newCVar 
 = newEmptyMVar >>= \ datum ->
   newMVar ()   >>= \ ack ->
   return (CVar datum ack)

writeCVar :: CVar a -> a -> IO ()

writeCVar (CVar datum ack) val
 = takeMVar ack      >> 
   putMVar datum val >>
   return ()

readCVar :: CVar a -> IO a
readCVar (CVar datum ack)
 = takeMVar datum >>= \ val ->
   putMVar ack () >> 
   return val
