-----------------------------------------------------------------------------
-- 
-- Module      :  Control.Concurrent.QSemN
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: QSemN.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $
--
-- Quantity semaphores
--
-----------------------------------------------------------------------------

module Control.Concurrent.QSemN
	( QSemN,	-- abstract
	  newQSemN,	-- :: Int   -> IO QSemN
	  waitQSemN,	-- :: QSemN -> Int -> IO ()
	  signalQSemN	-- :: QSemN -> Int -> IO ()
      ) where

import Prelude

import Control.Concurrent.MVar

newtype QSemN = QSemN (MVar (Int,[(Int,MVar ())]))

newQSemN :: Int -> IO QSemN 
newQSemN init = do
   sem <- newMVar (init,[])
   return (QSemN sem)

waitQSemN :: QSemN -> Int -> IO ()
waitQSemN (QSemN sem) sz = do
  (avail,blocked) <- takeMVar sem   -- gain ex. access
  if (avail - sz) >= 0 then
       -- discharging 'sz' still leaves the semaphore
       -- in an 'unblocked' state.
     putMVar sem (avail-sz,[])
   else do
     block <- newEmptyMVar
     putMVar sem (avail, blocked++[(sz,block)])
     takeMVar block

signalQSemN :: QSemN -> Int  -> IO ()
signalQSemN (QSemN sem) n = do
   (avail,blocked)   <- takeMVar sem
   (avail',blocked') <- free (avail+n) blocked
   putMVar sem (avail',blocked')
 where
   free avail []    = return (avail,[])
   free avail ((req,block):blocked)
     | avail >= req = do
	putMVar block ()
	free (avail-req) blocked
     | otherwise    = do
	(avail',blocked') <- free avail blocked
        return (avail',(req,block):blocked')
