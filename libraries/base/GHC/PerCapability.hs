{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module GHC.PerCapability
    ( PerCapability
    , newPerCapability
    , getPerCapability
    , freePerCapability
      -- Internal
    , capabilitiesChanged
    ) where

import GHC.Base
import GHC.Conc.Sync (getNumCapabilities, myThreadId, threadCapability, yield)
import GHC.Num ((-), (+))
import Data.Foldable (mapM_, forM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef)
import GHC.IOArray (IOArray, newIOArray, readIOArray, writeIOArray,
                    boundsIOArray)
import GHC.IO (unsafePerformIO)

-- | An array of values, one per capability
data PerCapability a
    = PerCapability { pcNewCap :: Int -> IO a
                    , pcFreeCap :: a -> IO ()
                    , pcArr :: !(IORef (IOArray Int a))
                    }

data SomePerCapability where
    SomePerCapability :: forall a. PerCapability a -> SomePerCapability

perCapabilityThings :: IORef [SomePerCapability]
perCapabilityThings = unsafePerformIO $ newIORef []
{-# NOINLINE perCapabilityThings #-}

newPerCapability
    :: (Int -> IO a)
    -> (a -> IO ())
    -> IO (PerCapability a)
newPerCapability newCap freeCap = do
    num_caps <- getNumCapabilities
    arr <- newIOArray (0, num_caps-1) uninitPerCap
    let (low, high) = boundsIOArray arr
    forM_ [low..high] $ \n -> do
        x <- newCap n
        writeIOArray arr n x
    arr_ref <- newIORef arr
    let pc = PerCapability
            { pcNewCap = newCap
            , pcFreeCap = freeCap
            , pcArr = arr_ref
            }
    atomicModifyIORef perCapabilityThings $ \pcs -> (SomePerCapability pc : pcs, ())
    return pc

getPerCapability
    :: PerCapability a
    -> IO a
getPerCapability pc = do
    t <- myThreadId
    (cap, _) <- threadCapability t
    arr <- readIORef (pcArr pc)
    -- It is possible that we've just increased the number of capabilities and the
    -- new EventManager has not yet been constructed by
    -- 'ioManagerCapabilitiesChanged'. We expect this to happen very rarely.
    -- T21561 exercises this.
    -- Two options to proceed:
    --  1) return the EventManager for capability 0. This is guaranteed to exist,
    --     and "shouldn't" cause any correctness issues.
    --  2) Busy wait, with or without a call to 'yield'. This can't deadlock,
    --     because we must be on a brand capability and there must be a call to
    --     'ioManagerCapabilitiesChanged' pending.
    --
    -- We take the second option, with the yield, judging it the most robust.
    if inRange (boundsIOArray arr) cap
      then readIOArray arr cap
      else yield >> getPerCapability pc

-- | Free all per-capability resources. 'getPerCapability'
-- must not be called concurrently.
freePerCapability
    :: PerCapability a
    -> IO ()
freePerCapability pc = do
    let err = error "freePerCapability"
    arr <- atomicModifyIORef (pcArr pc) (\arr -> (arr, err))
    let low, high :: Int
        (low, high) = boundsIOArray arr
    forM_ [low..high] $ \n -> do
        x <- readIOArray arr n
        pcFreeCap pc x

capabilitiesChanged :: IO ()
capabilitiesChanged = do
    things <- readIORef perCapabilityThings
    new_n_caps <- getNumCapabilities
    mapM_ (change_one new_n_caps) things
  where
    change_one new_n_caps (SomePerCapability pc) = do
        arr <- readIORef (pcArr pc)
        new_arr <- newIOArray (0, new_n_caps-1) uninitPerCap
        let (low, high) = boundsIOArray arr
        -- Copy the existing values into the new array
        forM_ [low .. high] $ \i -> do
            x <- readIOArray arr i
            writeIOArray new_arr i x

        -- Free any resoures associated with dead caps
        forM_ [new_n_caps .. high-1] $ \i -> do
            x <- readIOArray arr i
            pcFreeCap pc x

        -- Create new resources for the new caps
        forM_ [high+1 .. new_n_caps-1] $ \i -> do
            x <- pcNewCap pc i
            writeIOArray new_arr i x

        -- update the array reference
        writeIORef (pcArr pc) new_arr

uninitPerCap :: a
uninitPerCap = error "Uninitialized PerCapability slot"

