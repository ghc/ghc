{-# LANGUAGE RecordWildCards, LambdaCase #-}

-- We need to optimise to avoid leaking the leak indicators by building an IO
-- thunk in getLeakIndicators (see #19356)
{-# OPTIONS_GHC -O #-}

module GHCi.Leak
  ( LeakIndicators
  , getLeakIndicators
  , checkLeakIndicators
  ) where

import Control.Monad
import Data.Bits
import Foreign.Ptr (ptrToIntPtr, intPtrToPtr)
import GHC
import GHC.Ptr (Ptr (..))
import GHCi.Util
import GHC.Driver.Env
import GHC.Driver.Ppr
import GHC.Utils.Outputable
import GHC.Unit.Module.ModDetails
import GHC.Unit.Home.ModInfo
import GHC.Platform (target32Bit)
import GHC.Linker.Types
import Prelude
import System.Mem
import System.Mem.Weak
import GHC.Types.Unique.DFM

-- Checking for space leaks in GHCi. See #15111, and the
-- -fghci-leak-check flag.

data LeakIndicators = LeakIndicators ![LeakModIndicators]

data LeakModIndicators = LeakModIndicators
  { leakMod      :: !(Weak HomeModInfo)
  , leakIface    :: !(Weak ModIface)
  , leakDetails  :: !(Weak ModDetails)
  , leakLinkable :: !(Maybe (Weak Linkable))
  }

-- | Grab weak references to some of the data structures representing
-- the currently loaded modules.
getLeakIndicators :: HscEnv -> IO LeakIndicators
getLeakIndicators HscEnv{..} = do
  let
    go []     = return []
    go (x:xs) = do
      !x' <- f x
      !xs' <- go xs
      return (x':xs')

    f hmi@HomeModInfo{..} = do
      leakMod <- mkWeakPtr hmi Nothing
      leakIface <- mkWeakPtr hm_iface Nothing
      leakDetails <- mkWeakPtr hm_details Nothing
      leakLinkable <- case hm_linkable of
        Nothing -> return Nothing
        Just l  -> do
          l' <- mkWeakPtr l Nothing
          return $! (Just $! l')
      return $! LeakModIndicators{..}

  ls <- go (eltsUDFM hsc_HPT)
  return $! (LeakIndicators $! ls)

-- | Look at the LeakIndicators collected by an earlier call to
-- `getLeakIndicators`, and print messasges if any of them are still
-- alive.
checkLeakIndicators :: DynFlags -> LeakIndicators -> IO ()
checkLeakIndicators dflags (LeakIndicators leakmods)  = do
  performGC
  forM_ leakmods $ \LeakModIndicators{..} -> do
    deRefWeak leakMod >>= \case
      Nothing -> return ()
      Just hmi ->
        report ("HomeModInfo for " ++
          showSDoc dflags (ppr (mi_module (hm_iface hmi)))) (Just hmi)
    deRefWeak leakIface >>= report "ModIface"
    deRefWeak leakDetails >>= report "ModDetails"
    forM_ leakLinkable $ \l -> deRefWeak l >>= report "Linkable"
 where
  report :: String -> Maybe a -> IO ()
  report _ Nothing = return ()
  report msg (Just a) = do
    addr <- anyToPtr a
    putStrLn ("-fghci-leak-check: " ++ msg ++ " is still alive at " ++
              show (maskTagBits addr))

  tagBits
    | target32Bit (targetPlatform dflags) = 2
    | otherwise = 3

  maskTagBits :: Ptr a -> Ptr a
  maskTagBits p = intPtrToPtr (ptrToIntPtr p .&. complement (shiftL 1 tagBits - 1))
