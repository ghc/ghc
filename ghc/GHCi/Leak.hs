{-# LANGUAGE RecordWildCards, LambdaCase #-}
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
import Control.Exception

-- Checking for space leaks in GHCi. See #15111, and the
-- -fghci-leak-check flag.

data LeakIndicators = LeakIndicators [LeakModIndicators]

data LeakModIndicators = LeakModIndicators
  { leakMod :: Weak HomeModInfo
  , leakIface :: Weak ModIface
  , leakDetails :: Weak ModDetails
  , leakLinkable :: [Maybe (Weak Linkable)]
  }

-- | Grab weak references to some of the data structures representing
-- the currently loaded modules.
getLeakIndicators :: HscEnv -> IO LeakIndicators
getLeakIndicators hsc_env =
  fmap LeakIndicators $
    forM (eltsUDFM (hsc_HPT hsc_env)) $ \hmi@HomeModInfo{..} -> do
      leakMod <- mkWeakPtr hmi Nothing
      leakIface <- mkWeakPtr hm_iface Nothing
      leakDetails <- mkWeakPtr hm_details Nothing
      leakLinkable <-  mkWeakLinkables hm_linkable
      return $ LeakModIndicators{..}
  where
    mkWeakLinkables :: HomeModLinkable -> IO [Maybe (Weak Linkable)]
    mkWeakLinkables (HomeModLinkable mbc mo) =
      mapM (\ln -> traverse (flip mkWeakPtr Nothing <=< evaluate) ln) [mbc, mo]

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
    deRefWeak leakIface >>= \case
      Nothing -> return ()
      Just miface -> report ("ModIface:" ++ moduleNameString (moduleName (mi_module miface))) (Just miface)
    deRefWeak leakDetails >>= report "ModDetails"
    forM_ leakLinkable $ \l -> forM_ l $ \l' -> deRefWeak l' >>= report "Linkable"
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
