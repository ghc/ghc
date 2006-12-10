-----------------------------------------------------------------------------
--
-- GHC API breakpoints. This module includes the main API (BkptHandler) and
-- utility code for implementing a client to this API used in GHCi 
--
-- Pepe Iborra (supported by Google SoC) 2006
--
-----------------------------------------------------------------------------

module Breakpoints where

import {-#SOURCE#-} HscTypes     ( Session )

data BkptHandler a = BkptHandler {
     handleBreakpoint  :: forall b. Session -> [(Id,HValue)] -> BkptLocation a ->  String -> b -> IO b
   , isAutoBkptEnabled :: Session -> BkptLocation a -> IO Bool
   }

nullBkptHandler = BkptHandler {
    isAutoBkptEnabled = \ _ _     -> return False,
    handleBreakpoint  = \_ _ _ _ b -> putStrLn "null Bkpt Handler" >> return b
                              }

type BkptLocation a = (a, SiteNumber)
type SiteNumber   = Int
