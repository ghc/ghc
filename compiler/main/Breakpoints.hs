-----------------------------------------------------------------------------
--
-- GHC API breakpoints. This module includes the main API (BkptHandler) and
-- utility code for implementing a client to this API used in GHCi 
--
-- Pepe Iborra (supported by Google SoC) 2006
--
-----------------------------------------------------------------------------

module Breakpoints where

#ifdef GHCI
import {-#SOURCE#-} ByteCodeLink ( HValue ) 
#endif

import {-#SOURCE#-} HscTypes     ( Session )
import Name
import Var                       ( Id )
import PrelNames

import GHC.Exts

#ifdef GHCI
data BkptHandler a = BkptHandler {
     handleBreakpoint  :: forall b. Session -> [(Id,HValue)] -> BkptLocation a ->  String -> b -> IO b
   , isAutoBkptEnabled :: Session -> BkptLocation a -> IO Bool
   }

nullBkptHandler = BkptHandler {
    isAutoBkptEnabled = \ _ _     -> return False,
    handleBreakpoint  = \_ _ _ _ b -> putStrLn "null Bkpt Handler" >> return b
                              }
#endif

type BkptLocation a = (a, SiteNumber)
type SiteNumber   = Int

type SiteMap      = [(SiteNumber, Coord)]
type Coord        = (Int, Int)

noDbgSites :: SiteMap
noDbgSites = []

-- | Returns the 'identity' jumps
--   Used to deal with spliced code, where we don't want breakpoints
#ifdef GHCI
lookupBogusBreakpointVal :: Name -> Maybe HValue
lookupBogusBreakpointVal name 
  | name == breakpointJumpName     = Just$ unsafeCoerce# (\_ _ _ _ a->a)
  | name == breakpointAutoJumpName = Just$ unsafeCoerce# (\_ _ _ _ a->a)
  | name == breakpointCondJumpName = Just$ unsafeCoerce# (\_ _ _ _ _ a->a)
  | otherwise = Nothing
#else 
lookupBogusBreakpointVal _ = Nothing
#endif /* GHCI */

