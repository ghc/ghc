-----------------------------------------------------------------------------
--
-- GHCi Interactive debugging commands 
--
-- Pepe Iborra (supported by Google SoC) 2006
--
-----------------------------------------------------------------------------

module Debugger where

import Linker
import Breakpoints
import RtClosureInspect

import PrelNames
import HscTypes
import IdInfo
--import Id
import Var hiding ( varName )
import VarSet
import VarEnv
import Name 
import NameEnv
import RdrName
import Module
import Finder
import UniqSupply
import Type
import TyCon
import DataCon
import TcGadt
import GHC
import GhciMonad
import PackageConfig

import Outputable
import ErrUtils
import FastString
import SrcLoc
import Util

import Control.Exception
import Control.Monad
import qualified Data.Map as Map
import Data.Array.Unboxed
import Data.Traversable ( traverse )
import Data.Typeable             ( Typeable )
import Data.Maybe
import Data.IORef

import System.IO
import GHC.Exts

#include "HsVersions.h"

-----------------------------
-- | The :breakpoint command
-----------------------------
bkptOptions :: String -> GHCi ()
bkptOptions cmd = do 
  dflags <- getDynFlags
  bt     <- getBkptTable
  bkptOptions' (words cmd) bt
   where
    bkptOptions' ["list"] bt = do 
      let msgs = [ ppr mod <+> colon <+> ppr coords 
                   | (mod,site) <- btList bt
                   , let coords = getSiteCoords bt mod site]
          num_msgs = [parens (int n) <+> msg | (n,msg) <- zip [1..] msgs]
      msg <- showForUser$ if null num_msgs 
                            then text "There are no enabled breakpoints"
                            else vcat num_msgs
      io$ putStrLn msg

    bkptOptions' ["stop"] bt = do
        inside_break <- liftM not isTopLevel
        when inside_break $ throwDyn StopChildSession

    bkptOptions' ("add":cmds) bt 
      | [mod_name,line]<- cmds
      , [(lineNum,[])] <- reads line
      =  handleAdd mod_name $ (\mod->addBkptByLine mod lineNum)

      | [mod_name,line,col] <- cmds
      = handleAdd mod_name $ (\mod->addBkptByCoord mod (read line, read col))

      | otherwise = throwDyn $ CmdLineError $ 
                       "syntax: :breakpoint add Module line [col]"
       where 
         handleAdd mod_name f = do
           sess        <- getSession
           dflags      <- getDynFlags
           mod         <- io$ GHC.findModule sess (GHC.mkModuleName mod_name) Nothing
           ghciHandleDyn (handleBkptEx mod) $
            case f mod bt of
             (newTable, site)  -> do
               setBkptTable newTable 
               io (putStrLn ("Breakpoint set at " ++ 
                              show (getSiteCoords newTable mod site)))

    bkptOptions' ("del":cmds) bt 
      | [i']     <- cmds 
      , [(i,[])] <- reads i'
      , bkpts    <- btList bt
      = if i > length bkpts
           then throwDyn $ CmdLineError 
              "Not a valid breakpoint #. Use :breakpoint list to see the current breakpoints."
           else 
             let (mod, site) = bkpts !! (i-1)
             in handleDel mod $ delBkptBySite mod site

      | [fn,line]      <- cmds 
      , [(lineNum,[])] <- reads line
      , mod            <- GHC.mkModule mainPackageId (GHC.mkModuleName fn)
      = handleDel mod $  delBkptByLine mod lineNum

      | [fn,line,col]  <- cmds 
      , [(lineNum,[])] <- reads line
      , [(colNum,[])]  <- reads col
      , mod            <- GHC.mkModule mainPackageId (GHC.mkModuleName fn)
      = handleDel mod $ delBkptByCoord mod (lineNum, colNum)
        
      | otherwise = throwDyn $ CmdLineError $ 
             "syntax: :breakpoint del (breakpoint # | Module line [col])"

       where delMsg = "Breakpoint deleted"
             handleDel mod f = ghciHandleDyn (handleBkptEx mod) $ do
               modifyBkptTable f
               newTable <- getBkptTable
               sess <- getSession
               dflags <- getDynFlags
               io$ putStrLn delMsg

    bkptOptions' _ _ = throwDyn $ CmdLineError $ 
                         "syntax: :breakpoint (list|stop|add|del)"

    handleBkptEx :: Module -> Debugger.BkptException -> a
    handleBkptEx _ NoBkptFound = error "No suitable breakpoint site found"  --TODO Automatically add to the next suitable line
    handleBkptEx _ NotNeeded   = error "Nothing to do"
    handleBkptEx m NotHandled  = error$ "Module " ++ showSDoc (ppr m) ++  " was not loaded under debugging mode. Enable debugging mode and reload it"

-------------------------
-- Breakpoint Tables
-------------------------

data BkptTable a  = BkptTable { 
                           -- | An array of breaks, indexed by site number
     breakpoints :: Map.Map a (UArray Int Bool)  
                           -- | A list of lines, each line can have zero or more sites, which are annotated with a column number
   , sites       :: Map.Map a [[(SiteNumber, Int)]] 
   }

sitesOf :: Ord a => BkptTable a -> a -> Maybe [[(SiteNumber, Int)]] 
sitesOf bt fn = Map.lookup fn (sites bt)
bkptsOf bt fn = Map.lookup fn (breakpoints bt)


-- The functions for manipulating BkptTables do throw exceptions
data BkptException =
                    NotHandled
                  | NoBkptFound
                  | NotNeeded   -- Used when a breakpoint was already enabled
  deriving Typeable

emptyBkptTable :: Ord a => BkptTable a
addModule      :: Ord a => a -> [(SiteNumber,Coord)] -> BkptTable a -> BkptTable a
-- | Lines start at index 1
addBkptByLine  :: Ord a => a -> Int        -> BkptTable a -> (BkptTable a, SiteNumber)
addBkptByCoord :: Ord a => a -> Coord      -> BkptTable a -> (BkptTable a, SiteNumber)
delBkptByLine  :: Ord a => a -> Int        -> BkptTable a -> BkptTable a
delBkptBySite  :: Ord a => a -> SiteNumber -> BkptTable a -> BkptTable a
delBkptByCoord :: Ord a => a -> Coord      -> BkptTable a -> BkptTable a

isBkptEnabled  :: Ord a => BkptTable a -> BkptLocation a -> Bool
btElems        :: Ord a => BkptTable a -> [(a, [SiteNumber])]
btList         :: Ord a => BkptTable a -> [BkptLocation a]
sitesList      :: Ord a => BkptTable a -> [(a, [Coord])]
getSiteCoords  :: Ord a => BkptTable a -> a -> SiteNumber -> Coord

emptyBkptTable = BkptTable Map.empty Map.empty

addBkptByLine a i bt
   | Just lines    <- sitesOf bt a
   , Just bkptsArr <- bkptsOf bt a
   , i < length lines
   = case lines!!i of 
       []    -> throwDyn NoBkptFound
       (x:_) -> let (siteNum,col) = x
                    wasAlreadyOn  = bkptsArr ! siteNum
                    newArr        = bkptsArr // [(siteNum, True)]
                    newTable      = Map.insert a newArr (breakpoints bt)
        in if wasAlreadyOn 
           then throwDyn NotNeeded
           else (bt{breakpoints=newTable}, siteNum)

   | Just sites    <- sitesOf bt a
   = throwDyn NoBkptFound
   | otherwise     = throwDyn NotHandled  

addBkptByCoord a (r,c) bt 
   | Just lines    <- sitesOf bt a
   , Just bkptsArr <- bkptsOf bt a
   , r < length lines
       = case [ (sn,col) | (sn,col)<-lines!!r, col>=c] of 
       []    -> throwDyn NoBkptFound
       (x:_) -> let (siteNum, col) = x
                    wasAlreadyOn  = bkptsArr ! siteNum
                    newArr        = bkptsArr // [(siteNum, True)]
                    newTable      = Map.insert a newArr (breakpoints bt)
        in if wasAlreadyOn 
           then throwDyn NotNeeded
           else (bt{breakpoints=newTable}, siteNum)

   | Just sites    <- sitesOf bt a
   = throwDyn NoBkptFound
   | otherwise     = throwDyn NotHandled  

delBkptBySite a i bt 
   | Just bkptsArr <- bkptsOf bt a
   , not (inRange (bounds bkptsArr) i)
   = throwDyn NoBkptFound

   | Just bkptsArr <- bkptsOf bt a
   , bkptsArr ! i     -- Check that there was a enabled bkpt here 
   , newArr        <- bkptsArr // [(i,False)] 
   , newTable      <- Map.insert a newArr (breakpoints bt)
   = bt {breakpoints=newTable}

   | Just sites    <- sitesOf bt a
   = throwDyn NotNeeded

   | otherwise = throwDyn NotHandled

delBkptByLine a l bt 
   | Just sites    <- sitesOf bt a
   , (site:_)      <- [s | (s,c') <- sites !! l]
   = delBkptBySite a site bt

   | Just sites    <- sitesOf bt a
   = throwDyn NoBkptFound

   | otherwise = throwDyn NotHandled

delBkptByCoord a (r,c) bt 
   | Just sites    <- sitesOf bt a
   , (site:_)      <- [s | (s,c') <- sites !! r, c>=c', isBkptEnabled bt (a,s)]
   = delBkptBySite a site bt

   | Just sites    <- sitesOf bt a
   = throwDyn NoBkptFound

   | otherwise = throwDyn NotHandled

btElems bt = [ (a, [i | (i,True) <- assocs siteArr])
             | (a, siteArr) <- Map.assocs (breakpoints bt) ]

btList bt =  [(a,site) | (a, sites) <- btElems bt, site <- sites] 

sitesList bt = [ (a, sitesCoords sitesCols) | (a, sitesCols) <- Map.assocs (sites bt) ]
    where sitesCoords sitesCols = 
              [ (row,col) 
                | (row, cols) <- zip [0..] sitesCols, (_,col) <- cols ] 

getSiteCoords bt a site 
   | Just rows <- sitesOf bt a
   = head [ (r,c) | (r,row) <- zip [0..] rows
                  , (s,c)   <- row
                  , s == site ]

-- addModule is dumb and inefficient, but it does the job
--addModule fn siteCoords _ | trace ("addModule: " ++ moduleString (unsafeCoerce# fn) ++ " - " ++ show siteCoords) False = undefined
addModule a [] bt = bt
addModule a siteCoords bt 
   | nrows        <- maximum$ [i | (_,(i,j)) <- siteCoords ]
   , sitesByRow   <- [ [(s,c) | (s,(r,c)) <- siteCoords, r==i] 
                       | i <- [0..nrows] ]
   , nsites       <- length siteCoords
   , initialBkpts <- listArray (1, nsites) (repeat False) 
   = bt{ sites       = Map.insert a sitesByRow (sites bt) 
       , breakpoints = Map.insert a initialBkpts (breakpoints bt) }

isBkptEnabled bt (a,site) 
   | Just bkpts <- bkptsOf bt a 
   , inRange (bounds bkpts) site
   = bkpts ! site 
   | otherwise = throwDyn NotHandled            -- This is an error

-----------------
-- Other stuff
-----------------
refreshBkptTable :: [ModSummary] -> GHCi ()
refreshBkptTable [] = return ()
refreshBkptTable (ms:mod_sums) = do
    sess   <- getSession
    when (Opt_Debugging `elem` flags (GHC.ms_hspp_opts ms)) $ do
      old_table <- getBkptTable
      new_table <- addModuleGHC sess old_table (GHC.ms_mod ms)
      setBkptTable new_table
    refreshBkptTable mod_sums
  where addModuleGHC sess bt mod = do
          Just mod_info <- io$ GHC.getModuleInfo sess mod
          dflags <- getDynFlags
          let sites = GHC.modInfoBkptSites mod_info
          io$ debugTraceMsg dflags 2 
                (ppr mod <> text ": inserted " <> int (length sites) <>
                 text " breakpoints")
          return$ addModule mod sites bt
