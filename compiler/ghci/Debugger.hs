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
import Pretty                    ( Mode(..), showDocWith )
import ErrUtils
import FastString
import SrcLoc
import Util
import Maybes

import Control.Exception
import Control.Monad
import qualified Data.Map as Map
import Data.Array.Unboxed
import Data.Array.Base
import Data.List
import Data.Typeable             ( Typeable )
import Data.Maybe
import Data.IORef

import System.IO
import GHC.Exts

#include "HsVersions.h"

-------------------------------------
-- | The :print & friends commands
-------------------------------------
pprintClosureCommand :: Bool -> Bool -> String -> GHCi ()
pprintClosureCommand bindThings force str = do 
  cms <- getSession
  newvarsNames <- io$ do 
           uniques <- liftM uniqsFromSupply (mkSplitUniqSupply 'q')
           return$ map (\u-> (mkSysTvName u (mkFastString "a"))) uniques
  mb_ids  <- io$ mapM (cleanUp cms newvarsNames) (words str)
  mb_new_ids <- mapM (io . go cms) (catMaybes mb_ids)
  io$ updateIds cms (catMaybes mb_new_ids)
 where 
   -- Find the Id, clean up 'Unknowns'
   cleanUp :: Session -> [Name] -> String -> IO (Maybe Id)
   cleanUp cms newNames str = do
     tythings <- GHC.parseName cms str >>= mapM (GHC.lookupName cms)
     return$ listToMaybe (map (stripUnknowns newNames) 
                              [ i | Just (AnId i) <- tythings]) 

   -- Do the obtainTerm--bindSuspensions-refineIdType dance
   -- Warning! This function got a good deal of side-effects
   go :: Session -> Id -> IO (Maybe Id)
   go cms id = do
     mb_term <- obtainTerm cms force id
     maybe (return Nothing) `flip` mb_term $ \term -> do
       term'     <- if not bindThings then return term 
                     else bindSuspensions cms term                         
       showterm  <- pprTerm cms term'
       unqual    <- GHC.getPrintUnqual cms
       let showSDocForUserOneLine unqual doc = 
               showDocWith LeftMode (doc (mkErrStyle unqual))
       (putStrLn . showSDocForUserOneLine unqual) (ppr id <+> char '=' <+> showterm)
     -- Before leaving, we compare the type obtained to see if it's more specific
     -- Note how we need the Unknown-clear type returned by obtainTerm
       let Just reconstructedType = termType term  
       new_type  <- instantiateTyVarsToUnknown cms 
                    (mostSpecificType (idType id) reconstructedType)
       return . Just $ setIdType id new_type

   updateIds :: Session -> [Id] -> IO ()
   updateIds (Session ref) new_ids = do
     hsc_env <- readIORef ref
     let ictxt = hsc_IC hsc_env
         type_env = ic_type_env ictxt
         filtered_type_env = delListFromNameEnv type_env (map idName new_ids)
         new_type_env =  extendTypeEnvWithIds filtered_type_env new_ids
         new_ic = ictxt {ic_type_env = new_type_env }
     writeIORef ref (hsc_env {hsc_IC = new_ic })

isMoreSpecificThan :: Type -> Type -> Bool
ty `isMoreSpecificThan` ty1 
      | Just subst    <- tcUnifyTys bindOnlyTy1 [repType' ty] [repType' ty1] 
      , substFiltered <- filter (not.isTyVarTy) . varEnvElts . getTvSubstEnv $ subst
      , not . null $ substFiltered
      , all (flip notElemTvSubst subst) ty_vars
      = True
      | otherwise = False
      where bindOnlyTy1 tyv | tyv `elem` ty_vars = AvoidMe
                            | otherwise = BindMe
            ty_vars = varSetElems$ tyVarsOfType ty

mostSpecificType ty1 ty2 | ty1 `isMoreSpecificThan` ty2 = ty1
                         | otherwise = ty2

-- | Give names, and bind in the interactive environment, to all the suspensions
--   included (inductively) in a term
bindSuspensions :: Session -> Term -> IO Term
bindSuspensions cms@(Session ref) t = do 
      hsc_env <- readIORef ref
      inScope <- GHC.getBindings cms
      let ictxt        = hsc_IC hsc_env
          rn_env       = ic_rn_local_env ictxt
          type_env     = ic_type_env ictxt
          prefix       = "_t"
          alreadyUsedNames = map (occNameString . nameOccName . getName) inScope
          availNames   = map ((prefix++) . show) [1..] \\ alreadyUsedNames 
      availNames_var  <- newIORef availNames
      (t', stuff)     <- foldTerm (nameSuspensionsAndGetInfos availNames_var) t
      let (names, tys, hvals) = unzip3 stuff
      concrete_tys    <- mapM (instantiateTyVarsToUnknown cms) tys
      let ids = [ mkGlobalId VanillaGlobal name ty vanillaIdInfo
                  | (name,ty) <- zip names concrete_tys]
          new_type_env = extendTypeEnvWithIds type_env ids 
          new_rn_env   = extendLocalRdrEnv rn_env names
          new_ic       = ictxt { ic_rn_local_env = new_rn_env, 
                                 ic_type_env     = new_type_env }
      extendLinkEnv (zip names hvals)
      writeIORef ref (hsc_env {hsc_IC = new_ic })
      return t'
     where    

--    Processing suspensions. Give names and recopilate info
        nameSuspensionsAndGetInfos :: IORef [String] -> TermFold (IO (Term, [(Name,Type,HValue)]))
        nameSuspensionsAndGetInfos freeNames = TermFold 
                      {
                        fSuspension = doSuspension freeNames
                      , fTerm = \ty dc v tt -> do 
                                    tt' <- sequence tt 
                                    let (terms,names) = unzip tt' 
                                    return (Term ty dc v terms, concat names)
                      , fPrim    = \ty n ->return (Prim ty n,[])
                      }
        doSuspension freeNames ct mb_ty hval Nothing = do
          name <- atomicModifyIORef freeNames (\x->(tail x, head x))
          n <- newGrimName cms name
          let ty' = fromMaybe (error "unexpected") mb_ty
          return (Suspension ct mb_ty hval (Just n), [(n,ty',hval)])


--  A custom Term printer to enable the use of Show instances
pprTerm cms@(Session ref) = customPrintTerm customPrint
 where
  customPrint = \p-> customPrintShowable : customPrintTermBase p 
  customPrintShowable t@Term{ty=ty, dc=dc, val=val} = do
    let hasType = isEmptyVarSet (tyVarsOfType ty)  -- redundant
        isEvaled = isFullyEvaluatedTerm t
    if not isEvaled -- || not hasType
     then return Nothing
     else do 
        hsc_env <- readIORef ref
        dflags  <- GHC.getSessionDynFlags cms
        do
           (new_env, bname) <- bindToFreshName hsc_env ty "showme"
           writeIORef ref (new_env)
           let noop_log _ _ _ _ = return () 
               expr = "show " ++ showSDoc (ppr bname)
           GHC.setSessionDynFlags cms dflags{log_action=noop_log}
           mb_txt <- withExtendedLinkEnv [(bname, val)] 
                                         (GHC.compileExpr cms expr)
           case mb_txt of 
             Just txt -> return . Just . text . unsafeCoerce# $ txt
             Nothing  -> return Nothing
         `finally` do 
           writeIORef ref hsc_env
           GHC.setSessionDynFlags cms dflags
     
  bindToFreshName hsc_env ty userName = do
    name <- newGrimName cms userName 
    let ictxt    = hsc_IC hsc_env
        rn_env   = ic_rn_local_env ictxt
        type_env = ic_type_env ictxt
        id       = mkGlobalId VanillaGlobal name ty vanillaIdInfo
        new_type_env = extendTypeEnv type_env (AnId id)
        new_rn_env   = extendLocalRdrEnv rn_env [name]
        new_ic       = ictxt { ic_rn_local_env = new_rn_env, 
                               ic_type_env     = new_type_env }
    return (hsc_env {hsc_IC = new_ic }, name)

--    Create new uniques and give them sequentially numbered names
--    newGrimName :: Session -> String -> IO Name
newGrimName cms userName  = do
    us <- mkSplitUniqSupply 'b'
    let unique  = uniqFromSupply us
        occname = mkOccName varName userName
        name    = mkInternalName unique occname noSrcLoc
    return name

----------------------------------------------------------------------------
-- | Replace all the tyvars in a Term with the opaque type GHC.Base.Unknown
----------------------------------------------------------------------------
instantiateTyVarsToUnknown :: Session -> Type -> IO Type
instantiateTyVarsToUnknown cms ty
-- We have a GADT, so just fix its tyvars
    | Just (tycon, args) <- splitTyConApp_maybe ty
    , tycon /= funTyCon
    , isGADT tycon
    = mapM fixTyVars args >>= return . mkTyConApp tycon
-- We have a regular TyCon, so map recursively to its args
    | Just (tycon, args) <- splitTyConApp_maybe ty
    , tycon /= funTyCon
    = do unknownTyVar <- unknownTV
         args' <- mapM (instantiateTyVarsToUnknown cms) args
         return$ mkTyConApp tycon args'
-- we have a tyvar of kind *
    | Just tyvar <- getTyVar_maybe ty
    , ([],_) <- splitKindFunTys (tyVarKind tyvar) 
    = unknownTV
-- we have a higher kind tyvar, so insert an unknown of the appropriate kind
    | Just tyvar <- getTyVar_maybe ty
    , (args,_) <- splitKindFunTys (tyVarKind tyvar)
    = liftM mkTyConTy $ unknownTC !! length args
-- Base case
    | otherwise    = return ty 

 where unknownTV = do 
         Just (ATyCon unknown_tc) <- lookupName cms unknownTyConName
         return$ mkTyConTy unknown_tc
       unknownTC = [undefined, unknownTC1, unknownTC2, unknownTC3]
       unknownTC1 = do 
         Just (ATyCon unknown_tc) <- lookupName cms unknown1TyConName
         return unknown_tc
       unknownTC2 = do 
         Just (ATyCon unknown_tc) <- lookupName cms unknown2TyConName
         return unknown_tc
       unknownTC3 = do 
         Just (ATyCon unknown_tc) <- lookupName cms unknown3TyConName
         return unknown_tc
--       isGADT ty | pprTrace' "isGADT" (ppr ty <> colon <> ppr(isGadtSyntaxTyCon ty)) False = undefined
       isGADT tc | Just dcs <- tyConDataCons_maybe tc = any (not . null . dataConEqSpec) dcs
                 | otherwise = False
       fixTyVars ty 
           | Just (tycon, args) <- splitTyConApp_maybe ty
           = mapM fixTyVars args >>= return . mkTyConApp tycon
-- Fix the tyvar so that the interactive environment doesn't choke on it TODO 
           | Just tv <- getTyVar_maybe ty = return ty --TODO
           | otherwise = return ty

-- | The inverse function. Strip the GHC.Base.Unknowns in the type of the id, they correspond to tyvars. The caller must provide an infinite list of fresh names
stripUnknowns :: [Name] -> Id -> Id
stripUnknowns names id = setIdType id . fst . go names . idType 
                           $ id
 where 
   go tyvarsNames@(v:vv) ty 
    | Just (ty1,ty2) <- splitFunTy_maybe ty = let
               (ty1',vv') = go tyvarsNames ty1
               (ty2',vv'')= go vv' ty2
               in (mkFunTy ty1' ty2', vv'')
    | Just (ty1,ty2) <- splitAppTy_maybe ty = let
               (ty1',vv') = go tyvarsNames ty1
               (ty2',vv'')= go vv' ty2
               in (mkAppTy ty1' ty2', vv'')
    | Just (tycon, args) <- splitTyConApp_maybe ty 
    , Just (tycon', vv') <- (fixTycon tycon tyvarsNames)
    , (args',vv'') <- foldr (\arg (aa,vv) -> let (arg',vv') = go vv' arg 
                                             in (arg':aa,vv'))
                            ([],vv') args
    = (mkAppTys tycon' args',vv'')
    | Just (tycon, args) <- splitTyConApp_maybe ty
    , (args',vv') <- foldr (\arg (aa,vv) -> let (arg',vv') = go vv' arg 
                                            in (arg':aa,vv'))
                           ([],tyvarsNames) args
    = (mkTyConApp tycon args',vv')
    | otherwise = (ty, tyvarsNames)
    where  fixTycon tycon (v:vv) = do
               k <- lookup (tyConName tycon) kinds
               return (mkTyVarTy$ mkTyVar v k, vv)
           kinds = [ (unknownTyConName, liftedTypeKind)
                   , (unknown1TyConName, kind1)
                   , (unknown2TyConName, kind2)
                   , (unknown3TyConName, kind3)]
           kind1 = mkArrowKind liftedTypeKind liftedTypeKind
           kind2 = mkArrowKind kind1 liftedTypeKind
           kind3 = mkArrowKind kind2 liftedTypeKind
stripUnknowns _ id = id

-----------------------------
-- | The :breakpoint command
-----------------------------
bkptOptions :: String -> GHCi Bool
bkptOptions "continue" = -- We want to quit if in an inferior session
                         liftM not isTopLevel 
bkptOptions "stop" = do
  inside_break <- liftM not isTopLevel
  when inside_break $ throwDyn StopChildSession 
  return False

bkptOptions cmd = do 
  dflags <- getDynFlags
  bt     <- getBkptTable
  sess   <- getSession
  bkptOptions' sess (words cmd) bt
  return False
   where
    bkptOptions' _ ["list"] bt = do 
      let msgs = [ ppr mod <+> colon <+> ppr coords 
                   | (mod,site) <- btList bt
                   , let coords = getSiteCoords bt mod site]
          num_msgs = [parens (int n) <+> msg | (n,msg) <- zip [1..] msgs]
      msg <- showForUser$ if null num_msgs 
                            then text "There are no enabled breakpoints"
                            else vcat num_msgs
      io$ putStrLn msg

    bkptOptions' s ("add":cmds) bt 
      | [line]         <- cmds
      , [(lineNum,[])] <- reads line
      = do (toplevel,_) <- io$ GHC.getContext s
           case toplevel of
             (m:_) -> handleAdd (\mod->addBkptByLine mod lineNum) m
             [] -> throwDyn $ CmdLineError $ "No module loaded in debugging mode"

      | [mod_name,line]<- cmds
      , [(lineNum,[])] <- reads line
      = io(GHC.findModule s (GHC.mkModuleName mod_name) Nothing) >>=
         handleAdd (\mod->addBkptByLine mod lineNum)

      | [mod_name,line,col] <- cmds
      = io(GHC.findModule s (GHC.mkModuleName mod_name) Nothing) >>=
         handleAdd (\mod->addBkptByCoord mod (read line, read col))

      | otherwise = throwDyn $ CmdLineError $ 
                       "syntax: :breakpoint add Module line [col]"
       where 
         handleAdd f mod = 
           either 
             (handleBkptEx s mod)
             (\(newTable, site) -> do
               setBkptTable newTable
               let (x,y) = getSiteCoords newTable mod site
               io (putStrLn ("Breakpoint set at " ++ showSDoc (ppr mod) 
                    ++ ':' : show x  ++ ':' : show y)))
             (f mod bt) 

    bkptOptions' s ("del":cmds) bt 
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
             "syntax: :breakpoint del (breakpoint # | [Module] line [col])"

       where delMsg = "Breakpoint deleted"
             handleDel mod f = either (handleBkptEx s mod)
                                      (\newtable-> setBkptTable newtable >> io (putStrLn delMsg))
                                      (f bt)
                                      
    bkptOptions' _ _ _ = throwDyn $ CmdLineError $ 
                         "syntax: :breakpoint (list|continue|stop|add|del)"

-- Error messages
--    handleBkptEx :: Session -> Module -> Debugger.BkptException -> a
    handleBkptEx s m NotHandled  = io$ do
       isInterpreted <- findModSummary m >>= isModuleInterpreted s
       if isInterpreted
        then error$ "Module " ++ showSDoc (ppr m) ++  " was not loaded under debugging mode.\n" 
                 ++ "Enable debugging mode with -fdebugging (and reload your module)"
        else error$ "Module " ++ showSDoc (ppr m) ++  " was loaded in compiled (.o) mode.\n" 
                 ++ "You must load a module in interpreted mode and with -fdebugging on to debug it."
         where findModSummary m = do 
                 mod_graph <- getModuleGraph s 
                 return$ head [ modsum | modsum <- mod_graph, ms_mod modsum == m]
    handleBkptEx _ _ e = error (show e)

-------------------------
-- Breakpoint Tables
-------------------------

data BkptTable a  = BkptTable { 
                           -- | An array of breaks, indexed by site number
     breakpoints :: Map.Map a (UArray Int Bool)  
                           -- | A list of lines, each line can have zero or more sites, which are annotated with a column number
   , sites       :: Map.Map a [[(SiteNumber, Int)]] 
   }
                  deriving Show

sitesOf :: Ord a => BkptTable a -> a -> Maybe [[(SiteNumber, Int)]] 
sitesOf bt fn = Map.lookup fn (sites bt)
bkptsOf bt fn = Map.lookup fn (breakpoints bt)


data BkptError =
                    NotHandled  -- Trying to manipulate a element not handled by this BkptTable 
                  | NoBkptFound
                  | NotNeeded   -- Used when a breakpoint was already enabled
  deriving Typeable

instance Show BkptError where
  show NoBkptFound = "No suitable breakpoint site found"
  show NotNeeded  = "Nothing to do"
  show NotHandled  = "BkptTable: Element not controlled by this table"

emptyBkptTable :: Ord a => BkptTable a
addModule      :: Ord a => a -> [(SiteNumber,Coord)] -> BkptTable a -> BkptTable a
-- | Lines start at index 1
addBkptByLine  :: Ord a => a -> Int        -> BkptTable a -> Either BkptError (BkptTable a, SiteNumber)
addBkptByCoord :: Ord a => a -> Coord      -> BkptTable a -> Either BkptError (BkptTable a, SiteNumber)
delBkptByLine  :: Ord a => a -> Int        -> BkptTable a -> Either BkptError (BkptTable a)
delBkptBySite  :: Ord a => a -> SiteNumber -> BkptTable a -> Either BkptError (BkptTable a)
delBkptByCoord :: Ord a => a -> Coord      -> BkptTable a -> Either BkptError (BkptTable a)

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
   = case [line | line <- drop i lines, not (null line)] of 
       ((x:_):_) -> let (siteNum,col) = x
                        wasAlreadyOn  = bkptsArr ! siteNum
                        newArr        = bkptsArr // [(siteNum, True)]
                        newTable      = Map.insert a newArr (breakpoints bt)
        in if wasAlreadyOn 
            then Left NotNeeded
            else Right (bt{breakpoints=newTable}, siteNum)
       otherwise -> Left NoBkptFound

   | Just sites    <- sitesOf bt a
   = Left NoBkptFound
   | otherwise     = Left NotHandled  

addBkptByCoord a (r,c) bt 
   | Just lines    <- sitesOf bt a
   , Just bkptsArr <- bkptsOf bt a
   , r < length lines
       = case [ (sn,col) | (sn,col)<-lines!!r, col>=c] of 
       []    -> Left NoBkptFound
       (x:_) -> let (siteNum, col) = x
                    wasAlreadyOn  = bkptsArr ! siteNum
                    newArr        = bkptsArr // [(siteNum, True)]
                    newTable      = Map.insert a newArr (breakpoints bt)
        in if wasAlreadyOn 
           then Left NotNeeded
           else Right (bt{breakpoints=newTable}, siteNum)

   | Just sites    <- sitesOf bt a
   = Left NoBkptFound
   | otherwise     = Left NotHandled  

delBkptBySite a i bt 
   | Just bkptsArr <- bkptsOf bt a
   , not (inRange (bounds bkptsArr) i)
   = Left NoBkptFound

   | Just bkptsArr <- bkptsOf bt a
   , bkptsArr ! i     -- Check that there was a enabled bkpt here 
   , newArr        <- bkptsArr // [(i,False)] 
   , newTable      <- Map.insert a newArr (breakpoints bt)
   = Right bt {breakpoints=newTable}

   | Just sites    <- sitesOf bt a
   = Left NotNeeded

   | otherwise = Left NotHandled

delBkptByLine a l bt 
   | Just sites    <- sitesOf bt a
   , (site:_)      <- [s | (s,c') <- sites !! l]
   = delBkptBySite a site bt

   | Just sites    <- sitesOf bt a
   = Left NoBkptFound

   | otherwise = Left NotHandled

delBkptByCoord a (r,c) bt 
   | Just sites    <- sitesOf bt a
   , (site:_)      <- [s | (s,c') <- sites !! r, c>=c', isBkptEnabled bt (a,s)]
   = delBkptBySite a site bt

   | Just sites    <- sitesOf bt a
   = Left NoBkptFound

   | otherwise = Left NotHandled

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
addModule a [] bt = bt {sites = Map.insert a [] (sites bt)}
addModule a siteCoords bt 
   | nrows        <- maximum$ [i | (_,(i,j)) <- siteCoords ]
   , sitesByRow   <- [ [(s,c) | (s,(r,c)) <- siteCoords, r==i] 
                       | i <- [0..nrows] ]
   , nsites       <- length siteCoords
   , initialBkpts <- listArray (0, nsites+1) (repeat False) 
   = bt{ sites       = Map.insert a sitesByRow (sites bt) 
       , breakpoints = Map.insert a initialBkpts (breakpoints bt) }

-- This MUST be fast
isBkptEnabled bt site | bt `seq` site `seq` False = undefined
isBkptEnabled bt (a,site) 
   | Just bkpts <- bkptsOf bt a 
   = ASSERT (inRange (bounds bkpts) site) 
     unsafeAt bkpts site

-----------------
-- Other stuff
-----------------
refreshBkptTable :: Session -> BkptTable Module -> [ModSummary] -> IO (BkptTable Module)
refreshBkptTable sess = foldM updIfDebugging
  where 
   updIfDebugging bt ms = do
      isDebugging <- isDebuggingM ms
      if isDebugging 
           then addModuleGHC sess bt (GHC.ms_mod ms)
           else return bt
   addModuleGHC sess bt mod = do
      Just mod_info <- GHC.getModuleInfo sess mod
      dflags <- GHC.getSessionDynFlags sess
      let sites = GHC.modInfoBkptSites mod_info
      debugTraceMsg dflags 2 
                (ppr mod <> text ": inserted " <> int (length sites) <>
                 text " breakpoints")
      return$ addModule mod sites bt
#if defined(GHCI) && defined(DEBUGGER)
   isDebuggingM ms = isModuleInterpreted sess ms >>= \isInterpreted -> 
                     return (Opt_Debugging `elem` dflags && 
                             target == HscInterpreted && isInterpreted)
       where dflags = flags     (GHC.ms_hspp_opts ms)
             target = hscTarget (GHC.ms_hspp_opts ms)
#else
   isDebuggingM _ = return False
#endif
