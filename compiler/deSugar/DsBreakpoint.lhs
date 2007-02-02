-----------------------------------------------------------------------------
--
-- Support code for instrumentation and expansion of the breakpoint combinator
--
-- Pepe Iborra (supported by Google SoC) 2006
--
-----------------------------------------------------------------------------

\begin{code}
module DsBreakpoint( 
                     dsAndThenMaybeInsertBreakpoint
                   , maybeInsertBreakpoint
                   , breakpoints_enabled
                   , mkBreakpointExpr
                   ) where

import TysPrim
import TysWiredIn
import PrelNames        
import Module
import PackageConfig
import SrcLoc
import TyCon
import TypeRep
import DataCon          
import Type             
import Id 

import IdInfo
import BasicTypes
import OccName

import TcRnMonad
import HsSyn            
import HsLit
import CoreSyn
import CoreUtils
import Outputable
import ErrUtils
import FastString
import DynFlags
 
import DsMonad 
import {-#SOURCE#-}DsExpr ( dsLExpr ) 
import Control.Monad
import Data.IORef
import Foreign.StablePtr
import GHC.Exts
#ifdef GHCI
mkBreakpointExpr :: SrcSpan -> Id -> Type -> DsM (LHsExpr Id)
mkBreakpointExpr loc bkptFuncId ty = do
        scope <- getScope
        mod   <- getModuleDs
        u     <- newUnique
        let mod_name = moduleNameFS$ moduleName mod
            valId = mkUserLocal (mkVarOcc "_result") u ty noSrcLoc 
        when (not instrumenting) $
              warnDs (text "Extracted ids:" <+> (ppr scope $$ 
                                                   ppr (map idType scope)))
        stablePtr <- ioToIOEnv $ newStablePtr (valId:scope)
        site      <- if instrumenting
                        then recordBkpt (srcSpanStart loc)
                        else return 0
        ATyCon opaqueTyCon <- dsLookupGlobal opaqueTyConName
        jumpFuncId         <- mkJumpFunc bkptFuncId
        let [opaqueDataCon] = tyConDataCons opaqueTyCon
            opaqueId = dataConWrapId opaqueDataCon
            opaqueTy = mkTyConApp opaqueTyCon []
            wrapInOpaque id = 
                  l(HsApp (l(HsWrap (WpTyApp (idType id)) (HsVar opaqueId)))
                          (l(HsVar id)))
           -- Yes, I know... I'm gonna burn in hell.
            Ptr addr# = castStablePtrToPtr stablePtr
            locals    = ExplicitList opaqueTy (map wrapInOpaque scope)
            locInfo = nlTuple [ HsLit (HsString (packageIdFS$ modulePackageId mod))
                              , HsLit (HsString mod_name)
                              , HsLit (HsInt (fromIntegral site))]
            funE  = l$ HsVar jumpFuncId
            ptrE  = l (HsLit (HsInt (fromIntegral (I# (addr2Int# addr#)))))
            locsE = l locals
            locE  = l locInfo
            msgE  = l (srcSpanLit loc)
        return $  
            l(l(l(l(funE `HsApp` ptrE) `HsApp` locsE) `HsApp` locE) `HsApp` msgE)        
    where l = L loc
          nlTuple exps = ExplicitTuple (map noLoc exps) Boxed
          srcSpanLit :: SrcSpan -> HsExpr Id
          srcSpanLit span = HsLit (HsString (mkFastString (showSDoc (ppr span))))
          instrumenting = idName bkptFuncId == breakpointAutoName
#else
mkBreakpointExpr = undefined    -- A stage1 ghc doesn't care about breakpoints
#endif

getScope :: DsM [Id]
getScope = getLocalBindsDs >>= return . filter(isValidType .idType )
    where isValidType (FunTy a b)  = isValidType a && isValidType b
          isValidType (NoteTy _ t) = isValidType t
          isValidType (AppTy a b)  = isValidType a && isValidType b
          isValidType (TyConApp con ts) = not (isUnLiftedTyCon con) && 
                                          all isValidType ts
--        isValidType (PredTy p `FunTy` ty ) = False -- TODO: Too restrictive ? 
          isValidType _ = True

dynBreakpoint :: SrcSpan -> DsM (LHsExpr Id)
#ifdef DEBUG
dynBreakpoint loc | not (isGoodSrcSpan loc) = 
                         pprPanic "dynBreakpoint: bad SrcSpan" (ppr loc)
#endif
dynBreakpoint loc = do 
    let autoBreakpoint = Id.mkGlobalId VanillaGlobal breakpointAutoName 
                         breakpointAutoTy vanillaIdInfo
    dflags <- getDOptsDs 
    ioToIOEnv$ debugTraceMsg dflags 3 (text "Breakpoint inserted at " <> ppr loc)
    return$ L loc (HsVar autoBreakpoint)
  where breakpointAutoTy = (ForAllTy alphaTyVar
                                (FunTy (TyVarTy  alphaTyVar)
                                 (TyVarTy alphaTyVar)))

-- Records a breakpoint site and returns the site number
recordBkpt :: SrcLoc -> DsM (Int)
recordBkpt loc = do
    sites_var <- getBkptSitesDs
    sites     <- ioToIOEnv$ readIORef sites_var
    let site   = length sites + 1
    let coords = (srcLocLine loc, srcLocCol loc)
    ioToIOEnv$ writeIORef sites_var ((site, coords) : sites) 
    return site

mkJumpFunc :: Id -> DsM Id  
mkJumpFunc bkptFuncId
    | idName bkptFuncId == breakpointName 
    = build breakpointJumpName id
    | idName bkptFuncId == breakpointCondName 
    = build breakpointCondJumpName (FunTy boolTy)
    | idName bkptFuncId == breakpointAutoName 
    = build breakpointAutoJumpName id
  where 
        tyvar = alphaTyVar
        basicType extra opaqueTy = 
                           (FunTy intTy
                            (FunTy (mkListTy opaqueTy)
                             (FunTy (mkTupleType [stringTy, stringTy, intTy])
                              (FunTy stringTy
                          (ForAllTy tyvar
                               (extra
                                (FunTy (TyVarTy tyvar)
                                 (TyVarTy tyvar))))))))
        build name extra  = do 
            ATyCon opaqueTyCon <- dsLookupGlobal opaqueTyConName
            return$ Id.mkGlobalId VanillaGlobal name
                      (basicType extra (mkTyConApp opaqueTyCon [])) vanillaIdInfo
        mkTupleType tys = mkTupleTy Boxed (length tys) tys

debug_enabled, breakpoints_enabled :: DsM Bool
dsAndThenMaybeInsertBreakpoint :: LHsExpr Id -> DsM CoreExpr
maybeInsertBreakpoint :: LHsExpr Id -> Type ->  DsM (LHsExpr Id)

#if defined(GHCI) && defined(DEBUGGER)
debug_enabled = do
    debugging      <- doptDs Opt_Debugging
    b_enabled      <- breakpoints_enabled
    return (debugging && b_enabled)

breakpoints_enabled = do
    ghcMode            <- getGhcModeDs
    currentModule      <- getModuleDs
    ignore_breakpoints <- doptDs Opt_IgnoreBreakpoints
    return ( not ignore_breakpoints 
          && ghcMode == Interactive 
          && currentModule /= iNTERACTIVE )

maybeInsertBreakpoint lhsexpr@(L loc _) ty = do 
  instrumenting <- isInstrumentationSpot lhsexpr
  scope         <- getScope
  if instrumenting && not(isUnLiftedType ty) && 
     not(isEnabledNullScopeCoalescing && null scope)
         then do L _ dynBkpt <- dynBreakpoint loc 
                 return$ l(HsApp (l$ HsWrap (WpTyApp ty) dynBkpt) lhsexpr)
         else return lhsexpr
  where l = L loc
dsAndThenMaybeInsertBreakpoint expr@(L loc _) = do
  coreExpr      <- dsLExpr expr
  instrumenting <- isInstrumentationSpot expr
  scope         <- getScope
  let ty = exprType coreExpr
  if instrumenting && not (isUnLiftedType (exprType coreExpr)) &&
     not(isEnabledNullScopeCoalescing && null scope)
         then do L _ dynBkpt<- dynBreakpoint loc
                 bkptCore   <- dsLExpr (l$ HsWrap (WpTyApp ty) dynBkpt)
                 return (bkptCore `App` coreExpr)
         else return coreExpr
  where l = L loc
#else
maybeInsertBreakpoint expr _ = return expr
dsAndThenMaybeInsertBreakpoint coreExpr = dsLExpr coreExpr
breakpoints_enabled = return False
debug_enabled = return False
#endif


isInstrumentationSpot (L loc e) = do
  ghcmode   <- getGhcModeDs
  instrumenting <- debug_enabled 
  return$ instrumenting     
          && isGoodSrcSpan loc          -- Avoids 'derived' code
          && (not$ isRedundant e)

isEnabledNullScopeCoalescing = True
isRedundant HsLet  {} = True
isRedundant HsDo   {} = True
isRedundant HsCase {} = False
isRedundant     _     = False

\end{code}
