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

import IOEnv            ( ioToIOEnv )
import TysPrim          ( alphaTyVar )
import TysWiredIn       ( intTy, stringTy, mkTupleTy, mkListTy, boolTy )
import PrelNames        
import Module           ( moduleName, moduleNameFS, modulePackageId )
import PackageConfig    ( packageIdFS)
import SrcLoc           ( SrcLoc, Located(..), SrcSpan, srcSpanFile,
                          noLoc, noSrcLoc, isGoodSrcSpan,
                          srcLocLine, srcLocCol, srcSpanStart )

import TyCon            ( isUnLiftedTyCon, tyConDataCons )
import TypeRep          ( Type(..) )
import DataCon          
import Type             
import MkId             ( unsafeCoerceId, lazyId )
import Name             ( Name, mkInternalName )
import Var              ( mkTyVar )
import Id               ( Id, idType, mkGlobalId, idName )

import IdInfo           ( vanillaIdInfo, GlobalIdDetails (VanillaGlobal) )
import BasicTypes       ( Boxity(Boxed) )
import OccName          ( mkOccName, tvName )

import TcRnMonad
import HsSyn            
import HsLit            ( HsLit(HsString, HsInt) )
import CoreSyn          ( CoreExpr, Expr (App) )
import CoreUtils        ( exprType )
import Outputable
import ErrUtils         ( debugTraceMsg )
import FastString       ( mkFastString, unpackFS )
import DynFlags         ( GhcMode(..), DynFlag(..) )
 
import DsMonad 
import {-#SOURCE#-}DsExpr ( dsLExpr ) 
import Control.Monad
import Data.IORef
import Foreign.StablePtr ( newStablePtr, castStablePtrToPtr )
import GHC.Exts         ( Ptr(..), Int(..), addr2Int#, unsafeCoerce# )

mkBreakpointExpr :: SrcSpan -> Id -> DsM (LHsExpr Id)
mkBreakpointExpr loc bkptFuncId = do
        scope' <- getLocalBindsDs
        mod  <- getModuleDs
        let scope = filter (isValidType .idType ) scope'
            mod_name = moduleNameFS$ moduleName mod
        if null scope && instrumenting
         then return (l$ HsVar lazyId) 
         else do
          when (not instrumenting) $
              warnDs (text "Extracted ids:" <+> (ppr scope $$ 
                                                   ppr (map idType scope)))
          stablePtr <- ioToIOEnv $ newStablePtr scope
          site <- if instrumenting
                   then recordBkpt (srcSpanStart loc)
                   else return 0
          ATyCon opaqueTyCon <- dsLookupGlobal opaqueTyConName
          jumpFuncId <- mkJumpFunc bkptFuncId
          let [opaqueDataCon] = tyConDataCons opaqueTyCon
              opaqueId = dataConWrapId opaqueDataCon
              opaqueTy = mkTyConApp opaqueTyCon []
              wrapInOpaque id = 
                  l(HsApp (l(HsWrap (WpTyApp (idType id)) (HsVar opaqueId)))
                          (l(HsVar id)))
           -- Yes, I know... I'm gonna burn in hell.
              Ptr addr# = castStablePtrToPtr stablePtr
              hvals = ExplicitList opaqueTy (map wrapInOpaque scope)
              locInfo = nlTuple [ HsLit (HsString (packageIdFS$ modulePackageId mod))
                                , HsLit (HsString mod_name)
                                , HsLit (HsInt (fromIntegral site))]
              
              funE  = l$ HsVar jumpFuncId
              ptrE  = l (HsLit (HsInt (fromIntegral (I# (addr2Int# addr#)))))
              hvalE = l hvals
              locE  = l locInfo
              msgE  = l (srcSpanLit loc)
          return$  l(l(l(l(funE `HsApp` ptrE) `HsApp` hvalE) `HsApp` locE) `HsApp` msgE)
    where l = L loc
          nlTuple exps = ExplicitTuple (map noLoc exps) Boxed
--          isValidType (PredTy p `FunTy` ty ) = False -- TODO: Too restrictive ? 
          isValidType (FunTy a b) = isValidType a && isValidType b
          isValidType (NoteTy _ t) = isValidType t
          isValidType (AppTy a b) = isValidType a && isValidType b
          isValidType (TyConApp con ts) = not (isUnLiftedTyCon con) && all isValidType ts
          isValidType _ = True
          srcSpanLit :: SrcSpan -> HsExpr Id
          srcSpanLit span = HsLit (HsString (mkFastString (showSDoc (ppr span))))
          instrumenting = idName bkptFuncId == breakpointAutoName

debug_enabled :: DsM Bool
#if defined(GHCI) && defined(DEBUGGER)
debug_enabled = do
    debugging      <- doptDs Opt_Debugging
    b_enabled      <- breakpoints_enabled
    return (debugging && b_enabled)
#else
debug_enabled = return False
#endif

maybeInsertBreakpoint :: LHsExpr Id -> Type ->  DsM (LHsExpr Id)
--maybeInsertBreakpoint e | pprTrace("insertBreakpoint at" (ppr e) False = undefined

isInstrumentationSpot (L loc e) = do
  ghcmode   <- getGhcModeDs
  instrumenting <- debug_enabled 
  return$ instrumenting     
          && isGoodSrcSpan loc          -- Avoids 'derived' code
          && (not$ isRedundant e)

isRedundant HsLet  {} = True
isRedundant HsDo   {} = True
isRedundant HsCase {} = True
isRedundant     _     = False

dynBreakpoint :: SrcSpan -> DsM (LHsExpr Id)
dynBreakpoint loc | not (isGoodSrcSpan loc) = 
                         pprPanic "dynBreakpoint" (ppr loc)
dynBreakpoint loc = do 
    let autoBreakpoint = mkGlobalId VanillaGlobal breakpointAutoName 
                         breakpointAutoTy vanillaIdInfo
    dflags <- getDOptsDs 
    ioToIOEnv$ debugTraceMsg dflags 3 (text "Breakpoint inserted at " <> ppr loc)
    return$ L loc (HsVar autoBreakpoint)
  where breakpointAutoTy = (ForAllTy alphaTyVar
                                (FunTy (TyVarTy  alphaTyVar)
                                 (TyVarTy alphaTyVar)))

-- Records a breakpoint site and returns the site number
recordBkpt :: SrcLoc -> DsM (Int)
--recordBkpt | trace "recordBkpt" False = undefined
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
            return$ mkGlobalId VanillaGlobal name
                      (basicType extra (mkTyConApp opaqueTyCon [])) vanillaIdInfo
        mkTupleType tys = mkTupleTy Boxed (length tys) tys

breakpoints_enabled :: DsM Bool
dsAndThenMaybeInsertBreakpoint :: LHsExpr Id -> DsM CoreExpr

#ifdef GHCI
maybeInsertBreakpoint lhsexpr@(L loc _) ty = do 
  instrumenting <- isInstrumentationSpot lhsexpr
  if instrumenting
         then do L _ dynBkpt <- dynBreakpoint loc 
--                 return (l (HsApp (l$ TyApp dynBkpt [ty]) lhsexpr))
                 return$ l(HsApp (l$ HsWrap (WpTyApp ty) dynBkpt) lhsexpr)
         else return lhsexpr
  where l = L loc

dsAndThenMaybeInsertBreakpoint expr@(L loc _) = do
  coreExpr  <- dsLExpr expr
  instrumenting <- isInstrumentationSpot expr
  if instrumenting
         then do L _ dynBkpt<- dynBreakpoint loc
                 bkptCore   <- dsLExpr (l$ HsWrap (WpTyApp (exprType coreExpr)) dynBkpt)
                 return (bkptCore `App` coreExpr)
         else return coreExpr
  where l = L loc

breakpoints_enabled = do
    ghcMode            <- getGhcModeDs
    currentModule      <- getModuleDs
    ignore_breakpoints <- doptDs Opt_IgnoreBreakpoints
    return ( not ignore_breakpoints 
          && ghcMode == Interactive 
          && currentModule /= iNTERACTIVE )
#else
maybeInsertBreakpoint expr _ = return expr
dsAndThenMaybeInsertBreakpoint coreExpr = dsLExpr coreExpr
breakpoints_enabled = return False
#endif
\end{code}
