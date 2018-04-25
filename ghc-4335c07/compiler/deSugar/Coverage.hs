{-
(c) Galois, 2006
(c) University of Glasgow, 2007
-}

{-# LANGUAGE NondecreasingIndentation, RecordWildCards #-}

module Coverage (addTicksToBinds, hpcInitCode) where

import GhcPrelude as Prelude

import qualified GHCi
import GHCi.RemoteTypes
import Data.Array
import ByteCodeTypes
import GHC.Stack.CCS
import Type
import HsSyn
import Module
import Outputable
import DynFlags
import ConLike
import Control.Monad
import SrcLoc
import ErrUtils
import NameSet hiding (FreeVars)
import Name
import Bag
import CostCentre
import CoreSyn
import Id
import VarSet
import Data.List
import FastString
import HscTypes
import TyCon
import UniqSupply
import BasicTypes
import MonadUtils
import Maybes
import CLabel
import Util

import Data.Time
import System.Directory

import Trace.Hpc.Mix
import Trace.Hpc.Util

import Data.Map (Map)
import qualified Data.Map as Map

{-
************************************************************************
*                                                                      *
*              The main function: addTicksToBinds
*                                                                      *
************************************************************************
-}

addTicksToBinds
        :: HscEnv
        -> Module
        -> ModLocation          -- ... off the current module
        -> NameSet              -- Exported Ids.  When we call addTicksToBinds,
                                -- isExportedId doesn't work yet (the desugarer
                                -- hasn't set it), so we have to work from this set.
        -> [TyCon]              -- Type constructor in this module
        -> LHsBinds GhcTc
        -> IO (LHsBinds GhcTc, HpcInfo, Maybe ModBreaks)

addTicksToBinds hsc_env mod mod_loc exports tyCons binds
  | let dflags = hsc_dflags hsc_env
        passes = coveragePasses dflags, not (null passes),
    Just orig_file <- ml_hs_file mod_loc,
    not ("boot" `isSuffixOf` orig_file) = do

     us <- mkSplitUniqSupply 'C' -- for cost centres
     let  orig_file2 = guessSourceFile binds orig_file

          tickPass tickish (binds,st) =
            let env = TTE
                      { fileName     = mkFastString orig_file2
                      , declPath     = []
                      , tte_dflags   = dflags
                      , exports      = exports
                      , inlines      = emptyVarSet
                      , inScope      = emptyVarSet
                      , blackList    = Map.fromList
                                          [ (getSrcSpan (tyConName tyCon),())
                                          | tyCon <- tyCons ]
                      , density      = mkDensity tickish dflags
                      , this_mod     = mod
                      , tickishType  = tickish
}
                (binds',_,st') = unTM (addTickLHsBinds binds) env st
            in (binds', st')

          initState = TT { tickBoxCount = 0
                         , mixEntries   = []
                         , uniqSupply   = us
                         }

          (binds1,st) = foldr tickPass (binds, initState) passes

     let tickCount = tickBoxCount st
         entries = reverse $ mixEntries st
     hashNo <- writeMixEntries dflags mod tickCount entries orig_file2
     modBreaks <- mkModBreaks hsc_env mod tickCount entries

     when (dopt Opt_D_dump_ticked dflags) $
         putLogMsg dflags NoReason SevDump noSrcSpan
             (defaultDumpStyle dflags) (pprLHsBinds binds1)

     return (binds1, HpcInfo tickCount hashNo, Just modBreaks)

  | otherwise = return (binds, emptyHpcInfo False, Nothing)

guessSourceFile :: LHsBinds GhcTc -> FilePath -> FilePath
guessSourceFile binds orig_file =
     -- Try look for a file generated from a .hsc file to a
     -- .hs file, by peeking ahead.
     let top_pos = catMaybes $ foldrBag (\ (L pos _) rest ->
                                 srcSpanFileName_maybe pos : rest) [] binds
     in
     case top_pos of
        (file_name:_) | ".hsc" `isSuffixOf` unpackFS file_name
                      -> unpackFS file_name
        _ -> orig_file


mkModBreaks :: HscEnv -> Module -> Int -> [MixEntry_] -> IO ModBreaks
mkModBreaks hsc_env mod count entries
  | HscInterpreted <- hscTarget (hsc_dflags hsc_env) = do
    breakArray <- GHCi.newBreakArray hsc_env (length entries)
    ccs <- mkCCSArray hsc_env mod count entries
    let
           locsTicks  = listArray (0,count-1) [ span  | (span,_,_,_)  <- entries ]
           varsTicks  = listArray (0,count-1) [ vars  | (_,_,vars,_)  <- entries ]
           declsTicks = listArray (0,count-1) [ decls | (_,decls,_,_) <- entries ]
    return emptyModBreaks
                       { modBreaks_flags = breakArray
                       , modBreaks_locs  = locsTicks
                       , modBreaks_vars  = varsTicks
                       , modBreaks_decls = declsTicks
                       , modBreaks_ccs   = ccs
                       }
  | otherwise = return emptyModBreaks

mkCCSArray
  :: HscEnv -> Module -> Int -> [MixEntry_]
  -> IO (Array BreakIndex (RemotePtr GHC.Stack.CCS.CostCentre))
mkCCSArray hsc_env modul count entries = do
  if interpreterProfiled dflags
    then do
      let module_str = moduleNameString (moduleName modul)
      costcentres <- GHCi.mkCostCentres hsc_env module_str (map mk_one entries)
      return (listArray (0,count-1) costcentres)
    else do
      return (listArray (0,-1) [])
 where
    dflags = hsc_dflags hsc_env
    mk_one (srcspan, decl_path, _, _) = (name, src)
      where name = concat (intersperse "." decl_path)
            src = showSDoc dflags (ppr srcspan)


writeMixEntries
  :: DynFlags -> Module -> Int -> [MixEntry_] -> FilePath -> IO Int
writeMixEntries dflags mod count entries filename
  | not (gopt Opt_Hpc dflags) = return 0
  | otherwise   = do
        let
            hpc_dir = hpcDir dflags
            mod_name = moduleNameString (moduleName mod)

            hpc_mod_dir
              | moduleUnitId mod == mainUnitId  = hpc_dir
              | otherwise = hpc_dir ++ "/" ++ unitIdString (moduleUnitId mod)

            tabStop = 8 -- <tab> counts as a normal char in GHC's
                        -- location ranges.

        createDirectoryIfMissing True hpc_mod_dir
        modTime <- getModificationUTCTime filename
        let entries' = [ (hpcPos, box)
                       | (span,_,_,box) <- entries, hpcPos <- [mkHpcPos span] ]
        when (entries' `lengthIsNot` count) $ do
          panic "the number of .mix entries are inconsistent"
        let hashNo = mixHash filename modTime tabStop entries'
        mixCreate hpc_mod_dir mod_name
                       $ Mix filename modTime (toHash hashNo) tabStop entries'
        return hashNo


-- -----------------------------------------------------------------------------
-- TickDensity: where to insert ticks

data TickDensity
  = TickForCoverage       -- for Hpc
  | TickForBreakPoints    -- for GHCi
  | TickAllFunctions      -- for -prof-auto-all
  | TickTopFunctions      -- for -prof-auto-top
  | TickExportedFunctions -- for -prof-auto-exported
  | TickCallSites         -- for stack tracing
  deriving Eq

mkDensity :: TickishType -> DynFlags -> TickDensity
mkDensity tickish dflags = case tickish of
  HpcTicks             -> TickForCoverage
  SourceNotes          -> TickForCoverage
  Breakpoints          -> TickForBreakPoints
  ProfNotes ->
    case profAuto dflags of
      ProfAutoAll      -> TickAllFunctions
      ProfAutoTop      -> TickTopFunctions
      ProfAutoExports  -> TickExportedFunctions
      ProfAutoCalls    -> TickCallSites
      _other           -> panic "mkDensity"

-- | Decide whether to add a tick to a binding or not.
shouldTickBind  :: TickDensity
                -> Bool         -- top level?
                -> Bool         -- exported?
                -> Bool         -- simple pat bind?
                -> Bool         -- INLINE pragma?
                -> Bool

shouldTickBind density top_lev exported _simple_pat inline
 = case density of
      TickForBreakPoints    -> False
        -- we never add breakpoints to simple pattern bindings
        -- (there's always a tick on the rhs anyway).
      TickAllFunctions      -> not inline
      TickTopFunctions      -> top_lev && not inline
      TickExportedFunctions -> exported && not inline
      TickForCoverage       -> True
      TickCallSites         -> False

shouldTickPatBind :: TickDensity -> Bool -> Bool
shouldTickPatBind density top_lev
  = case density of
      TickForBreakPoints    -> False
      TickAllFunctions      -> True
      TickTopFunctions      -> top_lev
      TickExportedFunctions -> False
      TickForCoverage       -> False
      TickCallSites         -> False

-- -----------------------------------------------------------------------------
-- Adding ticks to bindings

addTickLHsBinds :: LHsBinds GhcTc -> TM (LHsBinds GhcTc)
addTickLHsBinds = mapBagM addTickLHsBind

addTickLHsBind :: LHsBind GhcTc -> TM (LHsBind GhcTc)
addTickLHsBind (L pos bind@(AbsBinds { abs_binds   = binds,
                                       abs_exports = abs_exports })) = do
  withEnv add_exports $ do
  withEnv add_inlines $ do
  binds' <- addTickLHsBinds binds
  return $ L pos $ bind { abs_binds = binds' }
 where
   -- in AbsBinds, the Id on each binding is not the actual top-level
   -- Id that we are defining, they are related by the abs_exports
   -- field of AbsBinds.  So if we're doing TickExportedFunctions we need
   -- to add the local Ids to the set of exported Names so that we know to
   -- tick the right bindings.
   add_exports env =
     env{ exports = exports env `extendNameSetList`
                      [ idName mid
                      | ABE{ abe_poly = pid, abe_mono = mid } <- abs_exports
                      , idName pid `elemNameSet` (exports env) ] }

   -- See Note [inline sccs]
   add_inlines env =
     env{ inlines = inlines env `extendVarSetList`
                      [ mid
                      | ABE{ abe_poly = pid, abe_mono = mid } <- abs_exports
                      , isInlinePragma (idInlinePragma pid) ] }

addTickLHsBind (L pos (funBind@(FunBind { fun_id = (L _ id)  }))) = do
  let name = getOccString id
  decl_path <- getPathEntry
  density <- getDensity

  inline_ids <- liftM inlines getEnv
  -- See Note [inline sccs]
  let inline   = isInlinePragma (idInlinePragma id)
                 || id `elemVarSet` inline_ids

  -- See Note [inline sccs]
  tickish <- tickishType `liftM` getEnv
  if inline && tickish == ProfNotes then return (L pos funBind) else do

  (fvs, mg@(MG { mg_alts = matches' })) <-
        getFreeVars $
        addPathEntry name $
        addTickMatchGroup False (fun_matches funBind)

  blackListed <- isBlackListed pos
  exported_names <- liftM exports getEnv

  -- We don't want to generate code for blacklisted positions
  -- We don't want redundant ticks on simple pattern bindings
  -- We don't want to tick non-exported bindings in TickExportedFunctions
  let simple = isSimplePatBind funBind
      toplev = null decl_path
      exported = idName id `elemNameSet` exported_names

  tick <- if not blackListed &&
               shouldTickBind density toplev exported simple inline
             then
                bindTick density name pos fvs
             else
                return Nothing

  let mbCons = maybe Prelude.id (:)
  return $ L pos $ funBind { fun_matches = mg { mg_alts = matches' }
                           , fun_tick = tick `mbCons` fun_tick funBind }

   where
   -- a binding is a simple pattern binding if it is a funbind with
   -- zero patterns
   isSimplePatBind :: HsBind a -> Bool
   isSimplePatBind funBind = matchGroupArity (fun_matches funBind) == 0

-- TODO: Revisit this
addTickLHsBind (L pos (pat@(PatBind { pat_lhs = lhs, pat_rhs = rhs }))) = do
  let name = "(...)"
  (fvs, rhs') <- getFreeVars $ addPathEntry name $ addTickGRHSs False False rhs
  let pat' = pat { pat_rhs = rhs'}

  -- Should create ticks here?
  density <- getDensity
  decl_path <- getPathEntry
  let top_lev = null decl_path
  if not (shouldTickPatBind density top_lev) then return (L pos pat') else do

    -- Allocate the ticks
    rhs_tick <- bindTick density name pos fvs
    let patvars = map getOccString (collectPatBinders lhs)
    patvar_ticks <- mapM (\v -> bindTick density v pos fvs) patvars

    -- Add to pattern
    let mbCons = maybe id (:)
        rhs_ticks = rhs_tick `mbCons` fst (pat_ticks pat')
        patvar_tickss = zipWith mbCons patvar_ticks
                        (snd (pat_ticks pat') ++ repeat [])
    return $ L pos $ pat' { pat_ticks = (rhs_ticks, patvar_tickss) }

-- Only internal stuff, not from source, uses VarBind, so we ignore it.
addTickLHsBind var_bind@(L _ (VarBind {})) = return var_bind
addTickLHsBind patsyn_bind@(L _ (PatSynBind {})) = return patsyn_bind


bindTick
  :: TickDensity -> String -> SrcSpan -> FreeVars -> TM (Maybe (Tickish Id))
bindTick density name pos fvs = do
  decl_path <- getPathEntry
  let
      toplev        = null decl_path
      count_entries = toplev || density == TickAllFunctions
      top_only      = density /= TickAllFunctions
      box_label     = if toplev then TopLevelBox [name]
                                else LocalBox (decl_path ++ [name])
  --
  allocATickBox box_label count_entries top_only pos fvs


-- Note [inline sccs]
--
-- It should be reasonable to add ticks to INLINE functions; however
-- currently this tickles a bug later on because the SCCfinal pass
-- does not look inside unfoldings to find CostCentres.  It would be
-- difficult to fix that, because SCCfinal currently works on STG and
-- not Core (and since it also generates CostCentres for CAFs,
-- changing this would be difficult too).
--
-- Another reason not to add ticks to INLINE functions is that this
-- sometimes handy for avoiding adding a tick to a particular function
-- (see #6131)
--
-- So for now we do not add any ticks to INLINE functions at all.
--
-- We used to use isAnyInlinePragma to figure out whether to avoid adding
-- ticks for this purpose. However, #12962 indicates that this contradicts
-- the documentation on profiling (which only mentions INLINE pragmas).
-- So now we're more careful about what we avoid adding ticks to.

-- -----------------------------------------------------------------------------
-- Decorate an LHsExpr with ticks

-- selectively add ticks to interesting expressions
addTickLHsExpr :: LHsExpr GhcTc -> TM (LHsExpr GhcTc)
addTickLHsExpr e@(L pos e0) = do
  d <- getDensity
  case d of
    TickForBreakPoints | isGoodBreakExpr e0 -> tick_it
    TickForCoverage    -> tick_it
    TickCallSites      | isCallSite e0      -> tick_it
    _other             -> dont_tick_it
 where
   tick_it      = allocTickBox (ExpBox False) False False pos $ addTickHsExpr e0
   dont_tick_it = addTickLHsExprNever e

-- Add a tick to an expression which is the RHS of an equation or a binding.
-- We always consider these to be breakpoints, unless the expression is a 'let'
-- (because the body will definitely have a tick somewhere).  ToDo: perhaps
-- we should treat 'case' and 'if' the same way?
addTickLHsExprRHS :: LHsExpr GhcTc -> TM (LHsExpr GhcTc)
addTickLHsExprRHS e@(L pos e0) = do
  d <- getDensity
  case d of
     TickForBreakPoints | HsLet{} <- e0 -> dont_tick_it
                        | otherwise     -> tick_it
     TickForCoverage -> tick_it
     TickCallSites   | isCallSite e0 -> tick_it
     _other          -> dont_tick_it
 where
   tick_it      = allocTickBox (ExpBox False) False False pos $ addTickHsExpr e0
   dont_tick_it = addTickLHsExprNever e

-- The inner expression of an evaluation context:
--    let binds in [], ( [] )
-- we never tick these if we're doing HPC, but otherwise
-- we treat it like an ordinary expression.
addTickLHsExprEvalInner :: LHsExpr GhcTc -> TM (LHsExpr GhcTc)
addTickLHsExprEvalInner e = do
   d <- getDensity
   case d of
     TickForCoverage -> addTickLHsExprNever e
     _otherwise      -> addTickLHsExpr e

-- | A let body is treated differently from addTickLHsExprEvalInner
-- above with TickForBreakPoints, because for breakpoints we always
-- want to tick the body, even if it is not a redex.  See test
-- break012.  This gives the user the opportunity to inspect the
-- values of the let-bound variables.
addTickLHsExprLetBody :: LHsExpr GhcTc -> TM (LHsExpr GhcTc)
addTickLHsExprLetBody e@(L pos e0) = do
  d <- getDensity
  case d of
     TickForBreakPoints | HsLet{} <- e0 -> dont_tick_it
                        | otherwise     -> tick_it
     _other -> addTickLHsExprEvalInner e
 where
   tick_it      = allocTickBox (ExpBox False) False False pos $ addTickHsExpr e0
   dont_tick_it = addTickLHsExprNever e

-- version of addTick that does not actually add a tick,
-- because the scope of this tick is completely subsumed by
-- another.
addTickLHsExprNever :: LHsExpr GhcTc -> TM (LHsExpr GhcTc)
addTickLHsExprNever (L pos e0) = do
    e1 <- addTickHsExpr e0
    return $ L pos e1

-- general heuristic: expressions which do not denote values are good
-- break points
isGoodBreakExpr :: HsExpr GhcTc -> Bool
isGoodBreakExpr (HsApp {})        = True
isGoodBreakExpr (HsAppTypeOut {}) = True
isGoodBreakExpr (OpApp {})        = True
isGoodBreakExpr _other            = False

isCallSite :: HsExpr GhcTc -> Bool
isCallSite HsApp{}        = True
isCallSite HsAppTypeOut{} = True
isCallSite OpApp{}        = True
isCallSite _ = False

addTickLHsExprOptAlt :: Bool -> LHsExpr GhcTc -> TM (LHsExpr GhcTc)
addTickLHsExprOptAlt oneOfMany (L pos e0)
  = ifDensity TickForCoverage
        (allocTickBox (ExpBox oneOfMany) False False pos $ addTickHsExpr e0)
        (addTickLHsExpr (L pos e0))

addBinTickLHsExpr :: (Bool -> BoxLabel) -> LHsExpr GhcTc -> TM (LHsExpr GhcTc)
addBinTickLHsExpr boxLabel (L pos e0)
  = ifDensity TickForCoverage
        (allocBinTickBox boxLabel pos $ addTickHsExpr e0)
        (addTickLHsExpr (L pos e0))


-- -----------------------------------------------------------------------------
-- Decorate the body of an HsExpr with ticks.
-- (Whether to put a tick around the whole expression was already decided,
-- in the addTickLHsExpr family of functions.)

addTickHsExpr :: HsExpr GhcTc -> TM (HsExpr GhcTc)
addTickHsExpr e@(HsVar (L _ id)) = do freeVar id; return e
addTickHsExpr (HsUnboundVar {})  = panic "addTickHsExpr.HsUnboundVar"
addTickHsExpr e@(HsConLikeOut con)
  | Just id <- conLikeWrapId_maybe con = do freeVar id; return e
addTickHsExpr e@(HsIPVar _)      = return e
addTickHsExpr e@(HsOverLit _)    = return e
addTickHsExpr e@(HsOverLabel{})  = return e
addTickHsExpr e@(HsLit _)        = return e
addTickHsExpr (HsLam matchgroup) = liftM HsLam (addTickMatchGroup True matchgroup)
addTickHsExpr (HsLamCase mgs)    = liftM HsLamCase (addTickMatchGroup True mgs)
addTickHsExpr (HsApp e1 e2)      = liftM2 HsApp (addTickLHsExprNever e1)
                                                (addTickLHsExpr      e2)
addTickHsExpr (HsAppTypeOut e ty) = liftM2 HsAppTypeOut (addTickLHsExprNever e)
                                                        (return ty)

addTickHsExpr (OpApp e1 e2 fix e3) =
        liftM4 OpApp
                (addTickLHsExpr e1)
                (addTickLHsExprNever e2)
                (return fix)
                (addTickLHsExpr e3)
addTickHsExpr (NegApp e neg) =
        liftM2 NegApp
                (addTickLHsExpr e)
                (addTickSyntaxExpr hpcSrcSpan neg)
addTickHsExpr (HsPar e) =
        liftM HsPar (addTickLHsExprEvalInner e)
addTickHsExpr (SectionL e1 e2) =
        liftM2 SectionL
                (addTickLHsExpr e1)
                (addTickLHsExprNever e2)
addTickHsExpr (SectionR e1 e2) =
        liftM2 SectionR
                (addTickLHsExprNever e1)
                (addTickLHsExpr e2)
addTickHsExpr (ExplicitTuple es boxity) =
        liftM2 ExplicitTuple
                (mapM addTickTupArg es)
                (return boxity)
addTickHsExpr (ExplicitSum tag arity e ty) = do
        e' <- addTickLHsExpr e
        return (ExplicitSum tag arity e' ty)
addTickHsExpr (HsCase e mgs) =
        liftM2 HsCase
                (addTickLHsExpr e) -- not an EvalInner; e might not necessarily
                                   -- be evaluated.
                (addTickMatchGroup False mgs)
addTickHsExpr (HsIf cnd e1 e2 e3) =
        liftM3 (HsIf cnd)
                (addBinTickLHsExpr (BinBox CondBinBox) e1)
                (addTickLHsExprOptAlt True e2)
                (addTickLHsExprOptAlt True e3)
addTickHsExpr (HsMultiIf ty alts)
  = do { let isOneOfMany = case alts of [_] -> False; _ -> True
       ; alts' <- mapM (liftL $ addTickGRHS isOneOfMany False) alts
       ; return $ HsMultiIf ty alts' }
addTickHsExpr (HsLet (L l binds) e) =
        bindLocals (collectLocalBinders binds) $
          liftM2 (HsLet . L l)
                  (addTickHsLocalBinds binds) -- to think about: !patterns.
                  (addTickLHsExprLetBody e)
addTickHsExpr (HsDo cxt (L l stmts) srcloc)
  = do { (stmts', _) <- addTickLStmts' forQual stmts (return ())
       ; return (HsDo cxt (L l stmts') srcloc) }
  where
        forQual = case cxt of
                    ListComp -> Just $ BinBox QualBinBox
                    _        -> Nothing
addTickHsExpr (ExplicitList ty wit es) =
        liftM3 ExplicitList
                (return ty)
                (addTickWit wit)
                (mapM (addTickLHsExpr) es)
             where addTickWit Nothing = return Nothing
                   addTickWit (Just fln)
                     = do fln' <- addTickSyntaxExpr hpcSrcSpan fln
                          return (Just fln')
addTickHsExpr (ExplicitPArr ty es) =
        liftM2 ExplicitPArr
                (return ty)
                (mapM (addTickLHsExpr) es)

addTickHsExpr (HsStatic fvs e) = HsStatic fvs <$> addTickLHsExpr e

addTickHsExpr expr@(RecordCon { rcon_flds = rec_binds })
  = do { rec_binds' <- addTickHsRecordBinds rec_binds
       ; return (expr { rcon_flds = rec_binds' }) }

addTickHsExpr expr@(RecordUpd { rupd_expr = e, rupd_flds = flds })
  = do { e' <- addTickLHsExpr e
       ; flds' <- mapM addTickHsRecField flds
       ; return (expr { rupd_expr = e', rupd_flds = flds' }) }

addTickHsExpr (ExprWithTySig e ty) =
        liftM2 ExprWithTySig
                (addTickLHsExprNever e) -- No need to tick the inner expression
                                    -- for expressions with signatures
                (return ty)
addTickHsExpr (ArithSeq  ty wit arith_seq) =
        liftM3 ArithSeq
                (return ty)
                (addTickWit wit)
                (addTickArithSeqInfo arith_seq)
             where addTickWit Nothing = return Nothing
                   addTickWit (Just fl) = do fl' <- addTickSyntaxExpr hpcSrcSpan fl
                                             return (Just fl')

-- We might encounter existing ticks (multiple Coverage passes)
addTickHsExpr (HsTick t e) =
        liftM (HsTick t) (addTickLHsExprNever e)
addTickHsExpr (HsBinTick t0 t1 e) =
        liftM (HsBinTick t0 t1) (addTickLHsExprNever e)

addTickHsExpr (HsTickPragma _ _ _ (L pos e0)) = do
    e2 <- allocTickBox (ExpBox False) False False pos $
                addTickHsExpr e0
    return $ unLoc e2
addTickHsExpr (PArrSeq   ty arith_seq) =
        liftM2 PArrSeq
                (return ty)
                (addTickArithSeqInfo arith_seq)
addTickHsExpr (HsSCC src nm e) =
        liftM3 HsSCC
                (return src)
                (return nm)
                (addTickLHsExpr e)
addTickHsExpr (HsCoreAnn src nm e) =
        liftM3 HsCoreAnn
                (return src)
                (return nm)
                (addTickLHsExpr e)
addTickHsExpr e@(HsBracket     {})   = return e
addTickHsExpr e@(HsTcBracketOut  {}) = return e
addTickHsExpr e@(HsRnBracketOut  {}) = return e
addTickHsExpr e@(HsSpliceE  {})      = return e
addTickHsExpr (HsProc pat cmdtop) =
        liftM2 HsProc
                (addTickLPat pat)
                (liftL (addTickHsCmdTop) cmdtop)
addTickHsExpr (HsWrap w e) =
        liftM2 HsWrap
                (return w)
                (addTickHsExpr e)       -- Explicitly no tick on inside

addTickHsExpr (ExprWithTySigOut e ty) =
        liftM2 ExprWithTySigOut
               (addTickLHsExprNever e) -- No need to tick the inner expression
               (return ty)             -- for expressions with signatures

-- Others should never happen in expression content.
addTickHsExpr e  = pprPanic "addTickHsExpr" (ppr e)

addTickTupArg :: LHsTupArg GhcTc -> TM (LHsTupArg GhcTc)
addTickTupArg (L l (Present e))  = do { e' <- addTickLHsExpr e
                                      ; return (L l (Present e')) }
addTickTupArg (L l (Missing ty)) = return (L l (Missing ty))

addTickMatchGroup :: Bool{-is lambda-} -> MatchGroup GhcTc (LHsExpr GhcTc)
                  -> TM (MatchGroup GhcTc (LHsExpr GhcTc))
addTickMatchGroup is_lam mg@(MG { mg_alts = L l matches }) = do
  let isOneOfMany = matchesOneOfMany matches
  matches' <- mapM (liftL (addTickMatch isOneOfMany is_lam)) matches
  return $ mg { mg_alts = L l matches' }

addTickMatch :: Bool -> Bool -> Match GhcTc (LHsExpr GhcTc)
             -> TM (Match GhcTc (LHsExpr GhcTc))
addTickMatch isOneOfMany isLambda match@(Match { m_pats = pats, m_grhss = gRHSs }) =
  bindLocals (collectPatsBinders pats) $ do
    gRHSs' <- addTickGRHSs isOneOfMany isLambda gRHSs
    return $ match { m_grhss = gRHSs' }

addTickGRHSs :: Bool -> Bool -> GRHSs GhcTc (LHsExpr GhcTc)
             -> TM (GRHSs GhcTc (LHsExpr GhcTc))
addTickGRHSs isOneOfMany isLambda (GRHSs guarded (L l local_binds)) = do
  bindLocals binders $ do
    local_binds' <- addTickHsLocalBinds local_binds
    guarded' <- mapM (liftL (addTickGRHS isOneOfMany isLambda)) guarded
    return $ GRHSs guarded' (L l local_binds')
  where
    binders = collectLocalBinders local_binds

addTickGRHS :: Bool -> Bool -> GRHS GhcTc (LHsExpr GhcTc)
            -> TM (GRHS GhcTc (LHsExpr GhcTc))
addTickGRHS isOneOfMany isLambda (GRHS stmts expr) = do
  (stmts',expr') <- addTickLStmts' (Just $ BinBox $ GuardBinBox) stmts
                        (addTickGRHSBody isOneOfMany isLambda expr)
  return $ GRHS stmts' expr'

addTickGRHSBody :: Bool -> Bool -> LHsExpr GhcTc -> TM (LHsExpr GhcTc)
addTickGRHSBody isOneOfMany isLambda expr@(L pos e0) = do
  d <- getDensity
  case d of
    TickForCoverage  -> addTickLHsExprOptAlt isOneOfMany expr
    TickAllFunctions | isLambda ->
       addPathEntry "\\" $
         allocTickBox (ExpBox False) True{-count-} False{-not top-} pos $
           addTickHsExpr e0
    _otherwise ->
       addTickLHsExprRHS expr

addTickLStmts :: (Maybe (Bool -> BoxLabel)) -> [ExprLStmt GhcTc]
              -> TM [ExprLStmt GhcTc]
addTickLStmts isGuard stmts = do
  (stmts, _) <- addTickLStmts' isGuard stmts (return ())
  return stmts

addTickLStmts' :: (Maybe (Bool -> BoxLabel)) -> [ExprLStmt GhcTc] -> TM a
               -> TM ([ExprLStmt GhcTc], a)
addTickLStmts' isGuard lstmts res
  = bindLocals (collectLStmtsBinders lstmts) $
    do { lstmts' <- mapM (liftL (addTickStmt isGuard)) lstmts
       ; a <- res
       ; return (lstmts', a) }

addTickStmt :: (Maybe (Bool -> BoxLabel)) -> Stmt GhcTc (LHsExpr GhcTc)
            -> TM (Stmt GhcTc (LHsExpr GhcTc))
addTickStmt _isGuard (LastStmt e noret ret) = do
        liftM3 LastStmt
                (addTickLHsExpr e)
                (pure noret)
                (addTickSyntaxExpr hpcSrcSpan ret)
addTickStmt _isGuard (BindStmt pat e bind fail ty) = do
        liftM5 BindStmt
                (addTickLPat pat)
                (addTickLHsExprRHS e)
                (addTickSyntaxExpr hpcSrcSpan bind)
                (addTickSyntaxExpr hpcSrcSpan fail)
                (return ty)
addTickStmt isGuard (BodyStmt e bind' guard' ty) = do
        liftM4 BodyStmt
                (addTick isGuard e)
                (addTickSyntaxExpr hpcSrcSpan bind')
                (addTickSyntaxExpr hpcSrcSpan guard')
                (return ty)
addTickStmt _isGuard (LetStmt (L l binds)) = do
        liftM (LetStmt . L l)
                (addTickHsLocalBinds binds)
addTickStmt isGuard (ParStmt pairs mzipExpr bindExpr ty) = do
    liftM4 ParStmt
        (mapM (addTickStmtAndBinders isGuard) pairs)
        (unLoc <$> addTickLHsExpr (L hpcSrcSpan mzipExpr))
        (addTickSyntaxExpr hpcSrcSpan bindExpr)
        (return ty)
addTickStmt isGuard (ApplicativeStmt args mb_join body_ty) = do
    args' <- mapM (addTickApplicativeArg isGuard) args
    return (ApplicativeStmt args' mb_join body_ty)

addTickStmt isGuard stmt@(TransStmt { trS_stmts = stmts
                                    , trS_by = by, trS_using = using
                                    , trS_ret = returnExpr, trS_bind = bindExpr
                                    , trS_fmap = liftMExpr }) = do
    t_s <- addTickLStmts isGuard stmts
    t_y <- fmapMaybeM  addTickLHsExprRHS by
    t_u <- addTickLHsExprRHS using
    t_f <- addTickSyntaxExpr hpcSrcSpan returnExpr
    t_b <- addTickSyntaxExpr hpcSrcSpan bindExpr
    L _ t_m <- addTickLHsExpr (L hpcSrcSpan liftMExpr)
    return $ stmt { trS_stmts = t_s, trS_by = t_y, trS_using = t_u
                  , trS_ret = t_f, trS_bind = t_b, trS_fmap = t_m }

addTickStmt isGuard stmt@(RecStmt {})
  = do { stmts' <- addTickLStmts isGuard (recS_stmts stmt)
       ; ret'   <- addTickSyntaxExpr hpcSrcSpan (recS_ret_fn stmt)
       ; mfix'  <- addTickSyntaxExpr hpcSrcSpan (recS_mfix_fn stmt)
       ; bind'  <- addTickSyntaxExpr hpcSrcSpan (recS_bind_fn stmt)
       ; return (stmt { recS_stmts = stmts', recS_ret_fn = ret'
                      , recS_mfix_fn = mfix', recS_bind_fn = bind' }) }

addTick :: Maybe (Bool -> BoxLabel) -> LHsExpr GhcTc -> TM (LHsExpr GhcTc)
addTick isGuard e | Just fn <- isGuard = addBinTickLHsExpr fn e
                  | otherwise          = addTickLHsExprRHS e

addTickApplicativeArg
  :: Maybe (Bool -> BoxLabel) -> (SyntaxExpr GhcTc, ApplicativeArg GhcTc GhcTc)
  -> TM (SyntaxExpr GhcTc, ApplicativeArg GhcTc GhcTc)
addTickApplicativeArg isGuard (op, arg) =
  liftM2 (,) (addTickSyntaxExpr hpcSrcSpan op) (addTickArg arg)
 where
  addTickArg (ApplicativeArgOne pat expr isBody) =
    ApplicativeArgOne
      <$> addTickLPat pat
      <*> addTickLHsExpr expr
      <*> pure isBody
  addTickArg (ApplicativeArgMany stmts ret pat) =
    ApplicativeArgMany
      <$> addTickLStmts isGuard stmts
      <*> (unLoc <$> addTickLHsExpr (L hpcSrcSpan ret))
      <*> addTickLPat pat

addTickStmtAndBinders :: Maybe (Bool -> BoxLabel) -> ParStmtBlock GhcTc GhcTc
                      -> TM (ParStmtBlock GhcTc GhcTc)
addTickStmtAndBinders isGuard (ParStmtBlock stmts ids returnExpr) =
    liftM3 ParStmtBlock
        (addTickLStmts isGuard stmts)
        (return ids)
        (addTickSyntaxExpr hpcSrcSpan returnExpr)

addTickHsLocalBinds :: HsLocalBinds GhcTc -> TM (HsLocalBinds GhcTc)
addTickHsLocalBinds (HsValBinds binds) =
        liftM HsValBinds
                (addTickHsValBinds binds)
addTickHsLocalBinds (HsIPBinds binds)  =
        liftM HsIPBinds
                (addTickHsIPBinds binds)
addTickHsLocalBinds (EmptyLocalBinds)  = return EmptyLocalBinds

addTickHsValBinds :: HsValBindsLR GhcTc a -> TM (HsValBindsLR GhcTc b)
addTickHsValBinds (ValBindsOut binds sigs) =
        liftM2 ValBindsOut
                (mapM (\ (rec,binds') ->
                                liftM2 (,)
                                        (return rec)
                                        (addTickLHsBinds binds'))
                        binds)
                (return sigs)
addTickHsValBinds _ = panic "addTickHsValBinds"

addTickHsIPBinds :: HsIPBinds GhcTc -> TM (HsIPBinds GhcTc)
addTickHsIPBinds (IPBinds ipbinds dictbinds) =
        liftM2 IPBinds
                (mapM (liftL (addTickIPBind)) ipbinds)
                (return dictbinds)

addTickIPBind :: IPBind GhcTc -> TM (IPBind GhcTc)
addTickIPBind (IPBind nm e) =
        liftM2 IPBind
                (return nm)
                (addTickLHsExpr e)

-- There is no location here, so we might need to use a context location??
addTickSyntaxExpr :: SrcSpan -> SyntaxExpr GhcTc -> TM (SyntaxExpr GhcTc)
addTickSyntaxExpr pos syn@(SyntaxExpr { syn_expr = x }) = do
        L _ x' <- addTickLHsExpr (L pos x)
        return $ syn { syn_expr = x' }
-- we do not walk into patterns.
addTickLPat :: LPat GhcTc -> TM (LPat GhcTc)
addTickLPat pat = return pat

addTickHsCmdTop :: HsCmdTop GhcTc -> TM (HsCmdTop GhcTc)
addTickHsCmdTop (HsCmdTop cmd tys ty syntaxtable) =
        liftM4 HsCmdTop
                (addTickLHsCmd cmd)
                (return tys)
                (return ty)
                (return syntaxtable)

addTickLHsCmd ::  LHsCmd GhcTc -> TM (LHsCmd GhcTc)
addTickLHsCmd (L pos c0) = do
        c1 <- addTickHsCmd c0
        return $ L pos c1

addTickHsCmd :: HsCmd GhcTc -> TM (HsCmd GhcTc)
addTickHsCmd (HsCmdLam matchgroup) =
        liftM HsCmdLam (addTickCmdMatchGroup matchgroup)
addTickHsCmd (HsCmdApp c e) =
        liftM2 HsCmdApp (addTickLHsCmd c) (addTickLHsExpr e)
{-
addTickHsCmd (OpApp e1 c2 fix c3) =
        liftM4 OpApp
                (addTickLHsExpr e1)
                (addTickLHsCmd c2)
                (return fix)
                (addTickLHsCmd c3)
-}
addTickHsCmd (HsCmdPar e) = liftM HsCmdPar (addTickLHsCmd e)
addTickHsCmd (HsCmdCase e mgs) =
        liftM2 HsCmdCase
                (addTickLHsExpr e)
                (addTickCmdMatchGroup mgs)
addTickHsCmd (HsCmdIf cnd e1 c2 c3) =
        liftM3 (HsCmdIf cnd)
                (addBinTickLHsExpr (BinBox CondBinBox) e1)
                (addTickLHsCmd c2)
                (addTickLHsCmd c3)
addTickHsCmd (HsCmdLet (L l binds) c) =
        bindLocals (collectLocalBinders binds) $
          liftM2 (HsCmdLet . L l)
                   (addTickHsLocalBinds binds) -- to think about: !patterns.
                   (addTickLHsCmd c)
addTickHsCmd (HsCmdDo (L l stmts) srcloc)
  = do { (stmts', _) <- addTickLCmdStmts' stmts (return ())
       ; return (HsCmdDo (L l stmts') srcloc) }

addTickHsCmd (HsCmdArrApp   e1 e2 ty1 arr_ty lr) =
        liftM5 HsCmdArrApp
               (addTickLHsExpr e1)
               (addTickLHsExpr e2)
               (return ty1)
               (return arr_ty)
               (return lr)
addTickHsCmd (HsCmdArrForm e f fix cmdtop) =
        liftM4 HsCmdArrForm
               (addTickLHsExpr e)
               (return f)
               (return fix)
               (mapM (liftL (addTickHsCmdTop)) cmdtop)

addTickHsCmd (HsCmdWrap w cmd)
  = liftM2 HsCmdWrap (return w) (addTickHsCmd cmd)

-- Others should never happen in a command context.
--addTickHsCmd e  = pprPanic "addTickHsCmd" (ppr e)

addTickCmdMatchGroup :: MatchGroup GhcTc (LHsCmd GhcTc)
                     -> TM (MatchGroup GhcTc (LHsCmd GhcTc))
addTickCmdMatchGroup mg@(MG { mg_alts = L l matches }) = do
  matches' <- mapM (liftL addTickCmdMatch) matches
  return $ mg { mg_alts = L l matches' }

addTickCmdMatch :: Match GhcTc (LHsCmd GhcTc) -> TM (Match GhcTc (LHsCmd GhcTc))
addTickCmdMatch match@(Match { m_pats = pats, m_grhss = gRHSs }) =
  bindLocals (collectPatsBinders pats) $ do
    gRHSs' <- addTickCmdGRHSs gRHSs
    return $ match { m_grhss = gRHSs' }

addTickCmdGRHSs :: GRHSs GhcTc (LHsCmd GhcTc) -> TM (GRHSs GhcTc (LHsCmd GhcTc))
addTickCmdGRHSs (GRHSs guarded (L l local_binds)) = do
  bindLocals binders $ do
    local_binds' <- addTickHsLocalBinds local_binds
    guarded' <- mapM (liftL addTickCmdGRHS) guarded
    return $ GRHSs guarded' (L l local_binds')
  where
    binders = collectLocalBinders local_binds

addTickCmdGRHS :: GRHS GhcTc (LHsCmd GhcTc) -> TM (GRHS GhcTc (LHsCmd GhcTc))
-- The *guards* are *not* Cmds, although the body is
-- C.f. addTickGRHS for the BinBox stuff
addTickCmdGRHS (GRHS stmts cmd)
  = do { (stmts',expr') <- addTickLStmts' (Just $ BinBox $ GuardBinBox)
                                   stmts (addTickLHsCmd cmd)
       ; return $ GRHS stmts' expr' }

addTickLCmdStmts :: [LStmt GhcTc (LHsCmd GhcTc)]
                 -> TM [LStmt GhcTc (LHsCmd GhcTc)]
addTickLCmdStmts stmts = do
  (stmts, _) <- addTickLCmdStmts' stmts (return ())
  return stmts

addTickLCmdStmts' :: [LStmt GhcTc (LHsCmd GhcTc)] -> TM a
                  -> TM ([LStmt GhcTc (LHsCmd GhcTc)], a)
addTickLCmdStmts' lstmts res
  = bindLocals binders $ do
        lstmts' <- mapM (liftL addTickCmdStmt) lstmts
        a <- res
        return (lstmts', a)
  where
        binders = collectLStmtsBinders lstmts

addTickCmdStmt :: Stmt GhcTc (LHsCmd GhcTc) -> TM (Stmt GhcTc (LHsCmd GhcTc))
addTickCmdStmt (BindStmt pat c bind fail ty) = do
        liftM5 BindStmt
                (addTickLPat pat)
                (addTickLHsCmd c)
                (return bind)
                (return fail)
                (return ty)
addTickCmdStmt (LastStmt c noret ret) = do
        liftM3 LastStmt
                (addTickLHsCmd c)
                (pure noret)
                (addTickSyntaxExpr hpcSrcSpan ret)
addTickCmdStmt (BodyStmt c bind' guard' ty) = do
        liftM4 BodyStmt
                (addTickLHsCmd c)
                (addTickSyntaxExpr hpcSrcSpan bind')
                (addTickSyntaxExpr hpcSrcSpan guard')
                (return ty)
addTickCmdStmt (LetStmt (L l binds)) = do
        liftM (LetStmt . L l)
                (addTickHsLocalBinds binds)
addTickCmdStmt stmt@(RecStmt {})
  = do { stmts' <- addTickLCmdStmts (recS_stmts stmt)
       ; ret'   <- addTickSyntaxExpr hpcSrcSpan (recS_ret_fn stmt)
       ; mfix'  <- addTickSyntaxExpr hpcSrcSpan (recS_mfix_fn stmt)
       ; bind'  <- addTickSyntaxExpr hpcSrcSpan (recS_bind_fn stmt)
       ; return (stmt { recS_stmts = stmts', recS_ret_fn = ret'
                      , recS_mfix_fn = mfix', recS_bind_fn = bind' }) }
addTickCmdStmt ApplicativeStmt{} =
  panic "ToDo: addTickCmdStmt ApplicativeLastStmt"

-- Others should never happen in a command context.
addTickCmdStmt stmt  = pprPanic "addTickHsCmd" (ppr stmt)

addTickHsRecordBinds :: HsRecordBinds GhcTc -> TM (HsRecordBinds GhcTc)
addTickHsRecordBinds (HsRecFields fields dd)
  = do  { fields' <- mapM addTickHsRecField fields
        ; return (HsRecFields fields' dd) }

addTickHsRecField :: LHsRecField' id (LHsExpr GhcTc)
                  -> TM (LHsRecField' id (LHsExpr GhcTc))
addTickHsRecField (L l (HsRecField id expr pun))
        = do { expr' <- addTickLHsExpr expr
             ; return (L l (HsRecField id expr' pun)) }


addTickArithSeqInfo :: ArithSeqInfo GhcTc -> TM (ArithSeqInfo GhcTc)
addTickArithSeqInfo (From e1) =
        liftM From
                (addTickLHsExpr e1)
addTickArithSeqInfo (FromThen e1 e2) =
        liftM2 FromThen
                (addTickLHsExpr e1)
                (addTickLHsExpr e2)
addTickArithSeqInfo (FromTo e1 e2) =
        liftM2 FromTo
                (addTickLHsExpr e1)
                (addTickLHsExpr e2)
addTickArithSeqInfo (FromThenTo e1 e2 e3) =
        liftM3 FromThenTo
                (addTickLHsExpr e1)
                (addTickLHsExpr e2)
                (addTickLHsExpr e3)

liftL :: (Monad m) => (a -> m a) -> Located a -> m (Located a)
liftL f (L loc a) = do
  a' <- f a
  return $ L loc a'

data TickTransState = TT { tickBoxCount:: Int
                         , mixEntries  :: [MixEntry_]
                         , uniqSupply  :: UniqSupply
                         }

data TickTransEnv = TTE { fileName     :: FastString
                        , density      :: TickDensity
                        , tte_dflags   :: DynFlags
                        , exports      :: NameSet
                        , inlines      :: VarSet
                        , declPath     :: [String]
                        , inScope      :: VarSet
                        , blackList    :: Map SrcSpan ()
                        , this_mod     :: Module
                        , tickishType  :: TickishType
                        }

--      deriving Show

data TickishType = ProfNotes | HpcTicks | Breakpoints | SourceNotes
                 deriving (Eq)

coveragePasses :: DynFlags -> [TickishType]
coveragePasses dflags =
    ifa (hscTarget dflags == HscInterpreted) Breakpoints $
    ifa (gopt Opt_Hpc dflags)                HpcTicks $
    ifa (gopt Opt_SccProfilingOn dflags &&
         profAuto dflags /= NoProfAuto)      ProfNotes $
    ifa (debugLevel dflags > 0)              SourceNotes []
  where ifa f x xs | f         = x:xs
                   | otherwise = xs

-- | Tickishs that only make sense when their source code location
-- refers to the current file. This might not always be true due to
-- LINE pragmas in the code - which would confuse at least HPC.
tickSameFileOnly :: TickishType -> Bool
tickSameFileOnly HpcTicks = True
tickSameFileOnly _other   = False

type FreeVars = OccEnv Id
noFVs :: FreeVars
noFVs = emptyOccEnv

-- Note [freevars]
--   For breakpoints we want to collect the free variables of an
--   expression for pinning on the HsTick.  We don't want to collect
--   *all* free variables though: in particular there's no point pinning
--   on free variables that are will otherwise be in scope at the GHCi
--   prompt, which means all top-level bindings.  Unfortunately detecting
--   top-level bindings isn't easy (collectHsBindsBinders on the top-level
--   bindings doesn't do it), so we keep track of a set of "in-scope"
--   variables in addition to the free variables, and the former is used
--   to filter additions to the latter.  This gives us complete control
--   over what free variables we track.

data TM a = TM { unTM :: TickTransEnv -> TickTransState -> (a,FreeVars,TickTransState) }
        -- a combination of a state monad (TickTransState) and a writer
        -- monad (FreeVars).

instance Functor TM where
    fmap = liftM

instance Applicative TM where
    pure a = TM $ \ _env st -> (a,noFVs,st)
    (<*>) = ap

instance Monad TM where
  (TM m) >>= k = TM $ \ env st ->
                                case m env st of
                                  (r1,fv1,st1) ->
                                     case unTM (k r1) env st1 of
                                       (r2,fv2,st2) ->
                                          (r2, fv1 `plusOccEnv` fv2, st2)

instance HasDynFlags TM where
  getDynFlags = TM $ \ env st -> (tte_dflags env, noFVs, st)

instance MonadUnique TM where
  getUniqueSupplyM = TM $ \_ st -> (uniqSupply st, noFVs, st)
  getUniqueM = TM $ \_ st -> let (u, us') = takeUniqFromSupply (uniqSupply st)
                             in (u, noFVs, st { uniqSupply = us' })

getState :: TM TickTransState
getState = TM $ \ _ st -> (st, noFVs, st)

setState :: (TickTransState -> TickTransState) -> TM ()
setState f = TM $ \ _ st -> ((), noFVs, f st)

getEnv :: TM TickTransEnv
getEnv = TM $ \ env st -> (env, noFVs, st)

withEnv :: (TickTransEnv -> TickTransEnv) -> TM a -> TM a
withEnv f (TM m) = TM $ \ env st ->
                                 case m (f env) st of
                                   (a, fvs, st') -> (a, fvs, st')

getDensity :: TM TickDensity
getDensity = TM $ \env st -> (density env, noFVs, st)

ifDensity :: TickDensity -> TM a -> TM a -> TM a
ifDensity d th el = do d0 <- getDensity; if d == d0 then th else el

getFreeVars :: TM a -> TM (FreeVars, a)
getFreeVars (TM m)
  = TM $ \ env st -> case m env st of (a, fv, st') -> ((fv,a), fv, st')

freeVar :: Id -> TM ()
freeVar id = TM $ \ env st ->
                if id `elemVarSet` inScope env
                   then ((), unitOccEnv (nameOccName (idName id)) id, st)
                   else ((), noFVs, st)

addPathEntry :: String -> TM a -> TM a
addPathEntry nm = withEnv (\ env -> env { declPath = declPath env ++ [nm] })

getPathEntry :: TM [String]
getPathEntry = declPath `liftM` getEnv

getFileName :: TM FastString
getFileName = fileName `liftM` getEnv

isGoodSrcSpan' :: SrcSpan -> Bool
isGoodSrcSpan' pos@(RealSrcSpan _) = srcSpanStart pos /= srcSpanEnd pos
isGoodSrcSpan' (UnhelpfulSpan _) = False

isGoodTickSrcSpan :: SrcSpan -> TM Bool
isGoodTickSrcSpan pos = do
  file_name <- getFileName
  tickish <- tickishType `liftM` getEnv
  let need_same_file = tickSameFileOnly tickish
      same_file      = Just file_name == srcSpanFileName_maybe pos
  return (isGoodSrcSpan' pos && (not need_same_file || same_file))

ifGoodTickSrcSpan :: SrcSpan -> TM a -> TM a -> TM a
ifGoodTickSrcSpan pos then_code else_code = do
  good <- isGoodTickSrcSpan pos
  if good then then_code else else_code

bindLocals :: [Id] -> TM a -> TM a
bindLocals new_ids (TM m)
  = TM $ \ env st ->
                 case m env{ inScope = inScope env `extendVarSetList` new_ids } st of
                   (r, fv, st') -> (r, fv `delListFromOccEnv` occs, st')
  where occs = [ nameOccName (idName id) | id <- new_ids ]

isBlackListed :: SrcSpan -> TM Bool
isBlackListed pos = TM $ \ env st ->
              case Map.lookup pos (blackList env) of
                Nothing -> (False,noFVs,st)
                Just () -> (True,noFVs,st)

-- the tick application inherits the source position of its
-- expression argument to support nested box allocations
allocTickBox :: BoxLabel -> Bool -> Bool -> SrcSpan -> TM (HsExpr GhcTc)
             -> TM (LHsExpr GhcTc)
allocTickBox boxLabel countEntries topOnly pos m =
  ifGoodTickSrcSpan pos (do
    (fvs, e) <- getFreeVars m
    env <- getEnv
    tickish <- mkTickish boxLabel countEntries topOnly pos fvs (declPath env)
    return (L pos (HsTick tickish (L pos e)))
  ) (do
    e <- m
    return (L pos e)
  )

-- the tick application inherits the source position of its
-- expression argument to support nested box allocations
allocATickBox :: BoxLabel -> Bool -> Bool -> SrcSpan -> FreeVars
              -> TM (Maybe (Tickish Id))
allocATickBox boxLabel countEntries topOnly  pos fvs =
  ifGoodTickSrcSpan pos (do
    let
      mydecl_path = case boxLabel of
                      TopLevelBox x -> x
                      LocalBox xs  -> xs
                      _ -> panic "allocATickBox"
    tickish <- mkTickish boxLabel countEntries topOnly pos fvs mydecl_path
    return (Just tickish)
  ) (return Nothing)


mkTickish :: BoxLabel -> Bool -> Bool -> SrcSpan -> OccEnv Id -> [String]
          -> TM (Tickish Id)
mkTickish boxLabel countEntries topOnly pos fvs decl_path = do

  let ids = filter (not . isUnliftedType . idType) $ occEnvElts fvs
          -- unlifted types cause two problems here:
          --   * we can't bind them  at the GHCi prompt
          --     (bindLocalsAtBreakpoint already fliters them out),
          --   * the simplifier might try to substitute a literal for
          --     the Id, and we can't handle that.

      me = (pos, decl_path, map (nameOccName.idName) ids, boxLabel)

      cc_name | topOnly   = head decl_path
              | otherwise = concat (intersperse "." decl_path)

  dflags <- getDynFlags
  env <- getEnv
  case tickishType env of
    HpcTicks -> do
      c <- liftM tickBoxCount getState
      setState $ \st -> st { tickBoxCount = c + 1
                           , mixEntries = me : mixEntries st }
      return $ HpcTick (this_mod env) c

    ProfNotes -> do
      ccUnique <- getUniqueM
      let cc = mkUserCC (mkFastString cc_name) (this_mod env) pos ccUnique
          count = countEntries && gopt Opt_ProfCountEntries dflags
      return $ ProfNote cc count True{-scopes-}

    Breakpoints -> do
      c <- liftM tickBoxCount getState
      setState $ \st -> st { tickBoxCount = c + 1
                           , mixEntries = me:mixEntries st }
      return $ Breakpoint c ids

    SourceNotes | RealSrcSpan pos' <- pos ->
      return $ SourceNote pos' cc_name

    _otherwise -> panic "mkTickish: bad source span!"


allocBinTickBox :: (Bool -> BoxLabel) -> SrcSpan -> TM (HsExpr GhcTc)
                -> TM (LHsExpr GhcTc)
allocBinTickBox boxLabel pos m = do
  env <- getEnv
  case tickishType env of
    HpcTicks -> do e <- liftM (L pos) m
                   ifGoodTickSrcSpan pos
                     (mkBinTickBoxHpc boxLabel pos e)
                     (return e)
    _other   -> allocTickBox (ExpBox False) False False pos m

mkBinTickBoxHpc :: (Bool -> BoxLabel) -> SrcSpan -> LHsExpr GhcTc
                -> TM (LHsExpr GhcTc)
mkBinTickBoxHpc boxLabel pos e =
 TM $ \ env st ->
  let meT = (pos,declPath env, [],boxLabel True)
      meF = (pos,declPath env, [],boxLabel False)
      meE = (pos,declPath env, [],ExpBox False)
      c = tickBoxCount st
      mes = mixEntries st
  in
             ( L pos $ HsTick (HpcTick (this_mod env) c) $ L pos $ HsBinTick (c+1) (c+2) e
           -- notice that F and T are reversed,
           -- because we are building the list in
           -- reverse...
             , noFVs
             , st {tickBoxCount=c+3 , mixEntries=meF:meT:meE:mes}
             )

mkHpcPos :: SrcSpan -> HpcPos
mkHpcPos pos@(RealSrcSpan s)
   | isGoodSrcSpan' pos = toHpcPos (srcSpanStartLine s,
                                    srcSpanStartCol s,
                                    srcSpanEndLine s,
                                    srcSpanEndCol s - 1)
                              -- the end column of a SrcSpan is one
                              -- greater than the last column of the
                              -- span (see SrcLoc), whereas HPC
                              -- expects to the column range to be
                              -- inclusive, hence we subtract one above.
mkHpcPos _ = panic "bad source span; expected such spans to be filtered out"

hpcSrcSpan :: SrcSpan
hpcSrcSpan = mkGeneralSrcSpan (fsLit "Haskell Program Coverage internals")

matchesOneOfMany :: [LMatch GhcTc body] -> Bool
matchesOneOfMany lmatches = sum (map matchCount lmatches) > 1
  where
        matchCount (L _ (Match { m_grhss = GRHSs grhss _binds })) = length grhss

type MixEntry_ = (SrcSpan, [String], [OccName], BoxLabel)

-- For the hash value, we hash everything: the file name,
--  the timestamp of the original source file, the tab stop,
--  and the mix entries. We cheat, and hash the show'd string.
-- This hash only has to be hashed at Mix creation time,
-- and is for sanity checking only.

mixHash :: FilePath -> UTCTime -> Int -> [MixEntry] -> Int
mixHash file tm tabstop entries = fromIntegral $ hashString
        (show $ Mix file tm 0 tabstop entries)

{-
************************************************************************
*                                                                      *
*              initialisation
*                                                                      *
************************************************************************

Each module compiled with -fhpc declares an initialisation function of
the form `hpc_init_<module>()`, which is emitted into the _stub.c file
and annotated with __attribute__((constructor)) so that it gets
executed at startup time.

The function's purpose is to call hs_hpc_module to register this
module with the RTS, and it looks something like this:

static void hpc_init_Main(void) __attribute__((constructor));
static void hpc_init_Main(void)
{extern StgWord64 _hpc_tickboxes_Main_hpc[];
 hs_hpc_module("Main",8,1150288664,_hpc_tickboxes_Main_hpc);}
-}

hpcInitCode :: Module -> HpcInfo -> SDoc
hpcInitCode _ (NoHpcInfo {}) = Outputable.empty
hpcInitCode this_mod (HpcInfo tickCount hashNo)
 = vcat
    [ text "static void hpc_init_" <> ppr this_mod
         <> text "(void) __attribute__((constructor));"
    , text "static void hpc_init_" <> ppr this_mod <> text "(void)"
    , braces (vcat [
        text "extern StgWord64 " <> tickboxes <>
               text "[]" <> semi,
        text "hs_hpc_module" <>
          parens (hcat (punctuate comma [
              doubleQuotes full_name_str,
              int tickCount, -- really StgWord32
              int hashNo,    -- really StgWord32
              tickboxes
            ])) <> semi
       ])
    ]
  where
    tickboxes = ppr (mkHpcTicksLabel $ this_mod)

    module_name  = hcat (map (text.charToC) $
                         bytesFS (moduleNameFS (Module.moduleName this_mod)))
    package_name = hcat (map (text.charToC) $
                         bytesFS (unitIdFS  (moduleUnitId this_mod)))
    full_name_str
       | moduleUnitId this_mod == mainUnitId
       = module_name
       | otherwise
       = package_name <> char '/' <> module_name
