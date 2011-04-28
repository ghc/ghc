%
% (c) Galois, 2006
% (c) University of Glasgow, 2007
%
\section[Coverage]{@coverage@: the main function}

\begin{code}
module Coverage (addCoverageTicksToBinds, hpcInitCode) where

import HsSyn
import Module
import Outputable
import DynFlags
import Control.Monad
import SrcLoc
import ErrUtils
import Name
import Bag
import Id
import VarSet
import Data.List
import FastString
import HscTypes	
import StaticFlags
import TyCon
import MonadUtils
import Maybes
import CLabel
import Util

import Data.Array
import System.Directory ( createDirectoryIfMissing )

import Trace.Hpc.Mix
import Trace.Hpc.Util

import BreakArray 
import Data.HashTable   ( hashString )
import Data.Map (Map)
import qualified Data.Map as Map
\end{code}


%************************************************************************
%*									*
%* 		The main function: addCoverageTicksToBinds
%*									*
%************************************************************************

\begin{code}
addCoverageTicksToBinds
        :: DynFlags
        -> Module
        -> ModLocation          -- of the current module
        -> [TyCon]		-- type constructor in this module
        -> LHsBinds Id
        -> IO (LHsBinds Id, HpcInfo, ModBreaks)

addCoverageTicksToBinds dflags mod mod_loc tyCons binds = 
 case ml_hs_file mod_loc of
 Nothing -> return (binds, emptyHpcInfo False, emptyModBreaks)
 Just orig_file -> do

  if "boot" `isSuffixOf` orig_file then return (binds, emptyHpcInfo False, emptyModBreaks) else do

  -- Now, we try look for a file generated from a .hsc file to a .hs file, by peeking ahead.

  let top_pos = catMaybes $ foldrBag (\ (L pos _) rest -> srcSpanFileName_maybe pos : rest) [] binds
  let orig_file2 = case top_pos of
		     (file_name:_) 
		       | ".hsc" `isSuffixOf` unpackFS file_name -> unpackFS file_name
		     _ -> orig_file

  let mod_name = moduleNameString (moduleName mod)

  let (binds1,_,st)
		 = unTM (addTickLHsBinds binds) 
		   (TTE
		       { fileName    = mkFastString orig_file2
		      , declPath     = []
                      , inScope      = emptyVarSet
		      , blackList    = Map.fromList [ (getSrcSpan (tyConName tyCon),()) 
		                                    | tyCon <- tyCons ]
		       })
		   (TT 
		      { tickBoxCount = 0
		      , mixEntries   = []
		      })

  let entries = reverse $ mixEntries st

  -- write the mix entries for this module
  hashNo <- if opt_Hpc then do
     let hpc_dir = hpcDir dflags

     let hpc_mod_dir = if modulePackageId mod == mainPackageId 
		       then hpc_dir
		       else hpc_dir ++ "/" ++ packageIdString (modulePackageId mod)

     let tabStop = 1 -- <tab> counts as a normal char in GHC's location ranges.
     createDirectoryIfMissing True hpc_mod_dir
     modTime <- getModificationTime orig_file2
     let entries' = [ (hpcPos, box) 
                    | (span,_,_,box) <- entries, hpcPos <- [mkHpcPos span] ]
     when (length entries' /= tickBoxCount st) $ do
       panic "the number of .mix entries are inconsistent"
     let hashNo = mixHash orig_file2 modTime tabStop entries'
     mixCreate hpc_mod_dir mod_name 
     	       $ Mix orig_file2 modTime (toHash hashNo) tabStop entries'
     return $ hashNo 
   else do
     return $ 0

  -- Todo: use proper src span type
  breakArray <- newBreakArray $ length entries

  let locsTicks = listArray (0,tickBoxCount st-1) 
                     [ span | (span,_,_,_) <- entries ]
      varsTicks = listArray (0,tickBoxCount st-1) 
                     [ vars | (_,_,vars,_) <- entries ]
      declsTicks= listArray (0,tickBoxCount st-1) 
                     [ decls | (_,decls,_,_) <- entries ]
      modBreaks = emptyModBreaks 
                  { modBreaks_flags = breakArray 
                  , modBreaks_locs  = locsTicks 
                  , modBreaks_vars  = varsTicks
                  , modBreaks_decls = declsTicks
                  } 

  doIfSet_dyn dflags  Opt_D_dump_hpc $ do
	  printDump (pprLHsBinds binds1)

  return (binds1, HpcInfo (tickBoxCount st) hashNo, modBreaks)
\end{code}


\begin{code}
liftL :: (Monad m) => (a -> m a) -> Located a -> m (Located a)
liftL f (L loc a) = do
  a' <- f a
  return $ L loc a'

addTickLHsBinds :: LHsBinds Id -> TM (LHsBinds Id)
addTickLHsBinds binds = mapBagM addTickLHsBind binds

addTickLHsBind :: LHsBind Id -> TM (LHsBind Id)
addTickLHsBind (L pos bind@(AbsBinds { abs_binds = binds })) = do
  binds' <- addTickLHsBinds binds
  return $ L pos $ bind { abs_binds = binds' }
addTickLHsBind (L pos (funBind@(FunBind { fun_id = (L _ id)  }))) = do 
  let name = getOccString id
  decl_path <- getPathEntry

  (fvs, (MatchGroup matches' ty)) <- 
        getFreeVars $
        addPathEntry name $
        addTickMatchGroup (fun_matches funBind)

  blackListed <- isBlackListed pos

  -- Todo: we don't want redundant ticks on simple pattern bindings
  -- We don't want to generate code for blacklisted positions
  if blackListed || (not opt_Hpc && isSimplePatBind funBind)
     then 
        return $ L pos $ funBind { fun_matches = MatchGroup matches' ty 
                                 , fun_tick = Nothing 
                                 }
     else do
        tick_no <- allocATickBox (if null decl_path
                                     then TopLevelBox [name]
                                     else LocalBox (decl_path ++ [name])) 
                                pos fvs

        return $ L pos $ funBind { fun_matches = MatchGroup matches' ty 
                                 , fun_tick = tick_no
                                 }
   where
   -- a binding is a simple pattern binding if it is a funbind with zero patterns
   isSimplePatBind :: HsBind a -> Bool
   isSimplePatBind funBind = matchGroupArity (fun_matches funBind) == 0

-- TODO: Revisit this
addTickLHsBind (L pos (pat@(PatBind { pat_rhs = rhs }))) = do
  let name = "(...)"
  rhs' <- addPathEntry name $ addTickGRHSs False rhs
{-
  decl_path <- getPathEntry
  tick_me <- allocTickBox (if null decl_path
			   then TopLevelBox [name]
			   else LocalBox (name : decl_path))
-}			   
  return $ L pos $ pat { pat_rhs = rhs' }

-- Only internal stuff, not from source, uses VarBind, so we ignore it.
addTickLHsBind var_bind@(L _ (VarBind {})) = return var_bind

-- Add a tick to the expression no matter what it is.  There is one exception:
-- for the debugger, if the expression is a 'let', then we don't want to add
-- a tick here because there will definititely be a tick on the body anyway.
addTickLHsExprAlways :: LHsExpr Id -> TM (LHsExpr Id)
addTickLHsExprAlways (L pos e0)
  | not opt_Hpc, HsLet _ _ <- e0 = addTickLHsExprNever (L pos e0)
  | otherwise = allocTickBox (ExpBox False) pos $ addTickHsExpr e0

addTickLHsExprNeverOrAlways :: LHsExpr Id -> TM (LHsExpr Id)
addTickLHsExprNeverOrAlways e
    | opt_Hpc   = addTickLHsExprNever e
    | otherwise = addTickLHsExprAlways e

addTickLHsExprNeverOrMaybe :: LHsExpr Id -> TM (LHsExpr Id)
addTickLHsExprNeverOrMaybe e
    | opt_Hpc   = addTickLHsExprNever e
    | otherwise = addTickLHsExpr e

-- version of addTick that does not actually add a tick,
-- because the scope of this tick is completely subsumed by 
-- another.
addTickLHsExprNever :: LHsExpr Id -> TM (LHsExpr Id)
addTickLHsExprNever (L pos e0) = do
    e1 <- addTickHsExpr e0
    return $ L pos e1

-- selectively add ticks to interesting expressions
addTickLHsExpr :: LHsExpr Id -> TM (LHsExpr Id)
addTickLHsExpr (L pos e0) = do
    if opt_Hpc || isGoodBreakExpr e0
       then do
          allocTickBox (ExpBox False) pos $ addTickHsExpr e0
       else do
          e1 <- addTickHsExpr e0
          return $ L pos e1 

-- general heuristic: expressions which do not denote values are good break points
isGoodBreakExpr :: HsExpr Id -> Bool
isGoodBreakExpr (HsApp {})     = True
isGoodBreakExpr (OpApp {})     = True
isGoodBreakExpr (NegApp {})    = True
isGoodBreakExpr (HsCase {})    = True
isGoodBreakExpr (HsIf {})      = True
isGoodBreakExpr (RecordCon {}) = True
isGoodBreakExpr (RecordUpd {}) = True
isGoodBreakExpr (ArithSeq {})  = True
isGoodBreakExpr (PArrSeq {})   = True
isGoodBreakExpr _other         = False 

addTickLHsExprOptAlt :: Bool -> LHsExpr Id -> TM (LHsExpr Id)
addTickLHsExprOptAlt oneOfMany (L pos e0)
  | not opt_Hpc = addTickLHsExpr (L pos e0)
  | otherwise =
    allocTickBox (ExpBox oneOfMany) pos $ 
        addTickHsExpr e0

addBinTickLHsExpr :: (Bool -> BoxLabel) -> LHsExpr Id -> TM (LHsExpr Id)
addBinTickLHsExpr boxLabel (L pos e0) =
    allocBinTickBox boxLabel pos $
        addTickHsExpr e0

addTickHsExpr :: HsExpr Id -> TM (HsExpr Id)
addTickHsExpr e@(HsVar id) = do freeVar id; return e
addTickHsExpr e@(HsIPVar _) = return e
addTickHsExpr e@(HsOverLit _) = return e
addTickHsExpr e@(HsLit _) = return e
addTickHsExpr (HsLam matchgroup) =
        liftM HsLam (addTickMatchGroup matchgroup)
addTickHsExpr (HsApp e1 e2) = 
	liftM2 HsApp (addTickLHsExprNever e1) (addTickLHsExpr e2)
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
addTickHsExpr (HsPar e) = liftM HsPar (addTickLHsExprNeverOrMaybe e)
addTickHsExpr (SectionL e1 e2) = 
	liftM2 SectionL
		(addTickLHsExpr e1)
		(addTickLHsExpr e2)
addTickHsExpr (SectionR e1 e2) = 
	liftM2 SectionR
		(addTickLHsExpr e1)
		(addTickLHsExpr e2)
addTickHsExpr (ExplicitTuple es boxity) =
        liftM2 ExplicitTuple
                (mapM addTickTupArg es)
                (return boxity)
addTickHsExpr (HsCase e mgs) = 
	liftM2 HsCase
		(addTickLHsExpr e) 
		(addTickMatchGroup mgs)
addTickHsExpr (HsIf cnd e1 e2 e3) = 
	liftM3 (HsIf cnd)
		(addBinTickLHsExpr (BinBox CondBinBox) e1)
		(addTickLHsExprOptAlt True e2)
		(addTickLHsExprOptAlt True e3)
addTickHsExpr (HsLet binds e) =
	bindLocals (collectLocalBinders binds) $
	liftM2 HsLet
		(addTickHsLocalBinds binds) -- to think about: !patterns.
                (addTickLHsExprNeverOrAlways e)
addTickHsExpr (HsDo cxt stmts last_exp return_exp srcloc) = do
        (stmts', last_exp') <- addTickLStmts' forQual stmts 
                                     (addTickLHsExpr last_exp)
        return_exp' <- addTickSyntaxExpr hpcSrcSpan return_exp
	return (HsDo cxt stmts' last_exp' return_exp' srcloc)
  where
	forQual = case cxt of
		    ListComp -> Just $ BinBox QualBinBox
		    _        -> Nothing
addTickHsExpr (ExplicitList ty es) = 
	liftM2 ExplicitList
		(return ty)
		(mapM (addTickLHsExpr) es)
addTickHsExpr (ExplicitPArr ty es) =
	liftM2 ExplicitPArr
		(return ty)
		(mapM (addTickLHsExpr) es)
addTickHsExpr (RecordCon id ty rec_binds) = 
	liftM3 RecordCon
		(return id)
		(return ty)
		(addTickHsRecordBinds rec_binds)
addTickHsExpr (RecordUpd e rec_binds cons tys1 tys2) =
	liftM5 RecordUpd
		(addTickLHsExpr e)
		(addTickHsRecordBinds rec_binds)
		(return cons) (return tys1) (return tys2)

addTickHsExpr (ExprWithTySigOut e ty) =
	liftM2 ExprWithTySigOut
		(addTickLHsExprNever e) -- No need to tick the inner expression
				    -- for expressions with signatures
		(return ty)
addTickHsExpr (ArithSeq	 ty arith_seq) =
	liftM2 ArithSeq	
		(return ty)
		(addTickArithSeqInfo arith_seq)
addTickHsExpr (HsTickPragma _ (L pos e0)) = do
    e2 <- allocTickBox (ExpBox False) pos $
                addTickHsExpr e0
    return $ unLoc e2
addTickHsExpr (PArrSeq	 ty arith_seq) =
	liftM2 PArrSeq	
		(return ty)
		(addTickArithSeqInfo arith_seq)
addTickHsExpr (HsSCC nm e) =
        liftM2 HsSCC 
                (return nm)
                (addTickLHsExpr e)
addTickHsExpr (HsCoreAnn nm e) = 
        liftM2 HsCoreAnn 
                (return nm)
                (addTickLHsExpr e)
addTickHsExpr e@(HsBracket     {}) = return e
addTickHsExpr e@(HsBracketOut  {}) = return e
addTickHsExpr e@(HsSpliceE  {}) = return e
addTickHsExpr (HsProc pat cmdtop) =
	liftM2 HsProc
		(addTickLPat pat)
		(liftL (addTickHsCmdTop) cmdtop)
addTickHsExpr (HsWrap w e) = 
	liftM2 HsWrap
		(return w)
		(addTickHsExpr e)	-- explicitly no tick on inside

addTickHsExpr (HsArrApp	 e1 e2 ty1 arr_ty lr) = 
        liftM5 HsArrApp
	       (addTickLHsExpr e1)
	       (addTickLHsExpr e2)
	       (return ty1)
	       (return arr_ty)
	       (return lr)

addTickHsExpr (HsArrForm e fix cmdtop) = 
        liftM3 HsArrForm
	       (addTickLHsExpr e)
	       (return fix)
	       (mapM (liftL (addTickHsCmdTop)) cmdtop)

addTickHsExpr e@(HsType _) = return e

-- Others dhould never happen in expression content.
addTickHsExpr e  = pprPanic "addTickHsExpr" (ppr e)

addTickTupArg :: HsTupArg Id -> TM (HsTupArg Id)
addTickTupArg (Present e)  = do { e' <- addTickLHsExpr e; return (Present e') }
addTickTupArg (Missing ty) = return (Missing ty)

addTickMatchGroup :: MatchGroup Id -> TM (MatchGroup Id)
addTickMatchGroup (MatchGroup matches ty) = do
  let isOneOfMany = matchesOneOfMany matches
  matches' <- mapM (liftL (addTickMatch isOneOfMany)) matches
  return $ MatchGroup matches' ty

addTickMatch :: Bool -> Match Id -> TM (Match Id)
addTickMatch isOneOfMany (Match pats opSig gRHSs) =
  bindLocals (collectPatsBinders pats) $ do
    gRHSs' <- addTickGRHSs isOneOfMany gRHSs
    return $ Match pats opSig gRHSs'

addTickGRHSs :: Bool -> GRHSs Id -> TM (GRHSs Id)
addTickGRHSs isOneOfMany (GRHSs guarded local_binds) = do
  bindLocals binders $ do
    local_binds' <- addTickHsLocalBinds local_binds
    guarded' <- mapM (liftL (addTickGRHS isOneOfMany)) guarded
    return $ GRHSs guarded' local_binds'
  where
    binders = collectLocalBinders local_binds

addTickGRHS :: Bool -> GRHS Id -> TM (GRHS Id)
addTickGRHS isOneOfMany (GRHS stmts expr) = do
  (stmts',expr') <- addTickLStmts' (Just $ BinBox $ GuardBinBox) stmts
                        (if opt_Hpc then addTickLHsExprOptAlt isOneOfMany expr
                                    else addTickLHsExprAlways expr)
  return $ GRHS stmts' expr'

addTickLStmts :: (Maybe (Bool -> BoxLabel)) -> [LStmt Id] -> TM [LStmt Id]
addTickLStmts isGuard stmts = do
  (stmts, _) <- addTickLStmts' isGuard stmts (return ())
  return stmts

addTickLStmts' :: (Maybe (Bool -> BoxLabel)) -> [LStmt Id] -> TM a 
               -> TM ([LStmt Id], a)
addTickLStmts' isGuard lstmts res
  = bindLocals binders $ do
        lstmts' <- mapM (liftL (addTickStmt isGuard)) lstmts
        a <- res
        return (lstmts', a)
  where
        binders = collectLStmtsBinders lstmts

addTickStmt :: (Maybe (Bool -> BoxLabel)) -> Stmt Id -> TM (Stmt Id)
addTickStmt _isGuard (BindStmt pat e bind fail) = do
	liftM4 BindStmt
		(addTickLPat pat)
		(addTickLHsExprAlways e)
		(addTickSyntaxExpr hpcSrcSpan bind)
		(addTickSyntaxExpr hpcSrcSpan fail)
addTickStmt isGuard (ExprStmt e bind' guard' ty) = do
	liftM4 ExprStmt
		(addTick isGuard e)
		(addTickSyntaxExpr hpcSrcSpan bind')
		(addTickSyntaxExpr hpcSrcSpan guard')
		(return ty)
addTickStmt _isGuard (LetStmt binds) = do
	liftM LetStmt
		(addTickHsLocalBinds binds)
addTickStmt isGuard (ParStmt pairs mzipExpr bindExpr returnExpr) = do
    liftM4 ParStmt 
        (mapM (addTickStmtAndBinders isGuard) pairs)
        (addTickSyntaxExpr hpcSrcSpan mzipExpr)
        (addTickSyntaxExpr hpcSrcSpan bindExpr)
        (addTickSyntaxExpr hpcSrcSpan returnExpr)

addTickStmt isGuard (TransformStmt stmts ids usingExpr maybeByExpr returnExpr bindExpr) = do
    t_s <- (addTickLStmts isGuard stmts)
    t_u <- (addTickLHsExprAlways usingExpr)
    t_m <- (addTickMaybeByLHsExpr maybeByExpr)
    t_r <- (addTickSyntaxExpr hpcSrcSpan returnExpr)
    t_b <- (addTickSyntaxExpr hpcSrcSpan bindExpr)
    return $ TransformStmt t_s ids t_u t_m t_r t_b

addTickStmt isGuard (GroupStmt stmts binderMap by using returnExpr bindExpr liftMExpr) = do
    t_s <- (addTickLStmts isGuard stmts)
    t_y <- (fmapMaybeM  addTickLHsExprAlways by)
    t_u <- (fmapEitherM addTickLHsExprAlways (addTickSyntaxExpr hpcSrcSpan) using)
    t_f <- (addTickSyntaxExpr hpcSrcSpan returnExpr)
    t_b <- (addTickSyntaxExpr hpcSrcSpan bindExpr)
    t_m <- (addTickSyntaxExpr hpcSrcSpan liftMExpr)
    return $ GroupStmt t_s binderMap t_y t_u t_b t_f t_m

addTickStmt isGuard stmt@(RecStmt {})
  = do { stmts' <- addTickLStmts isGuard (recS_stmts stmt)
       ; ret'   <- addTickSyntaxExpr hpcSrcSpan (recS_ret_fn stmt)
       ; mfix'  <- addTickSyntaxExpr hpcSrcSpan (recS_mfix_fn stmt)
       ; bind'  <- addTickSyntaxExpr hpcSrcSpan (recS_bind_fn stmt)
       ; return (stmt { recS_stmts = stmts', recS_ret_fn = ret'
                      , recS_mfix_fn = mfix', recS_bind_fn = bind' }) }

addTick :: Maybe (Bool -> BoxLabel) -> LHsExpr Id -> TM (LHsExpr Id)
addTick isGuard e | Just fn <- isGuard = addBinTickLHsExpr fn e
                  | otherwise          = addTickLHsExprAlways e

addTickStmtAndBinders :: Maybe (Bool -> BoxLabel) -> ([LStmt Id], a) 
                      -> TM ([LStmt Id], a)
addTickStmtAndBinders isGuard (stmts, ids) = 
    liftM2 (,) 
        (addTickLStmts isGuard stmts)
        (return ids)

addTickMaybeByLHsExpr :: Maybe (LHsExpr Id) -> TM (Maybe (LHsExpr Id))
addTickMaybeByLHsExpr maybeByExpr = 
    case maybeByExpr of
        Nothing -> return Nothing
        Just byExpr -> addTickLHsExprAlways byExpr >>= (return . Just)

addTickHsLocalBinds :: HsLocalBinds Id -> TM (HsLocalBinds Id)
addTickHsLocalBinds (HsValBinds binds) = 
	liftM HsValBinds 
		(addTickHsValBinds binds)
addTickHsLocalBinds (HsIPBinds binds)  = 
	liftM HsIPBinds 
		(addTickHsIPBinds binds)
addTickHsLocalBinds (EmptyLocalBinds)  = return EmptyLocalBinds

addTickHsValBinds :: HsValBindsLR Id a -> TM (HsValBindsLR Id b)
addTickHsValBinds (ValBindsOut binds sigs) =
	liftM2 ValBindsOut
		(mapM (\ (rec,binds') -> 
				liftM2 (,)
					(return rec)
					(addTickLHsBinds binds'))
			binds)
		(return sigs)
addTickHsValBinds _ = panic "addTickHsValBinds"

addTickHsIPBinds :: HsIPBinds Id -> TM (HsIPBinds Id)
addTickHsIPBinds (IPBinds ipbinds dictbinds) =
	liftM2 IPBinds
		(mapM (liftL (addTickIPBind)) ipbinds)
		(return dictbinds)

addTickIPBind :: IPBind Id -> TM (IPBind Id)
addTickIPBind (IPBind nm e) =
	liftM2 IPBind
		(return nm)
		(addTickLHsExpr e)

-- There is no location here, so we might need to use a context location??
addTickSyntaxExpr :: SrcSpan -> SyntaxExpr Id -> TM (SyntaxExpr Id)
addTickSyntaxExpr pos x = do
	L _ x' <- addTickLHsExpr (L pos x)
	return $ x'
-- we do not walk into patterns.
addTickLPat :: LPat Id -> TM (LPat Id)
addTickLPat pat = return pat

addTickHsCmdTop :: HsCmdTop Id -> TM (HsCmdTop Id)
addTickHsCmdTop (HsCmdTop cmd tys ty syntaxtable) =
	liftM4 HsCmdTop
		(addTickLHsCmd cmd)
		(return tys)
		(return ty)
		(return syntaxtable)

addTickLHsCmd ::  LHsCmd Id -> TM (LHsCmd Id)
addTickLHsCmd (L pos c0) = do
        c1 <- addTickHsCmd c0
        return $ L pos c1 

addTickHsCmd :: HsCmd Id -> TM (HsCmd Id)
addTickHsCmd (HsLam matchgroup) =
        liftM HsLam (addTickCmdMatchGroup matchgroup)
addTickHsCmd (HsApp e1 e2) = 
	liftM2 HsApp (addTickLHsExprNever e1) (addTickLHsExpr e2)
addTickHsCmd (OpApp e1 c2 fix c3) = 
	liftM4 OpApp 
		(addTickLHsExpr e1) 
		(addTickLHsCmd c2)
		(return fix)
		(addTickLHsCmd c3)
addTickHsCmd (HsPar e) = liftM HsPar (addTickLHsCmd e)
addTickHsCmd (HsCase e mgs) = 
	liftM2 HsCase
		(addTickLHsExpr e) 
		(addTickCmdMatchGroup mgs)
addTickHsCmd (HsIf cnd e1 c2 c3) = 
	liftM3 (HsIf cnd)
		(addBinTickLHsExpr (BinBox CondBinBox) e1)
		(addTickLHsCmd c2)
		(addTickLHsCmd c3)
addTickHsCmd (HsLet binds c) =
	bindLocals (collectLocalBinders binds) $
	liftM2 HsLet
		(addTickHsLocalBinds binds) -- to think about: !patterns.
                (addTickLHsCmd c)
addTickHsCmd (HsDo cxt stmts last_exp return_exp srcloc) = do
        (stmts', last_exp') <- addTickLCmdStmts' stmts (addTickLHsCmd last_exp)
        return_exp' <- addTickSyntaxExpr hpcSrcSpan return_exp
	return (HsDo cxt stmts' last_exp' return_exp' srcloc)

addTickHsCmd (HsArrApp	 e1 e2 ty1 arr_ty lr) = 
        liftM5 HsArrApp
	       (addTickLHsExpr e1)
	       (addTickLHsExpr e2)
	       (return ty1)
	       (return arr_ty)
	       (return lr)
addTickHsCmd (HsArrForm e fix cmdtop) = 
        liftM3 HsArrForm
	       (addTickLHsExpr e)
	       (return fix)
	       (mapM (liftL (addTickHsCmdTop)) cmdtop)

-- Others should never happen in a command context.
addTickHsCmd e  = pprPanic "addTickHsCmd" (ppr e)

addTickCmdMatchGroup :: MatchGroup Id -> TM (MatchGroup Id)
addTickCmdMatchGroup (MatchGroup matches ty) = do
  matches' <- mapM (liftL addTickCmdMatch) matches
  return $ MatchGroup matches' ty

addTickCmdMatch :: Match Id -> TM (Match Id)
addTickCmdMatch (Match pats opSig gRHSs) =
  bindLocals (collectPatsBinders pats) $ do
    gRHSs' <- addTickCmdGRHSs gRHSs
    return $ Match pats opSig gRHSs'

addTickCmdGRHSs :: GRHSs Id -> TM (GRHSs Id)
addTickCmdGRHSs (GRHSs guarded local_binds) = do
  bindLocals binders $ do
    local_binds' <- addTickHsLocalBinds local_binds
    guarded' <- mapM (liftL addTickCmdGRHS) guarded
    return $ GRHSs guarded' local_binds'
  where
    binders = collectLocalBinders local_binds

addTickCmdGRHS :: GRHS Id -> TM (GRHS Id)
addTickCmdGRHS (GRHS stmts cmd) = do
  (stmts',expr') <- addTickLCmdStmts' stmts (addTickLHsCmd cmd)
  return $ GRHS stmts' expr'

addTickLCmdStmts :: [LStmt Id] -> TM [LStmt Id]
addTickLCmdStmts stmts = do
  (stmts, _) <- addTickLCmdStmts' stmts (return ())
  return stmts

addTickLCmdStmts' :: [LStmt Id] -> TM a -> TM ([LStmt Id], a)
addTickLCmdStmts' lstmts res
  = bindLocals binders $ do
        lstmts' <- mapM (liftL addTickCmdStmt) lstmts
        a <- res
        return (lstmts', a)
  where
        binders = collectLStmtsBinders lstmts

addTickCmdStmt :: Stmt Id -> TM (Stmt Id)
addTickCmdStmt (BindStmt pat c bind fail) = do
	liftM4 BindStmt
		(addTickLPat pat)
		(addTickLHsCmd c)
		(return bind)
		(return fail)
addTickCmdStmt (ExprStmt c bind' guard' ty) = do
	liftM4 ExprStmt
		(addTickLHsCmd c)
		(addTickSyntaxExpr hpcSrcSpan bind')
                (addTickSyntaxExpr hpcSrcSpan guard')
		(return ty)
addTickCmdStmt (LetStmt binds) = do
	liftM LetStmt
		(addTickHsLocalBinds binds)
addTickCmdStmt stmt@(RecStmt {})
  = do { stmts' <- addTickLCmdStmts (recS_stmts stmt)
       ; ret'   <- addTickSyntaxExpr hpcSrcSpan (recS_ret_fn stmt)
       ; mfix'  <- addTickSyntaxExpr hpcSrcSpan (recS_mfix_fn stmt)
       ; bind'  <- addTickSyntaxExpr hpcSrcSpan (recS_bind_fn stmt)
       ; return (stmt { recS_stmts = stmts', recS_ret_fn = ret'
                      , recS_mfix_fn = mfix', recS_bind_fn = bind' }) }

-- Others should never happen in a command context.
addTickCmdStmt stmt  = pprPanic "addTickHsCmd" (ppr stmt)

addTickHsRecordBinds :: HsRecordBinds Id -> TM (HsRecordBinds Id)
addTickHsRecordBinds (HsRecFields fields dd) 
  = do	{ fields' <- mapM process fields
	; return (HsRecFields fields' dd) }
  where
    process (HsRecField ids expr doc)
	= do { expr' <- addTickLHsExpr expr
	     ; return (HsRecField ids expr' doc) }

addTickArithSeqInfo :: ArithSeqInfo Id -> TM (ArithSeqInfo Id)
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
\end{code}

\begin{code}
data TickTransState = TT { tickBoxCount:: Int
                         , mixEntries  :: [MixEntry_]
                         }                        

data TickTransEnv = TTE { fileName      :: FastString
			, declPath     :: [String]
                        , inScope      :: VarSet
			, blackList   :: Map SrcSpan ()
			}

--	deriving Show

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

instance Monad TM where
  return a = TM $ \ _env st -> (a,noFVs,st)
  (TM m) >>= k = TM $ \ env st -> 
		                case m env st of
				  (r1,fv1,st1) -> 
                                     case unTM (k r1) env st1 of
                                       (r2,fv2,st2) -> 
                                          (r2, fv1 `plusOccEnv` fv2, st2)

-- getState :: TM TickTransState
-- getState = TM $ \ env st -> (st, noFVs, st)

-- setState :: (TickTransState -> TickTransState) -> TM ()
-- setState f = TM $ \ env st -> ((), noFVs, f st)

getEnv :: TM TickTransEnv
getEnv = TM $ \ env st -> (env, noFVs, st)

withEnv :: (TickTransEnv -> TickTransEnv) -> TM a -> TM a
withEnv f (TM m) = TM $ \ env st -> 
		                 case m (f env) st of
                                   (a, fvs, st') -> (a, fvs, st')

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

sameFileName :: SrcSpan -> TM a -> TM a -> TM a
sameFileName pos out_of_scope in_scope = do
  file_name <- getFileName
  case srcSpanFileName_maybe pos of 
    Just file_name2 
      | file_name == file_name2 -> in_scope
    _ -> out_of_scope

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
allocTickBox :: BoxLabel -> SrcSpan -> TM (HsExpr Id) -> TM (LHsExpr Id)
allocTickBox boxLabel pos m | isGoodSrcSpan' pos = 
  sameFileName pos 
    (do e <- m; return (L pos e)) $ do
  (fvs, e) <- getFreeVars m
  TM $ \ env st ->
    let c = tickBoxCount st
        ids = occEnvElts fvs
        mes = mixEntries st
        me = (pos, declPath env, map (nameOccName.idName) ids, boxLabel)
    in
    ( L pos (HsTick c ids (L pos e))
    , fvs
    , st {tickBoxCount=c+1,mixEntries=me:mes}
    )
allocTickBox _boxLabel pos m = do e <- m; return (L pos e)

-- the tick application inherits the source position of its
-- expression argument to support nested box allocations 
allocATickBox :: BoxLabel -> SrcSpan -> FreeVars -> TM (Maybe (Int,[Id]))
allocATickBox boxLabel pos fvs | isGoodSrcSpan' pos = 
  sameFileName pos 
    (return Nothing) $ TM $ \ env st ->
  let mydecl_path
        | null (declPath env), TopLevelBox x <- boxLabel = x
        | otherwise = declPath env
      me = (pos, mydecl_path, map (nameOccName.idName) ids, boxLabel)
      c = tickBoxCount st
      mes = mixEntries st
      ids = occEnvElts fvs
  in ( Just (c, ids)
     , noFVs
     , st {tickBoxCount=c+1, mixEntries=me:mes}
     )
allocATickBox _boxLabel _pos _fvs = return Nothing

allocBinTickBox :: (Bool -> BoxLabel) -> SrcSpan -> TM (HsExpr Id)
                -> TM (LHsExpr Id)
allocBinTickBox boxLabel pos m
 | not opt_Hpc = allocTickBox (ExpBox False) pos m
 | isGoodSrcSpan' pos =
 do
 e <- m
 TM $ \ env st ->
  let meT = (pos,declPath env, [],boxLabel True)
      meF = (pos,declPath env, [],boxLabel False)
      meE = (pos,declPath env, [],ExpBox False)
      c = tickBoxCount st
      mes = mixEntries st
  in 
             ( L pos $ HsTick c [] $ L pos $ HsBinTick (c+1) (c+2) (L pos e)
           -- notice that F and T are reversed,
           -- because we are building the list in
           -- reverse...
             , noFVs
             , st {tickBoxCount=c+3 , mixEntries=meF:meT:meE:mes}
             )
allocBinTickBox _boxLabel pos m = do e <- m; return (L pos e)

isGoodSrcSpan' :: SrcSpan -> Bool
isGoodSrcSpan' pos
   | not (isGoodSrcSpan pos) = False
   | start == end            = False
   | otherwise		     = True
  where
   start = srcSpanStart pos
   end   = srcSpanEnd pos

mkHpcPos :: SrcSpan -> HpcPos
mkHpcPos pos 
   | not (isGoodSrcSpan' pos) = panic "bad source span; expected such spans to be filtered out"
   | otherwise		      = hpcPos
  where
   start = srcSpanStart pos
   end   = srcSpanEnd pos
   hpcPos = toHpcPos ( srcLocLine start
		     , srcLocCol start
		     , srcLocLine end
		     , srcLocCol end - 1
		     )

hpcSrcSpan :: SrcSpan
hpcSrcSpan = mkGeneralSrcSpan (fsLit "Haskell Program Coverage internals")
\end{code}


\begin{code}
matchesOneOfMany :: [LMatch Id] -> Bool
matchesOneOfMany lmatches = sum (map matchCount lmatches) > 1
  where
	matchCount (L _ (Match _pats _ty (GRHSs grhss _binds))) = length grhss
\end{code}


\begin{code}
type MixEntry_ = (SrcSpan, [String], [OccName], BoxLabel)

-- For the hash value, we hash everything: the file name, 
--  the timestamp of the original source file, the tab stop,
--  and the mix entries. We cheat, and hash the show'd string.
-- This hash only has to be hashed at Mix creation time,
-- and is for sanity checking only.

mixHash :: FilePath -> Integer -> Int -> [MixEntry] -> Int
mixHash file tm tabstop entries = fromIntegral $ hashString
	(show $ Mix file tm 0 tabstop entries)
\end{code}

%************************************************************************
%*									*
%*              initialisation
%*									*
%************************************************************************

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

\begin{code}
hpcInitCode :: Module -> HpcInfo -> SDoc
hpcInitCode _ (NoHpcInfo {}) = empty
hpcInitCode this_mod (HpcInfo tickCount hashNo)
 = vcat
    [ text "static void hpc_init_" <> ppr this_mod
         <> text "(void) __attribute__((constructor));"
    , text "static void hpc_init_" <> ppr this_mod <> text "(void)"
    , braces (vcat [
        ptext (sLit "extern StgWord64 ") <> tickboxes <>
               ptext (sLit "[]") <> semi,
        ptext (sLit "hs_hpc_module") <>
          parens (hcat (punctuate comma [
              doubleQuotes full_name_str,
              int tickCount, -- really StgWord32
              int hashNo,    -- really StgWord32
              tickboxes
            ])) <> semi
       ])
    ]
  where
    tickboxes = pprCLabel (mkHpcTicksLabel $ this_mod)

    module_name  = hcat (map (text.charToC) $
                         bytesFS (moduleNameFS (Module.moduleName this_mod)))
    package_name = hcat (map (text.charToC) $
                         bytesFS (packageIdFS  (modulePackageId this_mod)))
    full_name_str
       | modulePackageId this_mod == mainPackageId
       = module_name
       | otherwise
       = package_name <> char '/' <> module_name
\end{code}
