%
% (c) Galois, 2006
% (c) University of Glasgow, 2007
%
\section[Coverage]{@coverage@: the main function}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Coverage (addCoverageTicksToBinds) where

#include "HsVersions.h"

import HsSyn
import Module
import Outputable
import DynFlags
import Monad		
import SrcLoc
import ErrUtils
import Name
import Bag
import Var
import VarSet
import Data.List
import FastString
import HscTypes	
import StaticFlags
import UniqFM
import Type
import TyCon
import FiniteMap
import PackageConfig 

import Data.Array
import System.Time (ClockTime(..))
import System.IO   (FilePath)
#if __GLASGOW_HASKELL__ < 603
import Compat.Directory ( createDirectoryIfMissing )
#else
import System.Directory ( createDirectoryIfMissing )
#endif

import Trace.Hpc.Mix
import Trace.Hpc.Util

import BreakArray 
import Data.HashTable   ( hashString )
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

addCoverageTicksToBinds dflags mod mod_loc tyCons binds = do 

  let orig_file = 
             case ml_hs_file mod_loc of
		    Just file -> file
		    Nothing -> panic "can not find the original file during hpc trans"

  if "boot" `isSuffixOf` orig_file then return (binds, emptyHpcInfo False, emptyModBreaks) else do

  let mod_name = moduleNameString (moduleName mod)

  let (binds1,_,st)
		 = unTM (addTickLHsBinds binds) 
		   (TTE
		       { fileName    = mkFastString orig_file
		      , declPath     = []
                      , inScope      = emptyVarSet
		      , blackList    = listToFM [ (getSrcSpan (tyConName tyCon),()) 
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
     modTime <- getModificationTime orig_file
     let entries' = [ (hpcPos, box) 
                    | (span,_,box) <- entries, hpcPos <- [mkHpcPos span] ]
     when (length entries' /= tickBoxCount st) $ do
       panic "the number of .mix entries are inconsistent"
     let hashNo = mixHash orig_file modTime tabStop entries'
     mixCreate hpc_mod_dir mod_name 
     	       $ Mix orig_file modTime (toHash hashNo) tabStop entries'
     return $ hashNo 
   else do
     return $ 0

  -- Todo: use proper src span type
  breakArray <- newBreakArray $ length entries

  let locsTicks = listArray (0,tickBoxCount st-1) 
                     [ span | (span,_,_) <- entries ]
      varsTicks = listArray (0,tickBoxCount st-1) 
                     [ vars | (_,vars,_) <- entries ]
      modBreaks = emptyModBreaks 
                  { modBreaks_flags = breakArray 
                  , modBreaks_locs  = locsTicks 
                  , modBreaks_vars  = varsTicks
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
addTickLHsBind (L pos t@(AbsBinds abs_tvs abs_dicts abs_exports abs_binds)) = do
  abs_binds' <- addTickLHsBinds abs_binds
  return $ L pos $ AbsBinds abs_tvs abs_dicts abs_exports abs_binds'
addTickLHsBind (L pos (funBind@(FunBind { fun_id = (L _ id)  }))) = do 
  let name = getOccString id
  decl_path <- getPathEntry

  (fvs, mg@(MatchGroup matches' ty)) <- 
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
isGoodBreakExpr other          = False 

addTickLHsExprOptAlt :: Bool -> LHsExpr Id -> TM (LHsExpr Id)
addTickLHsExprOptAlt oneOfMany (L pos e0)
  | not opt_Hpc = addTickLHsExpr (L pos e0)
  | otherwise =
    allocTickBox (ExpBox oneOfMany) pos $ 
        addTickHsExpr e0

addBinTickLHsExpr :: (Bool -> BoxLabel) -> LHsExpr Id -> TM (LHsExpr Id)
addBinTickLHsExpr boxLabel (L pos e0) = do
    e1 <- addTickHsExpr e0
    allocBinTickBox boxLabel $ L pos e1

addTickHsExpr :: HsExpr Id -> TM (HsExpr Id)
addTickHsExpr e@(HsVar id) = do freeVar id; return e
addTickHsExpr e@(HsIPVar _) = return e
addTickHsExpr e@(HsOverLit _) = return e
addTickHsExpr e@(HsLit _) = return e
addTickHsExpr e@(HsLam matchgroup) =
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
addTickHsExpr (HsCase e mgs) = 
	liftM2 HsCase
		(addTickLHsExpr e) 
		(addTickMatchGroup mgs)
addTickHsExpr (HsIf	 e1 e2 e3) = 
	liftM3 HsIf
		(addBinTickLHsExpr (BinBox CondBinBox) e1)
		(addTickLHsExprOptAlt True e2)
		(addTickLHsExprOptAlt True e3)
addTickHsExpr (HsLet binds e) =
	bindLocals (map unLoc $ collectLocalBinders binds) $
	liftM2 HsLet
		(addTickHsLocalBinds binds) -- to think about: !patterns.
                (addTickLHsExprNeverOrAlways e)
addTickHsExpr (HsDo cxt stmts last_exp srcloc) = do
        (stmts', last_exp') <- addTickLStmts' forQual stmts 
                                     (addTickLHsExpr last_exp)
	return (HsDo cxt stmts' last_exp' srcloc)
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
addTickHsExpr (ExplicitTuple es box) =
	liftM2 ExplicitTuple
		(mapM (addTickLHsExpr) es)
		(return box)
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
addTickHsExpr (HsTickPragma (file,(l1,c1),(l2,c2)) (L pos e0)) = do
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

addTickHsExpr e@(HsType ty) = return e

-- Others dhould never happen in expression content.
addTickHsExpr e@(ExprWithTySig {}) = pprPanic "addTickHsExpr" (ppr e)
addTickHsExpr e@(EAsPat _ _)       = pprPanic "addTickHsExpr" (ppr e)
addTickHsExpr e@(ELazyPat _)       = pprPanic "addTickHsExpr" (ppr e)
addTickHsExpr e@(EWildPat)         = pprPanic "addTickHsExpr" (ppr e)
addTickHsExpr e@(HsBinTick _ _ _)  = pprPanic "addTickHsExpr" (ppr e)
addTickHsExpr e@(HsTick _ _ _)     = pprPanic "addTickHsExpr" (ppr e)

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
    binders = map unLoc (collectLocalBinders local_binds)

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
        binders = map unLoc (collectLStmtsBinders lstmts)

addTickStmt :: (Maybe (Bool -> BoxLabel)) -> Stmt Id -> TM (Stmt Id)
addTickStmt isGuard (BindStmt pat e bind fail) = do
	liftM4 BindStmt
		(addTickLPat pat)
		(addTickLHsExprAlways e)
		(addTickSyntaxExpr hpcSrcSpan bind)
		(addTickSyntaxExpr hpcSrcSpan fail)
addTickStmt isGuard (ExprStmt e bind' ty) = do
	liftM3 ExprStmt
		(addTick e)
		(addTickSyntaxExpr hpcSrcSpan bind')
		(return ty)
  where
   addTick e | Just fn <- isGuard = addBinTickLHsExpr fn e
             | otherwise          = addTickLHsExprAlways e

addTickStmt isGuard (LetStmt binds) = do
	liftM LetStmt
		(addTickHsLocalBinds binds)
addTickStmt isGuard (ParStmt pairs) = do
  	liftM ParStmt (mapM process pairs)
  where
	process (stmts,ids) = 
		liftM2 (,) 
			(addTickLStmts isGuard stmts)
			(return ids)
addTickStmt isGuard (RecStmt stmts ids1 ids2 tys dictbinds) = do
  	liftM5 RecStmt 
		(addTickLStmts isGuard stmts)
		(return ids1)
		(return ids2)
		(return tys)
		(addTickDictBinds dictbinds)

addTickHsLocalBinds :: HsLocalBinds Id -> TM (HsLocalBinds Id)
addTickHsLocalBinds (HsValBinds binds) = 
	liftM HsValBinds 
		(addTickHsValBinds binds)
addTickHsLocalBinds (HsIPBinds binds)  = 
	liftM HsIPBinds 
		(addTickHsIPBinds binds)
addTickHsLocalBinds (EmptyLocalBinds)  = return EmptyLocalBinds

addTickHsValBinds (ValBindsOut binds sigs) =
	liftM2 ValBindsOut
		(mapM (\ (rec,binds') -> 
				liftM2 (,)
					(return rec)
					(addTickLHsBinds binds'))
			binds)
		(return sigs)

addTickHsIPBinds (IPBinds ipbinds dictbinds) =
	liftM2 IPBinds
		(mapM (liftL (addTickIPBind)) ipbinds)
		(addTickDictBinds dictbinds)

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
addTickLHsCmd x = addTickLHsExpr x

addTickDictBinds :: DictBinds Id -> TM (DictBinds Id)
addTickDictBinds x = addTickLHsBinds x

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
			, blackList   :: FiniteMap SrcSpan ()
			}

--	deriving Show

type FreeVars = OccEnv Id
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
  return a = TM $ \ env st -> (a,noFVs,st)
  (TM m) >>= k = TM $ \ env st -> 
		                case m env st of
				  (r1,fv1,st1) -> 
                                     case unTM (k r1) env st1 of
                                       (r2,fv2,st2) -> 
                                          (r2, fv1 `plusOccEnv` fv2, st2)

-- getState :: TM TickTransState
-- getState = TM $ \ env st -> (st, noFVs, st)

setState :: (TickTransState -> TickTransState) -> TM ()
setState f = TM $ \ env st -> ((), noFVs, f st)

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
  case optSrcSpanFileName pos of 
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
	      case lookupFM (blackList env) pos of
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
        me = (pos, map (nameOccName.idName) ids, boxLabel)
    in
    ( L pos (HsTick c ids (L pos e))
    , fvs
    , st {tickBoxCount=c+1,mixEntries=me:mes}
    )
allocTickBox boxLabel pos m = do e <- m; return (L pos e)

-- the tick application inherits the source position of its
-- expression argument to support nested box allocations 
allocATickBox :: BoxLabel -> SrcSpan -> FreeVars -> TM (Maybe (Int,[Id]))
allocATickBox boxLabel pos fvs | isGoodSrcSpan' pos = 
  sameFileName pos 
    (return Nothing) $ TM $ \ env st ->
  let me = (pos, map (nameOccName.idName) ids, boxLabel)
      c = tickBoxCount st
      mes = mixEntries st
      ids = occEnvElts fvs
  in ( Just (c, ids)
     , noFVs
     , st {tickBoxCount=c+1, mixEntries=me:mes}
     )
allocATickBox boxLabel pos fvs = return Nothing

allocBinTickBox :: (Bool -> BoxLabel) -> LHsExpr Id -> TM (LHsExpr Id)
allocBinTickBox boxLabel (L pos e) | isGoodSrcSpan' pos = TM $ \ env st ->
  let meT = (pos,[],boxLabel True)
      meF = (pos,[],boxLabel False)
      meE = (pos,[],ExpBox False)
      c = tickBoxCount st
      mes = mixEntries st
  in 
     if opt_Hpc 
        then ( L pos $ HsTick c [] $ L pos $ HsBinTick (c+1) (c+2) (L pos e)
           -- notice that F and T are reversed,
           -- because we are building the list in
           -- reverse...
             , noFVs
             , st {tickBoxCount=c+3 , mixEntries=meF:meT:meE:mes}
             )
        else
             ( L pos $ HsTick c [] $ L pos e
             , noFVs
             , st {tickBoxCount=c+1,mixEntries=meE:mes}
             )

allocBinTickBox boxLabel e = return e

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
		     , srcLocCol start + 1
		     , srcLocLine end
		     , srcLocCol end
		     )

noHpcPos = toHpcPos (0,0,0,0)

hpcSrcSpan = mkGeneralSrcSpan (FSLIT("Haskell Program Coverage internals"))
\end{code}


\begin{code}
matchesOneOfMany :: [LMatch Id] -> Bool
matchesOneOfMany lmatches = sum (map matchCount lmatches) > 1
  where
	matchCount (L _ (Match _pats _ty (GRHSs grhss _binds))) = length grhss
\end{code}


\begin{code}
type MixEntry_ = (SrcSpan, [OccName], BoxLabel)

-- For the hash value, we hash everything: the file name, 
--  the timestamp of the original source file, the tab stop,
--  and the mix entries. We cheat, and hash the show'd string.
-- This hash only has to be hashed at Mix creation time,
-- and is for sanity checking only.

mixHash :: FilePath -> Integer -> Int -> [MixEntry] -> Int
mixHash file tm tabstop entries = fromIntegral $ hashString
	(show $ Mix file tm 0 tabstop entries)
\end{code}
