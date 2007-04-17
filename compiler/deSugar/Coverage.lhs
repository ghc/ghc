%
% (c) Galois, 2006
% (c) University of Glasgow, 2007
%
\section[Coverage]{@coverage@: the main function}

\begin{code}
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
import Data.List
import FastString
import StaticFlags

import Data.Array
import System.Time (ClockTime(..))
import System.Directory (getModificationTime)
import System.IO   (FilePath)
#if __GLASGOW_HASKELL__ < 603
import Compat.Directory ( createDirectoryIfMissing )
#else
import System.Directory ( createDirectoryIfMissing )
#endif

import HscTypes 
import BreakArray 
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
        -> LHsBinds Id
        -> IO (LHsBinds Id, Int, ModBreaks)

addCoverageTicksToBinds dflags mod mod_loc binds = do 
  let orig_file = 
             case ml_hs_file mod_loc of
		    Just file -> file
		    Nothing -> panic "can not find the original file during hpc trans"

  if "boot" `isSuffixOf` orig_file then return (binds, 0, emptyModBreaks) else do

  let mod_name = moduleNameString (moduleName mod)

  let (binds1,st)
		 = unTM (addTickLHsBinds binds) 
		 $ TT { modName      = mod_name
		      , declPath     = []
		      , tickBoxCount = 0
		      , mixEntries   = []
		      }

  let entries = reverse $ mixEntries st

  -- write the mix entries for this module
  when opt_Hpc $ do
     let hpc_dir = hpcDir dflags
     let tabStop = 1 -- <tab> counts as a normal char in GHC's location ranges.
     createDirectoryIfMissing True hpc_dir
     modTime <- getModificationTime' orig_file
     mixCreate hpc_dir mod_name (Mix orig_file modTime tabStop entries)

  -- Todo: use proper src span type
  breakArray <- newBreakArray $ length entries
  let fn = mkFastString orig_file
  let locsTicks = listArray (0,tickBoxCount st-1)
                        [ mkSrcSpan (mkSrcLoc fn r1 c1) (mkSrcLoc fn r2 c2)
                        | (P r1 c1 r2 c2, _box) <- entries ] 

  let modBreaks = emptyModBreaks 
                  { modBreaks_array = breakArray 
                  , modBreaks_ticks = locsTicks 
                  } 

  doIfSet_dyn dflags  Opt_D_dump_hpc $ do
	  printDump (pprLHsBinds binds1)

  return (binds1, tickBoxCount st, modBreaks)
\end{code}


\begin{code}
liftL :: (Monad m) => (a -> m a) -> Located a -> m (Located a)
liftL f (L loc a) = do
  a' <- f a
  return $ L loc a'

addTickLHsBinds :: LHsBinds Id -> TM (LHsBinds Id)
addTickLHsBinds binds = mapBagM addTickLHsBind binds

addTickLHsBind :: LHsBind Id -> TM (LHsBind Id)
addTickLHsBind (L pos (AbsBinds abs_tvs abs_dicts abs_exports abs_binds)) = do
  abs_binds' <- addTickLHsBinds abs_binds
  return $ L pos $ AbsBinds abs_tvs abs_dicts abs_exports abs_binds'

addTickLHsBind (L pos (funBind@(FunBind { fun_id = (L _ id)  }))) = do 
  let name = getOccString id
  decl_path <- getPathEntry

  mg@(MatchGroup matches' ty) <- addPathEntry name  
				 $ addTickMatchGroup (fun_matches funBind)

  -- Todo: we don't want redundant ticks on simple pattern bindings
  if not opt_Hpc && isSimplePatBind funBind
     then 
        return $ L pos $ funBind { fun_matches = MatchGroup matches' ty 
                                 , fun_tick = Nothing 
                                 }
     else do
        tick_no <- allocATickBox (if null decl_path
                                     then TopLevelBox [name]
                                     else LocalBox (name : decl_path)) pos

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

{- only internal stuff, not from source, uses VarBind, so we ignore it.
addTickLHsBind (VarBind var_id var_rhs) = do
  var_rhs' <- addTickLHsExpr var_rhs  
  return $ VarBind var_id var_rhs'
-}
addTickLHsBind other = return other

-- add a tick to the expression no matter what it is
addTickLHsExprAlways :: LHsExpr Id -> TM (LHsExpr Id)
addTickLHsExprAlways (L pos e0) = do
    e1 <- addTickHsExpr e0
    fn <- allocTickBox ExpBox pos 
    return $ fn $ L pos e1

-- always a breakpoint tick, maybe an HPC tick
addTickLHsExprBreakAlways :: LHsExpr Id -> TM (LHsExpr Id)
addTickLHsExprBreakAlways e
    | opt_Hpc   = addTickLHsExpr e
    | otherwise = addTickLHsExprAlways e

-- selectively add ticks to interesting expressions
addTickLHsExpr :: LHsExpr Id -> TM (LHsExpr Id)
addTickLHsExpr (L pos e0) = do
    e1 <- addTickHsExpr e0
    if opt_Hpc || isGoodBreakExpr e0
       then do
          fn <- allocTickBox ExpBox pos 
          return $ fn $ L pos e1
       else
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
  | otherwise = do
    e1 <- addTickHsExpr e0
    fn <- allocTickBox (if oneOfMany then AltBox else ExpBox) pos 
    return $ fn $ L pos e1

-- version of addTick that does not actually add a tick,
-- because the scope of this tick is completely subsumed by 
-- another.
addTickLHsExpr' :: LHsExpr Id -> TM (LHsExpr Id)
addTickLHsExpr' (L pos e0) = do
    e1 <- addTickHsExpr e0
    return $ L pos e1

addBinTickLHsExpr :: (Bool -> BoxLabel) -> LHsExpr Id -> TM (LHsExpr Id)
addBinTickLHsExpr boxLabel (L pos e0) = do
    e1 <- addTickHsExpr e0
    allocBinTickBox boxLabel $ L pos e1

addTickHsExpr :: HsExpr Id -> TM (HsExpr Id)
addTickHsExpr e@(HsVar _) = return e
addTickHsExpr e@(HsIPVar _) = return e
addTickHsExpr e@(HsOverLit _) = return e
addTickHsExpr e@(HsLit _) = return e
addTickHsExpr e@(HsLam matchgroup) =
        liftM HsLam (addTickMatchGroup matchgroup)
addTickHsExpr (HsApp e1 e2) = 
	liftM2 HsApp (addTickLHsExpr' e1) (addTickLHsExpr e2)
addTickHsExpr (OpApp e1 e2 fix e3) = 
	liftM4 OpApp 
		(addTickLHsExpr e1) 
		(addTickLHsExpr' e2)
		(return fix)
		(addTickLHsExpr e3)
addTickHsExpr (NegApp e neg) =
	liftM2 NegApp
		(addTickLHsExpr e) 
		(addTickSyntaxExpr hpcSrcSpan neg)
addTickHsExpr (HsPar e) = liftM HsPar (addTickLHsExpr' e)
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
		(addBinTickLHsExpr CondBinBox e1)
		(addTickLHsExprOptAlt True e2)
		(addTickLHsExprOptAlt True e3)
addTickHsExpr (HsLet binds e) =
	liftM2 HsLet
		(addTickHsLocalBinds binds)		-- to think about: !patterns.
		(addTickLHsExpr' e)
addTickHsExpr (HsDo cxt stmts last_exp srcloc) =
	liftM4 HsDo
		(return cxt)
		(mapM (liftL (addTickStmt forQual)) stmts)
		(addTickLHsExpr last_exp)
		(return srcloc)
  where
	forQual = case cxt of
		    ListComp -> Just QualBinBox
		    _        -> Nothing
addTickHsExpr (ExplicitList ty es) = 
	liftM2 ExplicitList 
		(return ty)
		(mapM (addTickLHsExpr) es)
addTickHsExpr (ExplicitPArr	 {}) = error "addTickHsExpr: ExplicitPArr"
addTickHsExpr (ExplicitTuple es box) =
	liftM2 ExplicitTuple
		(mapM (addTickLHsExpr) es)
		(return box)
addTickHsExpr (RecordCon	 id ty rec_binds) = 
	liftM3 RecordCon
		(return id)
		(return ty)
		(addTickHsRecordBinds rec_binds)
addTickHsExpr (RecordUpd	e rec_binds ty1 ty2) =
	liftM4 RecordUpd
		(addTickLHsExpr e)
		(addTickHsRecordBinds rec_binds)
		(return ty1)
		(return ty2)
addTickHsExpr (ExprWithTySig {}) = error "addTickHsExpr: ExprWithTySig"
addTickHsExpr (ExprWithTySigOut e ty) =
	liftM2 ExprWithTySigOut
		(addTickLHsExpr' e) -- No need to tick the inner expression
				    -- for expressions with signatures
		(return ty)
addTickHsExpr (ArithSeq	 ty arith_seq) =
	liftM2 ArithSeq	
		(return ty)
		(addTickArithSeqInfo arith_seq)
addTickHsExpr (HsTickPragma (file,(l1,c1),(l2,c2)) (L pos e0)) = do
    e1 <- addTickHsExpr e0
    fn <- allocTickBox (ExternalBox (unpackFS file) (P l1 c1 l2 c2)) pos
    let (L _ e2) = fn $ L pos e1
    return $ e2
addTickHsExpr (PArrSeq	 {}) = error "addTickHsExpr: PArrSeq"
addTickHsExpr (HsSCC	 {}) = error "addTickHsExpr: HsSCC"
addTickHsExpr (HsCoreAnn   {}) = error "addTickHsExpr: HsCoreAnn"
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

-- Should never happen in expression content.
addTickHsExpr (EAsPat _ _) = error "addTickHsExpr: EAsPat _ _"
addTickHsExpr (ELazyPat _) = error "addTickHsExpr: ELazyPat _"
addTickHsExpr (EWildPat) = error "addTickHsExpr: EWildPat"
addTickHsExpr (HsBinTick _ _ _) = error "addTickhsExpr: HsBinTick _ _ _"
addTickHsExpr (HsTick _ _) = error "addTickhsExpr: HsTick _ _"

addTickMatchGroup (MatchGroup matches ty) = do
  let isOneOfMany = matchesOneOfMany matches
  matches' <- mapM (liftL (addTickMatch isOneOfMany)) matches
  return $ MatchGroup matches' ty

addTickMatch :: Bool -> Match Id -> TM (Match Id)
addTickMatch isOneOfMany (Match pats opSig gRHSs) = do
  gRHSs' <- addTickGRHSs isOneOfMany gRHSs
  return $ Match pats opSig gRHSs'

addTickGRHSs :: Bool -> GRHSs Id -> TM (GRHSs Id)
addTickGRHSs isOneOfMany (GRHSs guarded local_binds) = do
  guarded' <- mapM (liftL (addTickGRHS isOneOfMany)) guarded
  local_binds' <- addTickHsLocalBinds local_binds
  return $ GRHSs guarded' local_binds'

addTickGRHS :: Bool -> GRHS Id -> TM (GRHS Id)
addTickGRHS isOneOfMany (GRHS stmts expr) = do
  stmts' <- mapM (liftL (addTickStmt (Just $ GuardBinBox))) stmts
  expr' <- if opt_Hpc then addTickLHsExprOptAlt isOneOfMany expr
                      else addTickLHsExprAlways expr 
  return $ GRHS stmts' expr'

addTickStmt :: (Maybe (Bool -> BoxLabel)) -> Stmt Id -> TM (Stmt Id)
addTickStmt isGuard (BindStmt pat e bind fail) =
	liftM4 BindStmt
		(addTickLPat pat)
		(addTickLHsExprBreakAlways e)
		(addTickSyntaxExpr hpcSrcSpan bind)
		(addTickSyntaxExpr hpcSrcSpan fail)
addTickStmt isGuard (ExprStmt e bind' ty) =
	liftM3 ExprStmt
		(addTick e)
		(addTickSyntaxExpr hpcSrcSpan bind')
		(return ty)
  where
   addTick e | Just fn <- isGuard = addBinTickLHsExpr fn e
             | otherwise          = addTickLHsExprBreakAlways e

addTickStmt isGuard (LetStmt binds) =
	liftM LetStmt
		(addTickHsLocalBinds binds)
addTickStmt isGuard (ParStmt pairs) =
  	liftM ParStmt (mapM process pairs)
  where
	process (stmts,ids) = 
		liftM2 (,) 
			(mapM (liftL (addTickStmt isGuard)) stmts)
			(return ids)
addTickStmt isGuard (RecStmt stmts ids1 ids2 tys dictbinds) =
  	liftM5 RecStmt 
		(mapM (liftL (addTickStmt isGuard)) stmts)
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
addTickHsRecordBinds (HsRecordBinds pairs) = liftM HsRecordBinds (mapM process pairs)
    where
	process (ids,expr) = 
		liftM2 (,) 
			(return ids)
			(addTickLHsExpr expr)			

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
data TickTransState = TT { modName     :: String
                         , declPath    :: [String]
                         , tickBoxCount:: Int
                         , mixEntries  :: [MixEntry]
                         }                        
	deriving Show

data TM a = TM { unTM :: TickTransState -> (a,TickTransState) }

instance Monad TM where
  return a = TM $ \ st -> (a,st)
  (TM m) >>= k = TM $ \ st -> case m st of
				(r1,st1) -> unTM (k r1) st1 

--addTick :: LHsExpr Id -> TM (LHsExpr Id)
--addTick e = TM $ \ uq -> (e,succ uq,[(uq,getLoc e)])

addPathEntry :: String -> TM a -> TM a
addPathEntry nm (TM m) = TM $ \ st -> case m (st { declPath = declPath st ++ [nm] }) of
					(r,st') -> (r,st' { declPath = declPath st })

getPathEntry :: TM [String]
getPathEntry = TM $ \ st -> (declPath st,st)

-- the tick application inherits the source position of its
-- expression argument to support nested box allocations 
allocTickBox :: BoxLabel -> SrcSpan -> TM (LHsExpr Id -> LHsExpr Id)
allocTickBox boxLabel pos | Just hpcPos <- mkHpcPos pos = TM $ \ st ->
  let me = (hpcPos,boxLabel)
      c = tickBoxCount st
      mes = mixEntries st
  in ( \ (L pos e) -> L pos $ HsTick c (L pos e)
     , st {tickBoxCount=c+1,mixEntries=me:mes}
     )
allocTickBox boxLabel e = return id

-- the tick application inherits the source position of its
-- expression argument to support nested box allocations 
allocATickBox :: BoxLabel -> SrcSpan -> TM (Maybe Int)
allocATickBox boxLabel pos | Just hpcPos <- mkHpcPos pos = TM $ \ st ->
  let me = (hpcPos,boxLabel)
      c = tickBoxCount st
      mes = mixEntries st
  in ( Just c
     , st {tickBoxCount=c+1,mixEntries=me:mes}
     )
allocATickBox boxLabel e = return Nothing

allocBinTickBox :: (Bool -> BoxLabel) -> LHsExpr Id -> TM (LHsExpr Id)
allocBinTickBox boxLabel (L pos e) | Just hpcPos <- mkHpcPos pos = TM $ \ st ->
  let meT = (hpcPos,boxLabel True)
      meF = (hpcPos,boxLabel False)
      meE = (hpcPos,ExpBox)
      c = tickBoxCount st
      mes = mixEntries st
  in 
     if opt_Hpc 
        then ( L pos $ HsTick c $ L pos $ HsBinTick (c+1) (c+2) (L pos e)
           -- notice that F and T are reversed,
           -- because we are building the list in
           -- reverse...
             , st {tickBoxCount=c+3,mixEntries=meF:meT:meE:mes}
             )
        else
             ( L pos $ HsTick c $ L pos e
             , st {tickBoxCount=c+1,mixEntries=meE:mes}
             )

allocBinTickBox boxLabel e = return e

mkHpcPos :: SrcSpan -> Maybe HpcPos
mkHpcPos pos 
   | not (isGoodSrcSpan pos) = Nothing
   | start == end            = Nothing	-- no actual location
   | otherwise		     = Just hpcPos
  where
   start = srcSpanStart pos
   end   = srcSpanEnd pos
   hpcPos = toHpcPos ( srcLocLine start
		     , srcLocCol start
		     , srcLocLine end
		     , srcLocCol end
		     )

hpcSrcSpan = mkGeneralSrcSpan (FSLIT("Haskell Program Coverage internals"))
\end{code}


\begin{code}
matchesOneOfMany :: [LMatch Id] -> Bool
matchesOneOfMany lmatches = sum (map matchCount lmatches) > 1
  where
	matchCount (L _ (Match _pats _ty (GRHSs grhss _binds))) = length grhss
\end{code}


\begin{code}
---------------------------------------------------------------
-- Datatypes and file-access routines for the per-module (.mix)
-- indexes used by Hpc.
-- Colin Runciman and Andy Gill, June 2006
---------------------------------------------------------------

-- a module index records the attributes of each tick-box that has
-- been introduced in that module, accessed by tick-number position
-- in the list

data Mix = Mix 
	     FilePath		-- location of original file
	     Integer		-- time (in seconds) of original file's last update, since 1970.
	     Int 		-- tab stop value 
	     [MixEntry] 	-- entries
	deriving (Show,Read)

-- We would rather use ClockTime in Mix, but ClockTime has no Read instance in 6.4 and before,
-- but does in 6.6. Definining the instance for ClockTime here is the Wrong Thing to do,
-- because if some other program also defined that instance, we will not be able to compile.

type MixEntry = (HpcPos, BoxLabel)

data BoxLabel = ExpBox
              | AltBox
              | TopLevelBox [String]
              | LocalBox [String]
              | GuardBinBox Bool
              | CondBinBox Bool
              | QualBinBox Bool
	      | ExternalBox String HpcPos
	      	   -- ^The position was generated from the named file/module,
		   -- with the stated position (inside the named file/module).
		   -- The HpcPos inside this MixEntry refers to the generated Haskell location.
              deriving (Read, Show)
                         
mixCreate :: String -> String -> Mix -> IO ()
mixCreate dirName modName mix =
   writeFile (mixName dirName modName) (show mix)

mixName :: FilePath -> String -> String
mixName dirName name = dirName ++ "/" ++ name ++ ".mix"

getModificationTime' :: FilePath -> IO Integer
getModificationTime' file = do
  (TOD sec _) <- System.Directory.getModificationTime file
  return $ sec

-- a program index records module names and numbers of tick-boxes
-- introduced in each module that has been transformed for coverage 

data HpcPos = P !Int !Int !Int !Int deriving (Eq)

toHpcPos :: (Int,Int,Int,Int) -> HpcPos
toHpcPos (l1,c1,l2,c2) = P l1 c1 l2 c2

instance Show HpcPos where
   show (P l1 c1 l2 c2) = show l1 ++ ':' : show c1 ++ '-' : show l2 ++ ':' : show c2

instance Read HpcPos where
  readsPrec _i pos = [(toHpcPos (read l1,read c1,read l2,read c2),after)]
      where
         (before,after)   = span (/= ',') pos
         (lhs,rhs)    = case span (/= '-') before of
	 		       (lhs,'-':rhs) -> (lhs,rhs)
			       (lhs,"")      -> (lhs,lhs)
         (l1,':':c1)	  = span (/= ':') lhs
         (l2,':':c2)	  = span (/= ':') rhs

\end{code}

