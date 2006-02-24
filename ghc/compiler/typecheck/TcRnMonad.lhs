\begin{code}
module TcRnMonad(
	module TcRnMonad,
	module TcRnTypes,
	module IOEnv
  ) where

#include "HsVersions.h"

import TcRnTypes	-- Re-export all
import IOEnv		-- Re-export all

import HsSyn		( emptyLHsBinds )
import HscTypes		( HscEnv(..), ModGuts(..), ModIface(..),
			  TyThing, TypeEnv, emptyTypeEnv, HscSource(..),
			  isHsBoot, ModSummary(..),
			  ExternalPackageState(..), HomePackageTable,
			  Deprecs(..), FixityEnv, FixItem, 
			  lookupType, unQualInScope )
import Module		( Module, unitModuleEnv )
import RdrName		( GlobalRdrEnv, emptyGlobalRdrEnv, 	
			  LocalRdrEnv, emptyLocalRdrEnv )
import Name		( Name, isInternalName, mkInternalName, tidyNameOcc, nameOccName, getSrcLoc )
import Type		( Type )
import TcType		( tcIsTyVarTy, tcGetTyVar )
import NameEnv		( extendNameEnvList, nameEnvElts )
import InstEnv		( emptyInstEnv )

import Var		( setTyVarName )
import VarSet		( emptyVarSet )
import VarEnv		( TidyEnv, emptyTidyEnv, extendVarEnv )
import ErrUtils		( Message, Messages, emptyMessages, errorsFound, 
			  mkWarnMsg, printErrorsAndWarnings,
			  mkLocMessage, mkLongErrMsg )
import Packages		( mkHomeModules )
import SrcLoc		( mkGeneralSrcSpan, isGoodSrcSpan, SrcSpan, Located(..) )
import NameEnv		( emptyNameEnv )
import NameSet		( NameSet, emptyDUs, emptyNameSet, unionNameSets, addOneToNameSet )
import OccName		( emptyOccEnv, tidyOccName )
import Bag		( emptyBag )
import Outputable
import UniqSupply	( UniqSupply, mkSplitUniqSupply, uniqFromSupply, splitUniqSupply )
import Unique		( Unique )
import DynFlags		( DynFlags(..), DynFlag(..), dopt, dopt_set, GhcMode )
import StaticFlags	( opt_PprStyle_Debug )
import Bag		( snocBag, unionBags )
import Panic		( showException )
 
import IO		( stderr )
import DATA_IOREF	( newIORef, readIORef )
import EXCEPTION	( Exception )
\end{code}



%************************************************************************
%*									*
			initTc
%*									*
%************************************************************************

\begin{code}
ioToTcRn :: IO r -> TcRn r
ioToTcRn = ioToIOEnv
\end{code}

\begin{code}
initTc :: HscEnv
       -> HscSource
       -> Module 
       -> TcM r
       -> IO (Messages, Maybe r)
		-- Nothing => error thrown by the thing inside
		-- (error messages should have been printed already)

initTc hsc_env hsc_src mod do_this
 = do { errs_var     <- newIORef (emptyBag, emptyBag) ;
      	tvs_var      <- newIORef emptyVarSet ;
	type_env_var <- newIORef emptyNameEnv ;
	dfuns_var    <- newIORef emptyNameSet ;
	keep_var     <- newIORef emptyNameSet ;
	th_var	     <- newIORef False ;
	dfun_n_var   <- newIORef 1 ;

      	let {
	     gbl_env = TcGblEnv {
		tcg_mod      = mod,
		tcg_src	     = hsc_src,
		tcg_rdr_env  = emptyGlobalRdrEnv,
		tcg_fix_env  = emptyNameEnv,
		tcg_default  = Nothing,
		tcg_type_env = emptyNameEnv,
		tcg_type_env_var = type_env_var,
		tcg_inst_env  = emptyInstEnv,
		tcg_inst_uses = dfuns_var,
		tcg_th_used   = th_var,
		tcg_exports  = emptyNameSet,
		tcg_imports  = init_imports,
		tcg_home_mods = home_mods,
		tcg_dus      = emptyDUs,
                tcg_rn_imports = Nothing,
                tcg_rn_exports = Nothing,
		tcg_rn_decls = Nothing,
		tcg_binds    = emptyLHsBinds,
		tcg_deprecs  = NoDeprecs,
		tcg_insts    = [],
		tcg_rules    = [],
		tcg_fords    = [],
		tcg_dfun_n   = dfun_n_var,
		tcg_keep     = keep_var
	     } ;
	     lcl_env = TcLclEnv {
		tcl_errs       = errs_var,
		tcl_loc	       = mkGeneralSrcSpan FSLIT("Top level"),
		tcl_ctxt       = [],
		tcl_rdr	       = emptyLocalRdrEnv,
		tcl_th_ctxt    = topStage,
		tcl_arrow_ctxt = NoArrowCtxt,
		tcl_env        = emptyNameEnv,
		tcl_tyvars     = tvs_var,
		tcl_lie	       = panic "initTc:LIE"	-- LIE only valid inside a getLIE
	     } ;
	} ;
   
	-- OK, here's the business end!
	maybe_res <- initTcRnIf 'a' hsc_env gbl_env lcl_env $
			     do { r <- tryM do_this 
				; case r of
				    Right res -> return (Just res)
				    Left _    -> return Nothing } ;

	-- Collect any error messages
	msgs <- readIORef errs_var ;

	let { dflags = hsc_dflags hsc_env
	    ; final_res | errorsFound dflags msgs = Nothing
			| otherwise	   	  = maybe_res } ;

	return (msgs, final_res)
    }
  where
    home_mods = mkHomeModules (map ms_mod (hsc_mod_graph hsc_env))
	-- A guess at the home modules.  This will be correct in
	-- --make and GHCi modes, but in one-shot mode we need to 
	-- fix it up after we know the real dependencies of the current
	-- module (see tcRnModule).
	-- Setting it here is necessary for the typechecker entry points
	-- other than tcRnModule: tcRnGetInfo, for example.  These are
	-- all called via the GHC module, so hsc_mod_graph will contain
	-- something sensible.

    init_imports = emptyImportAvails {imp_env = unitModuleEnv mod emptyNameSet}
	-- Initialise tcg_imports with an empty set of bindings for
	-- this module, so that if we see 'module M' in the export
	-- list, and there are no bindings in M, we don't bleat 
	-- "unknown module M".

initTcPrintErrors	-- Used from the interactive loop only
       :: HscEnv
       -> Module 
       -> TcM r
       -> IO (Maybe r)
initTcPrintErrors env mod todo = do
  (msgs, res) <- initTc env HsSrcFile mod todo
  printErrorsAndWarnings (hsc_dflags env) msgs
  return res

-- mkImpTypeEnv makes the imported symbol table
mkImpTypeEnv :: ExternalPackageState -> HomePackageTable
 	     -> Name -> Maybe TyThing
mkImpTypeEnv pcs hpt = lookup 
  where
    pte = eps_PTE pcs
    lookup name | isInternalName name = Nothing
	        | otherwise	      = lookupType hpt pte name
\end{code}


%************************************************************************
%*									*
		Initialisation
%*									*
%************************************************************************


\begin{code}
initTcRnIf :: Char		-- Tag for unique supply
	   -> HscEnv
	   -> gbl -> lcl 
	   -> TcRnIf gbl lcl a 
	   -> IO a
initTcRnIf uniq_tag hsc_env gbl_env lcl_env thing_inside
   = do	{ us     <- mkSplitUniqSupply uniq_tag ;
	; us_var <- newIORef us ;

	; let { env = Env { env_top = hsc_env,
			    env_us  = us_var,
			    env_gbl = gbl_env,
			    env_lcl = lcl_env } }

	; runIOEnv env thing_inside
	}
\end{code}

%************************************************************************
%*									*
		Simple accessors
%*									*
%************************************************************************

\begin{code}
getTopEnv :: TcRnIf gbl lcl HscEnv
getTopEnv = do { env <- getEnv; return (env_top env) }

getGblEnv :: TcRnIf gbl lcl gbl
getGblEnv = do { env <- getEnv; return (env_gbl env) }

updGblEnv :: (gbl -> gbl) -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
updGblEnv upd = updEnv (\ env@(Env { env_gbl = gbl }) -> 
			  env { env_gbl = upd gbl })

setGblEnv :: gbl -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
setGblEnv gbl_env = updEnv (\ env -> env { env_gbl = gbl_env })

getLclEnv :: TcRnIf gbl lcl lcl
getLclEnv = do { env <- getEnv; return (env_lcl env) }

updLclEnv :: (lcl -> lcl) -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
updLclEnv upd = updEnv (\ env@(Env { env_lcl = lcl }) -> 
			  env { env_lcl = upd lcl })

setLclEnv :: lcl' -> TcRnIf gbl lcl' a -> TcRnIf gbl lcl a
setLclEnv lcl_env = updEnv (\ env -> env { env_lcl = lcl_env })

getEnvs :: TcRnIf gbl lcl (gbl, lcl)
getEnvs = do { env <- getEnv; return (env_gbl env, env_lcl env) }

setEnvs :: (gbl', lcl') -> TcRnIf gbl' lcl' a -> TcRnIf gbl lcl a
setEnvs (gbl_env, lcl_env) = updEnv (\ env -> env { env_gbl = gbl_env, env_lcl = lcl_env })
\end{code}


Command-line flags

\begin{code}
getDOpts :: TcRnIf gbl lcl DynFlags
getDOpts = do { env <- getTopEnv; return (hsc_dflags env) }

doptM :: DynFlag -> TcRnIf gbl lcl Bool
doptM flag = do { dflags <- getDOpts; return (dopt flag dflags) }

setOptM :: DynFlag -> TcRnIf gbl lcl a -> TcRnIf gbl lcl a
setOptM flag = updEnv (\ env@(Env { env_top = top }) ->
			 env { env_top = top { hsc_dflags = dopt_set (hsc_dflags top) flag}} )

ifOptM :: DynFlag -> TcRnIf gbl lcl () -> TcRnIf gbl lcl ()	-- Do it flag is true
ifOptM flag thing_inside = do { b <- doptM flag; 
				if b then thing_inside else return () }

getGhciMode :: TcRnIf gbl lcl GhcMode
getGhciMode = do { env <- getTopEnv; return (ghcMode (hsc_dflags env)) }
\end{code}

\begin{code}
getEpsVar :: TcRnIf gbl lcl (TcRef ExternalPackageState)
getEpsVar = do { env <- getTopEnv; return (hsc_EPS env) }

getEps :: TcRnIf gbl lcl ExternalPackageState
getEps = do { env <- getTopEnv; readMutVar (hsc_EPS env) }

-- Updating the EPS.  This should be an atomic operation.
-- Note the delicate 'seq' which forces the EPS before putting it in the
-- variable.  Otherwise what happens is that we get
--	write eps_var (....(unsafeRead eps_var)....)
-- and if the .... is strict, that's obviously bottom.  By forcing it beforehand
-- we make the unsafeRead happen before we update the variable.

updateEps :: (ExternalPackageState -> (ExternalPackageState, a))
	  -> TcRnIf gbl lcl a
updateEps upd_fn = do	{ traceIf (text "updating EPS")
			; eps_var <- getEpsVar
			; eps <- readMutVar eps_var
			; let { (eps', val) = upd_fn eps }
			; seq eps' (writeMutVar eps_var eps')
			; return val }

updateEps_ :: (ExternalPackageState -> ExternalPackageState)
	   -> TcRnIf gbl lcl ()
updateEps_ upd_fn = do	{ traceIf (text "updating EPS_")
			; eps_var <- getEpsVar
			; eps <- readMutVar eps_var
			; let { eps' = upd_fn eps }
			; seq eps' (writeMutVar eps_var eps') }

getHpt :: TcRnIf gbl lcl HomePackageTable
getHpt = do { env <- getTopEnv; return (hsc_HPT env) }

getEpsAndHpt :: TcRnIf gbl lcl (ExternalPackageState, HomePackageTable)
getEpsAndHpt = do { env <- getTopEnv; eps <- readMutVar (hsc_EPS env)
		  ; return (eps, hsc_HPT env) }
\end{code}

%************************************************************************
%*									*
		Unique supply
%*									*
%************************************************************************

\begin{code}
newUnique :: TcRnIf gbl lcl Unique
newUnique = do { us <- newUniqueSupply ; 
		 return (uniqFromSupply us) }

newUniqueSupply :: TcRnIf gbl lcl UniqSupply
newUniqueSupply
 = do { env <- getEnv ;
	let { u_var = env_us env } ;
	us <- readMutVar u_var ;
    	let { (us1, us2) = splitUniqSupply us } ;
	writeMutVar u_var us1 ;
	return us2 }

newLocalName :: Name -> TcRnIf gbl lcl Name
newLocalName name	-- Make a clone
  = newUnique		`thenM` \ uniq ->
    returnM (mkInternalName uniq (nameOccName name) (getSrcLoc name))
\end{code}


%************************************************************************
%*									*
		Debugging
%*									*
%************************************************************************

\begin{code}
traceTc, traceRn :: SDoc -> TcRn ()
traceRn      = traceOptTcRn Opt_D_dump_rn_trace
traceTc      = traceOptTcRn Opt_D_dump_tc_trace
traceSplice  = traceOptTcRn Opt_D_dump_splices


traceIf :: SDoc -> TcRnIf m n ()	
traceIf      = traceOptIf Opt_D_dump_if_trace
traceHiDiffs = traceOptIf Opt_D_dump_hi_diffs


traceOptIf :: DynFlag -> SDoc -> TcRnIf m n ()  -- No RdrEnv available, so qualify everything
traceOptIf flag doc = ifOptM flag $
		     ioToIOEnv (printForUser stderr alwaysQualify doc)

traceOptTcRn :: DynFlag -> SDoc -> TcRn ()
traceOptTcRn flag doc = ifOptM flag $ do
			{ ctxt <- getErrCtxt
			; loc  <- getSrcSpanM
			; env0 <- tcInitTidyEnv
			; ctxt_msgs <- do_ctxt env0 ctxt 
			; let real_doc = mkLocMessage loc (vcat (doc : ctxt_to_use ctxt_msgs))
			; dumpTcRn real_doc }

dumpTcRn :: SDoc -> TcRn ()
dumpTcRn doc = do { rdr_env <- getGlobalRdrEnv ;
		    ioToTcRn (printForUser stderr (unQualInScope rdr_env) doc) }

dumpOptTcRn :: DynFlag -> SDoc -> TcRn ()
dumpOptTcRn flag doc = ifOptM flag (dumpTcRn doc)
\end{code}


%************************************************************************
%*									*
		Typechecker global environment
%*									*
%************************************************************************

\begin{code}
getModule :: TcRn Module
getModule = do { env <- getGblEnv; return (tcg_mod env) }

setModule :: Module -> TcRn a -> TcRn a
setModule mod thing_inside = updGblEnv (\env -> env { tcg_mod = mod }) thing_inside

tcIsHsBoot :: TcRn Bool
tcIsHsBoot = do { env <- getGblEnv; return (isHsBoot (tcg_src env)) }

getGlobalRdrEnv :: TcRn GlobalRdrEnv
getGlobalRdrEnv = do { env <- getGblEnv; return (tcg_rdr_env env) }

getImports :: TcRn ImportAvails
getImports = do { env <- getGblEnv; return (tcg_imports env) }

getFixityEnv :: TcRn FixityEnv
getFixityEnv = do { env <- getGblEnv; return (tcg_fix_env env) }

extendFixityEnv :: [(Name,FixItem)] -> RnM a -> RnM a
extendFixityEnv new_bit
  = updGblEnv (\env@(TcGblEnv { tcg_fix_env = old_fix_env }) -> 
		env {tcg_fix_env = extendNameEnvList old_fix_env new_bit})	     

getDefaultTys :: TcRn (Maybe [Type])
getDefaultTys = do { env <- getGblEnv; return (tcg_default env) }
\end{code}

%************************************************************************
%*									*
		Error management
%*									*
%************************************************************************

\begin{code}
getSrcSpanM :: TcRn SrcSpan
	-- Avoid clash with Name.getSrcLoc
getSrcSpanM = do { env <- getLclEnv; return (tcl_loc env) }

setSrcSpan :: SrcSpan -> TcRn a -> TcRn a
setSrcSpan loc thing_inside
  | isGoodSrcSpan loc = updLclEnv (\env -> env { tcl_loc = loc }) thing_inside
  | otherwise	      = thing_inside	-- Don't overwrite useful info with useless

addLocM :: (a -> TcM b) -> Located a -> TcM b
addLocM fn (L loc a) = setSrcSpan loc $ fn a

wrapLocM :: (a -> TcM b) -> Located a -> TcM (Located b)
wrapLocM fn (L loc a) = setSrcSpan loc $ do b <- fn a; return (L loc b)

wrapLocFstM :: (a -> TcM (b,c)) -> Located a -> TcM (Located b, c)
wrapLocFstM fn (L loc a) =
  setSrcSpan loc $ do
    (b,c) <- fn a
    return (L loc b, c)

wrapLocSndM :: (a -> TcM (b,c)) -> Located a -> TcM (b, Located c)
wrapLocSndM fn (L loc a) =
  setSrcSpan loc $ do
    (b,c) <- fn a
    return (b, L loc c)
\end{code}


\begin{code}
getErrsVar :: TcRn (TcRef Messages)
getErrsVar = do { env <- getLclEnv; return (tcl_errs env) }

setErrsVar :: TcRef Messages -> TcRn a -> TcRn a
setErrsVar v = updLclEnv (\ env -> env { tcl_errs =  v })

addErr :: Message -> TcRn ()
addErr msg = do { loc <- getSrcSpanM ; addErrAt loc msg }

addLocErr :: Located e -> (e -> Message) -> TcRn ()
addLocErr (L loc e) fn = addErrAt loc (fn e)

addErrAt :: SrcSpan -> Message -> TcRn ()
addErrAt loc msg = addLongErrAt loc msg empty

addLongErrAt :: SrcSpan -> Message -> Message -> TcRn ()
addLongErrAt loc msg extra
  = do { traceTc (ptext SLIT("Adding error:") <+> (mkLocMessage loc (msg $$ extra))) ;	
	 errs_var <- getErrsVar ;
	 rdr_env <- getGlobalRdrEnv ;
	 let { err = mkLongErrMsg loc (unQualInScope rdr_env) msg extra } ;
	 (warns, errs) <- readMutVar errs_var ;
  	 writeMutVar errs_var (warns, errs `snocBag` err) }

addErrs :: [(SrcSpan,Message)] -> TcRn ()
addErrs msgs = mappM_ add msgs
	     where
	       add (loc,msg) = addErrAt loc msg

addReport :: Message -> TcRn ()
addReport msg = do loc <- getSrcSpanM; addReportAt loc msg

addReportAt :: SrcSpan -> Message -> TcRn ()
addReportAt loc msg
  = do { errs_var <- getErrsVar ;
	 rdr_env <- getGlobalRdrEnv ;
	 let { warn = mkWarnMsg loc (unQualInScope rdr_env) msg } ;
	 (warns, errs) <- readMutVar errs_var ;
  	 writeMutVar errs_var (warns `snocBag` warn, errs) }

addWarn :: Message -> TcRn ()
addWarn msg = addReport (ptext SLIT("Warning:") <+> msg)

addWarnAt :: SrcSpan -> Message -> TcRn ()
addWarnAt loc msg = addReportAt loc (ptext SLIT("Warning:") <+> msg)

addLocWarn :: Located e -> (e -> Message) -> TcRn ()
addLocWarn (L loc e) fn = addReportAt loc (fn e)

checkErr :: Bool -> Message -> TcRn ()
-- Add the error if the bool is False
checkErr ok msg = checkM ok (addErr msg)

warnIf :: Bool -> Message -> TcRn ()
warnIf True  msg = addWarn msg
warnIf False msg = return ()

addMessages :: Messages -> TcRn ()
addMessages (m_warns, m_errs)
  = do { errs_var <- getErrsVar ;
	 (warns, errs) <- readMutVar errs_var ;
  	 writeMutVar errs_var (warns `unionBags` m_warns,
			       errs  `unionBags` m_errs) }

discardWarnings :: TcRn a -> TcRn a
-- Ignore warnings inside the thing inside;
-- used to ignore-unused-variable warnings inside derived code
-- With -dppr-debug, the effects is switched off, so you can still see
-- what warnings derived code would give
discardWarnings thing_inside
  | opt_PprStyle_Debug = thing_inside
  | otherwise
  = do	{ errs_var <- newMutVar emptyMessages
	; result <- setErrsVar errs_var thing_inside
	; (_warns, errs) <- readMutVar errs_var
	; addMessages (emptyBag, errs)
	; return result }
\end{code}


\begin{code}
try_m :: TcRn r -> TcRn (Either Exception r)
-- Does try_m, with a debug-trace on failure
try_m thing 
  = do { mb_r <- tryM thing ;
	 case mb_r of 
	     Left exn -> do { traceTc (exn_msg exn); return mb_r }
	     Right r  -> return mb_r }
  where
    exn_msg exn = text "tryTc/recoverM recovering from" <+> text (showException exn)

-----------------------
recoverM :: TcRn r 	-- Recovery action; do this if the main one fails
	 -> TcRn r	-- Main action: do this first
	 -> TcRn r
-- Errors in 'thing' are retained
recoverM recover thing 
  = do { mb_res <- try_m thing ;
	 case mb_res of
	   Left exn  -> recover
	   Right res -> returnM res }

-----------------------
tryTc :: TcRn a -> TcRn (Messages, Maybe a)
-- (tryTc m) executes m, and returns
--	Just r,  if m succeeds (returning r)
--	Nothing, if m fails
-- It also returns all the errors and warnings accumulated by m
-- It always succeeds (never raises an exception)
tryTc m 
 = do {	errs_var <- newMutVar emptyMessages ;
	res  <- try_m (setErrsVar errs_var m) ; 
	msgs <- readMutVar errs_var ;
	return (msgs, case res of
			    Left exn  -> Nothing
			    Right val -> Just val)
	-- The exception is always the IOEnv built-in
	-- in exception; see IOEnv.failM
   }

-----------------------
tryTcErrs :: TcRn a -> TcRn (Messages, Maybe a)
-- Run the thing, returning 
--	Just r,  if m succceeds with no error messages
--	Nothing, if m fails, or if it succeeds but has error messages
-- Either way, the messages are returned; even in the Just case
-- there might be warnings
tryTcErrs thing 
  = do  { (msgs, res) <- tryTc thing
	; dflags <- getDOpts
	; let errs_found = errorsFound dflags msgs
	; return (msgs, case res of
			  Nothing -> Nothing
			  Just val | errs_found -> Nothing
			 	   | otherwise  -> Just val)
	}

-----------------------
tryTcLIE :: TcM a -> TcM (Messages, Maybe a)
-- Just like tryTcErrs, except that it ensures that the LIE
-- for the thing is propagated only if there are no errors
-- Hence it's restricted to the type-check monad
tryTcLIE thing_inside
  = do  { ((msgs, mb_res), lie) <- getLIE (tryTcErrs thing_inside) ;
	; case mb_res of
	    Nothing  -> return (msgs, Nothing)
	    Just val -> do { extendLIEs lie; return (msgs, Just val) }
	}

-----------------------
tryTcLIE_ :: TcM r -> TcM r -> TcM r
-- (tryTcLIE_ r m) tries m; 
--	if m succeeds with no error messages, it's the answer
-- 	otherwise tryTcLIE_ drops everything from m and tries r instead.
tryTcLIE_ recover main
  = do	{ (msgs, mb_res) <- tryTcLIE main
	; case mb_res of
	     Just val -> do { addMessages msgs	-- There might be warnings
			     ; return val }
	     Nothing  -> recover		-- Discard all msgs
	}

-----------------------
checkNoErrs :: TcM r -> TcM r
-- (checkNoErrs m) succeeds iff m succeeds and generates no errors
-- If m fails then (checkNoErrsTc m) fails.
-- If m succeeds, it checks whether m generated any errors messages
--	(it might have recovered internally)
-- 	If so, it fails too.
-- Regardless, any errors generated by m are propagated to the enclosing context.
checkNoErrs main
  = do	{ (msgs, mb_res) <- tryTcLIE main
	; addMessages msgs
	; case mb_res of
	    Nothing   -> failM
	    Just val -> return val
	} 

ifErrsM :: TcRn r -> TcRn r -> TcRn r
--	ifErrsM bale_out main
-- does 'bale_out' if there are errors in errors collection
-- otherwise does 'main'
ifErrsM bale_out normal
 = do { errs_var <- getErrsVar ;
	msgs <- readMutVar errs_var ;
	dflags <- getDOpts ;
	if errorsFound dflags msgs then
	   bale_out
	else	
	   normal }

failIfErrsM :: TcRn ()
-- Useful to avoid error cascades
failIfErrsM = ifErrsM failM (return ())
\end{code}


%************************************************************************
%*									*
	Context management and error message generation
	  	    for the type checker
%*									*
%************************************************************************

\begin{code}
getErrCtxt :: TcM ErrCtxt
getErrCtxt = do { env <- getLclEnv; return (tcl_ctxt env) }

setErrCtxt :: ErrCtxt -> TcM a -> TcM a
setErrCtxt ctxt = updLclEnv (\ env -> env { tcl_ctxt = ctxt })

addErrCtxt :: Message -> TcM a -> TcM a
addErrCtxt msg = addErrCtxtM (\env -> returnM (env, msg))

addErrCtxtM :: (TidyEnv -> TcM (TidyEnv, Message)) -> TcM a -> TcM a
addErrCtxtM msg = updCtxt (\ msgs -> msg : msgs)

-- Helper function for the above
updCtxt :: (ErrCtxt -> ErrCtxt) -> TcM a -> TcM a
updCtxt upd = updLclEnv (\ env@(TcLclEnv { tcl_ctxt = ctxt }) -> 
			   env { tcl_ctxt = upd ctxt })

-- Conditionally add an error context
maybeAddErrCtxt :: Maybe Message -> TcM a -> TcM a
maybeAddErrCtxt (Just msg) thing_inside = addErrCtxt msg thing_inside
maybeAddErrCtxt Nothing    thing_inside = thing_inside

popErrCtxt :: TcM a -> TcM a
popErrCtxt = updCtxt (\ msgs -> case msgs of { [] -> []; (m:ms) -> ms })

getInstLoc :: InstOrigin -> TcM InstLoc
getInstLoc origin
  = do { loc <- getSrcSpanM ; env <- getLclEnv ;
	 return (InstLoc origin loc (tcl_ctxt env)) }

addInstCtxt :: InstLoc -> TcM a -> TcM a
-- Add the SrcSpan and context from the first Inst in the list
-- 	(they all have similar locations)
addInstCtxt (InstLoc _ src_loc ctxt) thing_inside
  = setSrcSpan src_loc (updCtxt (\ old_ctxt -> ctxt) thing_inside)
\end{code}

    The addErrTc functions add an error message, but do not cause failure.
    The 'M' variants pass a TidyEnv that has already been used to
    tidy up the message; we then use it to tidy the context messages

\begin{code}
addErrTc :: Message -> TcM ()
addErrTc err_msg = do { env0 <- tcInitTidyEnv
		      ; addErrTcM (env0, err_msg) }

addErrsTc :: [Message] -> TcM ()
addErrsTc err_msgs = mappM_ addErrTc err_msgs

addErrTcM :: (TidyEnv, Message) -> TcM ()
addErrTcM (tidy_env, err_msg)
  = do { ctxt <- getErrCtxt ;
	 loc  <- getSrcSpanM ;
	 add_err_tcm tidy_env err_msg loc ctxt }
\end{code}

The failWith functions add an error message and cause failure

\begin{code}
failWithTc :: Message -> TcM a		     -- Add an error message and fail
failWithTc err_msg 
  = addErrTc err_msg >> failM

failWithTcM :: (TidyEnv, Message) -> TcM a   -- Add an error message and fail
failWithTcM local_and_msg
  = addErrTcM local_and_msg >> failM

checkTc :: Bool -> Message -> TcM ()	     -- Check that the boolean is true
checkTc True  err = returnM ()
checkTc False err = failWithTc err
\end{code}

	Warnings have no 'M' variant, nor failure

\begin{code}
addWarnTc :: Message -> TcM ()
addWarnTc msg
 = do { ctxt <- getErrCtxt ;
	env0 <- tcInitTidyEnv ;
	ctxt_msgs <- do_ctxt env0 ctxt ;
	addWarn (vcat (msg : ctxt_to_use ctxt_msgs)) }

warnTc :: Bool -> Message -> TcM ()
warnTc warn_if_true warn_msg
  | warn_if_true = addWarnTc warn_msg
  | otherwise	 = return ()
\end{code}

-----------------------------------
	 Tidying

We initialise the "tidy-env", used for tidying types before printing,
by building a reverse map from the in-scope type variables to the
OccName that the programmer originally used for them

\begin{code}
tcInitTidyEnv :: TcM TidyEnv
tcInitTidyEnv
  = do	{ lcl_env <- getLclEnv
	; let nm_tv_prs = [ (name, tcGetTyVar "tcInitTidyEnv" ty)
			  | ATyVar name ty <- nameEnvElts (tcl_env lcl_env)
			  , tcIsTyVarTy ty ]
	; return (foldl add emptyTidyEnv nm_tv_prs) }
  where
    add (env,subst) (name, tyvar)
	= case tidyOccName env (nameOccName name) of
	    (env', occ') ->  (env', extendVarEnv subst tyvar tyvar')
		where
		  tyvar' = setTyVarName tyvar name'
		  name'  = tidyNameOcc name occ'
\end{code}

-----------------------------------
 	Other helper functions

\begin{code}
add_err_tcm tidy_env err_msg loc ctxt
 = do { ctxt_msgs <- do_ctxt tidy_env ctxt ;
	addLongErrAt loc err_msg (vcat (ctxt_to_use ctxt_msgs)) }

do_ctxt tidy_env []
 = return []
do_ctxt tidy_env (c:cs)
 = do {	(tidy_env', m) <- c tidy_env  ;
	ms	       <- do_ctxt tidy_env' cs  ;
	return (m:ms) }

ctxt_to_use ctxt | opt_PprStyle_Debug = ctxt
		 | otherwise	      = take 3 ctxt
\end{code}

debugTc is useful for monadic debugging code

\begin{code}
debugTc :: TcM () -> TcM ()
#ifdef DEBUG
debugTc thing = thing
#else
debugTc thing = return ()
#endif
\end{code}

 %************************************************************************
%*									*
	     Type constraints (the so-called LIE)
%*									*
%************************************************************************

\begin{code}
nextDFunIndex :: TcM Int	-- Get the next dfun index
nextDFunIndex = do { env <- getGblEnv
		   ; let dfun_n_var = tcg_dfun_n env
		   ; n <- readMutVar dfun_n_var
		   ; writeMutVar dfun_n_var (n+1)
		   ; return n }

getLIEVar :: TcM (TcRef LIE)
getLIEVar = do { env <- getLclEnv; return (tcl_lie env) }

setLIEVar :: TcRef LIE -> TcM a -> TcM a
setLIEVar lie_var = updLclEnv (\ env -> env { tcl_lie = lie_var })

getLIE :: TcM a -> TcM (a, [Inst])
-- (getLIE m) runs m, and returns the type constraints it generates
getLIE thing_inside
  = do { lie_var <- newMutVar emptyLIE ;
	 res <- updLclEnv (\ env -> env { tcl_lie = lie_var }) 
			  thing_inside ;
	 lie <- readMutVar lie_var ;
	 return (res, lieToList lie) }

extendLIE :: Inst -> TcM ()
extendLIE inst
  = do { lie_var <- getLIEVar ;
	 lie <- readMutVar lie_var ;
	 writeMutVar lie_var (inst `consLIE` lie) }

extendLIEs :: [Inst] -> TcM ()
extendLIEs [] 
  = returnM ()
extendLIEs insts
  = do { lie_var <- getLIEVar ;
	 lie <- readMutVar lie_var ;
	 writeMutVar lie_var (mkLIE insts `plusLIE` lie) }
\end{code}

\begin{code}
setLclTypeEnv :: TcLclEnv -> TcM a -> TcM a
-- Set the local type envt, but do *not* disturb other fields,
-- notably the lie_var
setLclTypeEnv lcl_env thing_inside
  = updLclEnv upd thing_inside
  where
    upd env = env { tcl_env = tcl_env lcl_env,
		    tcl_tyvars = tcl_tyvars lcl_env }
\end{code}


%************************************************************************
%*									*
	     Template Haskell context
%*									*
%************************************************************************

\begin{code}
recordThUse :: TcM ()
recordThUse = do { env <- getGblEnv; writeMutVar (tcg_th_used env) True }

keepAliveTc :: Name -> TcM ()  	-- Record the name in the keep-alive set
keepAliveTc n = do { env <- getGblEnv; 
		   ; updMutVar (tcg_keep env) (`addOneToNameSet` n) }

keepAliveSetTc :: NameSet -> TcM ()  	-- Record the name in the keep-alive set
keepAliveSetTc ns = do { env <- getGblEnv; 
		       ; updMutVar (tcg_keep env) (`unionNameSets` ns) }

getStage :: TcM ThStage
getStage = do { env <- getLclEnv; return (tcl_th_ctxt env) }

setStage :: ThStage -> TcM a -> TcM a 
setStage s = updLclEnv (\ env -> env { tcl_th_ctxt = s })
\end{code}


%************************************************************************
%*									*
	     Stuff for the renamer's local env
%*									*
%************************************************************************

\begin{code}
getLocalRdrEnv :: RnM LocalRdrEnv
getLocalRdrEnv = do { env <- getLclEnv; return (tcl_rdr env) }

setLocalRdrEnv :: LocalRdrEnv -> RnM a -> RnM a
setLocalRdrEnv rdr_env thing_inside 
  = updLclEnv (\env -> env {tcl_rdr = rdr_env}) thing_inside
\end{code}


%************************************************************************
%*									*
	     Stuff for interface decls
%*									*
%************************************************************************

\begin{code}
mkIfLclEnv :: Module -> SDoc -> IfLclEnv
mkIfLclEnv mod loc = IfLclEnv { if_mod     = mod,
			        if_loc     = loc,
			        if_tv_env  = emptyOccEnv,
			        if_id_env  = emptyOccEnv }

initIfaceTcRn :: IfG a -> TcRn a
initIfaceTcRn thing_inside
  = do  { tcg_env <- getGblEnv 
	; let { if_env = IfGblEnv { if_rec_types = Just (tcg_mod tcg_env, get_type_env) }
	      ; get_type_env = readMutVar (tcg_type_env_var tcg_env) }
	; setEnvs (if_env, ()) thing_inside }

initIfaceExtCore :: IfL a -> TcRn a
initIfaceExtCore thing_inside
  = do  { tcg_env <- getGblEnv 
	; let { mod = tcg_mod tcg_env
	      ; doc = ptext SLIT("External Core file for") <+> quotes (ppr mod)
	      ; if_env = IfGblEnv { 
			if_rec_types = Just (mod, return (tcg_type_env tcg_env)) }
	      ; if_lenv = mkIfLclEnv mod doc
	  }
	; setEnvs (if_env, if_lenv) thing_inside }

initIfaceCheck :: HscEnv -> IfG a -> IO a
-- Used when checking the up-to-date-ness of the old Iface
-- Initialise the environment with no useful info at all
initIfaceCheck hsc_env do_this
 = do	{ let gbl_env = IfGblEnv { if_rec_types = Nothing }
	; initTcRnIf 'i' hsc_env gbl_env () do_this
    }

initIfaceTc :: ModIface 
 	    -> (TcRef TypeEnv -> IfL a) -> TcRnIf gbl lcl a
-- Used when type-checking checking an up-to-date interface file
-- No type envt from the current module, but we do know the module dependencies
initIfaceTc iface do_this
 = do	{ tc_env_var <- newMutVar emptyTypeEnv
	; let { gbl_env = IfGblEnv { if_rec_types = Just (mod, readMutVar tc_env_var) } ;
	      ; if_lenv = mkIfLclEnv mod doc
	   }
	; setEnvs (gbl_env, if_lenv) (do_this tc_env_var)
    }
  where
    mod = mi_module iface
    doc = ptext SLIT("The interface for") <+> quotes (ppr mod)

initIfaceRules :: HscEnv -> ModGuts -> IfG a -> IO a
-- Used when sucking in new Rules in SimplCore
-- We have available the type envt of the module being compiled, and we must use it
initIfaceRules hsc_env guts do_this
 = do	{ let {
	     type_info = (mg_module guts, return (mg_types guts))
	   ; gbl_env = IfGblEnv { if_rec_types = Just type_info } ;
	   }

	-- Run the thing; any exceptions just bubble out from here
	; initTcRnIf 'i' hsc_env gbl_env () do_this
    }

initIfaceLcl :: Module -> SDoc -> IfL a -> IfM lcl a
initIfaceLcl mod loc_doc thing_inside 
  = setLclEnv (mkIfLclEnv mod loc_doc) thing_inside

getIfModule :: IfL Module
getIfModule = do { env <- getLclEnv; return (if_mod env) }

--------------------
failIfM :: Message -> IfL a
-- The Iface monad doesn't have a place to accumulate errors, so we
-- just fall over fast if one happens; it "shouldnt happen".
-- We use IfL here so that we can get context info out of the local env
failIfM msg
  = do 	{ env <- getLclEnv
	; let full_msg = (if_loc env <> colon) $$ nest 2 msg
	; ioToIOEnv (printErrs (full_msg defaultErrStyle))
	; failM }

--------------------
forkM_maybe :: SDoc -> IfL a -> IfL (Maybe a)
-- Run thing_inside in an interleaved thread.  
-- It shares everything with the parent thread, so this is DANGEROUS.  
--
-- It returns Nothing if the computation fails
-- 
-- It's used for lazily type-checking interface
-- signatures, which is pretty benign

forkM_maybe doc thing_inside
 = do {	unsafeInterleaveM $
	do { traceIf (text "Starting fork {" <+> doc)
	   ; mb_res <- tryM thing_inside ;
	     case mb_res of
		Right r  -> do	{ traceIf (text "} ending fork" <+> doc)
				; return (Just r) }
		Left exn -> do {

		    -- Bleat about errors in the forked thread, if -ddump-if-trace is on
		    -- Otherwise we silently discard errors. Errors can legitimately
		    -- happen when compiling interface signatures (see tcInterfaceSigs)
		      ifOptM Opt_D_dump_if_trace 
			     (print_errs (hang (text "forkM failed:" <+> doc)
				             4 (text (show exn))))

		    ; traceIf (text "} ending fork (badly)" <+> doc)
	  	    ; return Nothing }
	}}
  where
    print_errs sdoc = ioToIOEnv (printErrs (sdoc defaultErrStyle))

forkM :: SDoc -> IfL a -> IfL a
forkM doc thing_inside
 = do	{ mb_res <- forkM_maybe doc thing_inside
	; return (case mb_res of 
			Nothing -> pgmError "Cannot continue after interface file error"
				   -- pprPanic "forkM" doc
			Just r  -> r) }
\end{code}


