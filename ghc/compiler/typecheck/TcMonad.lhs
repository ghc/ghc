\begin{code}
module TcMonad(
	TcType, 
	TcTauType, TcPredType, TcThetaType, TcRhoType,
	TcTyVar, TcTyVarSet, TcClassContext,
	TcKind,

	TcM, NF_TcM, TcDown, TcEnv, 

	initTc,
	returnTc, thenTc, thenTc_, mapTc, mapTc_, listTc,
	foldrTc, foldlTc, mapAndUnzipTc, mapAndUnzip3Tc,
	mapBagTc, fixTc, tryTc, tryTc_, getErrsTc, 
	traceTc, ioToTc,

	uniqSMToTcM,

	returnNF_Tc, thenNF_Tc, thenNF_Tc_, mapNF_Tc, 
	fixNF_Tc, forkNF_Tc, foldrNF_Tc, foldlNF_Tc,

	listNF_Tc, mapAndUnzipNF_Tc, mapBagNF_Tc,

	checkTc, checkTcM, checkMaybeTc, checkMaybeTcM, 
	failTc, failWithTc, addErrTc, addErrsTc, warnTc, 
	recoverTc, checkNoErrsTc, recoverNF_Tc, discardErrsTc,
	addErrTcM, addInstErrTcM, failWithTcM,

	tcGetEnv, tcSetEnv,
	tcGetDefaultTys, tcSetDefaultTys,
	tcGetUnique, tcGetUniques, 
	doptsTc, getDOptsTc,

	tcAddSrcLoc, tcGetSrcLoc, tcGetInstLoc,
	tcAddErrCtxtM, tcSetErrCtxtM,
	tcAddErrCtxt, tcSetErrCtxt,

	tcNewMutVar, tcNewSigTyVar, tcReadMutVar, tcWriteMutVar, TcRef,
	tcNewMutTyVar, tcReadMutTyVar, tcWriteMutTyVar,

	InstOrigin(..), InstLoc, pprInstLoc, 

	TcError, TcWarning, TidyEnv, emptyTidyEnv,
	arityErr
  ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcEnv  ( TcEnv )

import RnHsSyn		( RenamedPat, RenamedArithSeqInfo, RenamedHsExpr, RenamedHsOverLit )
import Type		( Type, Kind, PredType, ThetaType, RhoType, TauType,
			)
import ErrUtils		( addShortErrLocLine, addShortWarnLocLine, ErrMsg, Message, WarnMsg )

import Bag		( Bag, emptyBag, isEmptyBag,
			  foldBag, unitBag, unionBags, snocBag )
import Class		( Class, ClassContext )
import Name		( Name )
import Var		( Id, TyVar, newMutTyVar, newSigTyVar, readMutTyVar, writeMutTyVar )
import VarEnv		( TidyEnv, emptyTidyEnv )
import VarSet		( TyVarSet )
import UniqSupply	( UniqSupply, uniqFromSupply, uniqsFromSupply, 
			  splitUniqSupply, mkSplitUniqSupply,
			  UniqSM, initUs_ )
import SrcLoc		( SrcLoc, noSrcLoc )
import UniqFM		( emptyUFM )
import Unique		( Unique )
import CmdLineOpts
import Outputable

import IOExts		( IORef, newIORef, readIORef, writeIORef,
			  unsafeInterleaveIO, fixIO
			)


infixr 9 `thenTc`, `thenTc_`, `thenNF_Tc`, `thenNF_Tc_` 
\end{code}


%************************************************************************
%*									*
\subsection{Types}
%*									*
%************************************************************************

\begin{code}
type TcTyVar    = TyVar		-- Might be a mutable tyvar
type TcTyVarSet = TyVarSet

type TcType = Type 		-- A TcType can have mutable type variables
	-- Invariant on ForAllTy in TcTypes:
	-- 	forall a. T
	-- a cannot occur inside a MutTyVar in T; that is,
	-- T is "flattened" before quantifying over a

type TcClassContext = ClassContext
type TcPredType     = PredType
type TcThetaType    = ThetaType
type TcRhoType      = RhoType
type TcTauType      = TauType
type TcKind         = TcType
\end{code}


%************************************************************************
%*									*
\subsection{The main monads: TcM, NF_TcM}
%*									*
%************************************************************************

\begin{code}
type NF_TcM r =  TcDown -> TcEnv -> IO r	-- Can't raise UserError
type TcM    r =  TcDown -> TcEnv -> IO r	-- Can raise UserError

type Either_TcM r =  TcDown -> TcEnv -> IO r	-- Either NF_TcM or TcM
	-- Used only in this file for type signatures which
	-- have a part that's polymorphic in whether it's NF_TcM or TcM
	-- E.g. thenNF_Tc

type TcRef a = IORef a
\end{code}

\begin{code}

initTc :: DynFlags 
       -> TcEnv
       -> TcM r
       -> IO (Maybe r, (Bag WarnMsg, Bag ErrMsg))

initTc dflags tc_env do_this
  = do {
      us       <- mkSplitUniqSupply 'a' ;
      us_var   <- newIORef us ;
      errs_var <- newIORef (emptyBag,emptyBag) ;
      tvs_var  <- newIORef emptyUFM ;

      let
          init_down = TcDown { tc_dflags = dflags, tc_def = [],
			       tc_us = us_var, tc_loc = noSrcLoc,
			       tc_ctxt = [], tc_errs = errs_var }
      ;

      maybe_res <- catch (do {  res <- do_this init_down tc_env ;
				return (Just res)})
			 (\_ -> return Nothing) ;
        
      (warns,errs) <- readIORef errs_var ;
      return (maybe_res, (warns, errs))
    }

-- Monadic operations

returnNF_Tc :: a -> NF_TcM a
returnTc    :: a -> TcM a
returnTc v down env = return v

thenTc    :: TcM a ->    (a -> TcM b)        -> TcM b
thenNF_Tc :: NF_TcM a -> (a -> Either_TcM b) -> Either_TcM b
thenTc m k down env = do { r <- m down env; k r down env }

thenTc_    :: TcM a    -> TcM b        -> TcM b
thenNF_Tc_ :: NF_TcM a -> Either_TcM b -> Either_TcM b
thenTc_ m k down env = do { m down env; k down env }

listTc    :: [TcM a]    -> TcM [a]
listNF_Tc :: [NF_TcM a] -> NF_TcM [a]
listTc []     = returnTc []
listTc (x:xs) = x			`thenTc` \ r ->
		listTc xs		`thenTc` \ rs ->
		returnTc (r:rs)

mapTc    :: (a -> TcM b)    -> [a] -> TcM [b]
mapTc_   :: (a -> TcM b)    -> [a] -> TcM ()
mapNF_Tc :: (a -> NF_TcM b) -> [a] -> NF_TcM [b]
mapTc f []     = returnTc []
mapTc f (x:xs) = f x		`thenTc` \ r ->
		 mapTc f xs	`thenTc` \ rs ->
		 returnTc (r:rs)
mapTc_ f xs = mapTc f xs  `thenTc_` returnTc ()


foldrTc    :: (a -> b -> TcM b)    -> b -> [a] -> TcM b
foldrNF_Tc :: (a -> b -> NF_TcM b) -> b -> [a] -> NF_TcM b
foldrTc k z []     = returnTc z
foldrTc k z (x:xs) = foldrTc k z xs	`thenTc` \r ->
		     k x r

foldlTc    :: (a -> b -> TcM a)    -> a -> [b] -> TcM a
foldlNF_Tc :: (a -> b -> NF_TcM a) -> a -> [b] -> NF_TcM a
foldlTc k z []     = returnTc z
foldlTc k z (x:xs) = k z x		`thenTc` \r ->
		     foldlTc k r xs

mapAndUnzipTc    :: (a -> TcM (b,c))    -> [a]   -> TcM ([b],[c])
mapAndUnzipNF_Tc :: (a -> NF_TcM (b,c)) -> [a]   -> NF_TcM ([b],[c])
mapAndUnzipTc f []     = returnTc ([],[])
mapAndUnzipTc f (x:xs) = f x			`thenTc` \ (r1,r2) ->
			 mapAndUnzipTc f xs	`thenTc` \ (rs1,rs2) ->
			 returnTc (r1:rs1, r2:rs2)

mapAndUnzip3Tc    :: (a -> TcM (b,c,d)) -> [a] -> TcM ([b],[c],[d])
mapAndUnzip3Tc f []     = returnTc ([],[],[])
mapAndUnzip3Tc f (x:xs) = f x			`thenTc` \ (r1,r2,r3) ->
			  mapAndUnzip3Tc f xs	`thenTc` \ (rs1,rs2,rs3) ->
			  returnTc (r1:rs1, r2:rs2, r3:rs3)

mapBagTc    :: (a -> TcM b)    -> Bag a -> TcM (Bag b)
mapBagNF_Tc :: (a -> NF_TcM b) -> Bag a -> NF_TcM (Bag b)
mapBagTc f bag
  = foldBag (\ b1 b2 -> b1 `thenTc` \ r1 -> 
		        b2 `thenTc` \ r2 -> 
		        returnTc (unionBags r1 r2))
	    (\ a -> f a `thenTc` \ r -> returnTc (unitBag r))
	    (returnTc emptyBag)
	    bag

fixTc    :: (a -> TcM a)    -> TcM a
fixNF_Tc :: (a -> NF_TcM a) -> NF_TcM a
fixTc m env down = fixIO (\ loop -> m loop env down)

recoverTc    :: TcM r -> TcM r -> TcM r
recoverNF_Tc :: NF_TcM r -> TcM r -> NF_TcM r
recoverTc recover m down env
  = catch (m down env) (\ _ -> recover down env)

returnNF_Tc 	 = returnTc
thenNF_Tc   	 = thenTc
thenNF_Tc_  	 = thenTc_
fixNF_Tc    	 = fixTc
recoverNF_Tc	 = recoverTc
mapNF_Tc    	 = mapTc
foldrNF_Tc  	 = foldrTc
foldlNF_Tc  	 = foldlTc
listNF_Tc   	 = listTc
mapAndUnzipNF_Tc = mapAndUnzipTc
mapBagNF_Tc      = mapBagTc
\end{code}

@forkNF_Tc@ runs a sub-typecheck action *lazily* in a separate state
thread.  Ideally, this elegantly ensures that it can't zap any type
variables that belong to the main thread.  But alas, the environment
contains TyCon and Class environments that include TcKind stuff,
which is a Royal Pain.  By the time this fork stuff is used they'll
have been unified down so there won't be any kind variables, but we
can't express that in the current typechecker framework.

So we compromise and use unsafeInterleaveSST.

We throw away any error messages!

\begin{code}
forkNF_Tc :: NF_TcM r -> NF_TcM r
forkNF_Tc m down@(TcDown { tc_us = u_var }) env
  = do
	-- Get a fresh unique supply
    	us <- readIORef u_var
	let (us1, us2) = splitUniqSupply us
	writeIORef u_var us1
    
	unsafeInterleaveIO (do {
		us_var'  <- newIORef us2 ;
	      	err_var' <- newIORef (emptyBag,emptyBag) ;
		let { down' = down { tc_us = us_var', tc_errs = err_var' } };
		m down' env
			-- ToDo: optionally dump any error messages
		})
\end{code}

\begin{code}
traceTc :: SDoc -> NF_TcM ()
traceTc doc down env = printDump doc

ioToTc :: IO a -> NF_TcM a
ioToTc io down env = io
\end{code}


%************************************************************************
%*									*
\subsection{Error handling}
%*									*
%************************************************************************

\begin{code}
getErrsTc :: NF_TcM (Bag WarnMsg, Bag ErrMsg)
getErrsTc down env
  = readIORef (getTcErrs down)

failTc :: TcM a
failTc down env = give_up

give_up :: IO a
give_up = IOERROR (userError "Typecheck failed")

failWithTc :: Message -> TcM a			-- Add an error message and fail
failWithTc err_msg = failWithTcM (emptyTidyEnv, err_msg)

addErrTc :: Message -> NF_TcM ()
addErrTc err_msg = addErrTcM (emptyTidyEnv, err_msg)

addErrsTc :: [Message] -> NF_TcM ()
addErrsTc []	   = returnNF_Tc ()
addErrsTc err_msgs = listNF_Tc (map addErrTc err_msgs)	`thenNF_Tc_` returnNF_Tc ()

-- The 'M' variants do the TidyEnv bit
failWithTcM :: (TidyEnv, Message) -> TcM a	-- Add an error message and fail
failWithTcM env_and_msg
  = addErrTcM env_and_msg	`thenNF_Tc_`
    failTc

checkTc :: Bool -> Message -> TcM ()		-- Check that the boolean is true
checkTc True  err = returnTc ()
checkTc False err = failWithTc err

checkTcM :: Bool -> TcM () -> TcM ()	-- Check that the boolean is true
checkTcM True  err = returnTc ()
checkTcM False err = err

checkMaybeTc :: Maybe val -> Message -> TcM val
checkMaybeTc (Just val) err = returnTc val
checkMaybeTc Nothing    err = failWithTc err

checkMaybeTcM :: Maybe val -> TcM val -> TcM val
checkMaybeTcM (Just val) err = returnTc val
checkMaybeTcM Nothing    err = err

addErrTcM :: (TidyEnv, Message) -> NF_TcM ()	-- Add an error message but don't fail
addErrTcM (tidy_env, err_msg) down env
  = add_err_tcm tidy_env err_msg ctxt loc down env
  where
    ctxt     = getErrCtxt down
    loc      = getLoc down

addInstErrTcM :: InstLoc -> (TidyEnv, Message) -> NF_TcM ()	-- Add an error message but don't fail
addInstErrTcM inst_loc@(_, loc, ctxt) (tidy_env, err_msg) down env
  = add_err_tcm tidy_env err_msg full_ctxt loc down env
  where
    full_ctxt = (\env -> returnNF_Tc (env, pprInstLoc inst_loc)) : ctxt

add_err_tcm tidy_env err_msg ctxt loc down env
  = do
	(warns, errs) <- readIORef errs_var
	ctxt_msgs     <- do_ctxt tidy_env ctxt down env
	let err = addShortErrLocLine loc $
		  vcat (err_msg : ctxt_to_use ctxt_msgs)
	writeIORef errs_var (warns, errs `snocBag` err)
  where
    errs_var = getTcErrs down

do_ctxt tidy_env [] down env
  = return []
do_ctxt tidy_env (c:cs) down env
  = do 
	(tidy_env', m) <- c tidy_env down env
	ms	       <- do_ctxt tidy_env' cs down env
	return (m:ms)

-- warnings don't have an 'M' variant
warnTc :: Bool -> Message -> NF_TcM ()
warnTc warn_if_true warn_msg down env
  | warn_if_true 
  = do
	(warns,errs) <- readIORef errs_var
	ctxt_msgs    <- do_ctxt emptyTidyEnv ctxt down env	
	let warn = addShortWarnLocLine loc $
	           vcat (warn_msg : ctxt_to_use ctxt_msgs)
	writeIORef errs_var (warns `snocBag` warn, errs)
  | otherwise
  = return ()
  where
    errs_var = getTcErrs down
    ctxt     = getErrCtxt down
    loc      = getLoc down

-- (tryTc r m) succeeds if m succeeds and generates no errors
-- If m fails then r is invoked, passing the warnings and errors from m
-- If m succeeds, (tryTc r m) checks whether m generated any errors messages
--	(it might have recovered internally)
-- 	If so, then r is invoked, passing the warnings and errors from m

tryTc :: ((Bag WarnMsg, Bag ErrMsg) -> TcM r)	-- Recovery action
      -> TcM r				-- Thing to try
      -> TcM r
tryTc recover main down env
  = do 
	m_errs_var <- newIORef (emptyBag,emptyBag)
	catch (my_main m_errs_var) (\ _ -> my_recover m_errs_var)
  where
    my_recover m_errs_var
      = do warns_and_errs <- readIORef m_errs_var
	   recover warns_and_errs down env

    my_main m_errs_var
       = do result <- main (setTcErrs down m_errs_var) env

		-- Check that m has no errors; if it has internal recovery
		-- mechanisms it might "succeed" but having found a bunch of
		-- errors along the way.
	    (m_warns, m_errs) <- readIORef m_errs_var
	    if isEmptyBag m_errs then
		return result
	      else
		give_up		-- This triggers the catch


-- (checkNoErrsTc m) succeeds iff m succeeds and generates no errors
-- If m fails then (checkNoErrsTc m) fails.
-- If m succeeds, it checks whether m generated any errors messages
--	(it might have recovered internally)
-- 	If so, it fails too.
-- Regardless, any errors generated by m are propagated to the enclosing context.
checkNoErrsTc :: TcM r -> TcM r
checkNoErrsTc main
  = tryTc my_recover main
  where
    my_recover (m_warns, m_errs) down env
	= do (warns, errs)     <- readIORef errs_var
	     writeIORef errs_var (warns `unionBags` m_warns,
				  errs  `unionBags` m_errs)
	     give_up
	where
	  errs_var = getTcErrs down


-- (tryTc_ r m) tries m; if it succeeds it returns it,
-- otherwise it returns r.  Any error messages added by m are discarded,
-- whether or not m succeeds.
tryTc_ :: TcM r -> TcM r -> TcM r
tryTc_ recover main
  = tryTc my_recover main
  where
    my_recover warns_and_errs = recover

-- (discardErrsTc m) runs m, but throw away all its error messages.
discardErrsTc :: Either_TcM r -> Either_TcM r
discardErrsTc main down env
  = do new_errs_var <- newIORef (emptyBag,emptyBag)
       main (setTcErrs down new_errs_var) env
\end{code}



%************************************************************************
%*									*
\subsection{Mutable variables}
%*									*
%************************************************************************

\begin{code}
tcNewMutVar :: a -> NF_TcM (TcRef a)
tcNewMutVar val down env = newIORef val

tcWriteMutVar :: TcRef a -> a -> NF_TcM ()
tcWriteMutVar var val down env = writeIORef var val

tcReadMutVar :: TcRef a -> NF_TcM a
tcReadMutVar var down env = readIORef var

tcNewMutTyVar :: Name -> Kind -> NF_TcM TyVar
tcNewMutTyVar name kind down env = newMutTyVar name kind

tcNewSigTyVar :: Name -> Kind -> NF_TcM TyVar
tcNewSigTyVar name kind down env = newSigTyVar name kind

tcReadMutTyVar :: TyVar -> NF_TcM (Maybe Type)
tcReadMutTyVar tyvar down env = readMutTyVar tyvar

tcWriteMutTyVar :: TyVar -> Maybe Type -> NF_TcM ()
tcWriteMutTyVar tyvar val down env = writeMutTyVar tyvar val
\end{code}


%************************************************************************
%*									*
\subsection{The environment}
%*									*
%************************************************************************

\begin{code}
tcGetEnv :: NF_TcM TcEnv
tcGetEnv down env = return env

tcSetEnv :: TcEnv -> Either_TcM a -> Either_TcM a
tcSetEnv new_env m down old_env = m down new_env
\end{code}


%************************************************************************
%*									*
\subsection{Source location}
%*									*
%************************************************************************

\begin{code}
tcGetDefaultTys :: NF_TcM [Type]
tcGetDefaultTys down env = return (getDefaultTys down)

tcSetDefaultTys :: [Type] -> TcM r -> TcM r
tcSetDefaultTys tys m down env = m (setDefaultTys down tys) env

tcAddSrcLoc :: SrcLoc -> Either_TcM a -> Either_TcM a
tcAddSrcLoc loc m down env = m (setLoc down loc) env

tcGetSrcLoc :: NF_TcM SrcLoc
tcGetSrcLoc down env = return (getLoc down)

tcGetInstLoc :: InstOrigin -> NF_TcM InstLoc
tcGetInstLoc origin down env = return (origin, getLoc down, getErrCtxt down)

tcSetErrCtxtM, tcAddErrCtxtM :: (TidyEnv -> NF_TcM (TidyEnv, Message))
			     -> TcM a -> TcM a
tcSetErrCtxtM msg m down env = m (setErrCtxt down msg) env
tcAddErrCtxtM msg m down env = m (addErrCtxt down msg) env

tcSetErrCtxt, tcAddErrCtxt :: Message -> Either_TcM r -> Either_TcM r
-- Usual thing
tcSetErrCtxt msg m down env = m (setErrCtxt down (\env -> returnNF_Tc (env, msg))) env
tcAddErrCtxt msg m down env = m (addErrCtxt down (\env -> returnNF_Tc (env, msg))) env
\end{code}


%************************************************************************
%*									*
\subsection{Unique supply}
%*									*
%************************************************************************

\begin{code}
tcGetUnique :: NF_TcM Unique
tcGetUnique down env
  = do  uniq_supply <- readIORef u_var
    	let (new_uniq_supply, uniq_s) = splitUniqSupply uniq_supply
	    uniq		      = uniqFromSupply uniq_s
	writeIORef u_var new_uniq_supply
	return uniq
  where
    u_var = getUniqSupplyVar down

tcGetUniques :: Int -> NF_TcM [Unique]
tcGetUniques n down env
  = do	uniq_supply <- readIORef u_var
	let (new_uniq_supply, uniq_s) = splitUniqSupply uniq_supply
	    uniqs		      = uniqsFromSupply n uniq_s
	writeIORef u_var new_uniq_supply
	return uniqs
  where
    u_var = getUniqSupplyVar down

uniqSMToTcM :: UniqSM a -> NF_TcM a
uniqSMToTcM m down env
  = do	uniq_supply <- readIORef u_var
	let (new_uniq_supply, uniq_s) = splitUniqSupply uniq_supply
	writeIORef u_var new_uniq_supply
	return (initUs_ uniq_s m)
  where
    u_var = getUniqSupplyVar down
\end{code}



%************************************************************************
%*									*
\subsection{TcDown}
%*									*
%************************************************************************

\begin{code}
data TcDown
   = TcDown {
        tc_dflags :: DynFlags,
	tc_def    :: [Type],			-- Types used for defaulting
	tc_us     :: (TcRef UniqSupply),	-- Unique supply
	tc_loc    :: SrcLoc,			-- Source location
	tc_ctxt   :: ErrCtxt,			-- Error context
	tc_errs   :: (TcRef (Bag WarnMsg, Bag ErrMsg))
   }

type ErrCtxt = [TidyEnv -> NF_TcM (TidyEnv, Message)]	
			-- Innermost first.  Monadic so that we have a chance
			-- to deal with bound type variables just before error
			-- message construction
\end{code}

-- These selectors are *local* to TcMonad.lhs

\begin{code}
getTcErrs (TcDown{tc_errs=errs}) = errs
setTcErrs down errs = down{tc_errs=errs}

getDefaultTys (TcDown{tc_def=def}) = def
setDefaultTys down def = down{tc_def=def}

getLoc (TcDown{tc_loc=loc}) = loc
setLoc down loc = down{tc_loc=loc}

getUniqSupplyVar (TcDown{tc_us=us}) = us

getErrCtxt (TcDown{tc_ctxt=ctxt}) = ctxt
setErrCtxt down msg = down{tc_ctxt=[msg]}
addErrCtxt down msg = down{tc_ctxt = msg : tc_ctxt down}

doptsTc :: DynFlag -> TcM Bool
doptsTc dflag (TcDown{tc_dflags=dflags}) env_down
   = return (dopt dflag dflags)

getDOptsTc :: TcM DynFlags
getDOptsTc (TcDown{tc_dflags=dflags}) env_down
   = return dflags
\end{code}




%************************************************************************
%*									*
\subsection{TypeChecking Errors}
%*									*
%************************************************************************

\begin{code}
type TcError   = Message
type TcWarning = Message

ctxt_to_use ctxt | opt_PprStyle_Debug = ctxt
		 | otherwise	      = takeAtMost 3 ctxt
		 where
		   takeAtMost :: Int -> [a] -> [a]
     		   takeAtMost 0 ls = []
     		   takeAtMost n [] = []
     		   takeAtMost n (x:xs) = x:takeAtMost (n-1) xs

arityErr kind name n m
  = hsep [ text kind, quotes (ppr name), ptext SLIT("should have"),
	   n_arguments <> comma, text "but has been given", int m]
    where
	n_arguments | n == 0 = ptext SLIT("no arguments")
		    | n == 1 = ptext SLIT("1 argument")
		    | True   = hsep [int n, ptext SLIT("arguments")]
\end{code}



%************************************************************************
%*									*
\subsection[Inst-origin]{The @InstOrigin@ type}
%*									*
%************************************************************************

The @InstOrigin@ type gives information about where a dictionary came from.
This is important for decent error message reporting because dictionaries
don't appear in the original source code.  Doubtless this type will evolve...

It appears in TcMonad because there are a couple of error-message-generation
functions that deal with it.

\begin{code}
type InstLoc = (InstOrigin, SrcLoc, ErrCtxt)

data InstOrigin
  = OccurrenceOf Id		-- Occurrence of an overloaded identifier

  | IPOcc Name			-- Occurrence of an implicit parameter
  | IPBind Name			-- Binding site of an implicit parameter

  | RecordUpdOrigin

  | DataDeclOrigin		-- Typechecking a data declaration

  | InstanceDeclOrigin		-- Typechecking an instance decl

  | LiteralOrigin RenamedHsOverLit	-- Occurrence of a literal

  | PatOrigin RenamedPat

  | ArithSeqOrigin RenamedArithSeqInfo -- [x..], [x..y] etc

  | SignatureOrigin		-- A dict created from a type signature
  | Rank2Origin			-- A dict created when typechecking the argument
				-- of a rank-2 typed function

  | DoOrigin			-- The monad for a do expression

  | ClassDeclOrigin		-- Manufactured during a class decl

  | InstanceSpecOrigin	Class	-- in a SPECIALIZE instance pragma
			Type

	-- When specialising instances the instance info attached to
	-- each class is not yet ready, so we record it inside the
	-- origin information.  This is a bit of a hack, but it works
	-- fine.  (Patrick is to blame [WDP].)

  | ValSpecOrigin	Name	-- in a SPECIALIZE pragma for a value

	-- Argument or result of a ccall
	-- Dictionaries with this origin aren't actually mentioned in the
	-- translated term, and so need not be bound.  Nor should they
	-- be abstracted over.

  | CCallOrigin		String			-- CCall label
			(Maybe RenamedHsExpr)	-- Nothing if it's the result
						-- Just arg, for an argument

  | LitLitOrigin	String	-- the litlit

  | UnknownOrigin	-- Help! I give up...
\end{code}

\begin{code}
pprInstLoc :: InstLoc -> SDoc
pprInstLoc (orig, locn, ctxt)
  = hsep [text "arising from", pp_orig orig, text "at", ppr locn]
  where
    pp_orig (OccurrenceOf id)
      	= hsep [ptext SLIT("use of"), quotes (ppr id)]
    pp_orig (IPOcc name)
      	= hsep [ptext SLIT("use of implicit parameter"), quotes (char '?' <> ppr name)]
    pp_orig (IPBind name)
      	= hsep [ptext SLIT("binding for implicit parameter"), quotes (char '?' <> ppr name)]
    pp_orig (LiteralOrigin lit)
	= hsep [ptext SLIT("the literal"), quotes (ppr lit)]
    pp_orig (PatOrigin pat)
	= hsep [ptext SLIT("the pattern"), quotes (ppr pat)]
    pp_orig (InstanceDeclOrigin)
	=  ptext SLIT("an instance declaration")
    pp_orig (ArithSeqOrigin seq)
	= hsep [ptext SLIT("the arithmetic sequence"), quotes (ppr seq)]
    pp_orig (SignatureOrigin)
	=  ptext SLIT("a type signature")
    pp_orig (Rank2Origin)
	=  ptext SLIT("a function with an overloaded argument type")
    pp_orig (DoOrigin)
	=  ptext SLIT("a do statement")
    pp_orig (ClassDeclOrigin)
	=  ptext SLIT("a class declaration")
    pp_orig (InstanceSpecOrigin clas ty)
	= hsep [text "a SPECIALIZE instance pragma; class",
	        quotes (ppr clas), text "type:", ppr ty]
    pp_orig (ValSpecOrigin name)
	= hsep [ptext SLIT("a SPECIALIZE user-pragma for"), quotes (ppr name)]
    pp_orig (CCallOrigin clabel Nothing{-ccall result-})
	= hsep [ptext SLIT("the result of the _ccall_ to"), quotes (text clabel)]
    pp_orig (CCallOrigin clabel (Just arg_expr))
	= hsep [ptext SLIT("an argument in the _ccall_ to"), quotes (text clabel) <> comma, 
		text "namely", quotes (ppr arg_expr)]
    pp_orig (LitLitOrigin s)
	= hsep [ptext SLIT("the ``literal-literal''"), quotes (text s)]
    pp_orig (UnknownOrigin)
	= ptext SLIT("...oops -- I don't know where the overloading came from!")
\end{code}
