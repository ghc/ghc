\begin{code}
module TcMonad(
	TcType, 
	TcTauType, TcPredType, TcThetaType, TcRhoType,
	TcTyVar, TcTyVarSet,
	TcKind,

	TcM, NF_TcM, TcDown, TcEnv, 

	initTc,
	returnTc, thenTc, thenTc_, mapTc, listTc,
	foldrTc, foldlTc, mapAndUnzipTc, mapAndUnzip3Tc,
	mapBagTc, fixTc, tryTc, tryTc_, getErrsTc, 
	traceTc, ioToTc,

	uniqSMToTcM,

	returnNF_Tc, thenNF_Tc, thenNF_Tc_, mapNF_Tc, 
	fixNF_Tc, forkNF_Tc, foldrNF_Tc, foldlNF_Tc,

	listNF_Tc, mapAndUnzipNF_Tc, mapBagNF_Tc,

	checkTc, checkTcM, checkMaybeTc, checkMaybeTcM, 
	failTc, failWithTc, addErrTc, warnTc, recoverTc, checkNoErrsTc, recoverNF_Tc, discardErrsTc,
	addErrTcM, addInstErrTcM, failWithTcM,

	tcGetEnv, tcSetEnv,
	tcGetDefaultTys, tcSetDefaultTys,
	tcGetUnique, tcGetUniques,

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

import HsSyn		( HsLit )
import RnHsSyn		( RenamedPat, RenamedArithSeqInfo, RenamedHsExpr )
import Type		( Type, Kind, PredType, ThetaType, RhoType, TauType,
			)
import ErrUtils		( addShortErrLocLine, addShortWarnLocLine, pprBagOfErrors, ErrMsg, Message, WarnMsg )
import CmdLineOpts      ( opt_PprStyle_Debug )

import Bag		( Bag, emptyBag, isEmptyBag,
			  foldBag, unitBag, unionBags, snocBag )
import Class		( Class )
import Name		( Name )
import Var		( Id, TyVar, newMutTyVar, newSigTyVar, readMutTyVar, writeMutTyVar )
import VarEnv		( TyVarEnv, emptyVarEnv, TidyEnv, emptyTidyEnv )
import VarSet		( TyVarSet )
import UniqSupply	( UniqSupply, uniqFromSupply, uniqsFromSupply, splitUniqSupply,
			  UniqSM, initUs_ )
import SrcLoc		( SrcLoc, noSrcLoc )
import FiniteMap	( FiniteMap, emptyFM )
import UniqFM		( UniqFM, emptyUFM )
import Unique		( Unique )
import BasicTypes	( Unused )
import Util
import Outputable
import FastString	( FastString )

import IOExts		( IORef, newIORef, readIORef, writeIORef,
			  unsafeInterleaveIO, fixIO
			)


infixr 9 `thenTc`, `thenTc_`, `thenNF_Tc`, `thenNF_Tc_` 
\end{code}


Types
~~~~~
\begin{code}
type TcTyVar    = TyVar		-- Might be a mutable tyvar
type TcTyVarSet = TyVarSet

type TcType = Type 		-- A TcType can have mutable type variables
	-- Invariant on ForAllTy in TcTypes:
	-- 	forall a. T
	-- a cannot occur inside a MutTyVar in T; that is,
	-- T is "flattened" before quantifying over a

type TcPredType  = PredType
type TcThetaType = ThetaType
type TcRhoType   = RhoType
type TcTauType   = TauType
type TcKind      = TcType
\end{code}


\section{TcM, NF_TcM: the type checker monads}
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
type NF_TcM s r =  TcDown -> TcEnv -> IO r	-- Can't raise UserError
type TcM    s r =  TcDown -> TcEnv -> IO r	-- Can raise UserError
	-- ToDo: nuke the 's' part
	-- The difference between the two is
	-- now for documentation purposes only

type Either_TcM s r =  TcDown -> TcEnv -> IO r	-- Either NF_TcM or TcM
	-- Used only in this file for type signatures which
	-- have a part that's polymorphic in whether it's NF_TcM or TcM
	-- E.g. thenNF_Tc

type TcRef a = IORef a
\end{code}

\begin{code}
-- initEnv is passed in to avoid module recursion between TcEnv & TcMonad.

initTc :: UniqSupply
       -> (TcRef (UniqFM a) -> TcEnv)
       -> TcM s r
       -> IO (Maybe r, Bag WarnMsg, Bag ErrMsg)

initTc us initenv do_this
  = do {
      us_var   <- newIORef us ;
      errs_var <- newIORef (emptyBag,emptyBag) ;
      tvs_var  <- newIORef emptyUFM ;

      let
          init_down = TcDown [] us_var
			     noSrcLoc
			     [] errs_var
	  init_env  = initenv tvs_var
      ;

      maybe_res <- catch (do {  res <- do_this init_down init_env ;
				return (Just res)})
			 (\_ -> return Nothing) ;
        
      (warns,errs) <- readIORef errs_var ;
      return (maybe_res, warns, errs)
    }

-- Monadic operations

returnNF_Tc :: a -> NF_TcM s a
returnTc    :: a -> TcM s a
returnTc v down env = return v

thenTc    :: TcM s a ->    (a -> TcM s b)        -> TcM s b
thenNF_Tc :: NF_TcM s a -> (a -> Either_TcM s b) -> Either_TcM s b
thenTc m k down env = do { r <- m down env; k r down env }

thenTc_    :: TcM s a    -> TcM s b        -> TcM s b
thenNF_Tc_ :: NF_TcM s a -> Either_TcM s b -> Either_TcM s b
thenTc_ m k down env = do { m down env; k down env }

listTc    :: [TcM s a]    -> TcM s [a]
listNF_Tc :: [NF_TcM s a] -> NF_TcM s [a]
listTc []     = returnTc []
listTc (x:xs) = x			`thenTc` \ r ->
		listTc xs		`thenTc` \ rs ->
		returnTc (r:rs)

mapTc    :: (a -> TcM s b)    -> [a] -> TcM s [b]
mapNF_Tc :: (a -> NF_TcM s b) -> [a] -> NF_TcM s [b]
mapTc f []     = returnTc []
mapTc f (x:xs) = f x		`thenTc` \ r ->
		 mapTc f xs	`thenTc` \ rs ->
		 returnTc (r:rs)

foldrTc    :: (a -> b -> TcM s b)    -> b -> [a] -> TcM s b
foldrNF_Tc :: (a -> b -> NF_TcM s b) -> b -> [a] -> NF_TcM s b
foldrTc k z []     = returnTc z
foldrTc k z (x:xs) = foldrTc k z xs	`thenTc` \r ->
		     k x r

foldlTc    :: (a -> b -> TcM s a)    -> a -> [b] -> TcM s a
foldlNF_Tc :: (a -> b -> NF_TcM s a) -> a -> [b] -> NF_TcM s a
foldlTc k z []     = returnTc z
foldlTc k z (x:xs) = k z x		`thenTc` \r ->
		     foldlTc k r xs

mapAndUnzipTc    :: (a -> TcM s (b,c))    -> [a]   -> TcM s ([b],[c])
mapAndUnzipNF_Tc :: (a -> NF_TcM s (b,c)) -> [a]   -> NF_TcM s ([b],[c])
mapAndUnzipTc f []     = returnTc ([],[])
mapAndUnzipTc f (x:xs) = f x			`thenTc` \ (r1,r2) ->
			 mapAndUnzipTc f xs	`thenTc` \ (rs1,rs2) ->
			 returnTc (r1:rs1, r2:rs2)

mapAndUnzip3Tc    :: (a -> TcM s (b,c,d)) -> [a] -> TcM s ([b],[c],[d])
mapAndUnzip3Tc f []     = returnTc ([],[],[])
mapAndUnzip3Tc f (x:xs) = f x			`thenTc` \ (r1,r2,r3) ->
			  mapAndUnzip3Tc f xs	`thenTc` \ (rs1,rs2,rs3) ->
			  returnTc (r1:rs1, r2:rs2, r3:rs3)

mapBagTc    :: (a -> TcM s b)    -> Bag a -> TcM s (Bag b)
mapBagNF_Tc :: (a -> NF_TcM s b) -> Bag a -> NF_TcM s (Bag b)
mapBagTc f bag
  = foldBag (\ b1 b2 -> b1 `thenTc` \ r1 -> 
		        b2 `thenTc` \ r2 -> 
		        returnTc (unionBags r1 r2))
	    (\ a -> f a `thenTc` \ r -> returnTc (unitBag r))
	    (returnTc emptyBag)
	    bag

fixTc    :: (a -> TcM s a)    -> TcM s a
fixNF_Tc :: (a -> NF_TcM s a) -> NF_TcM s a
fixTc m env down = fixIO (\ loop -> m loop env down)

recoverTc    :: TcM s r -> TcM s r -> TcM s r
recoverNF_Tc :: NF_TcM s r -> TcM s r -> NF_TcM s r
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
forkNF_Tc :: NF_TcM s r -> NF_TcM s r
forkNF_Tc m (TcDown deflts u_var src_loc err_cxt err_var) env
  = do
	-- Get a fresh unique supply
    	us <- readIORef u_var
	let (us1, us2) = splitUniqSupply us
	writeIORef u_var us1
    
	unsafeInterleaveIO (do {
		us_var'  <- newIORef us2 ;
	      	err_var' <- newIORef (emptyBag,emptyBag) ;
      		tv_var'  <- newIORef emptyUFM ;
		let { down' = TcDown deflts us_var' src_loc err_cxt err_var' } ;
		m down' env
			-- ToDo: optionally dump any error messages
		})
\end{code}

\begin{code}
traceTc :: SDoc -> NF_TcM s ()
traceTc doc down env = printErrs doc

ioToTc :: IO a -> NF_TcM s a
ioToTc io down env = io
\end{code}


%************************************************************************
%*									*
\subsection{Error handling}
%*									*
%************************************************************************

\begin{code}
getErrsTc :: NF_TcM s (Bag WarnMsg, Bag ErrMsg)
getErrsTc down env
  = readIORef (getTcErrs down)

failTc :: TcM s a
failTc down env = give_up

give_up :: IO a
give_up = IOERROR (userError "Typecheck failed")

failWithTc :: Message -> TcM s a			-- Add an error message and fail
failWithTc err_msg = failWithTcM (emptyTidyEnv, err_msg)

addErrTc :: Message -> NF_TcM s ()
addErrTc err_msg = addErrTcM (emptyTidyEnv, err_msg)

-- The 'M' variants do the TidyEnv bit
failWithTcM :: (TidyEnv, Message) -> TcM s a	-- Add an error message and fail
failWithTcM env_and_msg
  = addErrTcM env_and_msg	`thenNF_Tc_`
    failTc

checkTc :: Bool -> Message -> TcM s ()		-- Check that the boolean is true
checkTc True  err = returnTc ()
checkTc False err = failWithTc err

checkTcM :: Bool -> TcM s () -> TcM s ()	-- Check that the boolean is true
checkTcM True  err = returnTc ()
checkTcM False err = err

checkMaybeTc :: Maybe val -> Message -> TcM s val
checkMaybeTc (Just val) err = returnTc val
checkMaybeTc Nothing    err = failWithTc err

checkMaybeTcM :: Maybe val -> TcM s val -> TcM s val
checkMaybeTcM (Just val) err = returnTc val
checkMaybeTcM Nothing    err = err

addErrTcM :: (TidyEnv, Message) -> NF_TcM s ()	-- Add an error message but don't fail
addErrTcM (tidy_env, err_msg) down env
  = add_err_tcm tidy_env err_msg ctxt loc down env
  where
    ctxt     = getErrCtxt down
    loc      = getLoc down

addInstErrTcM :: InstLoc -> (TidyEnv, Message) -> NF_TcM s ()	-- Add an error message but don't fail
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
warnTc :: Bool -> Message -> NF_TcM s ()
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

tryTc :: ((Bag WarnMsg, Bag ErrMsg) -> TcM s r)	-- Recovery action
      -> TcM s r				-- Thing to try
      -> TcM s r
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
checkNoErrsTc :: TcM s r -> TcM s r
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
tryTc_ :: TcM s r -> TcM s r -> TcM s r
tryTc_ recover main
  = tryTc my_recover main
  where
    my_recover warns_and_errs = recover

-- (discardErrsTc m) runs m, but throw away all its error messages.
discardErrsTc :: Either_TcM s r -> Either_TcM s r
discardErrsTc main down env
  = do new_errs_var <- newIORef (emptyBag,emptyBag)
       main (setTcErrs down new_errs_var) env
\end{code}

Mutable variables
~~~~~~~~~~~~~~~~~
\begin{code}
tcNewMutVar :: a -> NF_TcM s (TcRef a)
tcNewMutVar val down env = newIORef val

tcWriteMutVar :: TcRef a -> a -> NF_TcM s ()
tcWriteMutVar var val down env = writeIORef var val

tcReadMutVar :: TcRef a -> NF_TcM s a
tcReadMutVar var down env = readIORef var

tcNewMutTyVar :: Name -> Kind -> NF_TcM s TyVar
tcNewMutTyVar name kind down env = newMutTyVar name kind

tcNewSigTyVar :: Name -> Kind -> NF_TcM s TyVar
tcNewSigTyVar name kind down env = newSigTyVar name kind

tcReadMutTyVar :: TyVar -> NF_TcM s (Maybe Type)
tcReadMutTyVar tyvar down env = readMutTyVar tyvar

tcWriteMutTyVar :: TyVar -> Maybe Type -> NF_TcM s ()
tcWriteMutTyVar tyvar val down env = writeMutTyVar tyvar val
\end{code}


Environment
~~~~~~~~~~~
\begin{code}
tcGetEnv :: NF_TcM s TcEnv
tcGetEnv down env = return env

tcSetEnv :: TcEnv -> Either_TcM s a -> Either_TcM s a
tcSetEnv new_env m down old_env = m down new_env
\end{code}


Source location
~~~~~~~~~~~~~~~
\begin{code}
tcGetDefaultTys :: NF_TcM s [Type]
tcGetDefaultTys down env = return (getDefaultTys down)

tcSetDefaultTys :: [Type] -> TcM s r -> TcM s r
tcSetDefaultTys tys m down env = m (setDefaultTys down tys) env

tcAddSrcLoc :: SrcLoc -> Either_TcM s a -> Either_TcM s a
tcAddSrcLoc loc m down env = m (setLoc down loc) env

tcGetSrcLoc :: NF_TcM s SrcLoc
tcGetSrcLoc down env = return (getLoc down)

tcGetInstLoc :: InstOrigin -> NF_TcM s InstLoc
tcGetInstLoc origin down env = return (origin, getLoc down, getErrCtxt down)

tcSetErrCtxtM, tcAddErrCtxtM :: (TidyEnv -> NF_TcM s (TidyEnv, Message))
			     -> TcM s a -> TcM s a
tcSetErrCtxtM msg m down env = m (setErrCtxt down msg) env
tcAddErrCtxtM msg m down env = m (addErrCtxt down msg) env

tcSetErrCtxt, tcAddErrCtxt :: Message -> Either_TcM s r -> Either_TcM s r
-- Usual thing
tcSetErrCtxt msg m down env = m (setErrCtxt down (\env -> returnNF_Tc (env, msg))) env
tcAddErrCtxt msg m down env = m (addErrCtxt down (\env -> returnNF_Tc (env, msg))) env
\end{code}


Unique supply
~~~~~~~~~~~~~
\begin{code}
tcGetUnique :: NF_TcM s Unique
tcGetUnique down env
  = do  uniq_supply <- readIORef u_var
    	let (new_uniq_supply, uniq_s) = splitUniqSupply uniq_supply
	    uniq		      = uniqFromSupply uniq_s
	writeIORef u_var new_uniq_supply
	return uniq
  where
    u_var = getUniqSupplyVar down

tcGetUniques :: Int -> NF_TcM s [Unique]
tcGetUniques n down env
  = do	uniq_supply <- readIORef u_var
	let (new_uniq_supply, uniq_s) = splitUniqSupply uniq_supply
	    uniqs		      = uniqsFromSupply n uniq_s
	writeIORef u_var new_uniq_supply
	return uniqs
  where
    u_var = getUniqSupplyVar down

uniqSMToTcM :: UniqSM a -> NF_TcM s a
uniqSMToTcM m down env
  = do	uniq_supply <- readIORef u_var
	let (new_uniq_supply, uniq_s) = splitUniqSupply uniq_supply
	writeIORef u_var new_uniq_supply
	return (initUs_ uniq_s m)
  where
    u_var = getUniqSupplyVar down
\end{code}


\section{TcDown}
%~~~~~~~~~~~~~~~

\begin{code}
data TcDown
  = TcDown
	[Type]			-- Types used for defaulting

	(TcRef UniqSupply)	-- Unique supply

	SrcLoc			-- Source location
	ErrCtxt			-- Error context
	(TcRef (Bag WarnMsg, 
		  Bag ErrMsg))

type ErrCtxt = [TidyEnv -> NF_TcM Unused (TidyEnv, Message)]	
			-- Innermost first.  Monadic so that we have a chance
			-- to deal with bound type variables just before error
			-- message construction
\end{code}

-- These selectors are *local* to TcMonad.lhs

\begin{code}
getTcErrs (TcDown def us loc ctxt errs)      = errs
setTcErrs (TcDown def us loc ctxt _   ) errs = TcDown def us loc ctxt errs

getDefaultTys (TcDown def us loc ctxt errs)     = def
setDefaultTys (TcDown _   us loc ctxt errs) def = TcDown def us loc ctxt errs

getLoc (TcDown def us loc ctxt errs)     = loc
setLoc (TcDown def us _   ctxt errs) loc = TcDown def us loc ctxt errs

getUniqSupplyVar (TcDown def us loc ctxt errs) = us

setErrCtxt (TcDown def us loc ctxt errs) msg = TcDown def us loc [msg]      errs
addErrCtxt (TcDown def us loc ctxt errs) msg = TcDown def us loc (msg:ctxt) errs
getErrCtxt (TcDown def us loc ctxt errs)     = ctxt
\end{code}




TypeChecking Errors
~~~~~~~~~~~~~~~~~~~

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

  | RecordUpdOrigin

  | DataDeclOrigin		-- Typechecking a data declaration

  | InstanceDeclOrigin		-- Typechecking an instance decl

  | LiteralOrigin HsLit		-- Occurrence of a literal

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
