\begin{code}
module TcMonad(
	TcType, TcMaybe(..), TcBox,
	TcTauType, TcThetaType, TcRhoType,
	TcTyVar, TcTyVarSet,
	TcKind,

	TcM, NF_TcM, TcDown, TcEnv, 
	SST_R, FSST_R,

	initTc,
	returnTc, thenTc, thenTc_, mapTc, listTc,
	foldrTc, foldlTc, mapAndUnzipTc, mapAndUnzip3Tc,
	mapBagTc, fixTc, tryTc, getErrsTc, 

	uniqSMToTcM,

	returnNF_Tc, thenNF_Tc, thenNF_Tc_, mapNF_Tc, 
	fixNF_Tc, forkNF_Tc, foldrNF_Tc, foldlNF_Tc,

	listNF_Tc, mapAndUnzipNF_Tc, mapBagNF_Tc,

	checkTc, checkTcM, checkMaybeTc, checkMaybeTcM, 
	failTc, failWithTc, addErrTc, warnTc, recoverTc, checkNoErrsTc, recoverNF_Tc, discardErrsTc,
	addErrTcM, failWithTcM,

	tcGetEnv, tcSetEnv,
	tcGetDefaultTys, tcSetDefaultTys,
	tcGetUnique, tcGetUniques,

	tcAddSrcLoc, tcGetSrcLoc,
	tcAddErrCtxtM, tcSetErrCtxtM,
	tcAddErrCtxt, tcSetErrCtxt,

	tcNewMutVar, tcReadMutVar, tcWriteMutVar, TcRef,

	TcError, TcWarning, TidyTypeEnv, emptyTidyEnv,
	arityErr
  ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcEnv  ( TcEnv )

import Type		( Type, GenType )
import ErrUtils		( addShortErrLocLine, addShortWarnLocLine, ErrMsg, Message, WarnMsg )
import CmdLineOpts      ( opt_PprStyle_Debug )

import SST
import Bag		( Bag, emptyBag, isEmptyBag,
			  foldBag, unitBag, unionBags, snocBag )
import Class		( Class )
import Var		( GenTyVar )
import VarEnv		( TyVarEnv, emptyVarEnv )
import VarSet		( GenTyVarSet )
import UniqSupply	( UniqSupply, uniqFromSupply, uniqsFromSupply, splitUniqSupply,
			  UniqSM, initUs )
import SrcLoc		( SrcLoc, noSrcLoc )
import FiniteMap	( FiniteMap, emptyFM )
import UniqFM		( UniqFM, emptyUFM )
import Unique		( Unique )
import Util
import Outputable

import GlaExts		( State#, RealWorld )


infixr 9 `thenTc`, `thenTc_`, `thenNF_Tc`, `thenNF_Tc_` 
\end{code}


Types
~~~~~
\begin{code}
type TcType s = GenType (TcBox s)	-- Used during typechecker
	-- Invariant on ForAllTy in TcTypes:
	-- 	forall a. T
	-- a cannot occur inside a MutTyVar in T; that is,
	-- T is "flattened" before quantifying over a

type TcKind s = TcType s

type TcThetaType s = [(Class, [TcType s])]
type TcRhoType s   = TcType s		-- No ForAllTys
type TcTauType s   = TcType s		-- No DictTys or ForAllTys

type TcBox s = TcRef s (TcMaybe s)

data TcMaybe s = UnBound
	       | BoundTo (TcType s)

-- Interestingly, you can't use (Maybe (TcType s)) instead of (TcMaybe s),
-- because you get a synonym loop if you do!

type TcTyVar s    = GenTyVar (TcBox s)
type TcTyVarSet s = GenTyVarSet (TcBox s)
\end{code}


\section{TcM, NF_TcM: the type checker monads}
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
type NF_TcM s r =  TcDown s -> TcEnv s -> SST s r
type TcM    s r =  TcDown s -> TcEnv s -> FSST s r ()
\end{code}

\begin{code}
-- With a builtin polymorphic type for runSST the type for
-- initTc should use  TcM s r  instead of  TcM RealWorld r 

-- initEnv is passed in to avoid module recursion between TcEnv & TcMonad.

initTc :: UniqSupply
       -> (TcRef RealWorld (UniqFM a) -> TcEnv RealWorld)
       -> TcM RealWorld r
       -> (Maybe r, Bag WarnMsg, Bag ErrMsg)

initTc us initenv do_this
  = runSST (
      newMutVarSST us 			`thenSST` \ us_var ->
      newMutVarSST (emptyBag,emptyBag)	`thenSST` \ errs_var ->
      newMutVarSST emptyUFM		`thenSST` \ tvs_var ->
      let
          init_down = TcDown [] us_var
			     noSrcLoc
			     [] errs_var
	  init_env  = initenv tvs_var
      in
      recoverSST
	(\_ -> returnSST Nothing)
        (do_this init_down init_env `thenFSST` \ res ->
	 returnFSST (Just res))
      					`thenSST` \ maybe_res ->
      readMutVarSST errs_var 		`thenSST` \ (warns,errs) ->
      returnSST (maybe_res, warns, errs)
    )

thenNF_Tc :: NF_TcM s a
	  -> (a -> TcDown s -> TcEnv s -> State# s -> b)
	  -> TcDown s -> TcEnv s -> State# s -> b
-- thenNF_Tc :: NF_TcM s a -> (a -> NF_TcM s b) -> NF_TcM s b
-- thenNF_Tc :: NF_TcM s a -> (a -> TcM s b)    -> TcM s b

thenNF_Tc m k down env
  = m down env	`thenSST` \ r ->
    k r down env

thenNF_Tc_ :: NF_TcM s a
	   -> (TcDown s -> TcEnv s -> State# s -> b)
	   -> TcDown s -> TcEnv s -> State# s -> b
-- thenNF_Tc :: NF_TcM s a -> NF_TcM s b -> NF_TcM s b
-- thenNF_Tc :: NF_TcM s a -> TcM s b    -> TcM s b

thenNF_Tc_ m k down env
  = m down env	`thenSST_` k down env

returnNF_Tc :: a -> NF_TcM s a
returnNF_Tc v down env = returnSST v

fixNF_Tc :: (a -> NF_TcM s a) -> NF_TcM s a
fixNF_Tc m env down = fixSST (\ loop -> m loop env down)

mapNF_Tc    :: (a -> NF_TcM s b) -> [a] -> NF_TcM s [b]
mapNF_Tc f []     = returnNF_Tc []
mapNF_Tc f (x:xs) = f x			`thenNF_Tc` \ r ->
		    mapNF_Tc f xs	`thenNF_Tc` \ rs ->
		    returnNF_Tc (r:rs)

foldrNF_Tc :: (a -> b -> NF_TcM s b) -> b -> [a] -> NF_TcM s b
foldrNF_Tc k z []     = returnNF_Tc z
foldrNF_Tc k z (x:xs) = foldrNF_Tc k z xs	`thenNF_Tc` \r ->
		        k x r

foldlNF_Tc :: (a -> b -> NF_TcM s a) -> a -> [b] -> NF_TcM s a
foldlNF_Tc k z []     = returnNF_Tc z
foldlNF_Tc k z (x:xs) = k z x		`thenNF_Tc` \r ->
		        foldlNF_Tc k r xs

listNF_Tc    :: [NF_TcM s a] -> NF_TcM s [a]
listNF_Tc []     = returnNF_Tc []
listNF_Tc (x:xs) = x			`thenNF_Tc` \ r ->
		   listNF_Tc xs		`thenNF_Tc` \ rs ->
		   returnNF_Tc (r:rs)

mapBagNF_Tc :: (a -> NF_TcM s b) -> Bag a -> NF_TcM s (Bag b)
mapBagNF_Tc f bag
  = foldBag (\ b1 b2 -> b1 `thenNF_Tc` \ r1 -> 
		        b2 `thenNF_Tc` \ r2 -> 
		        returnNF_Tc (unionBags r1 r2))
	    (\ a -> f a `thenNF_Tc` \ r -> returnNF_Tc (unitBag r))
	    (returnNF_Tc emptyBag)
	    bag

mapAndUnzipNF_Tc    :: (a -> NF_TcM s (b,c)) -> [a]   -> NF_TcM s ([b],[c])
mapAndUnzipNF_Tc f []     = returnNF_Tc ([],[])
mapAndUnzipNF_Tc f (x:xs) = f x				`thenNF_Tc` \ (r1,r2) ->
			    mapAndUnzipNF_Tc f xs	`thenNF_Tc` \ (rs1,rs2) ->
			    returnNF_Tc (r1:rs1, r2:rs2)

thenTc :: TcM s a -> (a -> TcM s b) -> TcM s b
thenTc m k down env
  = m down env	`thenFSST` \ r ->
    k r down env

thenTc_ :: TcM s a -> TcM s b -> TcM s b
thenTc_ m k down env
  = m down env	`thenFSST_`  k down env

returnTc :: a -> TcM s a
returnTc val down env = returnFSST val

mapTc    :: (a -> TcM s b) -> [a]   -> TcM s [b]
mapTc f []     = returnTc []
mapTc f (x:xs) = f x		`thenTc` \ r ->
		 mapTc f xs	`thenTc` \ rs ->
		 returnTc (r:rs)

listTc    :: [TcM s a] -> TcM s [a]
listTc []     = returnTc []
listTc (x:xs) = x			`thenTc` \ r ->
		listTc xs		`thenTc` \ rs ->
		returnTc (r:rs)

foldrTc :: (a -> b -> TcM s b) -> b -> [a] -> TcM s b
foldrTc k z []     = returnTc z
foldrTc k z (x:xs) = foldrTc k z xs	`thenTc` \r ->
		     k x r

foldlTc :: (a -> b -> TcM s a) -> a -> [b] -> TcM s a
foldlTc k z []     = returnTc z
foldlTc k z (x:xs) = k z x		`thenTc` \r ->
		     foldlTc k r xs

mapAndUnzipTc    :: (a -> TcM s (b,c)) -> [a]   -> TcM s ([b],[c])
mapAndUnzipTc f []     = returnTc ([],[])
mapAndUnzipTc f (x:xs) = f x			`thenTc` \ (r1,r2) ->
			 mapAndUnzipTc f xs	`thenTc` \ (rs1,rs2) ->
			 returnTc (r1:rs1, r2:rs2)

mapAndUnzip3Tc    :: (a -> TcM s (b,c,d)) -> [a] -> TcM s ([b],[c],[d])
mapAndUnzip3Tc f []     = returnTc ([],[],[])
mapAndUnzip3Tc f (x:xs) = f x			`thenTc` \ (r1,r2,r3) ->
			  mapAndUnzip3Tc f xs	`thenTc` \ (rs1,rs2,rs3) ->
			  returnTc (r1:rs1, r2:rs2, r3:rs3)

mapBagTc :: (a -> TcM s b) -> Bag a -> TcM s (Bag b)
mapBagTc f bag
  = foldBag (\ b1 b2 -> b1 `thenTc` \ r1 -> 
		        b2 `thenTc` \ r2 -> 
		        returnTc (unionBags r1 r2))
	    (\ a -> f a `thenTc` \ r -> returnTc (unitBag r))
	    (returnTc emptyBag)
	    bag

fixTc :: (a -> TcM s a) -> TcM s a
fixTc m env down = fixFSST (\ loop -> m loop env down)
\end{code}

@forkNF_Tc@ runs a sub-typecheck action *lazily* in a separate state
thread.  Ideally, this elegantly ensures that it can't zap any type
variables that belong to the main thread.  But alas, the environment
contains TyCon and Class environments that include (TcKind s) stuff,
which is a Royal Pain.  By the time this fork stuff is used they'll
have been unified down so there won't be any kind variables, but we
can't express that in the current typechecker framework.

So we compromise and use unsafeInterleaveSST.

We throw away any error messages!

\begin{code}
forkNF_Tc :: NF_TcM s r -> NF_TcM s r
forkNF_Tc m (TcDown deflts u_var src_loc err_cxt err_var) env
  = 	-- Get a fresh unique supply
    readMutVarSST u_var		`thenSST` \ us ->
    let
	(us1, us2) = splitUniqSupply us
    in
    writeMutVarSST u_var us1	`thenSST_`
    
    unsafeInterleaveSST (
	newMutVarSST us2 			`thenSST` \ us_var'   ->
      	newMutVarSST (emptyBag,emptyBag)	`thenSST` \ err_var' ->
      	newMutVarSST emptyUFM			`thenSST` \ tv_var'  ->
	let
            down' = TcDown deflts us_var' src_loc err_cxt err_var'
	in
	m down' env
	-- ToDo: optionally dump any error messages
    )
\end{code}


Error handling
~~~~~~~~~~~~~~
\begin{code}
getErrsTc :: NF_TcM s (Bag ErrMsg, Bag  WarnMsg)
getErrsTc down env
  = readMutVarSST errs_var 
  where
    errs_var = getTcErrs down


failTc :: TcM s a
failTc down env
  = failFSST ()

failWithTc :: Message -> TcM s a			-- Add an error message and fail
failWithTc err_msg = failWithTcM (emptyTidyEnv, err_msg)

addErrTc :: Message -> NF_TcM s ()
addErrTc err_msg = addErrTcM (emptyTidyEnv, err_msg)

-- The 'M' variants do the TidyTypeEnv bit
failWithTcM :: (TidyTypeEnv s, Message) -> TcM s a	-- Add an error message and fail
failWithTcM env_and_msg
  = addErrTcM env_and_msg	`thenNF_Tc_`
    failTc

addErrTcM :: (TidyTypeEnv s, Message) -> NF_TcM s ()	-- Add an error message but don't fail
addErrTcM (tidy_env, err_msg) down env
  = readMutVarSST errs_var		`thenSST` \ (warns,errs) ->
    do_ctxt tidy_env ctxt down env	`thenSST` \ ctxt_msgs ->
    let
	err = addShortErrLocLine loc $
	      vcat (err_msg : ctxt_to_use ctxt_msgs)
    in
    writeMutVarSST errs_var (warns, errs `snocBag` err)	`thenSST_`
    returnSST ()
  where
    errs_var = getTcErrs down
    ctxt     = getErrCtxt down
    loc      = getLoc down

do_ctxt tidy_env [] down env
  = returnSST []
do_ctxt tidy_env (c:cs) down env
  = c tidy_env down env 		`thenSST` \ (tidy_env', m) ->
    do_ctxt tidy_env' cs down env	`thenSST` \ ms ->
    returnSST (m:ms)

-- warnings don't have an 'M' variant
warnTc :: Bool -> Message -> NF_TcM s ()
warnTc warn_if_true warn_msg down env
  = if warn_if_true then
	readMutVarSST errs_var			`thenSST` \ (warns,errs) ->
	do_ctxt emptyTidyEnv ctxt down env	`thenSST` \ ctxt_msgs ->
	let
	    warn = addShortWarnLocLine loc $
	           vcat (warn_msg : ctxt_to_use ctxt_msgs)
	in
	writeMutVarSST errs_var (warns `snocBag` warn, errs) 	`thenSST_`
    	returnSST ()
    else
	returnSST ()
  where
    errs_var = getTcErrs down
    ctxt     = getErrCtxt down
    loc      = getLoc down

recoverTc :: TcM s r -> TcM s r -> TcM s r
recoverTc recover m down env
  = recoverFSST (\ _ -> recover down env) (m down env)

recoverNF_Tc :: NF_TcM s r -> TcM s r -> NF_TcM s r
recoverNF_Tc recover m down env
  = recoverSST (\ _ -> recover down env) (m down env)

-- (checkNoErrsTc m) succeeds iff m succeeds and generates no errors
-- If m fails then (checkNoErrsTc m) fails.
-- If m succeeds, it checks whether m generated any errors messages
--	(it might have recovered internally)
-- 	If so, it fails too.
-- Regardless, any errors generated by m are propagated to the enclosing
-- context.

checkNoErrsTc :: TcM s r -> TcM s r
checkNoErrsTc m down env
  = newMutVarSST (emptyBag,emptyBag)	`thenSST` \ m_errs_var ->
    let
	errs_var = getTcErrs down
	propagate_errs _
	 = readMutVarSST m_errs_var	`thenSST` \ (m_warns, m_errs) ->
	   readMutVarSST errs_var	`thenSST` \ (warns, errs) ->
	   writeMutVarSST errs_var (warns `unionBags` m_warns,
				    errs  `unionBags` m_errs)	`thenSST_`
	   failFSST()
    in
					    
    recoverFSST propagate_errs $

    m (setTcErrs down m_errs_var) env	`thenFSST` \ result ->

	-- Check that m has no errors; if it has internal recovery
	-- mechanisms it might "succeed" but having found a bunch of
	-- errors along the way.
    readMutVarSST m_errs_var		`thenSST` \ (m_warns, m_errs) ->
    if isEmptyBag m_errs then
	returnFSST result
    else
	failFSST ()	-- This triggers the recoverFSST

-- (tryTc r m) tries m; if it succeeds it returns it,
-- otherwise it returns r.  Any error messages added by m are discarded,
-- whether or not m succeeds.
tryTc :: TcM s r -> TcM s r -> TcM s r
tryTc recover m down env
  = recoverFSST (\ _ -> recover down env) $

    newMutVarSST (emptyBag,emptyBag)	`thenSST` \ new_errs_var ->
    m (setTcErrs down new_errs_var) env	`thenFSST` \ result ->

	-- Check that m has no errors; if it has internal recovery
	-- mechanisms it might "succeed" but having found a bunch of
	-- errors along the way. If so we want tryTc to use 
	-- "recover" instead
    readMutVarSST new_errs_var		`thenSST` \ (_,errs) ->
    if isEmptyBag errs then
	returnFSST result
    else
	recover down env

-- Run the thing inside, but throw away all its error messages.
-- discardErrsTc :: TcM s r -> TcM s r
-- discardErrsTc :: NF_TcM s r -> NF_TcM s r
discardErrsTc :: (TcDown s -> TcEnv s -> State# s -> a)
	      -> (TcDown s -> TcEnv s -> State# s -> a)
discardErrsTc m down env
  = newMutVarSST (emptyBag,emptyBag)	`thenSST` \ new_errs_var ->
    m (setTcErrs down new_errs_var) env

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
\end{code}

Mutable variables
~~~~~~~~~~~~~~~~~
\begin{code}
type TcRef s a = SSTRef s a

tcNewMutVar :: a -> NF_TcM s (TcRef s a)
tcNewMutVar val down env = newMutVarSST val

tcWriteMutVar :: TcRef s a -> a -> NF_TcM s ()
tcWriteMutVar var val down env = writeMutVarSST var val

tcReadMutVar :: TcRef s a -> NF_TcM s a
tcReadMutVar var down env = readMutVarSST var
\end{code}


Environment
~~~~~~~~~~~
\begin{code}
tcGetEnv :: NF_TcM s (TcEnv s)
tcGetEnv down env = returnSST env

tcSetEnv :: TcEnv s
	  -> (TcDown s -> TcEnv s -> State# s -> b)
	  ->  TcDown s -> TcEnv s -> State# s -> b
-- tcSetEnv :: TcEnv s -> TcM s a -> TcM s a
-- tcSetEnv :: TcEnv s -> NF_TcM s a -> NF_TcM s a

tcSetEnv new_env m down old_env = m down new_env
\end{code}


Source location
~~~~~~~~~~~~~~~
\begin{code}
tcGetDefaultTys :: NF_TcM s [Type]
tcGetDefaultTys down env = returnSST (getDefaultTys down)

tcSetDefaultTys :: [Type] -> TcM s r -> TcM s r
tcSetDefaultTys tys m down env = m (setDefaultTys down tys) env

-- tcAddSrcLoc :: SrcLoc -> TcM s a -> TcM s a
-- tcAddSrcLoc :: SrcLoc -> NF_TcM s a -> NF_TcM s a
tcAddSrcLoc :: SrcLoc -> (TcDown s -> env -> result)
		      -> (TcDown s -> env -> result)
tcAddSrcLoc loc m down env = m (setLoc down loc) env

tcGetSrcLoc :: NF_TcM s SrcLoc
tcGetSrcLoc down env = returnSST (getLoc down)

tcSetErrCtxtM, tcAddErrCtxtM :: (TidyTypeEnv s -> NF_TcM s (TidyTypeEnv s, Message))
			     -> TcM s a -> TcM s a
tcSetErrCtxtM msg m down env = m (setErrCtxt down msg) env
tcAddErrCtxtM msg m down env = m (addErrCtxt down msg) env

tcSetErrCtxt, tcAddErrCtxt 
	  :: Message
	  -> (TcDown s -> TcEnv s -> State# s -> b)
	  ->  TcDown s -> TcEnv s -> State# s -> b
-- Usual thing
tcSetErrCtxt msg m down env = m (setErrCtxt down (\env -> returnNF_Tc (env, msg))) env
tcAddErrCtxt msg m down env = m (addErrCtxt down (\env -> returnNF_Tc (env, msg))) env
\end{code}


Unique supply
~~~~~~~~~~~~~
\begin{code}
tcGetUnique :: NF_TcM s Unique
tcGetUnique down env
  = readMutVarSST u_var				`thenSST` \ uniq_supply ->
    let
      (new_uniq_supply, uniq_s) = splitUniqSupply uniq_supply
      uniq			= uniqFromSupply uniq_s
    in
    writeMutVarSST u_var new_uniq_supply		`thenSST_`
    returnSST uniq
  where
    u_var = getUniqSupplyVar down

tcGetUniques :: Int -> NF_TcM s [Unique]
tcGetUniques n down env
  = readMutVarSST u_var				`thenSST` \ uniq_supply ->
    let
      (new_uniq_supply, uniq_s) = splitUniqSupply uniq_supply
      uniqs			= uniqsFromSupply n uniq_s
    in
    writeMutVarSST u_var new_uniq_supply		`thenSST_`
    returnSST uniqs
  where
    u_var = getUniqSupplyVar down

uniqSMToTcM :: UniqSM a -> NF_TcM s a
uniqSMToTcM m down env
  = readMutVarSST u_var				`thenSST` \ uniq_supply ->
    let
      (new_uniq_supply, uniq_s) = splitUniqSupply uniq_supply
    in
    writeMutVarSST u_var new_uniq_supply		`thenSST_`
    returnSST (initUs uniq_s m)
  where
    u_var = getUniqSupplyVar down
\end{code}


\section{TcDown}
%~~~~~~~~~~~~~~~

\begin{code}
data TcDown s
  = TcDown
	[Type]				-- Types used for defaulting

	(TcRef s UniqSupply)	-- Unique supply

	SrcLoc				-- Source location
	(ErrCtxt s)			-- Error context
	(TcRef s (Bag WarnMsg, 
		  Bag ErrMsg))

-- The TidyTypeEnv gives us a chance to tidy up the type,
-- so it prints nicely in error messages
type TidyTypeEnv s = (FiniteMap FastString Int,	-- Says what the 'next' unique to use
						-- for this occname is
		      TyVarEnv (TcType s))	-- Current mapping

emptyTidyEnv :: TidyTypeEnv s
emptyTidyEnv = (emptyFM, emptyVarEnv)

type ErrCtxt s = [TidyTypeEnv s -> NF_TcM s (TidyTypeEnv s, Message)]	
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


