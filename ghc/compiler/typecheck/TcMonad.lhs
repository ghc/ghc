\begin{code}
#include "HsVersions.h"

module TcMonad(
	SYN_IE(TcM), SYN_IE(NF_TcM), TcDown, TcEnv, 
	SST_R, FSST_R,

	initTc,
	returnTc, thenTc, thenTc_, mapTc, listTc,
	foldrTc, foldlTc, mapAndUnzipTc, mapAndUnzip3Tc,
	mapBagTc, fixTc, tryTc, getErrsTc, 

	uniqSMToTcM,

	returnNF_Tc, thenNF_Tc, thenNF_Tc_, mapNF_Tc, fixNF_Tc, forkNF_Tc,

	listNF_Tc, mapAndUnzipNF_Tc, mapBagNF_Tc,

	checkTc, checkTcM, checkMaybeTc, checkMaybeTcM, 
	failTc, warnTc, recoverTc, recoverNF_Tc,

	tcGetEnv, tcSetEnv,
	tcGetDefaultTys, tcSetDefaultTys,
	tcGetUnique, tcGetUniques,

	tcAddSrcLoc, tcGetSrcLoc,
	tcAddErrCtxtM, tcSetErrCtxtM,
	tcAddErrCtxt, tcSetErrCtxt,

	tcNewMutVar, tcReadMutVar, tcWriteMutVar,

	SYN_IE(TcError), SYN_IE(TcWarning),
	mkTcErr, arityErr,

	-- For closure
	SYN_IE(MutableVar),
#if __GLASGOW_HASKELL__ == 201
	GHCbase.MutableArray
#elif __GLASGOW_HASKELL__ == 201
	GlaExts.MutableArray
#else
	_MutableArray
#endif
  ) where

IMP_Ubiq(){-uitous-}

IMPORT_DELOOPER(TcMLoop) ( TcEnv, initEnv, TcMaybe )  -- We need the type TcEnv and an initial Env

import Type		( SYN_IE(Type), GenType )
import TyVar		( SYN_IE(TyVar), GenTyVar )
import Usage		( SYN_IE(Usage), GenUsage )
import ErrUtils		( SYN_IE(Error), SYN_IE(Message), SYN_IE(Warning) )
import CmdLineOpts      ( opt_PprStyle_All, opt_PprUserLength )

import SST
import Bag		( Bag, emptyBag, isEmptyBag,
			  foldBag, unitBag, unionBags, snocBag )
import FiniteMap	( FiniteMap, emptyFM, isEmptyFM{-, keysFM ToDo:rm-} )
import Maybes		( MaybeErr(..) )
import SrcLoc		( SrcLoc, noSrcLoc )
import UniqFM		( UniqFM, emptyUFM )
import UniqSupply	( UniqSupply, getUnique, getUniques, splitUniqSupply,
			  SYN_IE(UniqSM), initUs )
import Unique		( Unique )
import Util
import Pretty
import Outputable	( PprStyle(..), Outputable(..) )


infixr 9 `thenTc`, `thenTc_`, `thenNF_Tc`, `thenNF_Tc_` 
\end{code}


\section{TcM, NF_TcM: the type checker monads}
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
type NF_TcM s r =  TcDown s -> TcEnv s -> SST s r
type TcM    s r =  TcDown s -> TcEnv s -> FSST s r ()
\end{code}

\begin{code}
#if __GLASGOW_HASKELL__ >= 200
# define REAL_WORLD RealWorld
#else
# define REAL_WORLD _RealWorld
#endif

-- With a builtin polymorphic type for runSST the type for
-- initTc should use  TcM s r  instead of  TcM RealWorld r 

initTc :: UniqSupply
       -> TcM REAL_WORLD r
       -> MaybeErr (r, Bag Warning)
		   (Bag Error, Bag  Warning)

initTc us do_this
  = runSST (
      newMutVarSST us 			`thenSST` \ us_var ->
      newMutVarSST (emptyBag,emptyBag)	`thenSST` \ errs_var ->
      newMutVarSST emptyUFM		`thenSST` \ tvs_var ->
      let
          init_down = TcDown [] us_var
			     noSrcLoc
			     [] errs_var
	  init_env  = initEnv tvs_var
      in
      recoverSST
	(\_ -> returnSST Nothing)
        (do_this init_down init_env `thenFSST` \ res ->
	 returnFSST (Just res))
      					`thenSST` \ maybe_res ->
      readMutVarSST errs_var 		`thenSST` \ (warns,errs) ->
      case (maybe_res, isEmptyBag errs) of
        (Just res, True) -> returnSST (Succeeded (res, warns))
	_ 		 -> returnSST (Failed (errs, warns))
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
getErrsTc :: NF_TcM s (Bag Error, Bag  Warning)
getErrsTc down env
  = readMutVarSST errs_var 
  where
    errs_var = getTcErrs down

failTc :: Message -> TcM s a
failTc err_msg down env
  = readMutVarSST errs_var	`thenSST` \ (warns,errs) ->
    listNF_Tc ctxt down env	`thenSST` \ ctxt_msgs ->
    let
	err = mkTcErr loc ctxt_msgs err_msg
    in
    writeMutVarSST errs_var (warns, errs `snocBag` err)	`thenSST_`
    failFSST ()
  where
    errs_var = getTcErrs down
    ctxt     = getErrCtxt down
    loc      = getLoc down

warnTc :: Bool -> Message -> NF_TcM s ()
warnTc warn_if_true warn down env
  = if warn_if_true then
	readMutVarSST errs_var					`thenSST` \ (warns,errs) ->
	writeMutVarSST errs_var (warns `snocBag` warn, errs) 	`thenSST_`
    	returnSST ()
    else
	returnSST ()
  where
    errs_var = getTcErrs down

recoverTc :: TcM s r -> TcM s r -> TcM s r
recoverTc recover m down env
  = recoverFSST (\ _ -> recover down env) (m down env)

recoverNF_Tc :: NF_TcM s r -> TcM s r -> NF_TcM s r
recoverNF_Tc recover m down env
  = recoverSST (\ _ -> recover down env) (m down env)

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

checkTc :: Bool -> Message -> TcM s ()		-- Check that the boolean is true
checkTc True  err = returnTc ()
checkTc False err = failTc err

checkTcM :: Bool -> TcM s () -> TcM s ()	-- Check that the boolean is true
checkTcM True  err = returnTc ()
checkTcM False err = err

checkMaybeTc :: Maybe val -> Message -> TcM s val
checkMaybeTc (Just val) err = returnTc val
checkMaybeTc Nothing    err = failTc err

checkMaybeTcM :: Maybe val -> TcM s val -> TcM s val
checkMaybeTcM (Just val) err = returnTc val
checkMaybeTcM Nothing    err = err
\end{code}

Mutable variables
~~~~~~~~~~~~~~~~~
\begin{code}
tcNewMutVar :: a -> NF_TcM s (MutableVar s a)
tcNewMutVar val down env = newMutVarSST val

tcWriteMutVar :: MutableVar s a -> a -> NF_TcM s ()
tcWriteMutVar var val down env = writeMutVarSST var val

tcReadMutVar :: MutableVar s a -> NF_TcM s a
tcReadMutVar var down env = readMutVarSST var
\end{code}


Environment
~~~~~~~~~~~
\begin{code}
tcGetEnv :: NF_TcM s (TcEnv s)
tcGetEnv down env = returnSST env

tcSetEnv :: TcEnv s -> TcM s a -> TcM s a
tcSetEnv new_env m down old_env = m down new_env
\end{code}


Source location
~~~~~~~~~~~~~~~
\begin{code}
tcGetDefaultTys :: NF_TcM s [Type]
tcGetDefaultTys down env = returnSST (getDefaultTys down)

tcSetDefaultTys :: [Type] -> TcM s r -> TcM s r
tcSetDefaultTys tys m down env = m (setDefaultTys down tys) env

tcAddSrcLoc :: SrcLoc -> TcM s a -> TcM s a
tcAddSrcLoc loc m down env = m (setLoc down loc) env

tcGetSrcLoc :: NF_TcM s SrcLoc
tcGetSrcLoc down env = returnSST (getLoc down)

tcSetErrCtxtM, tcAddErrCtxtM :: NF_TcM s Message -> TcM s a -> TcM s a
tcSetErrCtxtM msg m down env = m (setErrCtxt down msg) env
tcAddErrCtxtM msg m down env = m (addErrCtxt down msg) env

tcSetErrCtxt, tcAddErrCtxt :: Message -> TcM s a -> TcM s a
tcSetErrCtxt msg m down env = m (setErrCtxt down (returnNF_Tc msg)) env
tcAddErrCtxt msg m down env = m (addErrCtxt down (returnNF_Tc msg)) env
\end{code}


Unique supply
~~~~~~~~~~~~~
\begin{code}
tcGetUnique :: NF_TcM s Unique
tcGetUnique down env
  = readMutVarSST u_var				`thenSST` \ uniq_supply ->
    let
      (new_uniq_supply, uniq_s) = splitUniqSupply uniq_supply
      uniq			= getUnique uniq_s
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
      uniqs			= getUniques n uniq_s
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

	(MutableVar s UniqSupply)	-- Unique supply

	SrcLoc				-- Source location
	(ErrCtxt s)			-- Error context
	(MutableVar s (Bag Warning, 
		       Bag Error))

type ErrCtxt s = [NF_TcM s Message]	-- Innermost first.  Monadic so that we have a chance
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

mkTcErr :: SrcLoc 		-- Where
	-> [Message] 		-- Context
	-> Message 		-- What went wrong
	-> TcError		-- The complete error report

mkTcErr locn ctxt msg sty
  = hang (hcat [ppr (PprForUser opt_PprUserLength) locn, ptext SLIT(": "), msg sty])
    	 4 (vcat [msg sty | msg <- ctxt_to_use])
    where
     ctxt_to_use =
       if opt_PprStyle_All then
	  ctxt
       else
	  takeAtMost 4 ctxt

     takeAtMost :: Int -> [a] -> [a]
     takeAtMost 0 ls = []
     takeAtMost n [] = []
     takeAtMost n (x:xs) = x:takeAtMost (n-1) xs

arityErr kind name n m sty
  = hsep [ ppr sty name, ptext SLIT("should have"),
	   n_arguments <> comma, text "but has been given", int m, char '.']
    where
	errmsg = kind ++ " has too " ++ quantity ++ " arguments"
	quantity | m < n     = "few"
		 | otherwise = "many"
	n_arguments | n == 0 = ptext SLIT("no arguments")
		    | n == 1 = ptext SLIT("1 argument")
		    | True   = hsep [int n, ptext SLIT("arguments")]
\end{code}


