%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnMonad]{The monad used by the renamer}

\begin{code}
#include "HsVersions.h"

module RnMonad (
	RnMonad(..), RnM(..), RnM_Fixes(..), RnDown, SST_R,
	initRn, thenRn, thenRn_, andRn, returnRn,
	mapRn, mapAndUnzipRn, mapAndUnzip3Rn,

	addErrRn, addErrIfRn, addWarnRn, addWarnIfRn,
	failButContinueRn, warnAndContinueRn,
	setExtraRn, getExtraRn, getRnEnv,
	getModuleRn, pushSrcLocRn, getSrcLocRn,
	getSourceRn, getOccurrenceUpRn,
	getImplicitUpRn, ImplicitEnv(..), emptyImplicitEnv,
	rnGetUnique, rnGetUniques,

	newLocalNames,
	lookupValue, lookupConstr, lookupField, lookupClassOp,
	lookupTyCon, lookupClass, lookupTyConOrClass,
	extendSS2, extendSS,

	TyVarNamesEnv(..), mkTyVarNamesEnv, domTyVarNamesEnv,
	lookupTyVarName, nullTyVarNamesEnv, catTyVarNamesEnvs,

	fixIO
    ) where

IMP_Ubiq(){-uitous-}

import SST

import HsSyn		( FixityDecl )
import RnHsSyn		( RnName, mkRnName, mkRnUnbound, mkRnImplicit,
			  mkRnImplicitTyCon, mkRnImplicitClass, 
			  isRnLocal, isRnWired, isRnTyCon, isRnClass,
			  isRnTyConOrClass, isRnConstr, isRnField,
			  isRnClassOp, RenamedFixityDecl(..) )
import RnUtils		( RnEnv(..), extendLocalRnEnv,
			  lookupRnEnv, lookupGlobalRnEnv, lookupTcRnEnv,
			  qualNameErr, dupNamesErr
			)

import Bag		( Bag, emptyBag, isEmptyBag, snocBag )
import CmdLineOpts	( opt_WarnNameShadowing )
import ErrUtils		( addErrLoc, addShortErrLocLine, addShortWarnLocLine,
			  Error(..), Warning(..)
			)
import FiniteMap	( FiniteMap, emptyFM, lookupFM, addToFM, fmToList{-ToDo:rm-} )
import Maybes		( assocMaybe )
import Name		( Module(..), RdrName(..), isQual,
			  Name, mkLocalName, mkImplicitName,
			  getOccName, pprNonSym
			)
import PrelInfo		( builtinNameInfo, BuiltinNames(..), BuiltinKeys(..) )
import PrelMods		( pRELUDE )
import PprStyle{-ToDo:rm-}
import Outputable{-ToDo:rm-}
import Pretty--ToDo:rm		( Pretty(..), PrettyRep )
import SrcLoc		( SrcLoc, mkUnknownSrcLoc )
import UniqFM		( UniqFM, emptyUFM )
import UniqSet		( UniqSet(..), mkUniqSet, minusUniqSet )
import UniqSupply	( UniqSupply, getUnique, getUniques, splitUniqSupply )
import Unique		( Unique )
import Util

infixr 9 `thenRn`, `thenRn_`
\end{code}

\begin{code}
type RnM s r       = RnMonad () s r
type RnM_Fixes s r = RnMonad (UniqFM RenamedFixityDecl) s r

type RnMonad x s r = RnDown x s -> SST s r

data RnDown x s
  = RnDown
	x
	Module				-- Module name
	SrcLoc				-- Source location
	(RnMode s)			-- Source or Iface
	RnEnv				-- Renaming environment
	(MutableVar s UniqSupply)	-- Unique supply
	(MutableVar s (Bag Warning, 	-- Warnings and Errors
		       Bag Error))

data RnMode s
 = RnSource (MutableVar s (Bag (RnName, RdrName)))
	-- Renaming source; returning occurences

 | RnIface  BuiltinNames BuiltinKeys
	    (MutableVar s ImplicitEnv)
	-- Renaming interface; creating and returning implicit names
	-- ImplicitEnv: one map for Values and one for TyCons/Classes.

type ImplicitEnv = (FiniteMap RdrName RnName, FiniteMap RdrName RnName)
emptyImplicitEnv :: ImplicitEnv
emptyImplicitEnv = (emptyFM, emptyFM)

-- With a builtin polymorphic type for _runSST the type for
-- initTc should use  RnM s r  instead of  RnM _RealWorld r 

initRn :: Bool		-- True => Source; False => Iface
       -> Module
       -> RnEnv
       -> UniqSupply
       -> RnM _RealWorld r
       -> (r, Bag Error, Bag Warning)

initRn source mod env us do_rn
  = _runSST (
	newMutVarSST emptyBag			`thenSST` \ occ_var ->
	newMutVarSST emptyImplicitEnv		`thenSST` \ imp_var ->
	newMutVarSST us 			`thenSST` \ us_var ->
      	newMutVarSST (emptyBag,emptyBag)	`thenSST` \ errs_var ->
	let
	    mode = if source then
		       RnSource occ_var
	           else
		       case builtinNameInfo of { (wiredin_fm, key_fm, _) ->
		       RnIface wiredin_fm key_fm imp_var }

	    rn_down = RnDown () mod mkUnknownSrcLoc mode env us_var errs_var
	in
	-- do the buisness
    	do_rn rn_down 				`thenSST` \ res ->

	-- grab errors and return
	readMutVarSST errs_var 			`thenSST` \ (warns,errs) ->
	returnSST (res, errs, warns)
    )

{-# INLINE thenRn #-}
{-# INLINE thenRn_ #-}
{-# INLINE returnRn #-}
{-# INLINE andRn #-}

returnRn :: a -> RnMonad x s a
thenRn   :: RnMonad x s a -> (a -> RnMonad x s b) -> RnMonad x s b
thenRn_  :: RnMonad x s a -> RnMonad x s b -> RnMonad x s b
andRn    :: (a -> a -> a) -> RnMonad x s a -> RnMonad x s a -> RnMonad x s a
mapRn    :: (a -> RnMonad x s b) -> [a] -> RnMonad x s [b]
mapAndUnzipRn :: (a -> RnMonad x s (b,c)) -> [a] -> RnMonad x s ([b],[c])

returnRn v down  = returnSST v
thenRn m k down  = m down `thenSST` \ r -> k r down
thenRn_ m k down = m down `thenSST_` k down

andRn combiner m1 m2 down
  = m1 down `thenSST` \ res1 ->
    m2 down `thenSST` \ res2 ->
    returnSST (combiner res1 res2)

mapRn f []     = returnRn []
mapRn f (x:xs)
  = f x		`thenRn` \ r ->
    mapRn f xs 	`thenRn` \ rs ->
    returnRn (r:rs)

mapAndUnzipRn f [] = returnRn ([],[])
mapAndUnzipRn f (x:xs)
  = f x		    	`thenRn` \ (r1,  r2)  ->
    mapAndUnzipRn f xs	`thenRn` \ (rs1, rs2) ->
    returnRn (r1:rs1, r2:rs2)

mapAndUnzip3Rn f [] = returnRn ([],[],[])
mapAndUnzip3Rn f (x:xs)
  = f x		    	`thenRn` \ (r1,  r2,  r3)  ->
    mapAndUnzip3Rn f xs	`thenRn` \ (rs1, rs2, rs3) ->
    returnRn (r1:rs1, r2:rs2, r3:rs3)
\end{code}

For errors and warnings ...
\begin{code}
failButContinueRn :: a -> Error -> RnMonad x s a
failButContinueRn res err (RnDown _ _ _ _ _ _ errs_var)
  = readMutVarSST  errs_var  				`thenSST`  \ (warns,errs) ->
    writeMutVarSST errs_var (warns, errs `snocBag` err)	`thenSST_` 
    returnSST res

warnAndContinueRn :: a -> Warning -> RnMonad x s a
warnAndContinueRn res warn (RnDown _ _ _ _ _ _ errs_var)
  = readMutVarSST  errs_var  				 `thenSST`  \ (warns,errs) ->
    writeMutVarSST errs_var (warns `snocBag` warn, errs) `thenSST_` 
    returnSST res

addErrRn :: Error -> RnMonad x s ()
addErrRn err = failButContinueRn () err

addErrIfRn :: Bool -> Error -> RnMonad x s ()
addErrIfRn True err  = addErrRn err
addErrIfRn False err = returnRn ()

addWarnRn :: Warning -> RnMonad x s ()
addWarnRn warn = warnAndContinueRn () warn

addWarnIfRn :: Bool -> Warning -> RnMonad x s ()
addWarnIfRn True warn  = addWarnRn warn
addWarnIfRn False warn = returnRn ()
\end{code}


\begin{code}
getRnEnv :: RnMonad x s RnEnv
getRnEnv (RnDown _ _ _ _ env _ _)
  = returnSST env

setExtraRn :: x -> RnMonad x s r -> RnMonad y s r
setExtraRn x m (RnDown _ mod locn mode env us errs)
  = m (RnDown x mod locn mode env us errs)

getExtraRn :: RnMonad x s x
getExtraRn (RnDown x _ _ _ _ _ _)
  = returnSST x

getModuleRn :: RnMonad x s Module
getModuleRn (RnDown _ mod _ _ _ _ _)
  = returnSST mod

pushSrcLocRn :: SrcLoc -> RnMonad x s a -> RnMonad x s a
pushSrcLocRn locn m (RnDown x mod _ mode env us errs)
  = m (RnDown x mod locn mode env us errs)

getSrcLocRn :: RnMonad x s SrcLoc
getSrcLocRn (RnDown _ _ locn _ _ _ _)
  = returnSST locn

getSourceRn :: RnMonad x s Bool
getSourceRn (RnDown _ _ _ (RnSource _)    _ _ _) = returnSST True
getSourceRn (RnDown _ _ _ (RnIface _ _ _) _ _ _) = returnSST False

getOccurrenceUpRn :: RnMonad x s (Bag (RnName, RdrName))
getOccurrenceUpRn (RnDown _ _ _ (RnSource occ_var) _ _ _)
  = readMutVarSST occ_var
getOccurrenceUpRn (RnDown _ _ _ (RnIface _ _ _) _ _ _)
  = panic "getOccurrenceUpRn:RnIface"

getImplicitUpRn :: RnMonad x s ImplicitEnv
getImplicitUpRn (RnDown _ _ _ (RnIface _ _ imp_var) _ _ _)
  = readMutVarSST imp_var
getImplicitUpRn (RnDown _ _ _(RnSource _) _ _ _)
  = panic "getImplicitUpRn:RnIface"
\end{code}

\begin{code}
rnGetUnique :: RnMonad x s Unique
rnGetUnique (RnDown _ _ _ _ _ us_var _)
  = get_unique us_var

rnGetUniques :: Int -> RnMonad x s [Unique]
rnGetUniques n (RnDown _ _ _ _ _ us_var _)
  = get_uniques n us_var


get_unique us_var
  = readMutVarSST us_var			`thenSST` \ uniq_supply ->
    let
      (new_uniq_supply, uniq_s) = splitUniqSupply uniq_supply
      uniq			= getUnique uniq_s
    in
    writeMutVarSST us_var new_uniq_supply	`thenSST_`
    returnSST uniq

get_uniques n us_var
  = readMutVarSST us_var			`thenSST` \ uniq_supply ->
    let
      (new_uniq_supply, uniq_s) = splitUniqSupply uniq_supply
      uniqs			= getUniques n uniq_s
    in
    writeMutVarSST us_var new_uniq_supply	`thenSST_`
    returnSST uniqs

snoc_bag_var add bag_var
  = readMutVarSST bag_var	`thenSST` \ bag ->
    writeMutVarSST bag_var (bag `snocBag` add)

\end{code}

*********************************************************
*							*
\subsection{Making new names}
*							*
*********************************************************

@newLocalNames@ takes a bunch of RdrNames, which are defined together
in a group (eg a pattern or set of bindings), checks they are
unqualified and distinct, and creates new Names for them.

\begin{code}
newLocalNames :: String 		-- Documentation string
	      -> [(RdrName, SrcLoc)]
	      -> RnMonad x s [RnName]

newLocalNames str names_w_loc
  = mapRn (addErrRn . qualNameErr str) quals 	`thenRn_`
    mapRn (addErrRn . dupNamesErr str) dups  	`thenRn_`
    mkLocalNames these
  where
    quals = filter (isQual.fst) names_w_loc
    (these, dups) = removeDups cmp_fst names_w_loc
    cmp_fst (a,_) (b,_) = cmp a b
\end{code}

\begin{code}
mkLocalNames :: [(RdrName, SrcLoc)] -> RnMonad x s [RnName]
mkLocalNames names_w_locs
  = rnGetUniques (length names_w_locs) 	`thenRn` \ uniqs ->
    returnRn (zipWithEqual "mkLocalNames" new_local uniqs names_w_locs)
  where
    new_local uniq (Unqual str, srcloc)
      = mkRnName (mkLocalName uniq str False{-emph names-} srcloc)
\end{code}


*********************************************************
*							*
\subsection{Looking up values}
*							*
*********************************************************

Action to look up a value depends on the RnMode.
\begin{description}
\item[RnSource:]
Lookup value in RnEnv, recording occurrence for non-local values found.
If not found report error and return Unbound name.
\item[RnIface:]
Lookup value in RnEnv. If not found lookup in implicit name env.
If not found create new implicit name, adding it to the implicit env.
\end{description}

\begin{code}
lookupValue      :: RdrName -> RnMonad x s RnName
lookupConstr     :: RdrName -> RnMonad x s RnName
lookupField      :: RdrName -> RnMonad x s RnName
lookupClassOp    :: RnName  -> RdrName -> RnMonad x s RnName

lookupValue rdr
  = lookup_val rdr lookupRnEnv (\ rn -> True) (unknownNameErr "value")

lookupConstr rdr
  = lookup_val rdr lookupGlobalRnEnv isRnConstr (unknownNameErr "constructor")

lookupField rdr
  = lookup_val rdr lookupGlobalRnEnv isRnField (unknownNameErr "field")

lookupClassOp cls rdr
  = lookup_val rdr lookupGlobalRnEnv (\ rn -> isRnClassOp cls rn) (badClassOpErr cls)

-- Note: the lookup checks are only performed when renaming source

lookup_val rdr lookup check do_err down@(RnDown _ _ locn (RnSource occ_var) env _ _)
  = case lookup env rdr of
	Just name | check name -> succ name
		  | otherwise  -> fail
	Nothing                -> fail

  where
    succ name = if isRnLocal name || isRnWired name then
		    returnSST name
		else
		    snoc_bag_var (name,rdr) occ_var `thenSST_`
		    returnSST name
    fail = failButContinueRn (mkRnUnbound rdr) (do_err rdr locn) down

lookup_val rdr lookup check do_err down@(RnDown _ _ locn (RnIface b_names b_key imp_var) env us_var _)
  = case lookup env rdr of
	Just name -> returnSST name
	Nothing   -> lookup_nonexisting_val b_names b_key imp_var us_var rdr

lookup_nonexisting_val (b_names,_) b_key imp_var us_var rdr
  = let str_mod = case rdr of { Qual m n -> (n,m); Unqual n -> (n, pRELUDE) }
    in case (lookupFM b_names str_mod) of
	 Nothing -> lookup_or_create_implicit_val b_key imp_var us_var rdr
	 Just xx -> returnSST xx

lookup_or_create_implicit_val b_key imp_var us_var rdr
  = readMutVarSST imp_var `thenSST` \ (implicit_val_fm, implicit_tc_fm) ->
    case lookupFM implicit_val_fm rdr of
	Just implicit -> returnSST implicit
	Nothing ->
	  (let str_mod = case rdr of { Qual m n -> (n,m); Unqual n -> (n, pRELUDE) }
	   in case (lookupFM b_key str_mod) of
		Just (u,_) -> returnSST u
		_          -> get_unique us_var
	  )							`thenSST` \ uniq -> 
	  let
	      implicit   = mkRnImplicit (mkImplicitName uniq rdr)
	      new_val_fm = addToFM implicit_val_fm rdr implicit
	  in
	  writeMutVarSST imp_var (new_val_fm, implicit_tc_fm) 	`thenSST_`
	  returnSST implicit
\end{code}


\begin{code}
lookupTyCon   :: RdrName -> RnMonad x s RnName
lookupClass   :: RdrName -> RnMonad x s RnName

lookupTyCon rdr
  = lookup_tc rdr isRnTyCon mkRnImplicitTyCon "type constructor"

lookupClass rdr
  = lookup_tc rdr isRnClass mkRnImplicitClass "class"

lookupTyConOrClass rdr
  = lookup_tc rdr isRnTyConOrClass
	      (panic "lookupTC:mk_implicit") "class or type constructor"

lookup_tc rdr check mk_implicit err_str down@(RnDown _ _ locn (RnSource occ_var) env _ _)
  = case lookupTcRnEnv env rdr of
       Just name | check name -> succ name
	         | otherwise  -> fail
       Nothing                -> fail
  where
    succ name = snoc_bag_var (name,rdr) occ_var `thenSST_`
		returnSST name
    fail = failButContinueRn (mkRnUnbound rdr) (unknownNameErr err_str rdr locn) down

lookup_tc rdr check mk_implicit err_str down@(RnDown _ _ locn (RnIface b_names b_key imp_var) env us_var _)
  = case lookupTcRnEnv env rdr of
	Just name | check name -> returnSST name
		  | otherwise  -> fail
	Nothing -> lookup_nonexisting_tc check mk_implicit fail b_names b_key imp_var us_var rdr
  where
    fail = failButContinueRn (mkRnUnbound rdr) (unknownNameErr err_str rdr locn) down

lookup_nonexisting_tc check mk_implicit fail (_,b_names) b_key imp_var us_var rdr
  = let
	str_mod = case rdr of { Qual m n -> (n,m); Unqual n -> (n, pRELUDE) }
    in
    --pprTrace "lookup:" (ppAboves [case str_mod of {(n,m)->ppCat [ppPStr n, ppPStr m]}, ppAboves [ ppCat [ppPStr n, ppPStr m] | ((n,m), _) <- fmToList b_names]]) $
    case (lookupFM b_names str_mod) of
      Nothing -> lookup_or_create_implicit_tc check mk_implicit fail b_key imp_var us_var rdr
      Just xx -> returnSST xx

lookup_or_create_implicit_tc check mk_implicit fail b_key imp_var us_var rdr
  = readMutVarSST imp_var `thenSST` \ (implicit_val_fm, implicit_tc_fm) ->
    case lookupFM implicit_tc_fm rdr of
	Just implicit | check implicit -> returnSST implicit
		      | otherwise      -> fail
	Nothing ->
	  (let str_mod = case rdr of { Qual m n -> (n,m); Unqual n -> (n, pRELUDE) }
	   in case (lookupFM b_key str_mod) of
		Just (u,_) -> returnSST u
		_          -> get_unique us_var
	  )							`thenSST` \ uniq -> 
	  let
	      implicit  = mk_implicit (mkImplicitName uniq rdr)
	      new_tc_fm = addToFM implicit_tc_fm rdr implicit
	  in
	  writeMutVarSST imp_var (implicit_val_fm, new_tc_fm) 	`thenSST_`
	  returnSST implicit
\end{code}


@extendSS@ extends the scope; @extendSS2@ also removes the newly bound
free vars from the result.

\begin{code}
extendSS :: [RnName] 				-- Newly bound names
	 -> RnMonad x s a
	 -> RnMonad x s a

extendSS binders m down@(RnDown x mod locn mode env us errs)
  = (mapRn (addErrRn . shadowedNameWarn locn) dups `thenRn_`
     m) (RnDown x mod locn mode new_env us errs)
  where
    (new_env,dups) = extendLocalRnEnv opt_WarnNameShadowing env binders

extendSS2 :: [RnName] 				-- Newly bound names
	  -> RnMonad x s (a, UniqSet RnName)
	  -> RnMonad x s (a, UniqSet RnName)

extendSS2 binders m
  = extendSS binders m `thenRn` \ (r, fvs) ->
    returnRn (r, fvs `minusUniqSet` (mkUniqSet binders))
\end{code}

The free var set returned by @(extendSS binders m)@ is that returned
by @m@, {\em minus} binders.


*********************************************************
*							*
\subsection{TyVarNamesEnv}
*							*
*********************************************************

\begin{code}
type TyVarNamesEnv = [(RdrName, RnName)]

nullTyVarNamesEnv :: TyVarNamesEnv
nullTyVarNamesEnv = []

catTyVarNamesEnvs :: TyVarNamesEnv -> TyVarNamesEnv -> TyVarNamesEnv
catTyVarNamesEnvs e1 e2 = e1 ++ e2

domTyVarNamesEnv :: TyVarNamesEnv -> [RdrName]
domTyVarNamesEnv env = map fst env
\end{code}

@mkTyVarNamesEnv@ checks for duplicates, and complains if so.

\begin{code}
mkTyVarNamesEnv
	:: SrcLoc
	-> [RdrName]				-- The type variables
	-> RnMonad x s (TyVarNamesEnv,[RnName])	-- Environment and renamed tyvars

mkTyVarNamesEnv src_loc tyvars
  = newLocalNames "type variable"
	 (tyvars `zip` repeat src_loc) `thenRn`  \ rn_tyvars ->

	 -- rn_tyvars may not be in the same order as tyvars, so we need some
	 -- jiggery pokery to build the right tyvar env, and return the
	 -- renamed tyvars in the original order.
    let tv_occ_name_pairs 	= map tv_occ_name_pair rn_tyvars
    	tv_env	    	    	= map (lookup_occ_name tv_occ_name_pairs) tyvars
	rn_tyvars_in_orig_order	= map snd tv_env
    in
    returnRn (tv_env, rn_tyvars_in_orig_order)
  where
    tv_occ_name_pair :: RnName -> (RdrName, RnName)
    tv_occ_name_pair rn_name = (getOccName rn_name, rn_name)

    lookup_occ_name :: [(RdrName, RnName)] -> RdrName -> (RdrName, RnName)
    lookup_occ_name pairs tyvar_occ
      = (tyvar_occ, assoc "mkTyVarNamesEnv" pairs tyvar_occ)
\end{code}

\begin{code}
lookupTyVarName :: TyVarNamesEnv -> RdrName -> RnMonad x s RnName
lookupTyVarName env occ
  = case (assocMaybe env occ) of
      Just name -> returnRn name
      Nothing   -> getSrcLocRn	`thenRn` \ loc ->
		   failButContinueRn (mkRnUnbound occ)
		       (unknownNameErr "type variable" occ loc)
\end{code}


\begin{code}
fixIO :: (a -> IO a) -> IO a
fixIO k s = let
		result          = k loop s
		(Right loop, _) = result
	    in
	    result
\end{code}

*********************************************************
*							*
\subsection{Errors used in RnMonad}
*							*
*********************************************************

\begin{code}
unknownNameErr descriptor name locn
  = addShortErrLocLine locn $ \ sty ->
    ppBesides [ppStr "undefined ", ppStr descriptor, ppStr ": ", pprNonSym sty name]

badClassOpErr clas op locn
  = addErrLoc locn "" $ \ sty ->
    ppBesides [ppChar '`', pprNonSym sty op, ppStr "' is not an operation of class `",
	      ppr sty clas, ppStr "'"]

shadowedNameWarn locn shadow
  = addShortWarnLocLine locn $ \ sty ->
    ppBesides [ppStr "more than one value with the same name (shadowing): ", ppr sty shadow]
\end{code}
