%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[RenameMonad4]{The monad used by the fourth renamer pass}

\begin{code}
#include "HsVersions.h"

module RenameMonad4 (
	Rn4M(..),
	initRn4, thenRn4, thenRn4_, andRn4, returnRn4, mapRn4, mapAndUnzipRn4,
	addErrRn4, failButContinueRn4, recoverQuietlyRn4,
	pushSrcLocRn4,
	getSrcLocRn4,
	getSwitchCheckerRn4,
	lookupValue, lookupValueEvenIfInvisible,
	lookupClassOp, lookupFixityOp,
	lookupTyCon, lookupTyConEvenIfInvisible,
	lookupClass,
	extendSS2, extendSS,
	namesFromProtoNames,

	TyVarNamesEnv(..), mkTyVarNamesEnv, domTyVarNamesEnv,
	lookupTyVarName, nullTyVarNamesEnv, catTyVarNamesEnvs,

	-- for completeness
	Module, Bag, RenamedPat(..), InPat, Maybe, Name, Error(..),
	Pretty(..), PprStyle, PrettyRep, ProtoName, GlobalSwitch,
	GlobalNameFun(..), GlobalNameFuns(..), UniqSet(..), UniqFM, SrcLoc,
	Unique, SplitUniqSupply
	IF_ATTACK_PRAGMAS(COMMA splitUniqSupply)
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Pretty
import Outputable

import AbsSyn
import Bag
import CmdLineOpts	( GlobalSwitch(..) )
import Errors		( dupNamesErr, unknownNameErr, shadowedNameErr,
			  badClassOpErr, Error(..)
			)
import FiniteMap	( lookupFM, addToFM, addListToFM, emptyFM, FiniteMap )
import Maybes		( Maybe(..), assocMaybe )
import Name		( isTyConName, isClassName, isClassOpName,
			  isUnboundName, invisibleName
			)
import NameTypes	( mkShortName, ShortName )
import ProtoName	-- lots of stuff
import RenameAuxFuns	-- oh, why not ... all of it
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import SplitUniq
import UniqSet
import Unique
import Util

infixr 9 `thenRn4`, `thenRn4_`
\end{code}

%************************************************************************
%*									*
\subsection[RenameMonad]{Plain @Rename@ monadery}
%*									*
%************************************************************************

\begin{code}
type ScopeStack = FiniteMap FAST_STRING Name

type Rn4M result
  =  (GlobalSwitch -> Bool)
  -> GlobalNameFuns
  -> ScopeStack
  -> Bag Error
  -> SplitUniqSupply
  -> SrcLoc
  -> (result, Bag Error)

#ifdef __GLASGOW_HASKELL__
{-# INLINE andRn4 #-}
{-# INLINE thenRn4 #-}
{-# INLINE thenLazilyRn4 #-}
{-# INLINE thenRn4_ #-}
{-# INLINE returnRn4 #-}
#endif

initRn4 :: (GlobalSwitch -> Bool)
	-> GlobalNameFuns
	-> Rn4M result
	-> SplitUniqSupply
	-> (result, Bag Error)

initRn4 sw_chkr gnfs renamer init_us
  = renamer sw_chkr gnfs emptyFM emptyBag init_us mkUnknownSrcLoc

thenRn4, thenLazilyRn4
	 :: Rn4M a -> (a -> Rn4M b) -> Rn4M b
thenRn4_ :: Rn4M a -> Rn4M b -> Rn4M b
andRn4   :: (a -> a -> a) -> Rn4M a -> Rn4M a -> Rn4M a

thenRn4 expr cont sw_chkr gnfs ss errs uniqs locn
  = case (splitUniqSupply uniqs)    	    	   of { (s1, s2) ->
    case (expr      sw_chkr gnfs ss errs  s1 locn) of { (res1, errs1) ->
    case (cont res1 sw_chkr gnfs ss errs1 s2 locn) of { (res2, errs2) ->
    (res2, errs2) }}}

thenLazilyRn4 expr cont sw_chkr gnfs ss errs uniqs locn
  = let
	(s1, s2)      = splitUniqSupply uniqs
	(res1, errs1) = expr      sw_chkr gnfs ss errs  s1 locn
	(res2, errs2) = cont res1 sw_chkr gnfs ss errs1 s2 locn
    in
    (res2, errs2)

thenRn4_ expr cont sw_chkr gnfs ss errs uniqs locn
  = case (splitUniqSupply uniqs)    	      of { (s1, s2) ->
    case (expr sw_chkr gnfs ss errs  s1 locn) of { (_,    errs1) ->
    case (cont sw_chkr gnfs ss errs1 s2 locn) of { (res2, errs2) ->
    (res2, errs2) }}}

andRn4 combiner m1 m2 sw_chkr gnfs ss errs us locn
  = case (splitUniqSupply us)	    	    of { (s1, s2) ->
    case (m1 sw_chkr gnfs ss errs  s1 locn) of { (res1, errs1) ->
    case (m2 sw_chkr gnfs ss errs1 s2 locn) of { (res2, errs2) ->
    (combiner res1 res2, errs2) }}}

returnRn4 :: a -> Rn4M a
returnRn4 result sw_chkr gnfs ss errs_so_far uniqs locn
   = (result, errs_so_far)

failButContinueRn4 :: a -> Error -> Rn4M a
failButContinueRn4 res err sw_chkr gnfs ss errs_so_far uniqs locn
  = (res, errs_so_far `snocBag` err)

addErrRn4 :: Error -> Rn4M ()
addErrRn4 err sw_chkr gnfs ss errs_so_far uniqs locn
  = ((), errs_so_far `snocBag` err)
\end{code}

When we're looking at interface pragmas, we want to be able to recover
back to a ``I don't know anything pragmatic'' state if we encounter
some problem.  @recoverQuietlyRn4@ is given a ``use-this-instead'' value,
as well as the action to perform.  This code is intentionally very lazy,
returning a triple immediately, no matter what.
\begin{code}
recoverQuietlyRn4 :: a -> Rn4M a -> Rn4M a

recoverQuietlyRn4 use_this_if_err action sw_chkr gnfs ss errs_so_far uniqs locn
  = let
	(result, errs_out)
    	  = case (action sw_chkr gnfs ss emptyBag{-leav out errs-} uniqs locn) of
	      (result1, errs1) ->
		if isEmptyBag errs1 then -- all's well! (but retain incoming errs)
		    (result1, errs_so_far)
		else -- give up; return *incoming* UniqueSupply...
		    (use_this_if_err,
		     if sw_chkr ShowPragmaNameErrs
		     then errs_so_far `unionBags` errs1
		     else errs_so_far) -- toss errs, otherwise
    in
    (result, errs_out)
\end{code}

\begin{code}
mapRn4 :: (a -> Rn4M b) -> [a] -> Rn4M [b]

mapRn4 f []     = returnRn4 []
mapRn4 f (x:xs)
  = f x		`thenRn4` \ r ->
    mapRn4 f xs `thenRn4` \ rs ->
    returnRn4 (r:rs)

mapAndUnzipRn4  :: (a -> Rn4M (b,c))   -> [a] -> Rn4M ([b],[c])

mapAndUnzipRn4 f [] = returnRn4 ([],[])
mapAndUnzipRn4 f (x:xs)
  = f x		    	`thenRn4` \ (r1,  r2)  ->
    mapAndUnzipRn4 f xs	`thenRn4` \ (rs1, rs2) ->
    returnRn4 (r1:rs1, r2:rs2)
\end{code}

\begin{code}
pushSrcLocRn4 :: SrcLoc -> Rn4M a -> Rn4M a
pushSrcLocRn4 locn exp sw_chkr gnfs ss errs_so_far uniq_supply old_locn
  = exp sw_chkr gnfs ss errs_so_far uniq_supply locn

getSrcLocRn4 :: Rn4M SrcLoc

getSrcLocRn4 sw_chkr gnfs ss errs_so_far uniq_supply locn
  = returnRn4 locn sw_chkr gnfs ss errs_so_far uniq_supply locn

getSwitchCheckerRn4 :: Rn4M (GlobalSwitch -> Bool)

getSwitchCheckerRn4 sw_chkr gnfs ss errs_so_far uniq_supply locn
  = returnRn4 sw_chkr sw_chkr gnfs ss errs_so_far uniq_supply locn
\end{code}

\begin{code}
getNextUniquesFromRn4 :: Int -> Rn4M [Unique]
getNextUniquesFromRn4 n sw_chkr gnfs ss errs_so_far us locn
  = case (getSUniques n us) of { next_uniques ->
    (next_uniques, errs_so_far) }
\end{code}

*********************************************************
*							*
\subsection{Making new names}
*							*
*********************************************************

@namesFromProtoNames@ takes a bunch of protonames, which are defined
together in a group (eg a pattern or set of bindings), checks they
are distinct, and creates new full names for them.

\begin{code}
namesFromProtoNames :: String 		-- Documentation string
		    -> [(ProtoName, SrcLoc)]
		    -> Rn4M [Name]	

namesFromProtoNames kind pnames_w_src_loc sw_chkr gnfs ss errs_so_far us locn
  = (mapRn4 (addErrRn4 . dupNamesErr kind) dups `thenRn4_`
    mkNewNames goodies
    ) {-Rn4-} sw_chkr gnfs ss errs_so_far us locn
  where
    (goodies, dups) = removeDups cmp pnames_w_src_loc
	-- We want to compare their local names rather than their 
	-- full protonames.  It probably doesn't matter here, but it
	-- does in Rename3.lhs!
    cmp (a, _) (b, _) = cmpByLocalName a b
\end{code}

@mkNewNames@ assumes the names are unique.

\begin{code}
mkNewNames :: [(ProtoName, SrcLoc)] -> Rn4M [Name]	
mkNewNames pnames_w_locs
  = getNextUniquesFromRn4 (length pnames_w_locs) `thenRn4` \ uniqs ->
    returnRn4 (zipWith new_short_name uniqs pnames_w_locs)
  where
    new_short_name uniq (Unk str, srcloc)   -- gotta be an Unk...
      = Short uniq (mkShortName str srcloc)
\end{code}


*********************************************************
*							*
\subsection{Local scope extension and lookup}
*							*
*********************************************************

If the input name is an @Imp@, @lookupValue@ looks it up in the GNF.
If it is an @Unk@, it looks it up first in the local environment
(scope stack), and if it isn't found there, then in the value GNF.  If
it isn't found at all, @lookupValue@ adds an error message, and
returns an @Unbound@ name.

\begin{code}
unboundName :: ProtoName -> Name
unboundName pn
   = Unbound (grab_string pn)
   where
     grab_string (Unk s)       = s
     grab_string (Imp _ _ _ s) = s
\end{code}

@lookupValue@ looks up a non-invisible value;
@lookupValueEvenIfInvisible@ gives a successful lookup even if the
value is not visible to the user (e.g., came out of a pragma).
@lookup_val@ is the help function to do the work.

\begin{code}
lookupValue v {-Rn4-} sw_chkr gnfs ss errs_so_far us locn
  = (lookup_val v	`thenLazilyRn4` \ name ->
    if invisibleName name
    then failButContinueRn4 (unboundName v) (unknownNameErr "value" v mkUnknownSrcLoc)
    else returnRn4 name
    ) {-Rn4-} sw_chkr gnfs ss errs_so_far us locn

lookupValueEvenIfInvisible v = lookup_val v

lookup_val :: ProtoName -> Rn4M Name

lookup_val pname@(Unk v) sw_chkr gnfs@(v_gnf, tc_gnf) ss a b locn
  = case (lookupFM ss v) of
      Just name -> returnRn4 name sw_chkr gnfs ss a b locn
      Nothing   -> case (v_gnf pname) of
		     Just name  -> returnRn4 name sw_chkr gnfs ss a b locn
		     Nothing    -> failButContinueRn4 (unboundName pname)
					   (unknownNameErr "value" pname locn)
  					   sw_chkr gnfs ss a b locn

-- If it ain't an Unk it must be in the global name fun; that includes
-- prelude things.
lookup_val pname sw_chkr gnfs@(v_gnf, tc_gnf) ss a b locn
  = case (v_gnf pname) of
        Just name  -> returnRn4 name sw_chkr gnfs ss a b locn
	Nothing    -> failButContinueRn4 (unboundName pname)
			      (unknownNameErr "value" pname locn)
			      sw_chkr gnfs ss a b locn
\end{code}

Looking up the operators in a fixity decl is done differently.  We
want to simply drop any fixity decls which refer to operators which
aren't in scope.  Unfortunately, such fixity decls {\em will} appear
because the parser collects *all* the fixity decls from {\em all} the
imported interfaces (regardless of selective import), and dumps them
together as the module fixity decls.  This is really a bug.  In
particular:
\begin{itemize}
\item
We won't complain about fixity decls for operators which aren't
declared.
\item
We won't attach the right fixity to something which has been renamed.
\end{itemize}

We're not going to export Prelude-related fixities (ToDo: correctly),
so we nuke those, too.

\begin{code}
lookupFixityOp (Prel _) sw_chkr gnfs@(v_gnf, tc_gnf) = returnRn4 Nothing       sw_chkr gnfs
lookupFixityOp pname	sw_chkr gnfs@(v_gnf, tc_gnf) = returnRn4 (v_gnf pname) sw_chkr gnfs
\end{code}

\begin{code}
lookupTyCon, lookupTyConEvenIfInvisible :: ProtoName -> Rn4M Name
-- The global name funs handle Prel things

lookupTyCon tc {-Rn4-} sw_chkr gnfs ss errs_so_far us locn
  = (lookup_tycon tc `thenLazilyRn4` \ name ->
    if invisibleName name
    then failButContinueRn4 (unboundName tc) (unknownNameErr "type constructor" tc mkUnknownSrcLoc)
    else returnRn4 name
    ) {-Rn4-} sw_chkr gnfs ss errs_so_far us locn

lookupTyConEvenIfInvisible tc = lookup_tycon tc

lookup_tycon (Prel name) sw_chkr gnfs ss a b locn = returnRn4 name sw_chkr gnfs ss a b locn

lookup_tycon pname sw_chkr gnfs@(v_gnf, tc_gnf) ss a b locn
  = case (tc_gnf pname) of
     Just name | isTyConName name -> returnRn4 name sw_chkr gnfs ss a b locn
     _   -> failButContinueRn4 (unboundName pname)
		    (unknownNameErr "type constructor" pname locn)
		    sw_chkr gnfs ss a b locn
\end{code}

\begin{code}
lookupClass :: ProtoName -> Rn4M Name

lookupClass pname sw_chkr gnfs@(v_gnf, tc_gnf) ss a b locn
  = case (tc_gnf pname) of
     Just name | isClassName name -> returnRn4 name sw_chkr gnfs ss a b locn
     _   -> failButContinueRn4 (unboundName pname)
		    (unknownNameErr "class" pname locn)
		    sw_chkr gnfs ss a b locn
\end{code}

@lookupClassOp@ is used when looking up the lhs identifiers in a class
or instance decl.  It checks that the name it finds really is a class
op, and that its class matches that of the class or instance decl
being looked at.

\begin{code}
lookupClassOp :: Name -> ProtoName -> Rn4M Name

lookupClassOp class_name pname sw_chkr gnfs@(v_gnf, tc_gnf) ss a b locn
  = case v_gnf pname of
	 Just op_name |  isClassOpName class_name op_name
		      || isUnboundName class_name -- avoid spurious errors
		 -> returnRn4 op_name sw_chkr gnfs ss a b locn

	 other   -> failButContinueRn4 (unboundName pname)
			    (badClassOpErr class_name pname locn)
			    sw_chkr gnfs ss a b locn
\end{code}

@extendSS@ extends the scope; @extendSS2@ also removes the newly bound
free vars from the result.

\begin{code}
extendSS :: [Name] 				-- Newly bound names
	 -> Rn4M a
	 -> Rn4M a

extendSS binders expr sw_chkr gnfs ss errs us locn
  = case (extend binders ss sw_chkr gnfs ss errs us locn) of { (new_ss, new_errs) ->
    expr sw_chkr gnfs new_ss new_errs us locn }
  where
    extend :: [Name] -> ScopeStack -> Rn4M ScopeStack

    extend names ss
      = if (sw_chkr NameShadowingNotOK) then
	    hard_way names ss
	else -- ignore shadowing; blast 'em in
	    returnRn4 (
		addListToFM ss [ (getOccurrenceName x, n) | n@(Short _ x) <- names]
	    )

    hard_way [] ss = returnRn4 ss
    hard_way (name@(Short _ sname):names) ss
      = let
	    str = getOccurrenceName sname
	in
	(case (lookupFM ss str) of
	   Nothing -> returnRn4 (addToFM ss str name)
	   Just  _ -> failButContinueRn4 ss (shadowedNameErr name locn)

	)	`thenRn4` \ new_ss ->
	hard_way names new_ss

extendSS2 :: [Name] 				-- Newly bound names
	 -> Rn4M (a, UniqSet Name)
	 -> Rn4M (a, UniqSet Name)

extendSS2 binders expr sw_chkr gnfs ss errs_so_far us locn
  = case (extendSS binders expr sw_chkr gnfs ss errs_so_far us locn) of
     ((e2, freevars), errs)
       -> ((e2, freevars `minusUniqSet` (mkUniqSet binders)),
	   errs)
\end{code}

The free var set returned by @(extendSS binders m)@ is that returned
by @m@, {\em minus} binders.

*********************************************************
*							*
\subsection{mkTyVarNamesEnv}
*							*
*********************************************************

\begin{code}
type TyVarNamesEnv = [(ProtoName, Name)]

nullTyVarNamesEnv :: TyVarNamesEnv
nullTyVarNamesEnv = []

catTyVarNamesEnvs :: TyVarNamesEnv -> TyVarNamesEnv -> TyVarNamesEnv
catTyVarNamesEnvs e1 e2 = e1 ++ e2

domTyVarNamesEnv :: TyVarNamesEnv -> [ProtoName]
domTyVarNamesEnv env = map fst env
\end{code}

@mkTyVarNamesEnv@ checks for duplicates, and complains if so.

\begin{code}
mkTyVarNamesEnv
	:: SrcLoc
	-> [ProtoName]			-- The type variables
        -> Rn4M (TyVarNamesEnv,[Name])	-- Environment and renamed tyvars

mkTyVarNamesEnv src_loc tyvars {-Rn4-} sw_chkr gnfs ss errs_so_far us locn
  = (namesFromProtoNames "type variable"
	 (tyvars `zip` repeat src_loc)	`thenRn4`  \ tyvars2 ->

	 -- tyvars2 may not be in the same order as tyvars, so we need some
	 -- jiggery pokery to build the right tyvar env, and return the
	 -- renamed tyvars in the original order.
    let tv_string_name_pairs 	= extend tyvars2 []
    	tv_env	    	    	= map (lookup tv_string_name_pairs) tyvars
	tyvars2_in_orig_order	= map snd tv_env
    in
    returnRn4  (tv_env, tyvars2_in_orig_order)
    ) {-Rn4-} sw_chkr gnfs ss errs_so_far us locn
  where
    extend :: [Name] -> [(FAST_STRING, Name)] -> [(FAST_STRING, Name)]
    extend [] ss = ss
    extend (name@(Short _ sname):names) ss
      = (getOccurrenceName sname, name) : extend names ss

    lookup :: [(FAST_STRING, Name)] -> ProtoName -> (ProtoName, Name)
    lookup pairs tyvar_pn
      = (tyvar_pn, assoc "mkTyVarNamesEnv" pairs (getOccurrenceName tyvar_pn))
\end{code}

\begin{code}
lookupTyVarName :: TyVarNamesEnv -> ProtoName -> Rn4M Name
lookupTyVarName env pname {-Rn4-} sw_chkr gnfs ss errs_so_far us locn
  = (case (assoc_maybe env pname) of
     Just name -> returnRn4 name
     Nothing   -> getSrcLocRn4	`thenRn4` \ loc ->
		  failButContinueRn4 (unboundName pname)
			  (unknownNameErr "type variable" pname loc)
    ) {-Rn4-} sw_chkr gnfs ss errs_so_far us locn
  where
    assoc_maybe [] _ = Nothing
    assoc_maybe ((tv,xxx) : tvs) key
      = if tv `eqProtoName` key then Just xxx else assoc_maybe tvs key
\end{code}
