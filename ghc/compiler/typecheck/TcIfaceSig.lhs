%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcIfaceSig]{Type checking of type signatures in interface files}

\begin{code}
module TcIfaceSig ( tcInterfaceSigs,
		    tcCoreExpr,
		    tcCoreLamBndrs,
		    tcCoreBinds ) where

#include "HsVersions.h"

import HsSyn		( CoreDecl(..), TyClDecl(..), HsTupCon(..) )
import TcHsSyn		( TypecheckedCoreBind )
import TcRnTypes
import TcRnMonad
import TcMonoType	( tcIfaceType, kcHsSigType )
import TcEnv		( tcExtendTyVarEnv, tcExtendGlobalValEnv, tcLookupGlobalId,
			  tcLookupDataCon )

import RnHsSyn		( RenamedCoreDecl, RenamedTyClDecl )
import HsCore
import Literal		( Literal(..) )
import CoreSyn
import CoreUtils	( exprType )
import CoreUnfold
import CoreLint		( lintUnfolding )
import WorkWrap		( mkWrapper )

import Id		( Id, mkVanillaGlobal, mkLocalId )
import MkId		( mkFCallId )
import IdInfo
import TyCon		( tyConDataCons, tyConTyVars )
import DataCon		( DataCon, dataConWorkId, dataConExistentialTyVars, dataConArgTys )
import Type		( mkTyVarTys, splitTyConApp )
import TysWiredIn	( tupleCon )
import Var		( mkTyVar, tyVarKind )
import Name		( Name )
import UniqSupply	( initUs_ )
import Outputable	
import Util		( zipWithEqual, dropList, equalLength )
import HscTypes		( typeEnvIds )
import CmdLineOpts	( DynFlag(..) )
\end{code}

Ultimately, type signatures in interfaces will have pragmatic
information attached, so it is a good idea to have separate code to
check them.

As always, we do not have to worry about user-pragmas in interface
signatures.

\begin{code}
tcInterfaceSigs :: [RenamedTyClDecl]	-- Ignore non-sig-decls in these decls
		-> TcM TcGblEnv
		
-- May 2003: 
--	NOTE 1: careful about the side-effected EPS
--		in the two tcExtendGlobalValueEnv calls
--	NOTE 2: no point in tying the knot with fixM; all
--		the important knot-tying comes via the PCS global variable

tcInterfaceSigs decls = 
  zapEnv (fixM (tc_interface_sigs decls)) `thenM` \ (_,sig_ids) ->
	-- The zapEnv dramatically trims the environment, solely
	-- to plug the space leak that would otherwise be caused
	-- by a rich environment bound into lots of lazy thunks
	-- The thunks are the lazily-typechecked IdInfo of the 
	-- imported things.

  tcExtendGlobalValEnv sig_ids getGblEnv  `thenM` \ gbl_env ->
  returnM gbl_env
	-- We tie a knot so that the Ids read out of interfaces are in scope
	--   when we read their pragmas.
	-- What we rely on is that pragmas are typechecked lazily; if
	--   any type errors are found (ie there's an inconsistency)
	--   we silently discard the pragma
	--
	-- NOTE ALSO: the knot is in two parts:
	--	* Ids defined in this module are added to the typechecker envt
	--	  which is knot-tied by the fixM.
	--	* Imported Ids are side-effected into the PCS by the 
	--	  tcExtendGlobalValueEnv, so they will be seen there provided
	--	  we don't look them up too early. 
	--	In both cases, we must defer lookups until after the knot is tied
	--
	-- We used to have a much bigger loop (in TcRnDriver), so that the 
	-- interface pragmas could mention variables bound in this module 
	-- (by mutual recn), but
	--     (a) the knot is tiresomely big, and 
	--     (b) it black-holes when we have Template Haskell
	--
	-- For (b) consider: f = $(...h....)
	-- where h is imported, and calls f via an hi-boot file.  
	-- This is bad!  But it is not seen as a staging error, because h
	-- is indeed imported.  We don't want the type-checker to black-hole 
	-- when simplifying and compiling the splice!
	--
	-- Simple solution: discard any unfolding that mentions a variable
	-- bound in this module (and hence not yet processed).
	-- The discarding happens when forkM finds a type error.

tc_interface_sigs decls ~(unf_env, _)
  = sequenceM [do_one d | d@(IfaceSig {}) <- decls]	`thenM` \ sig_ids ->
    tcExtendGlobalValEnv sig_ids getGblEnv		`thenM` \ gbl_env ->
    returnM (gbl_env, sig_ids)
  where
    in_scope_vars = typeEnvIds (tcg_type_env unf_env)
	-- When we have hi-boot files, an unfolding might refer to
	-- something defined in this module, so we must build a
	-- suitable in-scope set.  This thunk will only be poked
	-- if -dcore-lint is on.

    do_one IfaceSig {tcdName   = name,     tcdType = ty, 
		     tcdIdInfo = id_infos, tcdLoc  = src_loc}
      = addSrcLoc src_loc 		 	$	
	addErrCtxt (ifaceSigCtxt name)		$
	tcIfaceType ty				`thenM` \ sigma_ty ->
	tcIdInfo unf_env in_scope_vars name 
		 sigma_ty id_infos		`thenM` \ id_info ->
	returnM (mkVanillaGlobal name sigma_ty id_info)
\end{code}

\begin{code}
tcIdInfo unf_env in_scope_vars name ty info_ins
  = setGblEnv unf_env $
	-- Use the knot-tied environment for the IdInfo
	-- In particular: typechecking unfoldings and worker names
    foldlM tcPrag init_info info_ins 
  where
    -- Set the CgInfo to something sensible but uninformative before
    -- we start; default assumption is that it has CAFs
    init_info = vanillaIdInfo

    tcPrag info HsNoCafRefs         = returnM (info `setCafInfo`   NoCafRefs)
    tcPrag info (HsArity arity)     = returnM (info `setArityInfo` arity)
    tcPrag info (HsStrictness str)  = returnM (info `setAllStrictnessInfo` Just str)
    tcPrag info (HsWorker nm arity) = tcWorkerInfo ty info nm arity

    tcPrag info (HsUnfold inline_prag expr)
	= tcPragExpr name in_scope_vars expr 	`thenM` \ maybe_expr' ->
	  let
		-- maybe_expr' doesn't get looked at if the unfolding
		-- is never inspected; so the typecheck doesn't even happen
		unfold_info = case maybe_expr' of
				Nothing    -> noUnfolding
				Just expr' -> mkTopUnfolding expr' 
	  in
 	  returnM (info `setUnfoldingInfoLazily` unfold_info
			`setInlinePragInfo`      inline_prag)
\end{code}

\begin{code}
tcWorkerInfo ty info wkr_name arity
  = forkM doc (tcVar wkr_name)	`thenM` \ maybe_wkr_id ->
	-- Watch out! We can't pull on unf_env too eagerly!
	-- Hence the forkM

	-- We return without testing maybe_wkr_id, but as soon as info is
	-- looked at we will test it.  That's ok, because its outside the
	-- knot; and there seems no big reason to further defer the
	-- tcVar lookup.  (Contrast with tcPragExpr, where postponing walking
	-- over the unfolding until it's actually used does seem worth while.)
    newUniqueSupply		`thenM` \ us ->
    returnM (case maybe_wkr_id of
	Nothing     -> info
	Just wkr_id -> info `setUnfoldingInfoLazily`  mk_unfolding us wkr_id
			    `setWorkerInfo`           HasWorker wkr_id arity)

  where
    doc = text "worker for" <+> ppr wkr_name

    mk_unfolding us wkr_id = mkTopUnfolding (initUs_ us (mkWrapper ty strict_sig) wkr_id)

    	-- We are relying here on strictness info always appearing 
	-- before worker info,  fingers crossed ....
    strict_sig = case newStrictnessInfo info of
		   Just sig -> sig
		   Nothing  -> pprPanic "Worker info but no strictness for" (ppr wkr_name)
\end{code}

For unfoldings we try to do the job lazily, so that we never type check
an unfolding that isn't going to be looked at.

\begin{code}
tcPragExpr :: Name -> [Id] -> UfExpr Name -> TcM (Maybe CoreExpr)
tcPragExpr name in_scope_vars expr
  = forkM doc $
    tcCoreExpr expr		`thenM` \ core_expr' ->

		-- Check for type consistency in the unfolding
    ifOptM Opt_DoCoreLinting (
	getSrcLocM		`thenM` \ src_loc -> 
	case lintUnfolding src_loc in_scope_vars core_expr' of
	  Nothing       -> returnM ()
	  Just fail_msg -> failWithTc ((doc <+> text "Failed Lint") $$ fail_msg)
    )				`thenM_`

   returnM core_expr'	
  where
    doc = text "unfolding of" <+> ppr name
\end{code}


Variables in unfoldings
~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
tcVar :: Name -> TcM Id
  -- Inside here we use only the Global environment, even for locally bound variables.
  -- Why? Because we know all the types and want to bind them to real Ids.
tcVar name = tcLookupGlobalId name
\end{code}

UfCore expressions.

\begin{code}
tcCoreExpr :: UfExpr Name -> TcM CoreExpr

tcCoreExpr (UfType ty)
  = tcIfaceType ty		`thenM` \ ty' ->
	-- It might not be of kind type
    returnM (Type ty')

tcCoreExpr (UfVar name)
  = tcVar name 	`thenM` \ id ->
    returnM (Var id)

tcCoreExpr (UfLit lit)
  = returnM (Lit lit)

tcCoreExpr (UfFCall cc ty)
  = tcIfaceType ty 	`thenM` \ ty' ->
    newUnique		`thenM` \ u ->
    returnM (Var (mkFCallId u cc ty'))

tcCoreExpr (UfTuple (HsTupCon boxity arity) args) 
  = mappM tcCoreExpr args	`thenM` \ args' ->
    let
	-- Put the missing type arguments back in
	con_args = map (Type . exprType) args' ++ args'
    in
    returnM (mkApps (Var con_id) con_args)
  where
    con_id = dataConWorkId (tupleCon boxity arity)
    

tcCoreExpr (UfLam bndr body)
  = tcCoreLamBndr bndr 		$ \ bndr' ->
    tcCoreExpr body		`thenM` \ body' ->
    returnM (Lam bndr' body')

tcCoreExpr (UfApp fun arg)
  = tcCoreExpr fun		`thenM` \ fun' ->
    tcCoreExpr arg		`thenM` \ arg' ->
    returnM (App fun' arg')

tcCoreExpr (UfCase scrut case_bndr alts) 
  = tcCoreExpr scrut					`thenM` \ scrut' ->
    let
	scrut_ty = exprType scrut'
	case_bndr' = mkLocalId case_bndr scrut_ty
    in
    tcExtendGlobalValEnv [case_bndr']	$
    mappM (tcCoreAlt scrut_ty) alts	`thenM` \ alts' ->
    returnM (Case scrut' case_bndr' alts')

tcCoreExpr (UfLet (UfNonRec bndr rhs) body)
  = tcCoreExpr rhs		`thenM` \ rhs' ->
    tcCoreValBndr bndr 		$ \ bndr' ->
    tcCoreExpr body		`thenM` \ body' ->
    returnM (Let (NonRec bndr' rhs') body')

tcCoreExpr (UfLet (UfRec pairs) body)
  = tcCoreValBndrs bndrs	$ \ bndrs' ->
    mappM tcCoreExpr rhss	`thenM` \ rhss' ->
    tcCoreExpr body		`thenM` \ body' ->
    returnM (Let (Rec (bndrs' `zip` rhss')) body')
  where
    (bndrs, rhss) = unzip pairs

tcCoreExpr (UfNote note expr) 
  = tcCoreExpr expr		`thenM` \ expr' ->
    case note of
	UfCoerce to_ty -> tcIfaceType to_ty	`thenM` \ to_ty' ->
			  returnM (Note (Coerce to_ty'
                                                 (exprType expr')) expr')
	UfInlineCall   -> returnM (Note InlineCall expr')
	UfInlineMe     -> returnM (Note InlineMe   expr')
	UfSCC cc       -> returnM (Note (SCC cc)   expr')
\end{code}

\begin{code}
tcCoreLamBndr (UfValBinder name ty) thing_inside
  = tcIfaceType ty		`thenM` \ ty' ->
    let
	id = mkLocalId name ty'
    in
    tcExtendGlobalValEnv [id] $
    thing_inside id
    
tcCoreLamBndr (UfTyBinder name kind) thing_inside
  = let
	tyvar = mkTyVar name kind
    in
    tcExtendTyVarEnv [tyvar] (thing_inside tyvar)
    
tcCoreLamBndrs []     thing_inside = thing_inside []
tcCoreLamBndrs (b:bs) thing_inside
  = tcCoreLamBndr b	$ \ b' ->
    tcCoreLamBndrs bs	$ \ bs' ->
    thing_inside (b':bs')

tcCoreValBndr (UfValBinder name ty) thing_inside
  = tcIfaceType ty			`thenM` \ ty' ->
    let
	id = mkLocalId name ty'
    in
    tcExtendGlobalValEnv [id] $
    thing_inside id
    
tcCoreValBndrs bndrs thing_inside		-- Expect them all to be ValBinders
  = mappM tcIfaceType tys		`thenM` \ tys' ->
    let
	ids = zipWithEqual "tcCoreValBndr" mkLocalId names tys'
    in
    tcExtendGlobalValEnv ids $
    thing_inside ids
  where
    names = [name | UfValBinder name _  <- bndrs]
    tys   = [ty   | UfValBinder _    ty <- bndrs]
\end{code}    

\begin{code}
tcCoreAlt scrut_ty (UfDefault, names, rhs)
  = ASSERT( null names )
    tcCoreExpr rhs		`thenM` \ rhs' ->
    returnM (DEFAULT, [], rhs')
  
tcCoreAlt scrut_ty (UfLitAlt lit, names, rhs)
  = ASSERT( null names )
    tcCoreExpr rhs		`thenM` \ rhs' ->
    returnM (LitAlt lit, [], rhs')

-- A case alternative is made quite a bit more complicated
-- by the fact that we omit type annotations because we can
-- work them out.  True enough, but its not that easy!
tcCoreAlt scrut_ty alt@(con, names, rhs)
  = tcConAlt con	`thenM` \ con ->
    let
	ex_tyvars  	  = dataConExistentialTyVars con
	(tycon, inst_tys) = splitTyConApp scrut_ty	-- NB: not tcSplitTyConApp
							-- We are looking at Core here
	main_tyvars	  = tyConTyVars tycon
	ex_tyvars'	  = [mkTyVar name (tyVarKind tv) | (name,tv) <- names `zip` ex_tyvars] 
	ex_tys'		  = mkTyVarTys ex_tyvars'
	arg_tys		  = dataConArgTys con (inst_tys ++ ex_tys')
	id_names	  = dropList ex_tyvars names
	arg_ids
#ifdef DEBUG
		| not (equalLength id_names arg_tys)
		= pprPanic "tcCoreAlts" (ppr (con, names, rhs) $$
					 (ppr main_tyvars <+> ppr ex_tyvars) $$
					 ppr arg_tys)
		| otherwise
#endif
		= zipWithEqual "tcCoreAlts" mkLocalId id_names arg_tys
    in
    ASSERT( con `elem` tyConDataCons tycon && equalLength inst_tys main_tyvars )
    tcExtendTyVarEnv ex_tyvars'			$
    tcExtendGlobalValEnv arg_ids		$
    tcCoreExpr rhs					`thenM` \ rhs' ->
    returnM (DataAlt con, ex_tyvars' ++ arg_ids, rhs')


tcConAlt :: UfConAlt Name -> TcM DataCon
tcConAlt (UfTupleAlt (HsTupCon boxity arity))
  = returnM (tupleCon boxity arity)

tcConAlt (UfDataAlt con_name)	-- When reading interface files
				-- the con_name will be the real name of
				-- the data con
  = tcLookupDataCon con_name
\end{code}

%************************************************************************
%*									*
\subsection{Core decls}
%*									*
%************************************************************************


\begin{code}
tcCoreBinds :: [RenamedCoreDecl] -> TcM [TypecheckedCoreBind]
-- We don't assume the bindings are in dependency order
-- So first build the environment, then check the RHSs
tcCoreBinds ls = mappM tcCoreBinder ls		`thenM` \ bndrs ->
		 tcExtendGlobalValEnv bndrs	$
		 mappM (tcCoreBind bndrs) ls

tcCoreBinder (CoreDecl nm ty _ _)
 = kcHsSigType ty	`thenM_`
   tcIfaceType ty 	`thenM` \ ty' ->
   returnM (mkLocalId nm ty')

tcCoreBind bndrs (CoreDecl nm _ rhs loc)
 = tcVar nm		`thenM` \ id ->
   tcCoreExpr rhs	`thenM` \ rhs' ->
   let
	mb_err = lintUnfolding loc bndrs rhs'
   in
   (case mb_err of
	Just err -> addErr err
	Nothing  -> returnM ())	`thenM_`

   returnM (id, rhs')
\end{code}


\begin{code}
ifaceSigCtxt sig_name
  = hsep [ptext SLIT("In an interface-file signature for"), ppr sig_name]
\end{code}

