%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[CoreLint]{A ``lint'' pass to check for Core correctness}

\begin{code}
module CoreLint (
	lintCoreBindings,
	lintUnfolding, 
	beginPass, endPass
    ) where

#include "HsVersions.h"

import IO	( hPutStr, stderr )

import CmdLineOpts      ( opt_D_show_passes, opt_DoCoreLinting )
import CoreSyn
import CoreUtils	( idFreeVars )

import Bag
import Const		( Con(..), DataCon, conType, conOkForApp, conOkForAlt )
import Id		( isConstantId, idMustBeINLINEd )
import Var		( IdOrTyVar, Id, TyVar, idType, tyVarKind, isTyVar )
import VarSet
import VarEnv		( mkVarEnv )
import Name		( isLocallyDefined, getSrcLoc )
import PprCore
import ErrUtils		( doIfSet, dumpIfSet, ghcExit, Message, 
			  ErrMsg, addErrLocHdrLine, pprBagOfErrors )
import PrimRep		( PrimRep(..) )
import SrcLoc		( SrcLoc, noSrcLoc, isNoSrcLoc )
import Type		( Type, Kind, tyVarsOfType,
			  splitFunTy_maybe, mkPiType, mkTyVarTy,
			  splitForAllTy_maybe, splitTyConApp_maybe,
			  isUnLiftedType, typeKind, substTy,
			  splitAlgTyConApp_maybe,
			  isUnboxedTupleType,
			  hasMoreBoxityInfo
			)
import TyCon		( TyCon, isPrimTyCon, tyConDataCons )
import Outputable

infixr 9 `thenL`, `seqL`, `thenMaybeL`
\end{code}

%************************************************************************
%*									*
\subsection{Start and end pass}
%*									*
%************************************************************************

@beginPass@ and @endPass@ don't really belong here, but it makes a convenient
place for them.  They print out stuff before and after core passes,
and do Core Lint when necessary.

\begin{code}
beginPass :: String -> IO ()
beginPass pass_name
  | opt_D_show_passes
  = hPutStr stderr ("*** " ++ pass_name ++ "\n")
  | otherwise
  = return ()


endPass :: String -> Bool -> [CoreBind] -> IO [CoreBind]
endPass pass_name dump_flag binds
  = do 
	-- Report verbosely, if required
	dumpIfSet dump_flag pass_name
		  (pprCoreBindings binds)

	-- Type check
	lintCoreBindings pass_name binds

	return binds
\end{code}


%************************************************************************
%*									*
\subsection[lintCoreBindings]{@lintCoreBindings@: Top-level interface}
%*									*
%************************************************************************

Checks that a set of core bindings is well-formed.  The PprStyle and String
just control what we print in the event of an error.  The Bool value
indicates whether we have done any specialisation yet (in which case we do
some extra checks).

We check for
	(a) type errors
	(b) Out-of-scope type variables
	(c) Out-of-scope local variables
	(d) Ill-kinded types

If we have done specialisation the we check that there are
	(a) No top-level bindings of primitive (unboxed type)

Outstanding issues:

    --
    -- Things are *not* OK if:
    --
    -- * Unsaturated type app before specialisation has been done;
    --
    -- * Oversaturated type app after specialisation (eta reduction
    --   may well be happening...);

\begin{code}
lintCoreBindings :: String -> [CoreBind] -> IO ()

lintCoreBindings whoDunnit binds
  | not opt_DoCoreLinting
  = return ()

lintCoreBindings whoDunnit binds
  = case (initL (lint_binds binds)) of
      Nothing       -> doIfSet opt_D_show_passes
			(hPutStr stderr ("*** Core Linted result of " ++ whoDunnit ++ "\n"))

      Just bad_news -> printDump (display bad_news)	>>
		       ghcExit 1
  where
    lint_binds [] = returnL ()
    lint_binds (bind:binds)
      = lintCoreBinding bind `thenL` \binders ->
	addInScopeVars binders (lint_binds binds)

    display bad_news
      = vcat [
		text ("*** Core Lint Errors: in result of " ++ whoDunnit ++ " ***"),
		bad_news,
		ptext SLIT("*** Offending Program ***"),
		pprCoreBindings binds,
		ptext SLIT("*** End of Offense ***")
	]
\end{code}

%************************************************************************
%*									*
\subsection[lintUnfolding]{lintUnfolding}
%*									*
%************************************************************************

We use this to check all unfoldings that come in from interfaces
(it is very painful to catch errors otherwise):

\begin{code}
lintUnfolding :: SrcLoc -> CoreExpr -> Maybe CoreExpr

lintUnfolding locn expr
  = case
      initL (addLoc (ImportedUnfolding locn) (lintCoreExpr expr))
    of
      Nothing  -> Just expr
      Just msg ->
        pprTrace "WARNING: Discarded bad unfolding from interface:\n"
	(vcat [msg,
		   ptext SLIT("*** Bad unfolding ***"),
		   ppr expr,
		   ptext SLIT("*** End unfolding ***")])
	Nothing
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreBinding]{lintCoreBinding}
%*									*
%************************************************************************

Check a core binding, returning the list of variables bound.

\begin{code}
lintCoreBinding :: CoreBind -> LintM [Id]

lintCoreBinding (NonRec binder rhs)
  = lintSingleBinding (binder,rhs) `seqL` returnL [binder]

lintCoreBinding (Rec pairs)
  = addInScopeVars binders (
      mapL lintSingleBinding pairs `seqL` returnL binders
    )
  where
    binders = map fst pairs

lintSingleBinding (binder,rhs)
  = addLoc (RhsOf binder) $

	-- Check the rhs
    lintCoreExpr rhs				`thenL` \ ty ->

	-- Check match to RHS type
    lintBinder binder				`seqL`
    checkTys binder_ty ty (mkRhsMsg binder ty)	`seqL`

	-- Check (not isUnLiftedType) (also checks for bogus unboxed tuples)
    checkL (not (isUnLiftedType binder_ty))
 	   (mkRhsPrimMsg binder rhs)		`seqL`

        -- Check whether binder's specialisations contain any out-of-scope variables
    mapL (checkBndrIdInScope binder) bndr_vars	`seqL`
    returnL ()
	  
	-- We should check the unfolding, if any, but this is tricky because
	-- the unfolding is a SimplifiableCoreExpr. Give up for now.
  where
    binder_ty = idType binder
    bndr_vars = varSetElems (idFreeVars binder)
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreExpr]{lintCoreExpr}
%*									*
%************************************************************************

\begin{code}
lintCoreExpr :: CoreExpr -> LintM Type

lintCoreExpr (Var var) 
  | isConstantId var = returnL (idType var)
	-- Micro-hack here... Class decls generate applications of their
	-- dictionary constructor, but don't generate a binding for the
	-- constructor (since it would never be used).  After a single round
	-- of simplification, these dictionary constructors have been
	-- inlined (from their UnfoldInfo) to CoCons.  Just between
	-- desugaring and simplfication, though, they appear as naked, unbound
	-- variables as the function in an application.
	-- The hack here simply doesn't check for out-of-scope-ness for
	-- data constructors (at least, in a function position).
	-- Ditto primitive Ids

  | otherwise    = checkIdInScope var `seqL` returnL (idType var)

lintCoreExpr (Note (Coerce to_ty from_ty) expr)
  = lintCoreExpr expr 	`thenL` \ expr_ty ->
    lintTy to_ty	`seqL`
    lintTy from_ty	`seqL`
    checkTys from_ty expr_ty (mkCoerceErr from_ty expr_ty)	`seqL`
    returnL to_ty

lintCoreExpr (Note other_note expr)
  = lintCoreExpr expr

lintCoreExpr (Let binds body)
  = lintCoreBinding binds `thenL` \binders ->
    if (null binders) then
	lintCoreExpr body  -- Can't add a new source location
    else
      addLoc (BodyOfLetRec binders)
	(addInScopeVars binders (lintCoreExpr body))

lintCoreExpr e@(Con con args)
  = addLoc (AnExpr e)	$
    checkL (conOkForApp con) (mkConAppMsg e)	`seqL`
    lintCoreArgs (conType con) args

lintCoreExpr e@(App fun arg)
  = lintCoreExpr fun 	`thenL` \ ty ->
    addLoc (AnExpr e)	$
    lintCoreArg ty arg

lintCoreExpr (Lam var expr)
  = addLoc (LambdaBodyOf var)	$
    checkL (not (isUnboxedTupleType (idType var))) (mkUnboxedTupleMsg var)
				`seqL`
    (addInScopeVars [var]	$
     lintCoreExpr expr		`thenL` \ ty ->
     returnL (mkPiType var ty))

lintCoreExpr e@(Case scrut var alts)
 = 	-- Check the scrutinee
   lintCoreExpr scrut			`thenL` \ scrut_ty ->

	-- Check the binder
   lintBinder var						`seqL`

    	-- If this is an unboxed tuple case, then the binder must be dead
   {-
   checkL (if isUnboxedTupleType (idType var) 
		then isDeadBinder var 
		else True) (mkUnboxedTupleMsg var)		`seqL`
   -}
		
   checkTys (idType var) scrut_ty (mkScrutMsg var scrut_ty)	`seqL`

   addInScopeVars [var]				(

	-- Check the alternatives
   checkAllCasesCovered e scrut_ty alts		`seqL`
   mapL (lintCoreAlt scrut_ty) alts		`thenL` \ (alt_ty : alt_tys) ->
   mapL (check alt_ty) alt_tys			`seqL`
   returnL alt_ty)
 where
   check alt_ty1 alt_ty2 = checkTys alt_ty1 alt_ty2 (mkCaseAltMsg e)

lintCoreExpr e@(Type ty)
  = addErrL (mkStrangeTyMsg e)
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreArgs]{lintCoreArgs}
%*									*
%************************************************************************

The boolean argument indicates whether we should flag type
applications to primitive types as being errors.

\begin{code}
lintCoreArgs :: Type -> [CoreArg] -> LintM Type

lintCoreArgs ty [] = returnL ty
lintCoreArgs ty (a : args)
  = lintCoreArg  ty a		`thenL` \ res ->
    lintCoreArgs res args
\end{code}

\begin{code}
lintCoreArg :: Type -> CoreArg -> LintM Type

lintCoreArg ty a@(Type arg_ty)
  = lintTy arg_ty			`seqL`
    lintTyApp ty arg_ty

lintCoreArg fun_ty arg
  = -- Make sure function type matches argument
    lintCoreExpr arg		`thenL` \ arg_ty ->
    case (splitFunTy_maybe fun_ty) of
      Just (arg,res) | (arg_ty == arg) -> returnL res
      _ 			       -> addErrL (mkAppMsg fun_ty arg_ty)
\end{code}

\begin{code}
lintTyApp ty arg_ty 
  = case splitForAllTy_maybe ty of
      Nothing -> addErrL (mkTyAppMsg ty arg_ty)

      Just (tyvar,body) ->
	let
	    tyvar_kind = tyVarKind tyvar
	    argty_kind = typeKind arg_ty
	in
	if argty_kind `hasMoreBoxityInfo` tyvar_kind
		-- Arg type might be boxed for a function with an uncommitted
		-- tyvar; notably this is used so that we can give
		-- 	error :: forall a:*. String -> a
		-- and then apply it to both boxed and unboxed types.
	 then
	    returnL (substTy (mkVarEnv [(tyvar,arg_ty)]) body)
	else
	    addErrL (mkKindErrMsg tyvar arg_ty)

lintTyApps fun_ty []
  = returnL fun_ty

lintTyApps fun_ty (arg_ty : arg_tys)
  = lintTyApp fun_ty arg_ty		`thenL` \ fun_ty' ->
    lintTyApps fun_ty' arg_tys
\end{code}



%************************************************************************
%*									*
\subsection[lintCoreAlts]{lintCoreAlts}
%*									*
%************************************************************************

\begin{code}
checkAllCasesCovered e ty [] = addErrL (mkNullAltsMsg e)

checkAllCasesCovered e ty [(DEFAULT,_,_)] = nopL

checkAllCasesCovered e scrut_ty alts
  = case splitTyConApp_maybe scrut_ty of {
	Nothing	-> addErrL (badAltsMsg e);
	Just (tycon, tycon_arg_tys) ->

    if isPrimTyCon tycon then
	checkL (hasDefault alts) (nonExhaustiveAltsMsg e)
    else
#ifdef DEBUG
	-- Algebraic cases are not necessarily exhaustive, because
	-- the simplifer correctly eliminates case that can't 
	-- possibly match.
	-- This code just emits a message to say so
    let
	missing_cons    = filter not_in_alts (tyConDataCons tycon)
	not_in_alts con = all (not_in_alt con) alts
	not_in_alt con (DataCon con', _, _) = con /= con'
	not_in_alt con other		    = True

	case_bndr = case e of { Case _ bndr alts -> bndr }
    in
    if not (hasDefault alts || null missing_cons) then
	pprTrace "Exciting (but not a problem)!  Non-exhaustive case:"
		 (ppr case_bndr <+> ppr missing_cons)
		 nopL
    else
#endif
    nopL }

hasDefault []			  = False
hasDefault ((DEFAULT,_,_) : alts) = True
hasDefault (alt		  : alts) = hasDefault alts
\end{code}

\begin{code}
lintCoreAlt :: Type  			-- Type of scrutinee
	    -> CoreAlt
	    -> LintM Type		-- Type of alternatives

lintCoreAlt scrut_ty alt@(DEFAULT, args, rhs)
  = checkL (null args) (mkDefaultArgsMsg args)	`seqL`
    lintCoreExpr rhs

lintCoreAlt scrut_ty alt@(con, args, rhs)
  = addLoc (CaseAlt alt) (

    checkL (conOkForAlt con) (mkConAltMsg con)	`seqL`

    mapL (\arg -> checkL (not (isUnboxedTupleType (idType arg))) 
			(mkUnboxedTupleMsg arg)) args `seqL`

    addInScopeVars args (

	-- Check the pattern
	-- Scrutinee type must be a tycon applicn; checked by caller
	-- This code is remarkably compact considering what it does!
	-- NB: args must be in scope here so that the lintCoreArgs line works.
    case splitTyConApp_maybe scrut_ty of { Just (tycon, tycon_arg_tys) ->
	lintTyApps (conType con) tycon_arg_tys	`thenL` \ con_type ->
	lintCoreArgs con_type (map mk_arg args)	`thenL` \ con_result_ty ->
	checkTys con_result_ty scrut_ty (mkBadPatMsg con_result_ty scrut_ty)
    }						`seqL`

	-- Check the RHS
    lintCoreExpr rhs
    ))
  where
    mk_arg b | isTyVar b = Type (mkTyVarTy b)
	     | otherwise = Var b
\end{code}

%************************************************************************
%*									*
\subsection[lint-types]{Types}
%*									*
%************************************************************************

\begin{code}
lintBinder :: IdOrTyVar -> LintM ()
lintBinder v = nopL
-- ToDo: lint its type

lintTy :: Type -> LintM ()
lintTy ty = mapL checkIdInScope (varSetElems (tyVarsOfType ty))	`seqL`
	    returnL ()
	-- ToDo: check the kind structure of the type
\end{code}

    
%************************************************************************
%*									*
\subsection[lint-monad]{The Lint monad}
%*									*
%************************************************************************

\begin{code}
type LintM a = [LintLocInfo] 	-- Locations
	    -> IdSet		-- Local vars in scope
	    -> Bag ErrMsg	-- Error messages so far
	    -> (Maybe a, Bag ErrMsg)	-- Result and error messages (if any)

data LintLocInfo
  = RhsOf Id		-- The variable bound
  | LambdaBodyOf Id	-- The lambda-binder
  | BodyOfLetRec [Id]	-- One of the binders
  | CaseAlt CoreAlt	-- Pattern of a case alternative
  | AnExpr CoreExpr	-- Some expression
  | ImportedUnfolding SrcLoc -- Some imported unfolding (ToDo: say which)
\end{code}

\begin{code}
initL :: LintM a -> Maybe Message
initL m
  = case (m [] emptyVarSet emptyBag) of { (_, errs) ->
    if isEmptyBag errs then
	Nothing
    else
	Just (pprBagOfErrors errs)
    }

returnL :: a -> LintM a
returnL r loc scope errs = (Just r, errs)

nopL :: LintM a
nopL loc scope errs = (Nothing, errs)

thenL :: LintM a -> (a -> LintM b) -> LintM b
thenL m k loc scope errs
  = case m loc scope errs of
      (Just r, errs')  -> k r loc scope errs'
      (Nothing, errs') -> (Nothing, errs')

seqL :: LintM a -> LintM b -> LintM b
seqL m k loc scope errs
  = case m loc scope errs of
      (_, errs') -> k loc scope errs'

mapL :: (a -> LintM b) -> [a] -> LintM [b]
mapL f [] = returnL []
mapL f (x:xs)
  = f x 	`thenL` \ r ->
    mapL f xs	`thenL` \ rs ->
    returnL (r:rs)
\end{code}

\begin{code}
checkL :: Bool -> Message -> LintM ()
checkL True  msg loc scope errs = (Nothing, errs)
checkL False msg loc scope errs = (Nothing, addErr errs msg loc)

addErrL :: Message -> LintM a
addErrL msg loc scope errs = (Nothing, addErr errs msg loc)

addErr :: Bag ErrMsg -> Message -> [LintLocInfo] -> Bag ErrMsg

addErr errs_so_far msg locs
  = ASSERT (not (null locs))
    errs_so_far `snocBag` mk_msg msg
  where
   (loc, pref) = dumpLoc (head locs)

   mk_msg msg
     | isNoSrcLoc loc = (loc, hang pref 4 msg)
     | otherwise      = addErrLocHdrLine loc pref msg

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m loc scope errs
  = m (extra_loc:loc) scope errs

addInScopeVars :: [IdOrTyVar] -> LintM a -> LintM a
addInScopeVars ids m loc scope errs
  = m loc (scope `unionVarSet` mkVarSet ids) errs
\end{code}

\begin{code}
checkIdInScope :: IdOrTyVar -> LintM ()
checkIdInScope id 
  = checkInScope (ptext SLIT("is out of scope")) id

checkBndrIdInScope :: IdOrTyVar -> IdOrTyVar -> LintM ()
checkBndrIdInScope binder id 
  = checkInScope msg id
    where
     msg = ptext SLIT("is out of scope inside info for") <+> 
	   ppr binder

checkInScope :: SDoc -> IdOrTyVar -> LintM ()
checkInScope loc_msg id loc scope errs
  |  isLocallyDefined id 
  && not (id `elemVarSet` scope)
  && not (idMustBeINLINEd id)	-- Constructors and dict selectors 
				-- don't have bindings, 
				-- just MustInline prags
  = (Nothing, addErr errs (hsep [ppr id, loc_msg]) loc)
  | otherwise
  = (Nothing,errs)

checkTys :: Type -> Type -> Message -> LintM ()
checkTys ty1 ty2 msg loc scope errs
  | ty1 == ty2 = (Nothing, errs)
  | otherwise  = (Nothing, addErr errs msg loc)
\end{code}


%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
dumpLoc (RhsOf v)
  = (getSrcLoc v, brackets (ptext SLIT("RHS of") <+> pp_binders [v]))

dumpLoc (LambdaBodyOf b)
  = (getSrcLoc b, brackets (ptext SLIT("in body of lambda with binder") <+> pp_binder b))

dumpLoc (BodyOfLetRec bs)
  = ( getSrcLoc (head bs), brackets (ptext SLIT("in body of letrec with binders") <+> pp_binders bs))

dumpLoc (AnExpr e)
  = (noSrcLoc, text "In the expression:" <+> ppr e)

dumpLoc (CaseAlt (con, args, rhs))
  = (noSrcLoc, text "In a case pattern:" <+> parens (ppr con <+> ppr args))

dumpLoc (ImportedUnfolding locn)
  = (locn, brackets (ptext SLIT("in an imported unfolding")))

pp_binders :: [Id] -> SDoc
pp_binders bs = sep (punctuate comma (map pp_binder bs))

pp_binder :: Id -> SDoc
pp_binder b = hsep [ppr b, dcolon, ppr (idType b)]
\end{code}

\begin{code}
------------------------------------------------------
--	Messages for case expressions

mkConAppMsg :: CoreExpr -> Message
mkConAppMsg e
  = hang (text "Application of newtype constructor:")
	 4 (ppr e)

mkConAltMsg :: Con -> Message
mkConAltMsg con
  = text "PrimOp in case pattern:" <+> ppr con

mkNullAltsMsg :: CoreExpr -> Message
mkNullAltsMsg e 
  = hang (text "Case expression with no alternatives:")
	 4 (ppr e)

mkDefaultArgsMsg :: [IdOrTyVar] -> Message
mkDefaultArgsMsg args 
  = hang (text "DEFAULT case with binders")
	 4 (ppr args)

mkCaseAltMsg :: CoreExpr -> Message
mkCaseAltMsg e
  = hang (text "Type of case alternatives not the same:")
	 4 (ppr e)

mkScrutMsg :: Id -> Type -> Message
mkScrutMsg var scrut_ty
  = vcat [text "Result binder in case doesn't match scrutinee:" <+> ppr var,
	  text "Result binder type:" <+> ppr (idType var),
	  text "Scrutinee type:" <+> ppr scrut_ty]

badAltsMsg :: CoreExpr -> Message
badAltsMsg e
  = hang (text "Case statement scrutinee is not a data type:")
	 4 (ppr e)

nonExhaustiveAltsMsg :: CoreExpr -> Message
nonExhaustiveAltsMsg e
  = hang (text "Case expression with non-exhaustive alternatives")
	 4 (ppr e)

mkBadPatMsg :: Type -> Type -> Message
mkBadPatMsg con_result_ty scrut_ty
  = vcat [
	text "In a case alternative, pattern result type doesn't match scrutinee type:",
	text "Pattern result type:" <+> ppr con_result_ty,
	text "Scrutinee type:" <+> ppr scrut_ty
    ]

------------------------------------------------------
--	Other error messages

mkAppMsg :: Type -> Type -> Message
mkAppMsg fun arg
  = vcat [ptext SLIT("Argument value doesn't match argument type:"),
	      hang (ptext SLIT("Fun type:")) 4 (ppr fun),
	      hang (ptext SLIT("Arg type:")) 4 (ppr arg)]

mkKindErrMsg :: TyVar -> Type -> Message
mkKindErrMsg tyvar arg_ty
  = vcat [ptext SLIT("Kinds don't match in type application:"),
	  hang (ptext SLIT("Type variable:"))
		 4 (ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)),
	  hang (ptext SLIT("Arg type:"))   
	         4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

mkTyAppMsg :: Type -> Type -> Message
mkTyAppMsg ty arg_ty
  = vcat [text "Illegal type application:",
	      hang (ptext SLIT("Exp type:"))
		 4 (ppr ty <+> dcolon <+> ppr (typeKind ty)),
	      hang (ptext SLIT("Arg type:"))   
	         4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

mkRhsMsg :: Id -> Type -> Message
mkRhsMsg binder ty
  = vcat
    [hsep [ptext SLIT("The type of this binder doesn't match the type of its RHS:"),
	    ppr binder],
     hsep [ptext SLIT("Binder's type:"), ppr (idType binder)],
     hsep [ptext SLIT("Rhs type:"), ppr ty]]

mkRhsPrimMsg :: Id -> CoreExpr -> Message
mkRhsPrimMsg binder rhs
  = vcat [hsep [ptext SLIT("The type of this binder is primitive:"),
		     ppr binder],
	      hsep [ptext SLIT("Binder's type:"), ppr (idType binder)]
	     ]

mkUnboxedTupleMsg :: Id -> Message
mkUnboxedTupleMsg binder
  = vcat [hsep [ptext SLIT("A variable has unboxed tuple type:"), ppr binder],
	  hsep [ptext SLIT("Binder's type:"), ppr (idType binder)]]

mkCoerceErr from_ty expr_ty
  = vcat [ptext SLIT("From-type of Coerce differs from type of enclosed expression"),
	  ptext SLIT("From-type:") <+> ppr from_ty,
	  ptext SLIT("Type of enclosed expr:") <+> ppr expr_ty
    ]

mkStrangeTyMsg e
  = ptext SLIT("Type where expression expected:") <+> ppr e
\end{code}
