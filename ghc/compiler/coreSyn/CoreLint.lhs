%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[CoreLint]{A ``lint'' pass to check for Core correctness}

\begin{code}
module CoreLint (
	lintCoreBindings,
	lintUnfolding, 
	showPass, endPass
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreFVs		( idFreeVars )
import CoreUtils	( findDefault, exprOkForSpeculation, coreBindsSize, mkPiType )

import Bag
import Literal		( literalType )
import DataCon		( dataConRepType )
import Var		( Var, Id, TyVar, idType, tyVarKind, isTyVar, isId, mustHaveLocalBinding )
import VarSet
import Subst		( substTyWith )
import Name		( getSrcLoc )
import PprCore
import ErrUtils		( dumpIfSet_core, ghcExit, Message, showPass,
			  addErrLocHdrLine )
import SrcLoc		( SrcLoc, noSrcLoc )
import Type		( Type, tyVarsOfType, eqType,
			  splitFunTy_maybe, mkTyVarTy,
			  splitForAllTy_maybe, splitTyConApp_maybe, splitTyConApp,
			  isUnLiftedType, typeKind, 
			  isUnboxedTupleType,
			  hasMoreBoxityInfo
			)
import TyCon		( isPrimTyCon )
import BasicTypes	( RecFlag(..), isNonRec )
import CmdLineOpts
import Outputable

#ifdef DEBUG
import Util             ( notNull )
#endif

import Maybe
import IO		( hPutStrLn, stderr )

infixr 9 `thenL`, `seqL`
\end{code}

%************************************************************************
%*									*
\subsection{End pass}
%*									*
%************************************************************************

@showPass@ and @endPass@ don't really belong here, but it makes a convenient
place for them.  They print out stuff before and after core passes,
and do Core Lint when necessary.

\begin{code}
endPass :: DynFlags -> String -> DynFlag -> [CoreBind] -> IO [CoreBind]
endPass dflags pass_name dump_flag binds
  = do 
	-- Report result size if required
	-- This has the side effect of forcing the intermediate to be evaluated
	if verbosity dflags >= 2 then
	   hPutStrLn stderr ("    Result size = " ++ show (coreBindsSize binds))
	 else
	   return ()

	-- Report verbosely, if required
	dumpIfSet_core dflags dump_flag pass_name (pprCoreBindings binds)

	-- Type check
	lintCoreBindings dflags pass_name binds

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
lintCoreBindings :: DynFlags -> String -> [CoreBind] -> IO ()

lintCoreBindings dflags whoDunnit binds
  | not (dopt Opt_DoCoreLinting dflags)
  = return ()

lintCoreBindings dflags whoDunnit binds
  = case (initL (lint_binds binds)) of
      Nothing       -> showPass dflags ("Core Linted result of " ++ whoDunnit)
      Just bad_news -> printDump (display bad_news)	>>
		       ghcExit 1
  where
	-- Put all the top-level binders in scope at the start
	-- This is because transformation rules can bring something
	-- into use 'unexpectedly'
    lint_binds binds = addInScopeVars (bindersOfBinds binds) $
		       mapL lint_bind binds

    lint_bind (Rec prs)		= mapL (lintSingleBinding Recursive) prs	`seqL`
				  returnL ()
    lint_bind (NonRec bndr rhs) = lintSingleBinding NonRecursive (bndr,rhs)

    display bad_news
      = vcat [  text ("*** Core Lint Errors: in result of " ++ whoDunnit ++ " ***"),
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
lintUnfolding :: SrcLoc
	      -> [Var]		-- Treat these as in scope
	      -> CoreExpr
	      -> Maybe Message	-- Nothing => OK

lintUnfolding locn vars expr
  = initL (addLoc (ImportedUnfolding locn) $
	   addInScopeVars vars	           $
	   lintCoreExpr expr)
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreBinding]{lintCoreBinding}
%*									*
%************************************************************************

Check a core binding, returning the list of variables bound.

\begin{code}
lintSingleBinding rec_flag (binder,rhs)
  = addLoc (RhsOf binder) $

	-- Check the rhs
    lintCoreExpr rhs				`thenL` \ ty ->

	-- Check match to RHS type
    lintBinder binder				`seqL`
    checkTys binder_ty ty (mkRhsMsg binder ty)	`seqL`

	-- Check (not isUnLiftedType) (also checks for bogus unboxed tuples)
    checkL (not (isUnLiftedType binder_ty)
            || (isNonRec rec_flag && exprOkForSpeculation rhs))
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

lintCoreExpr (Var var) = checkIdInScope var `seqL` returnL (idType var)
lintCoreExpr (Lit lit) = returnL (literalType lit)

lintCoreExpr (Note (Coerce to_ty from_ty) expr)
  = lintCoreExpr expr 	`thenL` \ expr_ty ->
    lintTy to_ty	`seqL`
    lintTy from_ty	`seqL`
    checkTys from_ty expr_ty (mkCoerceErr from_ty expr_ty)	`seqL`
    returnL to_ty

lintCoreExpr (Note other_note expr)
  = lintCoreExpr expr

lintCoreExpr (Let (NonRec bndr rhs) body)
  = lintSingleBinding NonRecursive (bndr,rhs)	`seqL`
    addLoc (BodyOfLetRec [bndr])
	   (addInScopeVars [bndr] (lintCoreExpr body))

lintCoreExpr (Let (Rec pairs) body)
  = addInScopeVars bndrs	$
    mapL (lintSingleBinding Recursive) pairs	`seqL`
    addLoc (BodyOfLetRec bndrs) (lintCoreExpr body)
  where
    bndrs = map fst pairs

lintCoreExpr e@(App fun arg)
  = lintCoreExpr fun 	`thenL` \ ty ->
    addLoc (AnExpr e)	$
    lintCoreArg ty arg

lintCoreExpr (Lam var expr)
  = addLoc (LambdaBodyOf var)	$
    (if isId var then    
       checkL (not (isUnboxedTupleType (idType var))) (mkUnboxedTupleMsg var)
     else
       returnL ())
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
   checkCaseAlts e scrut_ty alts		`seqL`

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

The basic version of these functions checks that the argument is a
subtype of the required type, as one would expect.

\begin{code}
lintCoreArgs :: Type -> [CoreArg] -> LintM Type
lintCoreArgs = lintCoreArgs0 checkTys

lintCoreArg :: Type -> CoreArg -> LintM Type
lintCoreArg = lintCoreArg0 checkTys
\end{code}

The primitive version of these functions takes a check argument,
allowing a different comparison.

\begin{code}
lintCoreArgs0 check_tys ty [] = returnL ty
lintCoreArgs0 check_tys ty (a : args)
  = lintCoreArg0  check_tys ty a	`thenL` \ res ->
    lintCoreArgs0 check_tys res args

lintCoreArg0 check_tys ty a@(Type arg_ty)
  = lintTy arg_ty			`seqL`
    lintTyApp ty arg_ty

lintCoreArg0 check_tys fun_ty arg
  = -- Make sure function type matches argument
    lintCoreExpr arg		`thenL` \ arg_ty ->
    let
      err = mkAppMsg fun_ty arg_ty
    in
    case splitFunTy_maybe fun_ty of
      Just (arg,res) -> check_tys arg arg_ty err `seqL`
                        returnL res
      _              -> addErrL err
\end{code}

\begin{code}
lintTyApp ty arg_ty 
  = case splitForAllTy_maybe ty of
      Nothing -> addErrL (mkTyAppMsg ty arg_ty)

      Just (tyvar,body) ->
        if not (isTyVar tyvar) then addErrL (mkTyAppMsg ty arg_ty) else
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
	    returnL (substTyWith [tyvar] [arg_ty] body)
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
checkCaseAlts :: CoreExpr -> Type -> [CoreAlt] -> LintM ()
-- a) Check that the alts are non-empty
-- b) Check that the DEFAULT comes first, if it exists
-- c) Check that there's a default for infinite types
-- NB: Algebraic cases are not necessarily exhaustive, because
--     the simplifer correctly eliminates case that can't 
--     possibly match.

checkCaseAlts e ty [] 
  = addErrL (mkNullAltsMsg e)

checkCaseAlts e ty alts
  = checkL (all non_deflt con_alts) (mkNonDefltMsg e)	`seqL`
    checkL (isJust maybe_deflt || not is_infinite_ty)
	   (nonExhaustiveAltsMsg e)
  where
    (con_alts, maybe_deflt) = findDefault alts

    non_deflt (DEFAULT, _, _) = False
    non_deflt alt	      = True

    is_infinite_ty = case splitTyConApp_maybe ty of
			Nothing			    -> False
			Just (tycon, tycon_arg_tys) -> isPrimTyCon tycon
\end{code}

\begin{code}
lintCoreAlt :: Type  			-- Type of scrutinee
	    -> CoreAlt
	    -> LintM Type		-- Type of alternatives

lintCoreAlt scrut_ty alt@(DEFAULT, args, rhs)
  = checkL (null args) (mkDefaultArgsMsg args)	`seqL`
    lintCoreExpr rhs

lintCoreAlt scrut_ty alt@(LitAlt lit, args, rhs)
  = checkL (null args) (mkDefaultArgsMsg args)	`seqL`
    checkTys lit_ty scrut_ty
	     (mkBadPatMsg lit_ty scrut_ty)	`seqL`
    lintCoreExpr rhs
  where
    lit_ty = literalType lit

lintCoreAlt scrut_ty alt@(DataAlt con, args, rhs)
  = addLoc (CaseAlt alt) (

    mapL (\arg -> checkL (not (isUnboxedTupleType (idType arg)))
			(mkUnboxedTupleMsg arg)) args `seqL`

    addInScopeVars args (

	-- Check the pattern
	-- Scrutinee type must be a tycon applicn; checked by caller
	-- This code is remarkably compact considering what it does!
	-- NB: args must be in scope here so that the lintCoreArgs line works.
	-- NB: relies on existential type args coming *after* ordinary type args
    case splitTyConApp scrut_ty of { (tycon, tycon_arg_tys) ->
	lintTyApps (dataConRepType con) tycon_arg_tys	`thenL` \ con_type ->
	lintCoreArgs con_type (map mk_arg args)		`thenL` \ con_result_ty ->
	checkTys con_result_ty scrut_ty (mkBadPatMsg con_result_ty scrut_ty)
    }						`seqL`

	-- Check the RHS
    lintCoreExpr rhs
    ))
  where
    mk_arg b | isTyVar b = Type (mkTyVarTy b)
	     | isId    b = Var b
             | otherwise = pprPanic "lintCoreAlt:mk_arg " (ppr b)
\end{code}

%************************************************************************
%*									*
\subsection[lint-types]{Types}
%*									*
%************************************************************************

\begin{code}
lintBinder :: Var -> LintM ()
lintBinder v = nopL
-- ToDo: lint its type
-- ToDo: lint its rules

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
	    -> Bag Message	-- Error messages so far
	    -> (Maybe a, Bag Message)  -- Result and error messages (if any)

data LintLocInfo
  = RhsOf Id		-- The variable bound
  | LambdaBodyOf Id	-- The lambda-binder
  | BodyOfLetRec [Id]	-- One of the binders
  | CaseAlt CoreAlt	-- Pattern of a case alternative
  | AnExpr CoreExpr	-- Some expression
  | ImportedUnfolding SrcLoc -- Some imported unfolding (ToDo: say which)
\end{code}

\begin{code}
initL :: LintM a -> Maybe Message {- errors -}
initL m
  = case m [] emptyVarSet emptyBag of
      (_, errs) | isEmptyBag errs -> Nothing
		| otherwise	  -> Just (vcat (punctuate (text "") (bagToList errs)))

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
checkL True  msg = nopL
checkL False msg = addErrL msg

addErrL :: Message -> LintM a
addErrL msg loc scope errs = (Nothing, addErr errs msg loc)

addErr :: Bag Message -> Message -> [LintLocInfo] -> Bag Message
addErr errs_so_far msg locs
  = ASSERT( notNull locs )
    errs_so_far `snocBag` mk_msg msg
  where
   (loc, cxt1) = dumpLoc (head locs)
   cxts        = [snd (dumpLoc loc) | loc <- locs]   
   context     | opt_PprStyle_Debug = vcat (reverse cxts) $$ cxt1
	       | otherwise	    = cxt1
 
   mk_msg msg = addErrLocHdrLine loc context msg

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m loc scope errs
  = m (extra_loc:loc) scope errs

addInScopeVars :: [Var] -> LintM a -> LintM a
addInScopeVars ids m loc scope errs
  = m loc (scope `unionVarSet` mkVarSet ids) errs
\end{code}

\begin{code}
checkIdInScope :: Var -> LintM ()
checkIdInScope id 
  = checkInScope (ptext SLIT("is out of scope")) id

checkBndrIdInScope :: Var -> Var -> LintM ()
checkBndrIdInScope binder id 
  = checkInScope msg id
    where
     msg = ptext SLIT("is out of scope inside info for") <+> 
	   ppr binder

checkInScope :: SDoc -> Var -> LintM ()
checkInScope loc_msg var loc scope errs
  |  mustHaveLocalBinding var && not (var `elemVarSet` scope)
  = (Nothing, addErr errs (hsep [ppr var, loc_msg]) loc)
  | otherwise
  = nopL loc scope errs

checkTys :: Type -> Type -> Message -> LintM ()
-- check ty2 is subtype of ty1 (ie, has same structure but usage
-- annotations need only be consistent, not equal)
checkTys ty1 ty2 msg
  | ty1 `eqType` ty2 = nopL
  | otherwise        = addErrL msg
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

dumpLoc (BodyOfLetRec [])
  = (noSrcLoc, brackets (ptext SLIT("In body of a letrec with no binders")))

dumpLoc (BodyOfLetRec bs@(_:_))
  = ( getSrcLoc (head bs), brackets (ptext SLIT("in body of letrec with binders") <+> pp_binders bs))

dumpLoc (AnExpr e)
  = (noSrcLoc, text "In the expression:" <+> ppr e)

dumpLoc (CaseAlt (con, args, rhs))
  = (noSrcLoc, text "In a case pattern:" <+> parens (ppr con <+> ppr args))

dumpLoc (ImportedUnfolding locn)
  = (locn, brackets (ptext SLIT("in an imported unfolding")))

pp_binders :: [Var] -> SDoc
pp_binders bs = sep (punctuate comma (map pp_binder bs))

pp_binder :: Var -> SDoc
pp_binder b | isId b    = hsep [ppr b, dcolon, ppr (idType b)]
            | isTyVar b = hsep [ppr b, dcolon, ppr (tyVarKind b)]
\end{code}

\begin{code}
------------------------------------------------------
--	Messages for case expressions

mkNullAltsMsg :: CoreExpr -> Message
mkNullAltsMsg e 
  = hang (text "Case expression with no alternatives:")
	 4 (ppr e)

mkDefaultArgsMsg :: [Var] -> Message
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


mkNonDefltMsg e
  = hang (text "Case expression with DEFAULT not at the beginnning") 4 (ppr e)

nonExhaustiveAltsMsg :: CoreExpr -> Message
nonExhaustiveAltsMsg e
  = hang (text "Case expression with non-exhaustive alternatives") 4 (ppr e)

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
