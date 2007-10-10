%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module RnTypes ( 
	-- Type related stuff
	rnHsType, rnLHsType, rnLHsTypes, rnContext,
	rnHsSigType, rnHsTypeFVs,

	-- Precence related stuff
	mkOpAppRn, mkNegAppRn, mkOpFormRn, mkConOpPatRn,
	checkPrecMatch, checkSectionPrec
  ) where

import DynFlags
import HsSyn
import RdrHsSyn		( extractHsRhoRdrTyVars )
import RnHsSyn		( extractHsTyNames, parrTyCon_name, tupleTyCon_name, 
			  listTyCon_name
			)
import RnHsDoc          ( rnLHsDoc )
import RnEnv		( lookupOccRn, lookupBndrRn, lookupSyntaxName,
			  lookupLocatedOccRn, lookupLocatedBndrRn,
			  lookupLocatedGlobalOccRn, bindTyVarsRn, 
			  lookupFixityRn, lookupTyFixityRn, lookupConstructorFields,
			  lookupRecordBndr, mapFvRn, warnUnusedMatches,
			  newIPNameRn, bindPatSigTyVarsFV)
import TcRnMonad
import RdrName
import PrelNames	( eqClassName, integralClassName, geName, eqName,
		  	  negateName, minusName, lengthPName, indexPName,
			  plusIntegerName, fromIntegerName, timesIntegerName,
			  ratioDataConName, fromRationalName, fromStringName )
import TypeRep		( funTyCon )
import Constants	( mAX_TUPLE_SIZE )
import Name
import SrcLoc
import NameSet

import Literal		( inIntRange, inCharRange )
import BasicTypes	( compareFixity, funTyFixity, negateFixity, 
			  Fixity(..), FixityDirection(..) )
import ListSetOps	( removeDups, minusList )
import Outputable

#include "HsVersions.h"
\end{code}

These type renamers are in a separate module, rather than in (say) RnSource,
to break several loop.

%*********************************************************
%*							*
\subsection{Renaming types}
%*							*
%*********************************************************

\begin{code}
rnHsTypeFVs :: SDoc -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
rnHsTypeFVs doc_str ty 
  = rnLHsType doc_str ty	`thenM` \ ty' ->
    returnM (ty', extractHsTyNames ty')

rnHsSigType :: SDoc -> LHsType RdrName -> RnM (LHsType Name)
	-- rnHsSigType is used for source-language type signatures,
	-- which use *implicit* universal quantification.
rnHsSigType doc_str ty
  = rnLHsType (text "In the type signature for" <+> doc_str) ty
\end{code}

rnHsType is here because we call it from loadInstDecl, and I didn't
want a gratuitous knot.

\begin{code}
rnLHsType  :: SDoc -> LHsType RdrName -> RnM (LHsType Name)
rnLHsType doc = wrapLocM (rnHsType doc)

rnHsType :: SDoc -> HsType RdrName -> RnM (HsType Name)

rnHsType doc (HsForAllTy Implicit _ ctxt ty)
	-- Implicit quantifiction in source code (no kinds on tyvars)
	-- Given the signature  C => T  we universally quantify 
	-- over FV(T) \ {in-scope-tyvars} 
  = getLocalRdrEnv		`thenM` \ name_env ->
    let
	mentioned = extractHsRhoRdrTyVars ctxt ty

	-- Don't quantify over type variables that are in scope;
	-- when GlasgowExts is off, there usually won't be any, except for
	-- class signatures:
	--	class C a where { op :: a -> a }
	forall_tyvars = filter (not . (`elemLocalRdrEnv` name_env) . unLoc) mentioned
	tyvar_bndrs   = userHsTyVarBndrs forall_tyvars
    in
    rnForAll doc Implicit tyvar_bndrs ctxt ty

rnHsType doc (HsForAllTy Explicit forall_tyvars ctxt tau)
	-- Explicit quantification.
	-- Check that the forall'd tyvars are actually 
	-- mentioned in the type, and produce a warning if not
  = let
	mentioned	   = map unLoc (extractHsRhoRdrTyVars ctxt tau)
	forall_tyvar_names = hsLTyVarLocNames forall_tyvars

	-- Explicitly quantified but not mentioned in ctxt or tau
	warn_guys = filter ((`notElem` mentioned) . unLoc) forall_tyvar_names
    in
    mappM_ (forAllWarn doc tau) warn_guys	`thenM_`
    rnForAll doc Explicit forall_tyvars ctxt tau

rnHsType doc (HsTyVar tyvar)
  = lookupOccRn tyvar 		`thenM` \ tyvar' ->
    returnM (HsTyVar tyvar')

rnHsType doc ty@(HsOpTy ty1 (L loc op) ty2)
  = setSrcSpan loc $ 
    do	{ ty_ops_ok <- doptM Opt_TypeOperators
	; checkErr ty_ops_ok (opTyErr op ty)
	; op' <- lookupOccRn op
	; let l_op' = L loc op'
	; fix <- lookupTyFixityRn l_op'
	; ty1' <- rnLHsType doc ty1
	; ty2' <- rnLHsType doc ty2
	; mkHsOpTyRn (\t1 t2 -> HsOpTy t1 l_op' t2) (ppr op') fix ty1' ty2' }

rnHsType doc (HsParTy ty)
  = rnLHsType doc ty 	        `thenM` \ ty' ->
    returnM (HsParTy ty')

rnHsType doc (HsBangTy b ty)
  = rnLHsType doc ty 	        `thenM` \ ty' ->
    returnM (HsBangTy b ty')

rnHsType doc (HsNumTy i)
  | i == 1    = returnM (HsNumTy i)
  | otherwise = addErr err_msg	`thenM_`  returnM (HsNumTy i)
  where
    err_msg = ptext SLIT("Only unit numeric type pattern is valid")
			   

rnHsType doc (HsFunTy ty1 ty2)
  = rnLHsType doc ty1	`thenM` \ ty1' ->
	-- Might find a for-all as the arg of a function type
    rnLHsType doc ty2	`thenM` \ ty2' ->
	-- Or as the result.  This happens when reading Prelude.hi
	-- when we find return :: forall m. Monad m -> forall a. a -> m a

	-- Check for fixity rearrangements
    mkHsOpTyRn HsFunTy (ppr funTyCon) funTyFixity ty1' ty2'

rnHsType doc (HsListTy ty)
  = rnLHsType doc ty				`thenM` \ ty' ->
    returnM (HsListTy ty')

rnHsType doc (HsKindSig ty k)
  = rnLHsType doc ty				`thenM` \ ty' ->
    returnM (HsKindSig ty' k)

rnHsType doc (HsPArrTy ty)
  = rnLHsType doc ty				`thenM` \ ty' ->
    returnM (HsPArrTy ty')

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsType doc (HsTupleTy tup_con tys)
  = mappM (rnLHsType doc) tys	  	`thenM` \ tys' ->
    returnM (HsTupleTy tup_con tys')

rnHsType doc (HsAppTy ty1 ty2)
  = rnLHsType doc ty1		`thenM` \ ty1' ->
    rnLHsType doc ty2		`thenM` \ ty2' ->
    returnM (HsAppTy ty1' ty2')

rnHsType doc (HsPredTy pred)
  = rnPred doc pred	`thenM` \ pred' ->
    returnM (HsPredTy pred')

rnHsType doc (HsSpliceTy _)
  = do	{ addErr (ptext SLIT("Type splices are not yet implemented"))
 	; failM }

rnHsType doc (HsDocTy ty haddock_doc)
  = rnLHsType doc ty 	        `thenM` \ ty' ->
    rnLHsDoc haddock_doc	`thenM` \ haddock_doc' ->
    returnM (HsDocTy ty' haddock_doc')

rnLHsTypes doc tys = mappM (rnLHsType doc) tys
\end{code}


\begin{code}
rnForAll :: SDoc -> HsExplicitForAll -> [LHsTyVarBndr RdrName]
	 -> LHsContext RdrName -> LHsType RdrName -> RnM (HsType Name)

rnForAll doc exp [] (L _ []) (L _ ty) = rnHsType doc ty
	-- One reason for this case is that a type like Int#
	-- starts off as (HsForAllTy Nothing [] Int), in case
	-- there is some quantification.  Now that we have quantified
	-- and discovered there are no type variables, it's nicer to turn
	-- it into plain Int.  If it were Int# instead of Int, we'd actually
	-- get an error, because the body of a genuine for-all is
	-- of kind *.

rnForAll doc exp forall_tyvars ctxt ty
  = bindTyVarsRn doc forall_tyvars	$ \ new_tyvars ->
    rnContext doc ctxt			`thenM` \ new_ctxt ->
    rnLHsType doc ty			`thenM` \ new_ty ->
    returnM (HsForAllTy exp new_tyvars new_ctxt new_ty)
	-- Retain the same implicit/explicit flag as before
	-- so that we can later print it correctly
\end{code}

%*********************************************************
%*							*
\subsection{Contexts and predicates}
%*							*
%*********************************************************

\begin{code}
rnContext :: SDoc -> LHsContext RdrName -> RnM (LHsContext Name)
rnContext doc = wrapLocM (rnContext' doc)

rnContext' :: SDoc -> HsContext RdrName -> RnM (HsContext Name)
rnContext' doc ctxt = mappM (rnLPred doc) ctxt

rnLPred :: SDoc -> LHsPred RdrName -> RnM (LHsPred Name)
rnLPred doc  = wrapLocM (rnPred doc)

rnPred doc (HsClassP clas tys)
  = do { clas_name <- lookupOccRn clas
       ; tys' <- rnLHsTypes doc tys
       ; returnM (HsClassP clas_name tys')
       }
rnPred doc (HsEqualP ty1 ty2)
  = do { ty1' <- rnLHsType doc ty1
       ; ty2' <- rnLHsType doc ty2
       ; returnM (HsEqualP ty1' ty2')
       }
rnPred doc (HsIParam n ty)
  = do { name <- newIPNameRn n
       ; ty' <- rnLHsType doc ty
       ; returnM (HsIParam name ty')
       }
\end{code}


%************************************************************************
%*									*
	Fixities and precedence parsing
%*									*
%************************************************************************

@mkOpAppRn@ deals with operator fixities.  The argument expressions
are assumed to be already correctly arranged.  It needs the fixities
recorded in the OpApp nodes, because fixity info applies to the things
the programmer actually wrote, so you can't find it out from the Name.

Furthermore, the second argument is guaranteed not to be another
operator application.  Why? Because the parser parses all
operator appications left-associatively, EXCEPT negation, which
we need to handle specially.
Infix types are read in a *right-associative* way, so that
	a `op` b `op` c
is always read in as
	a `op` (b `op` c)

mkHsOpTyRn rearranges where necessary.  The two arguments
have already been renamed and rearranged.  It's made rather tiresome
by the presence of ->, which is a separate syntactic construct.

\begin{code}
---------------
-- Building (ty1 `op1` (ty21 `op2` ty22))
mkHsOpTyRn :: (LHsType Name -> LHsType Name -> HsType Name)
	   -> SDoc -> Fixity -> LHsType Name -> LHsType Name 
	   -> RnM (HsType Name)

mkHsOpTyRn mk1 pp_op1 fix1 ty1 (L loc2 (HsOpTy ty21 op2 ty22))
  = do  { fix2 <- lookupTyFixityRn op2
	; mk_hs_op_ty mk1 pp_op1 fix1 ty1 
		      (\t1 t2 -> HsOpTy t1 op2 t2)
		      (ppr op2) fix2 ty21 ty22 loc2 }

mkHsOpTyRn mk1 pp_op1 fix1 ty1 ty2@(L loc2 (HsFunTy ty21 ty22))
  = mk_hs_op_ty mk1 pp_op1 fix1 ty1 
		HsFunTy (ppr funTyCon) funTyFixity ty21 ty22 loc2

mkHsOpTyRn mk1 pp_op1 fix1 ty1 ty2 		-- Default case, no rearrangment
  = return (mk1 ty1 ty2)

---------------
mk_hs_op_ty :: (LHsType Name -> LHsType Name -> HsType Name)
	    -> SDoc -> Fixity -> LHsType Name
	    -> (LHsType Name -> LHsType Name -> HsType Name)
	    -> SDoc -> Fixity -> LHsType Name -> LHsType Name -> SrcSpan
	    -> RnM (HsType Name)
mk_hs_op_ty mk1 pp_op1 fix1 ty1 
	    mk2 pp_op2 fix2 ty21 ty22 loc2
  | nofix_error     = do { addErr (precParseErr (quotes pp_op1,fix1) 
			 		        (quotes pp_op2,fix2))
		         ; return (mk1 ty1 (L loc2 (mk2 ty21 ty22))) }
  | associate_right = return (mk1 ty1 (L loc2 (mk2 ty21 ty22)))
  | otherwise	    = do { -- Rearrange to ((ty1 `op1` ty21) `op2` ty22)
			   new_ty <- mkHsOpTyRn mk1 pp_op1 fix1 ty1 ty21
			 ; return (mk2 (noLoc new_ty) ty22) }
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2


---------------------------
mkOpAppRn :: LHsExpr Name			-- Left operand; already rearranged
	  -> LHsExpr Name -> Fixity 		-- Operator and fixity
	  -> LHsExpr Name			-- Right operand (not an OpApp, but might
						-- be a NegApp)
	  -> RnM (HsExpr Name)

-- (e11 `op1` e12) `op2` e2
mkOpAppRn e1@(L _ (OpApp e11 op1 fix1 e12)) op2 fix2 e2
  | nofix_error
  = addErr (precParseErr (ppr_op op1,fix1) (ppr_op op2,fix2))	`thenM_`
    returnM (OpApp e1 op2 fix2 e2)

  | associate_right
  = mkOpAppRn e12 op2 fix2 e2		`thenM` \ new_e ->
    returnM (OpApp e11 op1 fix1 (L loc' new_e))
  where
    loc'= combineLocs e12 e2
    (nofix_error, associate_right) = compareFixity fix1 fix2

---------------------------
--	(- neg_arg) `op` e2
mkOpAppRn e1@(L _ (NegApp neg_arg neg_name)) op2 fix2 e2
  | nofix_error
  = addErr (precParseErr (pp_prefix_minus,negateFixity) (ppr_op op2,fix2))	`thenM_`
    returnM (OpApp e1 op2 fix2 e2)

  | associate_right
  = mkOpAppRn neg_arg op2 fix2 e2	`thenM` \ new_e ->
    returnM (NegApp (L loc' new_e) neg_name)
  where
    loc' = combineLocs neg_arg e2
    (nofix_error, associate_right) = compareFixity negateFixity fix2

---------------------------
--	e1 `op` - neg_arg
mkOpAppRn e1 op1 fix1 e2@(L _ (NegApp neg_arg _))	-- NegApp can occur on the right
  | not associate_right				-- We *want* right association
  = addErr (precParseErr (ppr_op op1, fix1) (pp_prefix_minus, negateFixity))	`thenM_`
    returnM (OpApp e1 op1 fix1 e2)
  where
    (_, associate_right) = compareFixity fix1 negateFixity

---------------------------
--	Default case
mkOpAppRn e1 op fix e2 			-- Default case, no rearrangment
  = ASSERT2( right_op_ok fix (unLoc e2),
	     ppr e1 $$ text "---" $$ ppr op $$ text "---" $$ ppr fix $$ text "---" $$ ppr e2
    )
    returnM (OpApp e1 op fix e2)

-- Parser left-associates everything, but 
-- derived instances may have correctly-associated things to
-- in the right operarand.  So we just check that the right operand is OK
right_op_ok fix1 (OpApp _ _ fix2 _)
  = not error_please && associate_right
  where
    (error_please, associate_right) = compareFixity fix1 fix2
right_op_ok fix1 other
  = True

-- Parser initially makes negation bind more tightly than any other operator
-- And "deriving" code should respect this (use HsPar if not)
mkNegAppRn :: LHsExpr id -> SyntaxExpr id -> RnM (HsExpr id)
mkNegAppRn neg_arg neg_name
  = ASSERT( not_op_app (unLoc neg_arg) )
    returnM (NegApp neg_arg neg_name)

not_op_app (OpApp _ _ _ _) = False
not_op_app other	   = True

---------------------------
mkOpFormRn :: LHsCmdTop Name		-- Left operand; already rearranged
	  -> LHsExpr Name -> Fixity 	-- Operator and fixity
	  -> LHsCmdTop Name		-- Right operand (not an infix)
	  -> RnM (HsCmd Name)

-- (e11 `op1` e12) `op2` e2
mkOpFormRn a1@(L loc (HsCmdTop (L _ (HsArrForm op1 (Just fix1) [a11,a12])) _ _ _))
	op2 fix2 a2
  | nofix_error
  = addErr (precParseErr (ppr_op op1,fix1) (ppr_op op2,fix2))	`thenM_`
    returnM (HsArrForm op2 (Just fix2) [a1, a2])

  | associate_right
  = mkOpFormRn a12 op2 fix2 a2		`thenM` \ new_c ->
    returnM (HsArrForm op1 (Just fix1)
	[a11, L loc (HsCmdTop (L loc new_c) [] placeHolderType [])])
	-- TODO: locs are wrong
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2

--	Default case
mkOpFormRn arg1 op fix arg2 			-- Default case, no rearrangment
  = returnM (HsArrForm op (Just fix) [arg1, arg2])


--------------------------------------
mkConOpPatRn :: Located Name -> Fixity -> LPat Name -> LPat Name
	     -> RnM (Pat Name)

mkConOpPatRn op2 fix2 p1@(L loc (ConPatIn op1 (InfixCon p11 p12))) p2
  = do	{ fix1 <- lookupFixityRn (unLoc op1)
	; let (nofix_error, associate_right) = compareFixity fix1 fix2

	; if nofix_error then do
		{ addErr (precParseErr (ppr_op op1,fix1) (ppr_op op2,fix2))
		; return (ConPatIn op2 (InfixCon p1 p2)) }

	  else if associate_right then do
		{ new_p <- mkConOpPatRn op2 fix2 p12 p2
		; return (ConPatIn op1 (InfixCon p11 (L loc new_p))) } -- XXX loc right?
	  else return (ConPatIn op2 (InfixCon p1 p2)) }

mkConOpPatRn op fix p1 p2 			-- Default case, no rearrangment
  = ASSERT( not_op_pat (unLoc p2) )
    returnM (ConPatIn op (InfixCon p1 p2))

not_op_pat (ConPatIn _ (InfixCon _ _)) = False
not_op_pat other   	               = True

--------------------------------------
checkPrecMatch :: Bool -> Name -> MatchGroup Name -> RnM ()
	-- True indicates an infix lhs
	-- See comments with rnExpr (OpApp ...) about "deriving"

checkPrecMatch False fn match 
  = returnM ()
checkPrecMatch True op (MatchGroup ms _)	
  = mapM_ check ms			 	
  where
    check (L _ (Match (p1:p2:_) _ _))
      = checkPrec op (unLoc p1) False	`thenM_`
        checkPrec op (unLoc p2) True

    check _ = return ()	
	-- This can happen.  Consider
	--	a `op` True = ...
	--	op          = ...
	-- The infix flag comes from the first binding of the group
	-- but the second eqn has no args (an error, but not discovered
	-- until the type checker).  So we don't want to crash on the
	-- second eqn.

checkPrec op (ConPatIn op1 (InfixCon _ _)) right
  = lookupFixityRn op		`thenM` \  op_fix@(Fixity op_prec  op_dir) ->
    lookupFixityRn (unLoc op1)	`thenM` \ op1_fix@(Fixity op1_prec op1_dir) ->
    let
	inf_ok = op1_prec > op_prec || 
	         (op1_prec == op_prec &&
		  (op1_dir == InfixR && op_dir == InfixR && right ||
		   op1_dir == InfixL && op_dir == InfixL && not right))

	info  = (ppr_op op,  op_fix)
	info1 = (ppr_op op1, op1_fix)
	(infol, infor) = if right then (info, info1) else (info1, info)
    in
    checkErr inf_ok (precParseErr infol infor)

checkPrec op pat right
  = returnM ()

-- Check precedence of (arg op) or (op arg) respectively
-- If arg is itself an operator application, then either
--   (a) its precedence must be higher than that of op
--   (b) its precedency & associativity must be the same as that of op
checkSectionPrec :: FixityDirection -> HsExpr RdrName
	-> LHsExpr Name -> LHsExpr Name -> RnM ()
checkSectionPrec direction section op arg
  = case unLoc arg of
	OpApp _ op fix _ -> go_for_it (ppr_op op)     fix
	NegApp _ _	 -> go_for_it pp_prefix_minus negateFixity
	other		 -> returnM ()
  where
    L _ (HsVar op_name) = op
    go_for_it pp_arg_op arg_fix@(Fixity arg_prec assoc)
	= lookupFixityRn op_name	`thenM` \ op_fix@(Fixity op_prec _) ->
	  checkErr (op_prec < arg_prec
		     || op_prec == arg_prec && direction == assoc)
		  (sectionPrecErr (ppr_op op_name, op_fix) 	
		  (pp_arg_op, arg_fix) section)
\end{code}

Precedence-related error messages

\begin{code}
precParseErr op1 op2 
  = hang (ptext SLIT("precedence parsing error"))
      4 (hsep [ptext SLIT("cannot mix"), ppr_opfix op1, ptext SLIT("and"), 
	       ppr_opfix op2,
	       ptext SLIT("in the same infix expression")])

sectionPrecErr op arg_op section
 = vcat [ptext SLIT("The operator") <+> ppr_opfix op <+> ptext SLIT("of a section"),
	 nest 4 (ptext SLIT("must have lower precedence than the operand") <+> ppr_opfix arg_op),
	 nest 4 (ptext SLIT("in the section:") <+> quotes (ppr section))]

pp_prefix_minus = ptext SLIT("prefix `-'")
ppr_op op = quotes (ppr op)	-- Here, op can be a Name or a (Var n), where n is a Name
ppr_opfix (pp_op, fixity) = pp_op <+> brackets (ppr fixity)
\end{code}

%*********************************************************
%*							*
\subsection{Errors}
%*							*
%*********************************************************

\begin{code}
forAllWarn doc ty (L loc tyvar)
  = ifOptM Opt_WarnUnusedMatches 	$
    addWarnAt loc (sep [ptext SLIT("The universally quantified type variable") <+> quotes (ppr tyvar),
		 	nest 4 (ptext SLIT("does not appear in the type") <+> quotes (ppr ty))]
		   $$
		   doc)

opTyErr op ty 
  = hang (ptext SLIT("Illegal operator") <+> quotes (ppr op) <+> ptext SLIT("in type") <+> quotes (ppr ty))
	 2 (parens (ptext SLIT("Use -XTypeOperators to allow operators in types")))
\end{code}
