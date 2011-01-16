%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnTypes ( 
	-- Type related stuff
	rnHsType, rnLHsType, rnLHsTypes, rnContext,
	rnHsSigType, rnHsTypeFVs, rnConDeclFields, rnLPred,

	-- Precence related stuff
	mkOpAppRn, mkNegAppRn, mkOpFormRn, mkConOpPatRn,
	checkPrecMatch, checkSectionPrec,

	-- Splice related stuff
	rnSplice, checkTH
  ) where

import {-# SOURCE #-} RnExpr( rnLExpr )
#ifdef GHCI
import {-# SOURCE #-} TcSplice( runQuasiQuoteType )
#endif 	/* GHCI */

import DynFlags
import HsSyn
import RdrHsSyn		( extractHsRhoRdrTyVars )
import RnHsSyn		( extractHsTyNames )
import RnHsDoc          ( rnLHsDoc, rnMbLHsDoc )
import RnEnv
import TcRnMonad
import RdrName
import PrelNames
import TypeRep		( funTyConName )
import Name
import SrcLoc
import NameSet

import BasicTypes	( compareFixity, funTyFixity, negateFixity, 
			  Fixity(..), FixityDirection(..) )
import Outputable
import FastString
import Control.Monad	( unless )

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
rnHsTypeFVs doc_str ty  = do
    ty' <- rnLHsType doc_str ty
    return (ty', extractHsTyNames ty')

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

rnHsType doc (HsForAllTy Implicit _ ctxt ty) = do
	-- Implicit quantifiction in source code (no kinds on tyvars)
	-- Given the signature  C => T  we universally quantify 
	-- over FV(T) \ {in-scope-tyvars} 
    name_env <- getLocalRdrEnv
    let
	mentioned = extractHsRhoRdrTyVars ctxt ty

	-- Don't quantify over type variables that are in scope;
	-- when GlasgowExts is off, there usually won't be any, except for
	-- class signatures:
	--	class C a where { op :: a -> a }
	forall_tyvars = filter (not . (`elemLocalRdrEnv` name_env) . unLoc) mentioned
	tyvar_bndrs   = userHsTyVarBndrs forall_tyvars

    rnForAll doc Implicit tyvar_bndrs ctxt ty

rnHsType doc (HsForAllTy Explicit forall_tyvars ctxt tau) = do
	-- Explicit quantification.
	-- Check that the forall'd tyvars are actually 
	-- mentioned in the type, and produce a warning if not
    let
	mentioned	   = map unLoc (extractHsRhoRdrTyVars ctxt tau)
	forall_tyvar_names = hsLTyVarLocNames forall_tyvars

	-- Explicitly quantified but not mentioned in ctxt or tau
	warn_guys = filter ((`notElem` mentioned) . unLoc) forall_tyvar_names

    mapM_ (forAllWarn doc tau) warn_guys
    rnForAll doc Explicit forall_tyvars ctxt tau

rnHsType _ (HsTyVar tyvar) = do
    tyvar' <- lookupOccRn tyvar
    return (HsTyVar tyvar')

-- If we see (forall a . ty), without foralls on, the forall will give
-- a sensible error message, but we don't want to complain about the dot too
-- Hence the jiggery pokery with ty1
rnHsType doc ty@(HsOpTy ty1 (L loc op) ty2)
  = setSrcSpan loc $ 
    do	{ ops_ok <- xoptM Opt_TypeOperators
	; op' <- if ops_ok
		 then lookupOccRn op 
		 else do { addErr (opTyErr op ty)
			 ; return (mkUnboundName op) }	-- Avoid double complaint
	; let l_op' = L loc op'
	; fix <- lookupTyFixityRn l_op'
	; ty1' <- rnLHsType doc ty1
	; ty2' <- rnLHsType doc ty2
	; mkHsOpTyRn (\t1 t2 -> HsOpTy t1 l_op' t2) op' fix ty1' ty2' }

rnHsType doc (HsParTy ty) = do
    ty' <- rnLHsType doc ty
    return (HsParTy ty')

rnHsType doc (HsBangTy b ty)
  = do { ty' <- rnLHsType doc ty
       ; return (HsBangTy b ty') }

rnHsType doc (HsRecTy flds)
  = do { flds' <- rnConDeclFields doc flds
       ; return (HsRecTy flds') }

rnHsType _ (HsNumberTy i) = return (HsNumberTy i)

rnHsType _ (HsNumTy i)
  | i == 1    = return (HsNumTy i)
  | otherwise = addErr err_msg >> return (HsNumTy i)
  where
    err_msg = ptext (sLit "Only unit numeric type pattern is valid")
			   

rnHsType doc (HsFunTy ty1 ty2) = do
    ty1' <- rnLHsType doc ty1
	-- Might find a for-all as the arg of a function type
    ty2' <- rnLHsType doc ty2
	-- Or as the result.  This happens when reading Prelude.hi
	-- when we find return :: forall m. Monad m -> forall a. a -> m a

	-- Check for fixity rearrangements
    mkHsOpTyRn HsFunTy funTyConName funTyFixity ty1' ty2'

rnHsType doc (HsListTy ty) = do
    ty' <- rnLHsType doc ty
    return (HsListTy ty')

rnHsType doc (HsKindSig ty k)
  = do { kind_sigs_ok <- xoptM Opt_KindSignatures
       ; unless kind_sigs_ok (addErr (kindSigErr ty))
       ; ty' <- rnLHsType doc ty
       ; return (HsKindSig ty' k) }

rnHsType doc (HsPArrTy ty) = do
    ty' <- rnLHsType doc ty
    return (HsPArrTy ty')

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsType doc (HsTupleTy tup_con tys) = do
    tys' <- mapM (rnLHsType doc) tys
    return (HsTupleTy tup_con tys')

rnHsType doc (HsAppTy ty1 ty2) = do
    ty1' <- rnLHsType doc ty1
    ty2' <- rnLHsType doc ty2
    return (HsAppTy ty1' ty2')

rnHsType doc (HsPredTy pred) = do
    pred' <- rnPred doc pred
    return (HsPredTy pred')

rnHsType _ (HsSpliceTy sp _ k)
  = do { (sp', fvs) <- rnSplice sp	-- ToDo: deal with fvs
       ; return (HsSpliceTy sp' fvs k) }

rnHsType doc (HsDocTy ty haddock_doc) = do
    ty' <- rnLHsType doc ty
    haddock_doc' <- rnLHsDoc haddock_doc
    return (HsDocTy ty' haddock_doc')

#ifndef GHCI
rnHsType _ ty@(HsQuasiQuoteTy _) = pprPanic "Can't do quasiquotation without GHCi" (ppr ty)
#else
rnHsType doc (HsQuasiQuoteTy qq) = do { ty <- runQuasiQuoteType qq
                                      ; rnHsType doc (unLoc ty) }
#endif
rnHsType _ (HsCoreTy ty) = return (HsCoreTy ty)

--------------
rnLHsTypes :: SDoc -> [LHsType RdrName]
           -> IOEnv (Env TcGblEnv TcLclEnv) [LHsType Name]
rnLHsTypes doc tys = mapM (rnLHsType doc) tys
\end{code}


\begin{code}
rnForAll :: SDoc -> HsExplicitFlag -> [LHsTyVarBndr RdrName]
	 -> LHsContext RdrName -> LHsType RdrName -> RnM (HsType Name)

rnForAll doc _ [] (L _ []) (L _ ty) = rnHsType doc ty
	-- One reason for this case is that a type like Int#
	-- starts off as (HsForAllTy Nothing [] Int), in case
	-- there is some quantification.  Now that we have quantified
	-- and discovered there are no type variables, it's nicer to turn
	-- it into plain Int.  If it were Int# instead of Int, we'd actually
	-- get an error, because the body of a genuine for-all is
	-- of kind *.

rnForAll doc exp forall_tyvars ctxt ty
  = bindTyVarsRn forall_tyvars $ \ new_tyvars -> do
    new_ctxt <- rnContext doc ctxt
    new_ty <- rnLHsType doc ty
    return (HsForAllTy exp new_tyvars new_ctxt new_ty)
	-- Retain the same implicit/explicit flag as before
	-- so that we can later print it correctly

rnConDeclFields :: SDoc -> [ConDeclField RdrName] -> RnM [ConDeclField Name]
rnConDeclFields doc fields = mapM (rnField doc) fields

rnField :: SDoc -> ConDeclField RdrName -> RnM (ConDeclField Name)
rnField doc (ConDeclField name ty haddock_doc)
  = do { new_name <- lookupLocatedTopBndrRn name
       ; new_ty <- rnLHsType doc ty
       ; new_haddock_doc <- rnMbLHsDoc haddock_doc
       ; return (ConDeclField new_name new_ty new_haddock_doc) }
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
rnContext' doc ctxt = mapM (rnLPred doc) ctxt

rnLPred :: SDoc -> LHsPred RdrName -> RnM (LHsPred Name)
rnLPred doc  = wrapLocM (rnPred doc)

rnPred :: SDoc -> HsPred RdrName
       -> IOEnv (Env TcGblEnv TcLclEnv) (HsPred Name)
rnPred doc (HsClassP clas tys)
  = do { clas_name <- lookupOccRn clas
       ; tys' <- rnLHsTypes doc tys
       ; return (HsClassP clas_name tys')
       }
rnPred doc (HsEqualP ty1 ty2)
  = do { ty1' <- rnLHsType doc ty1
       ; ty2' <- rnLHsType doc ty2
       ; return (HsEqualP ty1' ty2')
       }
rnPred doc (HsIParam n ty)
  = do { name <- newIPNameRn n
       ; ty' <- rnLHsType doc ty
       ; return (HsIParam name ty')
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
	   -> Name -> Fixity -> LHsType Name -> LHsType Name 
	   -> RnM (HsType Name)

mkHsOpTyRn mk1 pp_op1 fix1 ty1 (L loc2 (HsOpTy ty21 op2 ty22))
  = do  { fix2 <- lookupTyFixityRn op2
	; mk_hs_op_ty mk1 pp_op1 fix1 ty1 
		      (\t1 t2 -> HsOpTy t1 op2 t2)
		      (unLoc op2) fix2 ty21 ty22 loc2 }

mkHsOpTyRn mk1 pp_op1 fix1 ty1 (L loc2 (HsFunTy ty21 ty22))
  = mk_hs_op_ty mk1 pp_op1 fix1 ty1 
		HsFunTy funTyConName funTyFixity ty21 ty22 loc2

mkHsOpTyRn mk1 _ _ ty1 ty2 		-- Default case, no rearrangment
  = return (mk1 ty1 ty2)

---------------
mk_hs_op_ty :: (LHsType Name -> LHsType Name -> HsType Name)
	    -> Name -> Fixity -> LHsType Name
	    -> (LHsType Name -> LHsType Name -> HsType Name)
	    -> Name -> Fixity -> LHsType Name -> LHsType Name -> SrcSpan
	    -> RnM (HsType Name)
mk_hs_op_ty mk1 op1 fix1 ty1 
	    mk2 op2 fix2 ty21 ty22 loc2
  | nofix_error     = do { precParseErr (op1,fix1) (op2,fix2)
		         ; return (mk1 ty1 (L loc2 (mk2 ty21 ty22))) }
  | associate_right = return (mk1 ty1 (L loc2 (mk2 ty21 ty22)))
  | otherwise	    = do { -- Rearrange to ((ty1 `op1` ty21) `op2` ty22)
			   new_ty <- mkHsOpTyRn mk1 op1 fix1 ty1 ty21
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
  = do precParseErr (get_op op1,fix1) (get_op op2,fix2)
       return (OpApp e1 op2 fix2 e2)

  | associate_right = do
    new_e <- mkOpAppRn e12 op2 fix2 e2
    return (OpApp e11 op1 fix1 (L loc' new_e))
  where
    loc'= combineLocs e12 e2
    (nofix_error, associate_right) = compareFixity fix1 fix2

---------------------------
--	(- neg_arg) `op` e2
mkOpAppRn e1@(L _ (NegApp neg_arg neg_name)) op2 fix2 e2
  | nofix_error
  = do precParseErr (negateName,negateFixity) (get_op op2,fix2)
       return (OpApp e1 op2 fix2 e2)

  | associate_right 
  = do new_e <- mkOpAppRn neg_arg op2 fix2 e2
       return (NegApp (L loc' new_e) neg_name)
  where
    loc' = combineLocs neg_arg e2
    (nofix_error, associate_right) = compareFixity negateFixity fix2

---------------------------
--	e1 `op` - neg_arg
mkOpAppRn e1 op1 fix1 e2@(L _ (NegApp _ _))	-- NegApp can occur on the right
  | not associate_right			-- We *want* right association
  = do precParseErr (get_op op1, fix1) (negateName, negateFixity)
       return (OpApp e1 op1 fix1 e2)
  where
    (_, associate_right) = compareFixity fix1 negateFixity

---------------------------
--	Default case
mkOpAppRn e1 op fix e2 			-- Default case, no rearrangment
  = ASSERT2( right_op_ok fix (unLoc e2),
	     ppr e1 $$ text "---" $$ ppr op $$ text "---" $$ ppr fix $$ text "---" $$ ppr e2
    )
    return (OpApp e1 op fix e2)

----------------------------
get_op :: LHsExpr Name -> Name
get_op (L _ (HsVar n)) = n
get_op other           = pprPanic "get_op" (ppr other)

-- Parser left-associates everything, but 
-- derived instances may have correctly-associated things to
-- in the right operarand.  So we just check that the right operand is OK
right_op_ok :: Fixity -> HsExpr Name -> Bool
right_op_ok fix1 (OpApp _ _ fix2 _)
  = not error_please && associate_right
  where
    (error_please, associate_right) = compareFixity fix1 fix2
right_op_ok _ _
  = True

-- Parser initially makes negation bind more tightly than any other operator
-- And "deriving" code should respect this (use HsPar if not)
mkNegAppRn :: LHsExpr id -> SyntaxExpr id -> RnM (HsExpr id)
mkNegAppRn neg_arg neg_name
  = ASSERT( not_op_app (unLoc neg_arg) )
    return (NegApp neg_arg neg_name)

not_op_app :: HsExpr id -> Bool
not_op_app (OpApp _ _ _ _) = False
not_op_app _    	   = True

---------------------------
mkOpFormRn :: LHsCmdTop Name		-- Left operand; already rearranged
	  -> LHsExpr Name -> Fixity 	-- Operator and fixity
	  -> LHsCmdTop Name		-- Right operand (not an infix)
	  -> RnM (HsCmd Name)

-- (e11 `op1` e12) `op2` e2
mkOpFormRn a1@(L loc (HsCmdTop (L _ (HsArrForm op1 (Just fix1) [a11,a12])) _ _ _))
	op2 fix2 a2
  | nofix_error
  = do precParseErr (get_op op1,fix1) (get_op op2,fix2)
       return (HsArrForm op2 (Just fix2) [a1, a2])

  | associate_right
  = do new_c <- mkOpFormRn a12 op2 fix2 a2
       return (HsArrForm op1 (Just fix1)
	          [a11, L loc (HsCmdTop (L loc new_c) [] placeHolderType [])])
	-- TODO: locs are wrong
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2

--	Default case
mkOpFormRn arg1 op fix arg2 			-- Default case, no rearrangment
  = return (HsArrForm op (Just fix) [arg1, arg2])


--------------------------------------
mkConOpPatRn :: Located Name -> Fixity -> LPat Name -> LPat Name
	     -> RnM (Pat Name)

mkConOpPatRn op2 fix2 p1@(L loc (ConPatIn op1 (InfixCon p11 p12))) p2
  = do	{ fix1 <- lookupFixityRn (unLoc op1)
	; let (nofix_error, associate_right) = compareFixity fix1 fix2

	; if nofix_error then do
		{ precParseErr (unLoc op1,fix1) (unLoc op2,fix2)
		; return (ConPatIn op2 (InfixCon p1 p2)) }

	  else if associate_right then do
		{ new_p <- mkConOpPatRn op2 fix2 p12 p2
		; return (ConPatIn op1 (InfixCon p11 (L loc new_p))) } -- XXX loc right?
	  else return (ConPatIn op2 (InfixCon p1 p2)) }

mkConOpPatRn op _ p1 p2 			-- Default case, no rearrangment
  = ASSERT( not_op_pat (unLoc p2) )
    return (ConPatIn op (InfixCon p1 p2))

not_op_pat :: Pat Name -> Bool
not_op_pat (ConPatIn _ (InfixCon _ _)) = False
not_op_pat _        	               = True

--------------------------------------
checkPrecMatch :: Name -> MatchGroup Name -> RnM ()
  -- Check precedence of a function binding written infix
  --   eg  a `op` b `C` c = ...
  -- See comments with rnExpr (OpApp ...) about "deriving"

checkPrecMatch op (MatchGroup ms _)	
  = mapM_ check ms			 	
  where
    check (L _ (Match (L l1 p1 : L l2 p2 :_) _ _))
      = setSrcSpan (combineSrcSpans l1 l2) $
        do checkPrec op p1 False
           checkPrec op p2 True

    check _ = return ()	
	-- This can happen.  Consider
	--	a `op` True = ...
	--	op          = ...
	-- The infix flag comes from the first binding of the group
	-- but the second eqn has no args (an error, but not discovered
	-- until the type checker).  So we don't want to crash on the
	-- second eqn.

checkPrec :: Name -> Pat Name -> Bool -> IOEnv (Env TcGblEnv TcLclEnv) ()
checkPrec op (ConPatIn op1 (InfixCon _ _)) right = do
    op_fix@(Fixity op_prec  op_dir) <- lookupFixityRn op
    op1_fix@(Fixity op1_prec op1_dir) <- lookupFixityRn (unLoc op1)
    let
	inf_ok = op1_prec > op_prec || 
	         (op1_prec == op_prec &&
		  (op1_dir == InfixR && op_dir == InfixR && right ||
		   op1_dir == InfixL && op_dir == InfixL && not right))

	info  = (op,        op_fix)
	info1 = (unLoc op1, op1_fix)
	(infol, infor) = if right then (info, info1) else (info1, info)
    unless inf_ok (precParseErr infol infor)

checkPrec _ _ _
  = return ()

-- Check precedence of (arg op) or (op arg) respectively
-- If arg is itself an operator application, then either
--   (a) its precedence must be higher than that of op
--   (b) its precedency & associativity must be the same as that of op
checkSectionPrec :: FixityDirection -> HsExpr RdrName
	-> LHsExpr Name -> LHsExpr Name -> RnM ()
checkSectionPrec direction section op arg
  = case unLoc arg of
	OpApp _ op fix _ -> go_for_it (get_op op) fix
	NegApp _ _	 -> go_for_it negateName  negateFixity
	_    		 -> return ()
  where
    op_name = get_op op
    go_for_it arg_op arg_fix@(Fixity arg_prec assoc) = do
          op_fix@(Fixity op_prec _) <- lookupFixityRn op_name
	  unless (op_prec < arg_prec
	          || (op_prec == arg_prec && direction == assoc))
		 (sectionPrecErr (op_name, op_fix) 	
				 (arg_op, arg_fix) section)
\end{code}

Precedence-related error messages

\begin{code}
precParseErr :: (Name, Fixity) -> (Name, Fixity) -> RnM ()
precParseErr op1@(n1,_) op2@(n2,_) 
  | isUnboundName n1 || isUnboundName n2
  = return ()	  -- Avoid error cascade
  | otherwise
  = addErr $ hang (ptext (sLit "Precedence parsing error"))
      4 (hsep [ptext (sLit "cannot mix"), ppr_opfix op1, ptext (sLit "and"), 
	       ppr_opfix op2,
	       ptext (sLit "in the same infix expression")])

sectionPrecErr :: (Name, Fixity) -> (Name, Fixity) -> HsExpr RdrName -> RnM ()
sectionPrecErr op@(n1,_) arg_op@(n2,_) section
  | isUnboundName n1 || isUnboundName n2
  = return ()	  -- Avoid error cascade
  | otherwise
  = addErr $ vcat [ptext (sLit "The operator") <+> ppr_opfix op <+> ptext (sLit "of a section"),
	 nest 4 (sep [ptext (sLit "must have lower precedence than that of the operand,"),
	      	      nest 2 (ptext (sLit "namely") <+> ppr_opfix arg_op)]),
	 nest 4 (ptext (sLit "in the section:") <+> quotes (ppr section))]

ppr_opfix :: (Name, Fixity) -> SDoc
ppr_opfix (op, fixity) = pp_op <+> brackets (ppr fixity)
   where
     pp_op | op == negateName = ptext (sLit "prefix `-'")
     	   | otherwise        = quotes (ppr op)
\end{code}

%*********************************************************
%*							*
\subsection{Errors}
%*							*
%*********************************************************

\begin{code}
forAllWarn :: SDoc -> LHsType RdrName -> Located RdrName
           -> TcRnIf TcGblEnv TcLclEnv ()
forAllWarn doc ty (L loc tyvar)
  = ifDOptM Opt_WarnUnusedMatches 	$
    addWarnAt loc (sep [ptext (sLit "The universally quantified type variable") <+> quotes (ppr tyvar),
		 	nest 4 (ptext (sLit "does not appear in the type") <+> quotes (ppr ty))]
		   $$
		   doc)

opTyErr :: RdrName -> HsType RdrName -> SDoc
opTyErr op ty@(HsOpTy ty1 _ _)
  = hang (ptext (sLit "Illegal operator") <+> quotes (ppr op) <+> ptext (sLit "in type") <+> quotes (ppr ty))
	 2 extra
  where
    extra | op == dot_tv_RDR && forall_head ty1
	  = perhapsForallMsg
	  | otherwise 
	  = ptext (sLit "Use -XTypeOperators to allow operators in types")

    forall_head (L _ (HsTyVar tv))   = tv == forall_tv_RDR
    forall_head (L _ (HsAppTy ty _)) = forall_head ty
    forall_head _other		     = False
opTyErr _ ty = pprPanic "opTyErr: Not an op" (ppr ty)
\end{code}

%*********************************************************
%*							*
		Splices
%*							*
%*********************************************************

Note [Splices]
~~~~~~~~~~~~~~
Consider
	f = ...
	h = ...$(thing "f")...

The splice can expand into literally anything, so when we do dependency
analysis we must assume that it might mention 'f'.  So we simply treat
all locally-defined names as mentioned by any splice.  This is terribly
brutal, but I don't see what else to do.  For example, it'll mean
that every locally-defined thing will appear to be used, so no unused-binding
warnings.  But if we miss the dependency, then we might typecheck 'h' before 'f',
and that will crash the type checker because 'f' isn't in scope.

Currently, I'm not treating a splice as also mentioning every import,
which is a bit inconsistent -- but there are a lot of them.  We might
thereby get some bogus unused-import warnings, but we won't crash the
type checker.  Not very satisfactory really.

\begin{code}
rnSplice :: HsSplice RdrName -> RnM (HsSplice Name, FreeVars)
rnSplice (HsSplice n expr)
  = do	{ checkTH expr "splice"
	; loc  <- getSrcSpanM
	; n' <- newLocalBndrRn (L loc n)
	; (expr', fvs) <- rnLExpr expr

	-- Ugh!  See Note [Splices] above
	; lcl_rdr <- getLocalRdrEnv
	; gbl_rdr <- getGlobalRdrEnv
	; let gbl_names = mkNameSet [gre_name gre | gre <- globalRdrEnvElts gbl_rdr, 
						    isLocalGRE gre]
	      lcl_names = mkNameSet (occEnvElts lcl_rdr)

	; return (HsSplice n' expr', fvs `plusFV` lcl_names `plusFV` gbl_names) }

checkTH :: Outputable a => a -> String -> RnM ()
#ifdef GHCI 
checkTH _ _ = return ()	-- OK
#else
checkTH e what 	-- Raise an error in a stage-1 compiler
  = addErr (vcat [ptext (sLit "Template Haskell") <+> text what <+>  
	          ptext (sLit "illegal in a stage-1 compiler"),
	          nest 2 (ppr e)])
#endif   
\end{code}
