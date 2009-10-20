%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcExpr]{Typecheck an expression}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module TcExpr ( tcPolyExpr, tcPolyExprNC, tcMonoExpr, tcMonoExprNC, 
                tcInferRho, tcInferRhoNC, tcSyntaxOp, 
                addExprErrCtxt ) where

#include "HsVersions.h"

#ifdef GHCI 	/* Only if bootstrapped */
import {-# SOURCE #-}	TcSplice( tcSpliceExpr, tcBracket )
import qualified DsMeta
#endif

import HsSyn
import TcHsSyn
import TcRnMonad
import TcUnify
import BasicTypes
import Inst
import TcBinds
import TcEnv
import TcArrows
import TcMatches
import TcHsType
import TcPat
import TcMType
import TcType
import TcIface	( checkWiredInTyCon )
import Id
import DataCon
import Name
import TyCon
import Type
import TypeRep
import Coercion
import Var
import VarSet
import TysWiredIn
import PrelNames
import PrimOp
import DynFlags
import StaticFlags
import HscTypes
import SrcLoc
import Util
import ListSetOps
import Maybes
import Outputable
import FastString

import Data.List( partition )
import Control.Monad
\end{code}

%************************************************************************
%*									*
\subsection{Main wrappers}
%*									*
%************************************************************************

\begin{code}
tcPolyExpr, tcPolyExprNC
	 :: LHsExpr Name		-- Expession to type check
       	 -> BoxySigmaType		-- Expected type (could be a polytpye)
       	 -> TcM (LHsExpr TcId)	-- Generalised expr with expected type

-- tcPolyExpr is a convenient place (frequent but not too frequent) place
-- to add context information.
-- The NC version does not do so, usually because the caller wants
-- to do so himself.

tcPolyExpr expr res_ty 	
  = addExprErrCtxt expr $
    (do {traceTc (text "tcPolyExpr") ; tcPolyExprNC expr res_ty })

tcPolyExprNC expr res_ty 
  | isSigmaTy res_ty
  = do	{ traceTc (text "tcPolyExprNC" <+> ppr res_ty)
	; (gen_fn, expr') <- tcGen res_ty emptyVarSet Nothing $ \ _ res_ty ->
			     tcPolyExprNC expr res_ty
		-- Note the recursive call to tcPolyExpr, because the
		-- type may have multiple layers of for-alls
		-- E.g. forall a. Eq a => forall b. Ord b => ....
	; return (mkLHsWrap gen_fn expr') }

  | otherwise
  = tcMonoExprNC expr res_ty

---------------
tcPolyExprs :: [LHsExpr Name] -> [TcType] -> TcM [LHsExpr TcId]
tcPolyExprs [] [] = return []
tcPolyExprs (expr:exprs) (ty:tys)
 = do 	{ expr'  <- tcPolyExpr  expr  ty
	; exprs' <- tcPolyExprs exprs tys
	; return (expr':exprs') }
tcPolyExprs exprs tys = pprPanic "tcPolyExprs" (ppr exprs $$ ppr tys)

---------------
tcMonoExpr, tcMonoExprNC 
    :: LHsExpr Name	-- Expression to type check
    -> BoxyRhoType 	-- Expected type (could be a type variable)
			-- Definitely no foralls at the top
			-- Can contain boxes, which will be filled in
    -> TcM (LHsExpr TcId)

tcMonoExpr expr res_ty
  = addErrCtxt (exprCtxt expr) $
    tcMonoExprNC expr res_ty

tcMonoExprNC (L loc expr) res_ty
  = ASSERT( not (isSigmaTy res_ty) )
    setSrcSpan loc $
    do	{ expr' <- tcExpr expr res_ty
	; return (L loc expr') }

---------------
tcInferRho, tcInferRhoNC :: LHsExpr Name -> TcM (LHsExpr TcId, TcRhoType)
tcInferRho   expr = tcInfer (tcMonoExpr expr)
tcInferRhoNC expr = tcInfer (tcMonoExprNC expr)
\end{code}


%************************************************************************
%*									*
	tcExpr: the main expression typechecker
%*									*
%************************************************************************

\begin{code}
tcExpr :: HsExpr Name -> BoxyRhoType -> TcM (HsExpr TcId)
tcExpr e res_ty | debugIsOn && isSigmaTy res_ty     -- Sanity check
       	        = pprPanic "tcExpr: sigma" (ppr res_ty $$ ppr e)

tcExpr (HsVar name)     res_ty = tcId (OccurrenceOf name) name res_ty

tcExpr (HsLit lit)	res_ty = do { let lit_ty = hsLitType lit
				    ; coi <- boxyUnify lit_ty res_ty
				    ; return $ mkHsWrapCoI coi (HsLit lit)
				    }

tcExpr (HsPar expr)     res_ty = do { expr' <- tcMonoExprNC expr res_ty
				    ; return (HsPar expr') }

tcExpr (HsSCC lbl expr) res_ty = do { expr' <- tcMonoExpr expr res_ty
				    ; return (HsSCC lbl expr') }
tcExpr (HsTickPragma info expr) res_ty 
       		     	       = do { expr' <- tcMonoExpr expr res_ty
				    ; return (HsTickPragma info expr') }

tcExpr (HsCoreAnn lbl expr) res_ty 	 -- hdaume: core annotation
  = do	{ expr' <- tcMonoExpr expr res_ty
	; return (HsCoreAnn lbl expr') }

tcExpr (HsOverLit lit) res_ty  
  = do 	{ lit' <- tcOverloadedLit (LiteralOrigin lit) lit res_ty
	; return (HsOverLit lit') }

tcExpr (NegApp expr neg_expr) res_ty
  = do	{ neg_expr' <- tcSyntaxOp NegateOrigin neg_expr
				  (mkFunTy res_ty res_ty)
	; expr' <- tcMonoExpr expr res_ty
	; return (NegApp expr' neg_expr') }

tcExpr (HsIPVar ip) res_ty
  = do	{ let origin = IPOccOrigin ip
	 	-- Implicit parameters must have a *tau-type* not a 
		-- type scheme.  We enforce this by creating a fresh
		-- type variable as its type.  (Because res_ty may not
		-- be a tau-type.)
	; ip_ty <- newFlexiTyVarTy argTypeKind	-- argTypeKind: it can't be an unboxed tuple
	; co_fn <- tcSubExp origin ip_ty res_ty
	; (ip', inst) <- newIPDict origin ip ip_ty
	; extendLIE inst
	; return (mkHsWrap co_fn (HsIPVar ip')) }

tcExpr (HsApp e1 e2) res_ty 
  = go e1 [e2]
  where
    go :: LHsExpr Name -> [LHsExpr Name] -> TcM (HsExpr TcId)
    go (L _ (HsApp e1 e2)) args = go e1 (e2:args)
    go lfun@(L loc fun) args
	= do { (fun', args') <- -- addErrCtxt (callCtxt lfun args) $
				tcApp fun (length args) (tcArgs lfun args) res_ty
	     ; traceTc (text "tcExpr args': " <+> ppr args')
	     ; return (unLoc (foldl mkHsApp (L loc fun') args')) }

tcExpr (HsLam match) res_ty
  = do	{ (co_fn, match') <- tcMatchLambda match res_ty
	; return (mkHsWrap co_fn (HsLam match')) }

tcExpr in_expr@(ExprWithTySig expr sig_ty) res_ty
 = do	{ sig_tc_ty <- tcHsSigType ExprSigCtxt sig_ty

	-- Remember to extend the lexical type-variable environment
	; (gen_fn, expr') <- tcGen sig_tc_ty emptyVarSet (Just ExprSigCtxt) $ \ skol_tvs res_ty ->
		             tcExtendTyVarEnv2 (hsExplicitTvs sig_ty `zip` mkTyVarTys skol_tvs) $
				-- See Note [More instantiated than scoped] in TcBinds
			     tcMonoExprNC expr res_ty

	; co_fn <- tcSubExp ExprSigOrigin sig_tc_ty res_ty
	; return (mkHsWrap co_fn (ExprWithTySigOut (mkLHsWrap gen_fn expr') sig_ty)) }

tcExpr (HsType ty) res_ty
  = failWithTc (text "Can't handle type argument:" <+> ppr ty)
	-- This is the syntax for type applications that I was planning
	-- but there are difficulties (e.g. what order for type args)
	-- so it's not enabled yet.
	-- Can't eliminate it altogether from the parser, because the
	-- same parser parses *patterns*.
\end{code}


%************************************************************************
%*									*
		Infix operators and sections
%*									*
%************************************************************************

\begin{code}
tcExpr in_expr@(OpApp arg1 lop@(L loc op) fix arg2) res_ty
  = do	{ (op', [arg1', arg2']) <- tcApp op 2 (tcArgs lop [arg1,arg2]) res_ty
	; return (OpApp arg1' (L loc op') fix arg2') }

-- Left sections, equivalent to
--	\ x -> e op x,
-- or
--	\ x -> op e x,
-- or, if PostfixOperators is enabled, just
-- 	op e
--
-- With PostfixOperators we don't
-- actually require the function to take two arguments
-- at all.  For example, (x `not`) means (not x);
-- you get postfix operators!  Not Haskell 98,
-- but it's less work and kind of useful.

tcExpr in_expr@(SectionL arg1 lop@(L loc op)) res_ty
  = do { dflags <- getDOpts
       ; if dopt Opt_PostfixOperators dflags
         then do { (op', [arg1']) <- tcApp op 1 (tcArgs lop [arg1]) res_ty
                 ; return (SectionL arg1' (L loc op')) }
         else do 
       { (co_fn, expr')
             <- subFunTys doc 1 res_ty Nothing $ \ [arg2_ty'] res_ty' ->
                do { (op', (arg1', co_arg2)) <- tcApp op 2 (tc_args arg2_ty') res_ty'
		   ; let coi = mkFunTyCoI arg2_ty' co_arg2 res_ty' IdCo
                   ; return (mkHsWrapCoI coi (SectionL arg1' (L loc op'))) }
       ; return (mkHsWrap co_fn expr') } }
  where
    doc = ptext (sLit "The section") <+> quotes (ppr in_expr)
		<+> ptext (sLit "takes one argument")
    tc_args arg2_ty' qtvs qtys [arg1_ty, arg2_ty] 
	= do { co_arg2 <- boxyUnify (substTyWith qtvs qtys arg2_ty) arg2_ty' 
	     ; arg1'   <- tcArg lop 1 arg1 qtvs qtys arg1_ty
	     ; qtys'   <- mapM refineBox qtys	-- c.f. tcArgs 
	     ; return (qtys', (arg1', co_arg2)) }
    tc_args _ _ _ _ = panic "tcExpr SectionL"

-- Right sections, equivalent to \ x -> x `op` expr, or
--	\ x -> op x expr
 
tcExpr in_expr@(SectionR lop@(L loc op) arg2) res_ty
  = do	{ (co_fn, expr') 
              <- subFunTys doc 1 res_ty Nothing $ \ [arg1_ty'] res_ty' ->
		 do { (op', (co_arg1, arg2')) <- tcApp op 2 (tc_args arg1_ty') res_ty'
		    ; let coi = mkFunTyCoI arg1_ty' co_arg1 res_ty' IdCo
                    ; return (mkHsWrapCoI coi $ SectionR (L loc op') arg2') }
	; return (mkHsWrap co_fn expr') }
  where
    doc = ptext (sLit "The section") <+> quotes (ppr in_expr)
		<+> ptext (sLit "takes one argument")
    tc_args arg1_ty' qtvs qtys [arg1_ty, arg2_ty] 
	= do { co_arg1 <- boxyUnify (substTyWith qtvs qtys arg1_ty) arg1_ty'
	     ; arg2'   <- tcArg lop 2 arg2 qtvs qtys arg2_ty 
	     ; qtys'   <- mapM refineBox qtys	-- c.f. tcArgs 
	     ; return (qtys', (co_arg1, arg2')) }
    tc_args arg1_ty' _ _ _ = panic "tcExpr SectionR"

-- For tuples, take care to preserve rigidity
-- E.g. 	case (x,y) of ....
-- 	   The scrutinee should have a rigid type if x,y do
-- The general scheme is the same as in tcIdApp
tcExpr in_expr@(ExplicitTuple tup_args boxity) res_ty
  = do { let kind = case boxity of { Boxed   -> liftedTypeKind
                                   ; Unboxed -> argTypeKind }
             arity = length tup_args
             tup_tc = tupleTyCon boxity arity
             mk_tup_res_ty arg_tys 
                 = mkFunTys [ty | (ty, Missing _) <- arg_tys `zip` tup_args]
                            (mkTyConApp tup_tc arg_tys)

       ; checkWiredInTyCon tup_tc       -- Ensure instances are available
       ; tvs <- newBoxyTyVars (replicate arity kind)
       ; let arg_tys1 = map mkTyVarTy tvs
       ; arg_tys2 <- preSubType tvs (mkVarSet tvs) (mk_tup_res_ty arg_tys1) res_ty
       
       ; let go (Missing _,    arg_ty) = return (Missing arg_ty)
             go (Present expr, arg_ty) = do { expr' <- tcPolyExpr expr arg_ty
			                    ; return (Present expr') }
       ; tup_args' <- mapM go (tup_args `zip` arg_tys2)
       
       ; arg_tys3 <- mapM refineBox arg_tys2
       ; co_fn <- tcSubExp TupleOrigin (mk_tup_res_ty arg_tys3) res_ty
       ; return (mkHsWrap co_fn (ExplicitTuple tup_args' boxity)) }
\end{code}

\begin{code}
tcExpr (HsLet binds expr) res_ty
  = do	{ (binds', expr') <- tcLocalBinds binds $
			     tcMonoExpr expr res_ty   
	; return (HsLet binds' expr') }

tcExpr (HsCase scrut matches) exp_ty
  = do	{  -- We used to typecheck the case alternatives first.
	   -- The case patterns tend to give good type info to use
	   -- when typechecking the scrutinee.  For example
	   --	case (map f) of
	   --	  (x:xs) -> ...
	   -- will report that map is applied to too few arguments
	   --
	   -- But now, in the GADT world, we need to typecheck the scrutinee
	   -- first, to get type info that may be refined in the case alternatives
	  (scrut', scrut_ty) <- tcInferRho scrut

	; traceTc (text "HsCase" <+> ppr scrut_ty)
	; matches' <- tcMatchesCase match_ctxt scrut_ty matches exp_ty
	; return (HsCase scrut' matches') }
 where
    match_ctxt = MC { mc_what = CaseAlt,
		      mc_body = tcBody }

tcExpr (HsIf pred b1 b2) res_ty
  = do	{ pred' <- tcMonoExpr pred boolTy
	; b1' <- tcMonoExpr b1 res_ty
	; b2' <- tcMonoExpr b2 res_ty
	; return (HsIf pred' b1' b2') }

tcExpr (HsDo do_or_lc stmts body _) res_ty
  = tcDoStmts do_or_lc stmts body res_ty

tcExpr in_expr@(ExplicitList _ exprs) res_ty
  = do 	{ (elt_ty, coi) <- boxySplitListTy res_ty
	; exprs' <- mapM (tc_elt elt_ty) exprs
	; when (null exprs) (zapToMonotype elt_ty >> return ())
		-- If there are no expressions in the comprehension
		-- we must still fill in the box
		--
		-- The GHC front end never generates an empty ExplicitList
		-- (instead it generates the [] data constructor) but
		-- Template Haskell might.  We could fix the bit of 
		-- TH that generates ExplicitList, but it seems less
		-- fragile to just handle the case here.
	; return $ mkHsWrapCoI coi (ExplicitList elt_ty exprs') }
  where
    tc_elt elt_ty expr = tcPolyExpr expr elt_ty

tcExpr in_expr@(ExplicitPArr _ exprs) res_ty	-- maybe empty
  = do	{ (elt_ty, coi) <- boxySplitPArrTy res_ty
    	; exprs' <- mapM (tc_elt elt_ty) exprs	
	; when (null exprs) (zapToMonotype elt_ty >> return ())
		-- If there are no expressions in the comprehension
		-- we must still fill in the box
		-- (Not needed for [] and () becuase they happen
		--  to parse as data constructors.)
	; return $ mkHsWrapCoI coi (ExplicitPArr elt_ty exprs') }
  where
    tc_elt elt_ty expr = tcPolyExpr expr elt_ty

tcExpr (HsProc pat cmd) res_ty
  = do	{ (pat', cmd', coi) <- tcProc pat cmd res_ty
	; return $ mkHsWrapCoI coi (HsProc pat' cmd') }

tcExpr e@(HsArrApp _ _ _ _ _) _
  = failWithTc (vcat [ptext (sLit "The arrow command"), nest 2 (ppr e), 
                      ptext (sLit "was found where an expression was expected")])

tcExpr e@(HsArrForm _ _ _) _
  = failWithTc (vcat [ptext (sLit "The arrow command"), nest 2 (ppr e), 
                      ptext (sLit "was found where an expression was expected")])
\end{code}

%************************************************************************
%*									*
		Record construction and update
%*									*
%************************************************************************

\begin{code}
tcExpr expr@(RecordCon (L loc con_name) _ rbinds) res_ty
  = do	{ data_con <- tcLookupDataCon con_name

 	-- Check for missing fields
	; checkMissingFields data_con rbinds

	; let arity = dataConSourceArity data_con
	      check_fields qtvs qtys arg_tys 
		  = do	{ let arg_tys' = substTys (zipOpenTvSubst qtvs qtys) arg_tys
			; rbinds' <- tcRecordBinds data_con arg_tys' rbinds
		 	; qtys' <- mapM refineBoxToTau qtys
			; return (qtys', rbinds') }
		-- The refineBoxToTau ensures that all the boxes in arg_tys are indeed
		-- filled, which is the invariant expected by tcIdApp
		-- How could this not be the case?  Consider a record construction
		-- that does not mention all the fields.

	; (con_expr, rbinds') <- tcIdApp con_name arity check_fields res_ty

	; return (RecordCon (L loc (dataConWrapId data_con)) con_expr rbinds') }
\end{code}

Note [Type of a record update]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The main complication with RecordUpd is that we need to explicitly
handle the *non-updated* fields.  Consider:

	data T a b c = MkT1 { fa :: a, fb :: (b,c) }
		     | MkT2 { fa :: a, fb :: (b,c), fc :: c -> c }
		     | MkT3 { fd :: a }
	
	upd :: T a b c -> (b',c) -> T a b' c
	upd t x = t { fb = x}

The result type should be (T a b' c)
not (T a b c),   because 'b' *is not* mentioned in a non-updated field
not (T a b' c'), becuase 'c' *is*     mentioned in a non-updated field
NB that it's not good enough to look at just one constructor; we must
look at them all; cf Trac #3219

After all, upd should be equivalent to:
	upd t x = case t of 
			MkT1 p q -> MkT1 p x
			MkT2 a b -> MkT2 p b
			MkT3 d   -> error ...

So we need to give a completely fresh type to the result record,
and then constrain it by the fields that are *not* updated ("p" above).
We call these the "fixed" type variables, and compute them in getFixedTyVars.

Note that because MkT3 doesn't contain all the fields being updated,
its RHS is simply an error, so it doesn't impose any type constraints.
Hence the use of 'relevant_cont'.

Note [Implict type sharing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We also take into account any "implicit" non-update fields.  For example
	data T a b where { MkT { f::a } :: T a a; ... }
So the "real" type of MkT is: forall ab. (a~b) => a -> T a b

Then consider
	upd t x = t { f=x }
We infer the type
	upd :: T a b -> a -> T a b
	upd (t::T a b) (x::a)
	   = case t of { MkT (co:a~b) (_:a) -> MkT co x }
We can't give it the more general type
	upd :: T a b -> c -> T c b

Note [Criteria for update]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to allow update for existentials etc, provided the updated
field isn't part of the existential. For example, this should be ok.
  data T a where { MkT { f1::a, f2::b->b } :: T a }
  f :: T a -> b -> T b
  f t b = t { f1=b }

The criterion we use is this:

  The types of the updated fields
  mention only the universally-quantified type variables
  of the data constructor

NB: this is not (quite) the same as being a "naughty" record selector
(See Note [Naughty record selectors]) in TcTyClsDecls), at least 
in the case of GADTs. Consider
   data T a where { MkT :: { f :: a } :: T [a] }
Then f is not "naughty" because it has a well-typed record selector.
But we don't allow updates for 'f'.  (One could consider trying to
allow this, but it makes my head hurt.  Badly.  And no one has asked
for it.)

In principle one could go further, and allow
  g :: T a -> T a
  g t = t { f2 = \x -> x }
because the expression is polymorphic...but that seems a bridge too far.

Note [Data family example]
~~~~~~~~~~~~~~~~~~~~~~~~~~
    data instance T (a,b) = MkT { x::a, y::b }
  --->
    data :TP a b = MkT { a::a, y::b }
    coTP a b :: T (a,b) ~ :TP a b

Suppose r :: T (t1,t2), e :: t3
Then  r { x=e } :: T (t3,t1)
  --->
      case r |> co1 of
	MkT x y -> MkT e y |> co2
      where co1 :: T (t1,t2) ~ :TP t1 t2
	    co2 :: :TP t3 t2 ~ T (t3,t2)
The wrapping with co2 is done by the constructor wrapper for MkT

Outgoing invariants
~~~~~~~~~~~~~~~~~~~
In the outgoing (HsRecordUpd scrut binds cons in_inst_tys out_inst_tys):

  * cons are the data constructors to be updated

  * in_inst_tys, out_inst_tys have same length, and instantiate the
	*representation* tycon of the data cons.  In Note [Data 
	family example], in_inst_tys = [t1,t2], out_inst_tys = [t3,t2]
	
\begin{code}
tcExpr expr@(RecordUpd record_expr rbinds _ _ _) res_ty
  = ASSERT( notNull upd_fld_names )
    do	{
	-- STEP 0
	-- Check that the field names are really field names
	; sel_ids <- mapM tcLookupField upd_fld_names
			-- The renamer has already checked that
			-- selectors are all in scope
	; let bad_guys = [ setSrcSpan loc $ addErrTc (notSelector fld_name) 
		       	 | (fld, sel_id) <- rec_flds rbinds `zip` sel_ids,
		       	   not (isRecordSelector sel_id), 	-- Excludes class ops
		       	   let L loc fld_name = hsRecFieldId fld ]
	; unless (null bad_guys) (sequence bad_guys >> failM)
    
	-- STEP 1
	-- Figure out the tycon and data cons from the first field name
	; let	-- It's OK to use the non-tc splitters here (for a selector)
	      sel_id : _  = sel_ids
	      (tycon, _)  = recordSelectorFieldLabel sel_id	-- We've failed already if
	      data_cons   = tyConDataCons tycon			-- it's not a field label
	      	-- NB: for a data type family, the tycon is the instance tycon

	      relevant_cons   = filter is_relevant data_cons
	      is_relevant con = all (`elem` dataConFieldLabels con) upd_fld_names
		-- A constructor is only relevant to this process if
		-- it contains *all* the fields that are being updated
		-- Other ones will cause a runtime error if they occur

		-- Take apart a representative constructor
	      con1 = ASSERT( not (null relevant_cons) ) head relevant_cons
	      (con1_tvs, _, _, _, _, con1_arg_tys, _) = dataConFullSig con1
	      con1_flds = dataConFieldLabels con1
	      con1_res_ty = mkFamilyTyConApp tycon (mkTyVarTys con1_tvs)
   	      
	-- Step 2
	-- Check that at least one constructor has all the named fields
	-- i.e. has an empty set of bad fields returned by badFields
	; checkTc (not (null relevant_cons)) (badFieldsUpd rbinds)

	-- STEP 3    Note [Criteria for update]
	-- Check that each updated field is polymorphic; that is, its type
	-- mentions only the universally-quantified variables of the data con
	; let flds1_w_tys = zipEqual "tcExpr:RecConUpd" con1_flds con1_arg_tys
	      (upd_flds1_w_tys, fixed_flds1_w_tys) = partition is_updated flds1_w_tys
	      is_updated (fld,ty) = fld `elem` upd_fld_names

	      bad_upd_flds = filter bad_fld upd_flds1_w_tys
	      con1_tv_set = mkVarSet con1_tvs
	      bad_fld (fld, ty) = fld `elem` upd_fld_names &&
				      not (tyVarsOfType ty `subVarSet` con1_tv_set)
	; checkTc (null bad_upd_flds) (badFieldTypes bad_upd_flds)

	-- STEP 4  Note [Type of a record update]
	-- Figure out types for the scrutinee and result
	-- Both are of form (T a b c), with fresh type variables, but with
	-- common variables where the scrutinee and result must have the same type
	-- These are variables that appear in *any* arg of *any* of the
	-- relevant constructors *except* in the updated fields
	-- 
	; let fixed_tvs = getFixedTyVars con1_tvs relevant_cons
	      is_fixed_tv tv = tv `elemVarSet` fixed_tvs
	      mk_inst_ty tv result_inst_ty 
	        | is_fixed_tv tv = return result_inst_ty	    -- Same as result type
	        | otherwise      = newFlexiTyVarTy (tyVarKind tv)  -- Fresh type, of correct kind

	; (_, result_inst_tys, result_inst_env) <- tcInstTyVars con1_tvs
	; scrut_inst_tys <- zipWithM mk_inst_ty con1_tvs result_inst_tys

	; let result_ty     = substTy result_inst_env con1_res_ty
	      con1_arg_tys' = map (substTy result_inst_env) con1_arg_tys
	      scrut_subst   = zipTopTvSubst con1_tvs scrut_inst_tys
	      scrut_ty      = substTy scrut_subst con1_res_ty

	-- STEP 5
	-- Typecheck the thing to be updated, and the bindings
	; record_expr' <- tcMonoExpr record_expr scrut_ty
	; rbinds'      <- tcRecordBinds con1 con1_arg_tys' rbinds
	
	; let origin = RecordUpdOrigin
	; co_fn <- tcSubExp origin result_ty res_ty

	-- STEP 6: Deal with the stupid theta
	; let theta' = substTheta scrut_subst (dataConStupidTheta con1)
	; instStupidTheta origin theta'

	-- Step 7: make a cast for the scrutinee, in the case that it's from a type family
	; let scrut_co | Just co_con <- tyConFamilyCoercion_maybe tycon 
		       = WpCast $ mkTyConApp co_con scrut_inst_tys
		       | otherwise
		       = idHsWrapper

	-- Phew!
	; return (mkHsWrap co_fn (RecordUpd (mkLHsWrap scrut_co record_expr') rbinds'
				        relevant_cons scrut_inst_tys result_inst_tys)) }
  where
    upd_fld_names = hsRecFields rbinds

    getFixedTyVars :: [TyVar] -> [DataCon] -> TyVarSet
    -- These tyvars must not change across the updates
    getFixedTyVars tvs1 cons
      = mkVarSet [tv1 | con <- cons
    		      , let (tvs, theta, arg_tys, _) = dataConSig con
		      	    flds = dataConFieldLabels con
    			    fixed_tvs = exactTyVarsOfTypes fixed_tys
			    	    -- fixed_tys: See Note [Type of a record update]
			    	        `unionVarSet` tyVarsOfTheta theta 
				    -- Universally-quantified tyvars that
				    -- appear in any of the *implicit*
				    -- arguments to the constructor are fixed
			    	    -- See Note [Implict type sharing]
			    	        
		            fixed_tys = [ty | (fld,ty) <- zip flds arg_tys
                                            , not (fld `elem` upd_fld_names)]
                      , (tv1,tv) <- tvs1 `zip` tvs	-- Discards existentials in tvs
        	      , tv `elemVarSet` fixed_tvs ]
\end{code}

%************************************************************************
%*									*
	Arithmetic sequences			e.g. [a,b..]
	and their parallel-array counterparts	e.g. [: a,b.. :]
		
%*									*
%************************************************************************

\begin{code}
tcExpr (ArithSeq _ seq@(From expr)) res_ty
  = do	{ (elt_ty, coi) <- boxySplitListTy res_ty
	; expr' <- tcPolyExpr expr elt_ty
	; enum_from <- newMethodFromName (ArithSeqOrigin seq) 
			      elt_ty enumFromName
	; return $ mkHsWrapCoI coi (ArithSeq (HsVar enum_from) (From expr')) }

tcExpr in_expr@(ArithSeq _ seq@(FromThen expr1 expr2)) res_ty
  = do	{ (elt_ty, coi) <- boxySplitListTy res_ty
	; expr1' <- tcPolyExpr expr1 elt_ty
	; expr2' <- tcPolyExpr expr2 elt_ty
	; enum_from_then <- newMethodFromName (ArithSeqOrigin seq) 
			      elt_ty enumFromThenName
	; return $ mkHsWrapCoI coi 
                    (ArithSeq (HsVar enum_from_then) (FromThen expr1' expr2')) }

tcExpr in_expr@(ArithSeq _ seq@(FromTo expr1 expr2)) res_ty
  = do	{ (elt_ty, coi) <- boxySplitListTy res_ty
	; expr1' <- tcPolyExpr expr1 elt_ty
	; expr2' <- tcPolyExpr expr2 elt_ty
	; enum_from_to <- newMethodFromName (ArithSeqOrigin seq) 
		  	      elt_ty enumFromToName
	; return $ mkHsWrapCoI coi 
                     (ArithSeq (HsVar enum_from_to) (FromTo expr1' expr2')) }

tcExpr in_expr@(ArithSeq _ seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = do	{ (elt_ty, coi) <- boxySplitListTy res_ty
	; expr1' <- tcPolyExpr expr1 elt_ty
	; expr2' <- tcPolyExpr expr2 elt_ty
	; expr3' <- tcPolyExpr expr3 elt_ty
	; eft <- newMethodFromName (ArithSeqOrigin seq) 
		      elt_ty enumFromThenToName
	; return $ mkHsWrapCoI coi 
                     (ArithSeq (HsVar eft) (FromThenTo expr1' expr2' expr3')) }

tcExpr in_expr@(PArrSeq _ seq@(FromTo expr1 expr2)) res_ty
  = do	{ (elt_ty, coi) <- boxySplitPArrTy res_ty
	; expr1' <- tcPolyExpr expr1 elt_ty
	; expr2' <- tcPolyExpr expr2 elt_ty
	; enum_from_to <- newMethodFromName (PArrSeqOrigin seq) 
				      elt_ty enumFromToPName
	; return $ mkHsWrapCoI coi 
                     (PArrSeq (HsVar enum_from_to) (FromTo expr1' expr2')) }

tcExpr in_expr@(PArrSeq _ seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = do	{ (elt_ty, coi) <- boxySplitPArrTy res_ty
	; expr1' <- tcPolyExpr expr1 elt_ty
	; expr2' <- tcPolyExpr expr2 elt_ty
	; expr3' <- tcPolyExpr expr3 elt_ty
	; eft <- newMethodFromName (PArrSeqOrigin seq)
		      elt_ty enumFromThenToPName
	; return $ mkHsWrapCoI coi 
                     (PArrSeq (HsVar eft) (FromThenTo expr1' expr2' expr3')) }

tcExpr (PArrSeq _ _) _ 
  = panic "TcExpr.tcMonoExpr: Infinite parallel array!"
    -- the parser shouldn't have generated it and the renamer shouldn't have
    -- let it through
\end{code}


%************************************************************************
%*									*
		Template Haskell
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI	/* Only if bootstrapped */
	-- Rename excludes these cases otherwise
tcExpr (HsSpliceE splice) res_ty = tcSpliceExpr splice res_ty
tcExpr (HsBracket brack)  res_ty = do	{ e <- tcBracket brack res_ty
					; return (unLoc e) }
tcExpr e@(HsQuasiQuoteE _) res_ty =
    pprPanic "Should never see HsQuasiQuoteE in type checker" (ppr e)
#endif /* GHCI */
\end{code}


%************************************************************************
%*									*
		Catch-all
%*									*
%************************************************************************

\begin{code}
tcExpr other _ = pprPanic "tcMonoExpr" (ppr other)
\end{code}


%************************************************************************
%*									*
		Applications
%*									*
%************************************************************************

\begin{code}
---------------------------
tcApp :: HsExpr Name				-- Function
      -> Arity					-- Number of args reqd
      -> ArgChecker results
      -> BoxyRhoType				-- Result type
      -> TcM (HsExpr TcId, results)		

-- (tcFun fun n_args arg_checker res_ty)
-- The argument type checker, arg_checker, will be passed exactly n_args types

tcApp (HsVar fun_name) n_args arg_checker res_ty
  = tcIdApp fun_name n_args arg_checker res_ty

tcApp fun n_args arg_checker res_ty	-- The vanilla case (rula APP)
  = do	{ arg_boxes  <- newBoxyTyVars (replicate n_args argTypeKind)
	; fun'       <- tcExpr fun (mkFunTys (mkTyVarTys arg_boxes) res_ty)
	; arg_tys'   <- mapM readFilledBox arg_boxes
	; (_, args') <- arg_checker [] [] arg_tys'	-- Yuk
	; return (fun', args') }

---------------------------
tcIdApp :: Name					-- Function
        -> Arity				-- Number of args reqd
        -> ArgChecker results	-- The arg-checker guarantees to fill all boxes in the arg types
        -> BoxyRhoType				-- Result type
        -> TcM (HsExpr TcId, results)		

-- Call 	(f e1 ... en) :: res_ty
-- Type		f :: forall a b c. theta => fa_1 -> ... -> fa_k -> fres
-- 			(where k <= n; fres has the rest)
-- NB:	if k < n then the function doesn't have enough args, and
--	presumably fres is a type variable that we are going to 
--	instantiate with a function type
--
-- Then		fres <= bx_(k+1) -> ... -> bx_n -> res_ty

tcIdApp fun_name n_args arg_checker res_ty
  = do	{ let orig = OccurrenceOf fun_name
	; (fun, fun_ty) <- lookupFun orig fun_name

	-- Split up the function type
	; let (tv_theta_prs, rho) = tcMultiSplitSigmaTy fun_ty
	      (fun_arg_tys, fun_res_ty) = tcSplitFunTysN rho n_args

	      qtvs = concatMap fst tv_theta_prs		-- Quantified tyvars
	      arg_qtvs = exactTyVarsOfTypes fun_arg_tys
	      res_qtvs = exactTyVarsOfType fun_res_ty
		-- NB: exactTyVarsOfType.  See Note [Silly type synonyms in smart-app]
	      tau_qtvs = arg_qtvs `unionVarSet` res_qtvs
	      k  	     = length fun_arg_tys	-- k <= n_args
	      n_missing_args = n_args - k		-- Always >= 0

	-- Match the result type of the function with the
	-- result type of the context, to get an inital substitution
	; extra_arg_boxes <- newBoxyTyVars (replicate n_missing_args argTypeKind)
	; let extra_arg_tys' = mkTyVarTys extra_arg_boxes
	      res_ty' 	     = mkFunTys extra_arg_tys' res_ty
	; qtys' <- preSubType qtvs tau_qtvs fun_res_ty res_ty'

	-- Typecheck the arguments!
	-- Doing so will fill arg_qtvs and extra_arg_tys'
	; (qtys'', args') <- arg_checker qtvs qtys' (fun_arg_tys ++ extra_arg_tys')

	-- Strip boxes from the qtvs that have been filled in by the arg checking
	; extra_arg_tys'' <- mapM readFilledBox extra_arg_boxes

	-- Result subsumption
	-- This fills in res_qtvs
	; let res_subst = zipOpenTvSubst qtvs qtys''
	      fun_res_ty'' = substTy res_subst fun_res_ty
	      res_ty'' = mkFunTys extra_arg_tys'' res_ty
	; co_fn <- tcSubExp orig fun_res_ty'' res_ty''
			    
	-- And pack up the results
	-- By applying the coercion just to the *function* we can make
	-- tcFun work nicely for OpApp and Sections too
	; fun' <- instFun orig fun res_subst tv_theta_prs
	; co_fn' <- wrapFunResCoercion (substTys res_subst fun_arg_tys) co_fn
	; traceTc (text "tcIdApp: " <+> ppr (mkHsWrap co_fn' fun') <+> ppr tv_theta_prs <+> ppr co_fn' <+> ppr fun')
	; return (mkHsWrap co_fn' fun', args') }
\end{code}

Note [Silly type synonyms in smart-app]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we call sripBoxyType, all of the boxes should be filled
in.  But we need to be careful about type synonyms:
	type T a = Int
	f :: T a -> Int
	...(f x)...
In the call (f x) we'll typecheck x, expecting it to have type
(T box).  Usually that would fill in the box, but in this case not;
because 'a' is discarded by the silly type synonym T.  So we must
use exactTyVarsOfType to figure out which type variables are free 
in the argument type.

\begin{code}
-- tcId is a specialisation of tcIdApp when there are no arguments
-- tcId f ty = do { (res, _) <- tcIdApp f [] (\[] -> return ()) ty
--		  ; return res }

tcId :: InstOrigin
     -> Name					-- Function
     -> BoxyRhoType				-- Result type
     -> TcM (HsExpr TcId)
tcId orig fun_name res_ty
  = do	{ traceTc (text "tcId" <+> ppr fun_name <+> ppr res_ty)
	; (fun, fun_ty) <- lookupFun orig fun_name

	-- Split up the function type
	; let (tv_theta_prs, fun_tau) = tcMultiSplitSigmaTy fun_ty
	      qtvs = concatMap fst tv_theta_prs	-- Quantified tyvars
	      tau_qtvs = exactTyVarsOfType fun_tau	-- Mentioned in the tau part
	; qtv_tys <- preSubType qtvs tau_qtvs fun_tau res_ty

	-- Do the subsumption check wrt the result type
	; let res_subst = zipTopTvSubst qtvs qtv_tys
	      fun_tau'  = substTy res_subst fun_tau

	; co_fn <- tcSubExp orig fun_tau' res_ty

	-- And pack up the results
	; fun' <- instFun orig fun res_subst tv_theta_prs 
	; traceTc (text "tcId yields" <+> ppr (mkHsWrap co_fn fun'))
	; return (mkHsWrap co_fn fun') }

-- 	Note [Push result type in]
--
-- Unify with expected result before (was: after) type-checking the args
-- so that the info from res_ty (was: args) percolates to args (was actual_res_ty).
-- This is when we might detect a too-few args situation.
-- (One can think of cases when the opposite order would give
-- a better error message.)
-- [March 2003: I'm experimenting with putting this first.  Here's an 
--		example where it actually makes a real difference
--    class C t a b | t a -> b
--    instance C Char a Bool
--
--    data P t a = forall b. (C t a b) => MkP b
--    data Q t   = MkQ (forall a. P t a)

--    f1, f2 :: Q Char;
--    f1 = MkQ (MkP True)
--    f2 = MkQ (MkP True :: forall a. P Char a)
--
-- With the change, f1 will type-check, because the 'Char' info from
-- the signature is propagated into MkQ's argument. With the check
-- in the other order, the extra signature in f2 is reqd.]

---------------------------
tcSyntaxOp :: InstOrigin -> HsExpr Name -> TcType -> TcM (HsExpr TcId)
-- Typecheck a syntax operator, checking that it has the specified type
-- The operator is always a variable at this stage (i.e. renamer output)
-- This version assumes ty is a monotype
tcSyntaxOp orig (HsVar op) ty = tcId orig op ty
tcSyntaxOp orig other 	   ty = pprPanic "tcSyntaxOp" (ppr other) 
                        
---------------------------
instFun :: InstOrigin
	-> HsExpr TcId
	-> TvSubst		  -- The instantiating substitution
	-> [([TyVar], ThetaType)] -- Stuff to instantiate
	-> TcM (HsExpr TcId)	

instFun orig fun subst []
  = return fun		-- Common short cut

instFun orig fun subst tv_theta_prs
  = do 	{ let ty_theta_prs' = map subst_pr tv_theta_prs
	; traceTc (text "instFun" <+> ppr ty_theta_prs')
                -- Make two ad-hoc checks 
	; doStupidChecks fun ty_theta_prs'

		-- Now do normal instantiation
        ; method_sharing <- doptM Opt_MethodSharing
	; result <- go method_sharing True fun ty_theta_prs' 
	; traceTc (text "instFun result" <+> ppr result)
	; return result
	}
  where
    subst_pr (tvs, theta) 
	= (substTyVars subst tvs, substTheta subst theta)

    go _ _ fun [] = do {traceTc (text "go _ _ fun [] returns" <+> ppr fun) ; return fun }

    go method_sharing True (HsVar fun_id) ((tys,theta) : prs)
	| want_method_inst method_sharing theta
	= do { traceTc (text "go (HsVar fun_id) ((tys,theta) : prs) | want_method_inst theta")
	     ; meth_id <- newMethodWithGivenTy orig fun_id tys
	     ; go method_sharing False (HsVar meth_id) prs }
		-- Go round with 'False' to prevent further use
		-- of newMethod: see Note [Multiple instantiation]

    go method_sharing _ fun ((tys, theta) : prs)
	= do { co_fn <- instCall orig tys theta
	     ; traceTc (text "go yields co_fn" <+> ppr co_fn)
	     ; go method_sharing False (HsWrap co_fn fun) prs }

	-- See Note [No method sharing]
    want_method_inst method_sharing theta =  not (null theta)	-- Overloaded
		   	                  && method_sharing
\end{code}

Note [Multiple instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are careful never to make a MethodInst that has, as its meth_id, another MethodInst.
For example, consider
	f :: forall a. Eq a => forall b. Ord b => a -> b
At a call to f, at say [Int, Bool], it's tempting to translate the call to 

	f_m1
  where
	f_m1 :: forall b. Ord b => Int -> b
	f_m1 = f Int dEqInt

	f_m2 :: Int -> Bool
	f_m2 = f_m1 Bool dOrdBool

But notice that f_m2 has f_m1 as its meth_id.  Now the danger is that if we do
a tcSimplCheck with a Given f_mx :: f Int dEqInt, we may make a binding
	f_m1 = f_mx
But it's entirely possible that f_m2 will continue to float out, because it
mentions no type variables.  Result, f_m1 isn't in scope.

Here's a concrete example that does this (test tc200):

    class C a where
      f :: Eq b => b -> a -> Int
      baz :: Eq a => Int -> a -> Int

    instance C Int where
      baz = f

Current solution: only do the "method sharing" thing for the first type/dict
application, not for the iterated ones.  A horribly subtle point.

Note [No method sharing]
~~~~~~~~~~~~~~~~~~~~~~~~
The -fno-method-sharing flag controls what happens so far as the LIE
is concerned.  The default case is that for an overloaded function we 
generate a "method" Id, and add the Method Inst to the LIE.  So you get
something like
	f :: Num a => a -> a
	f = /\a (d:Num a) -> let m = (+) a d in \ (x:a) -> m x x
If you specify -fno-method-sharing, the dictionary application 
isn't shared, so we get
	f :: Num a => a -> a
	f = /\a (d:Num a) (x:a) -> (+) a d x x
This gets a bit less sharing, but
	a) it's better for RULEs involving overloaded functions
	b) perhaps fewer separated lambdas

Note [Left to right]
~~~~~~~~~~~~~~~~~~~~
tcArgs implements a left-to-right order, which goes beyond what is described in the
impredicative type inference paper.  In particular, it allows
	runST $ foo
where runST :: (forall s. ST s a) -> a
When typechecking the application of ($)::(a->b) -> a -> b, we first check that
runST has type (a->b), thereby filling in a=forall s. ST s a.  Then we un-box this type
before checking foo.  The left-to-right order really helps here.

\begin{code}
tcArgs :: LHsExpr Name				-- The function (for error messages)
       -> [LHsExpr Name] 			-- Actual args
       -> ArgChecker [LHsExpr TcId]

type ArgChecker results
   = [TyVar] -> [TcSigmaType] 		-- Current instantiation
   -> [TcSigmaType]			-- Expected arg types (**before** applying the instantiation)
   -> TcM ([TcSigmaType], results)	-- Resulting instantiation and args

tcArgs fun args qtvs qtys arg_tys
  = go 1 qtys args arg_tys
  where
    go n qtys [] [] = return (qtys, [])
    go n qtys (arg:args) (arg_ty:arg_tys)
	= do { arg' <- tcArg fun n arg qtvs qtys arg_ty
	     ; qtys' <- mapM refineBox qtys	-- Exploit new info
	     ; (qtys'', args') <- go (n+1) qtys' args arg_tys
	     ; return (qtys'', arg':args') }
    go n qtys args arg_tys = panic "tcArgs"

tcArg :: LHsExpr Name				-- The function
      -> Int					--   and arg number (for error messages)
      -> LHsExpr Name
      -> [TyVar] -> [TcSigmaType] 		-- Instantiate the arg type like this
      -> BoxySigmaType
      -> TcM (LHsExpr TcId)			-- Resulting argument
tcArg fun arg_no arg qtvs qtys ty
  = addErrCtxt (funAppCtxt fun arg arg_no) $
    tcPolyExprNC arg (substTyWith qtvs qtys ty)
\end{code}


Note [tagToEnum#]
~~~~~~~~~~~~~~~~~
Nasty check to ensure that tagToEnum# is applied to a type that is an
enumeration TyCon.  Unification may refine the type later, but this
check won't see that, alas.  It's crude but it works.

Here's are two cases that should fail
 	f :: forall a. a
	f = tagToEnum# 0	-- Can't do tagToEnum# at a type variable

	g :: Int
	g = tagToEnum# 0	-- Int is not an enumeration


\begin{code}
doStupidChecks :: HsExpr TcId
	       -> [([TcType], ThetaType)]
	       -> TcM ()
-- Check two tiresome and ad-hoc cases
-- (a) the "stupid theta" for a data con; add the constraints
--     from the "stupid theta" of a data constructor (sigh)
-- (b) deal with the tagToEnum# problem: see Note [tagToEnum#]

doStupidChecks (HsVar fun_id) ((tys,_):_)
  | Just con <- isDataConId_maybe fun_id   -- (a)
  = addDataConStupidTheta con tys

  | fun_id `hasKey` tagToEnumKey           -- (b)
  = do	{ tys' <- zonkTcTypes tys
	; checkTc (ok tys') (tagToEnumError tys')
	}
  where
    ok [] 	= False
    ok (ty:tys) = case tcSplitTyConApp_maybe ty of
			Just (tc,_) -> isEnumerationTyCon tc
			Nothing     -> False

doStupidChecks fun tv_theta_prs
  = return () -- The common case
				      

tagToEnumError tys
  = hang (ptext (sLit "Bad call to tagToEnum#") <+> at_type)
	 2 (vcat [ptext (sLit "Specify the type by giving a type signature"),
		  ptext (sLit "e.g. (tagToEnum# x) :: Bool")])
  where
    at_type | null tys = empty	-- Probably never happens
	    | otherwise = ptext (sLit "at type") <+> ppr (head tys)
\end{code}

%************************************************************************
%*									*
\subsection{@tcId@ typechecks an identifier occurrence}
%*									*
%************************************************************************

\begin{code}
lookupFun :: InstOrigin -> Name -> TcM (HsExpr TcId, TcType)
lookupFun orig id_name
  = do 	{ thing <- tcLookup id_name
	; case thing of
    	    AGlobal (ADataCon con) -> return (HsVar wrap_id, idType wrap_id)
				   where
				      wrap_id = dataConWrapId con

    	    AGlobal (AnId id) 
	    	| isNaughtyRecordSelector id -> failWithTc (naughtyRecordSel id)
	    	| otherwise		     -> return (HsVar id, idType id)
	    	-- A global cannot possibly be ill-staged
	    	-- nor does it need the 'lifting' treatment

    	    ATcId { tct_id = id, tct_type = ty, tct_co = mb_co, tct_level = lvl }
	    	| isNaughtyRecordSelector id -> failWithTc (naughtyRecordSel id)
		  			  -- Note [Local record selectors]
		| otherwise
		-> do { thLocalId orig id ty lvl
		      ; case mb_co of
			  Unrefineable    -> return (HsVar id, ty)
			  Rigid co        -> return (mkHsWrap co (HsVar id), ty) 	
			  Wobbly 	  -> traceTc (text "lookupFun" <+> ppr id) >> return (HsVar id, ty)	-- Wobbly, or no free vars
			  WobblyInvisible -> failWithTc (ppr id_name <+> ptext (sLit " not in scope because it has a wobbly type (solution: add a type annotation)"))
		      }

    	    other -> failWithTc (ppr other <+> ptext (sLit "used where a value identifer was expected"))
    }

#ifndef GHCI  /* GHCI and TH is off */
--------------------------------------
thLocalId :: InstOrigin -> Id -> TcType -> ThLevel -> TcM ()
-- Check for cross-stage lifting
thLocalId orig id id_ty bind_lvl
  = return ()

#else	      /* GHCI and TH is on */
thLocalId orig id id_ty bind_lvl 
  = do	{ use_stage <- getStage	-- TH case
	; let use_lvl = thLevel use_stage
	; checkWellStaged (quotes (ppr id)) bind_lvl use_lvl
	; traceTc (text "thLocalId" <+> ppr id <+> ppr bind_lvl <+> ppr use_stage <+> ppr use_lvl)
	; when (use_lvl > bind_lvl) $
          checkCrossStageLifting orig id id_ty bind_lvl use_stage }

--------------------------------------
checkCrossStageLifting :: InstOrigin -> Id -> TcType -> ThLevel -> ThStage -> TcM ()
-- We are inside brackets, and (use_lvl > bind_lvl)
-- Now we must check whether there's a cross-stage lift to do
-- Examples   \x -> [| x |]  
--            [| map |]

checkCrossStageLifting _ _ _ _ Comp   = return ()
checkCrossStageLifting _ _ _ _ Splice = return ()

checkCrossStageLifting orig id id_ty bind_lvl (Brack _ ps_var lie_var) 
  | thTopLevelId id
  =	-- Top-level identifiers in this module,
	-- (which have External Names)
	-- are just like the imported case:
	-- no need for the 'lifting' treatment
	-- E.g.  this is fine:
	--   f x = x
	--   g y = [| f 3 |]
	-- But we do need to put f into the keep-alive
	-- set, because after desugaring the code will
	-- only mention f's *name*, not f itself.
    keepAliveTc id

  | otherwise	-- bind_lvl = outerLevel presumably,
		-- but the Id is not bound at top level
  = 	-- Nested identifiers, such as 'x' in
	-- E.g. \x -> [| h x |]
	-- We must behave as if the reference to x was
	--	h $(lift x)	
	-- We use 'x' itself as the splice proxy, used by 
	-- the desugarer to stitch it all back together.
	-- If 'x' occurs many times we may get many identical
	-- bindings of the same splice proxy, but that doesn't
	-- matter, although it's a mite untidy.
    do 	{ checkTc (isTauTy id_ty) (polySpliceErr id)
	       -- If x is polymorphic, its occurrence sites might
	       -- have different instantiations, so we can't use plain
	       -- 'x' as the splice proxy name.  I don't know how to 
	       -- solve this, and it's probably unimportant, so I'm
	       -- just going to flag an error for now
   
	; id_ty' <- zapToMonotype id_ty
		-- The id_ty might have an OpenTypeKind, but we
		-- can't instantiate the Lift class at that kind,
		-- so we zap it to a LiftedTypeKind monotype
		-- C.f. the call in TcPat.newLitInst

	; lift <- if isStringTy id_ty' then
	       	     tcLookupId DsMeta.liftStringName
		     -- See Note [Lifting strings]
	          else
                     setLIEVar lie_var	$ do  -- Put the 'lift' constraint into the right LIE
                     newMethodFromName orig id_ty' DsMeta.liftName
	   
		   -- Update the pending splices
	; ps <- readMutVar ps_var
	; writeMutVar ps_var ((idName id, nlHsApp (nlHsVar lift) (nlHsVar id)) : ps)

	; return () }
#endif /* GHCI */
\end{code}

Note [Lifting strings]
~~~~~~~~~~~~~~~~~~~~~~
If we see $(... [| s |] ...) where s::String, we don't want to
generate a mass of Cons (CharL 'x') (Cons (CharL 'y') ...)) etc.
So this conditional short-circuits the lifting mechanism to generate
(liftString "xy") in that case.  I didn't want to use overlapping instances
for the Lift class in TH.Syntax, because that can lead to overlapping-instance
errors in a polymorphic situation.  

If this check fails (which isn't impossible) we get another chance; see
Note [Converting strings] in Convert.lhs 

Local record selectors
~~~~~~~~~~~~~~~~~~~~~~
Record selectors for TyCons in this module are ordinary local bindings,
which show up as ATcIds rather than AGlobals.  So we need to check for
naughtiness in both branches.  c.f. TcTyClsBindings.mkAuxBinds.


%************************************************************************
%*									*
\subsection{Record bindings}
%*									*
%************************************************************************

Game plan for record bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. Find the TyCon for the bindings, from the first field label.

2. Instantiate its tyvars and unify (T a1 .. an) with expected_ty.

For each binding field = value

3. Instantiate the field type (from the field label) using the type
   envt from step 2.

4  Type check the value using tcArg, passing the field type as 
   the expected argument type.

This extends OK when the field types are universally quantified.

	
\begin{code}
tcRecordBinds
	:: DataCon
	-> [TcType]	-- Expected type for each field
	-> HsRecordBinds Name
	-> TcM (HsRecordBinds TcId)

tcRecordBinds data_con arg_tys (HsRecFields rbinds dd)
  = do	{ mb_binds <- mapM do_bind rbinds
	; return (HsRecFields (catMaybes mb_binds) dd) }
  where
    flds_w_tys = zipEqual "tcRecordBinds" (dataConFieldLabels data_con) arg_tys
    do_bind fld@(HsRecField { hsRecFieldId = L loc field_lbl, hsRecFieldArg = rhs })
      | Just field_ty <- assocMaybe flds_w_tys field_lbl
      = addErrCtxt (fieldCtxt field_lbl)	$
	do { rhs' <- tcPolyExprNC rhs field_ty
	   ; let field_id = mkUserLocal (nameOccName field_lbl)
				        (nameUnique field_lbl)
					field_ty loc 
		-- Yuk: the field_id has the *unique* of the selector Id
		-- 	    (so we can find it easily)
		--      but is a LocalId with the appropriate type of the RHS
		--	    (so the desugarer knows the type of local binder to make)
	   ; return (Just (fld { hsRecFieldId = L loc field_id, hsRecFieldArg = rhs' })) }
      | otherwise
      = do { addErrTc (badFieldCon data_con field_lbl)
	   ; return Nothing }

checkMissingFields :: DataCon -> HsRecordBinds Name -> TcM ()
checkMissingFields data_con rbinds
  | null field_labels 	-- Not declared as a record;
			-- But C{} is still valid if no strict fields
  = if any isMarkedStrict field_strs then
	-- Illegal if any arg is strict
	addErrTc (missingStrictFields data_con [])
    else
	return ()
			
  | otherwise = do		-- A record
    unless (null missing_s_fields)
	   (addErrTc (missingStrictFields data_con missing_s_fields))

    warn <- doptM Opt_WarnMissingFields
    unless (not (warn && notNull missing_ns_fields))
	   (warnTc True (missingFields data_con missing_ns_fields))

  where
    missing_s_fields
	= [ fl | (fl, str) <- field_info,
	  	 isMarkedStrict str,
	  	 not (fl `elem` field_names_used)
	  ]
    missing_ns_fields
	= [ fl | (fl, str) <- field_info,
	  	 not (isMarkedStrict str),
	  	 not (fl `elem` field_names_used)
	  ]

    field_names_used = hsRecFields rbinds
    field_labels     = dataConFieldLabels data_con

    field_info = zipEqual "missingFields"
			  field_labels
	  		  field_strs

    field_strs = dataConStrictMarks data_con
\end{code}

%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

Boring and alphabetical:
\begin{code}
addExprErrCtxt :: OutputableBndr id => LHsExpr id -> TcM a -> TcM a
addExprErrCtxt expr = addErrCtxt (exprCtxt (unLoc expr))

exprCtxt expr
  = hang (ptext (sLit "In the expression:")) 4 (ppr expr)

fieldCtxt field_name
  = ptext (sLit "In the") <+> quotes (ppr field_name) <+> ptext (sLit "field of a record")

funAppCtxt fun arg arg_no
  = hang (hsep [ ptext (sLit "In the"), speakNth arg_no, ptext (sLit "argument of"), 
		    quotes (ppr fun) <> text ", namely"])
	 4 (quotes (ppr arg))

badFieldTypes prs
  = hang (ptext (sLit "Record update for insufficiently polymorphic field")
			 <> plural prs <> colon)
       2 (vcat [ ppr f <+> dcolon <+> ppr ty | (f,ty) <- prs ])

badFieldsUpd rbinds
  = hang (ptext (sLit "No constructor has all these fields:"))
	 4 (pprQuotedList (hsRecFields rbinds))

naughtyRecordSel sel_id
  = ptext (sLit "Cannot use record selector") <+> quotes (ppr sel_id) <+> 
    ptext (sLit "as a function due to escaped type variables") $$ 
    ptext (sLit "Probable fix: use pattern-matching syntax instead")

notSelector field
  = hsep [quotes (ppr field), ptext (sLit "is not a record selector")]

missingStrictFields :: DataCon -> [FieldLabel] -> SDoc
missingStrictFields con fields
  = header <> rest
  where
    rest | null fields = empty	-- Happens for non-record constructors 
				-- with strict fields
	 | otherwise   = colon <+> pprWithCommas ppr fields

    header = ptext (sLit "Constructor") <+> quotes (ppr con) <+> 
	     ptext (sLit "does not have the required strict field(s)") 
	  
missingFields :: DataCon -> [FieldLabel] -> SDoc
missingFields con fields
  = ptext (sLit "Fields of") <+> quotes (ppr con) <+> ptext (sLit "not initialised:") 
	<+> pprWithCommas ppr fields

-- callCtxt fun args = ptext (sLit "In the call") <+> parens (ppr (foldl mkHsApp fun args))

#ifdef GHCI
polySpliceErr :: Id -> SDoc
polySpliceErr id
  = ptext (sLit "Can't splice the polymorphic local variable") <+> quotes (ppr id)
#endif
\end{code}
