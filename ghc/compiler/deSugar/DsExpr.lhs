%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsExpr]{Matching expressions (Exprs)}

\begin{code}
module DsExpr ( dsExpr, dsLet ) where

#include "HsVersions.h"


import HsSyn		( failureFreePat,
			  HsExpr(..), OutPat(..), HsLit(..), ArithSeqInfo(..),
			  Stmt(..), StmtCtxt(..), Match(..), HsBinds(..), MonoBinds(..), 
			  mkSimpleMatch
			)
import TcHsSyn		( TypecheckedHsExpr, TypecheckedHsBinds,
			  TypecheckedStmt
			)
import CoreSyn
import PprCore		( {- instance Outputable Expr -} )
import CoreUtils	( exprType, mkIfThenElse, bindNonRec )

import DsMonad
import DsBinds		( dsMonoBinds, AutoScc(..) )
import DsGRHSs		( dsGuarded )
import DsCCall		( dsCCall, resultWrapper )
import DsListComp	( dsListComp )
import DsUtils		( mkErrorAppDs, mkDsLets, mkConsExpr, mkNilExpr )
import Match		( matchWrapper, matchSimply )

import CostCentre	( mkUserCC )
import FieldLabel	( FieldLabel )
import Id		( Id, idType, recordSelectorFieldLabel )
import DataCon		( DataCon, dataConWrapId, dataConTyCon, dataConArgTys, dataConFieldLabels )
import PrelInfo		( rEC_CON_ERROR_ID, rEC_UPD_ERROR_ID, iRREFUT_PAT_ERROR_ID, addr2IntegerId )
import TyCon		( isNewTyCon )
import DataCon		( isExistentialDataCon )
import Literal		( Literal(..), inIntRange )
import Type		( splitFunTys, mkTyConApp,
			  splitAlgTyConApp, splitAlgTyConApp_maybe, splitTyConApp_maybe, 
			  isNotUsgTy, unUsgTy,
			  splitAppTy, isUnLiftedType, Type
			)
import TysWiredIn	( tupleCon, unboxedTupleCon,
			  listTyCon, mkListTy,
			  charDataCon, charTy, stringTy,
			  smallIntegerDataCon, isIntegerTy
			)
import BasicTypes	( RecFlag(..) )
import Maybes		( maybeToBool )
import Unique		( Uniquable(..), ratioTyConKey )
import Util		( zipEqual, zipWithEqual )
import Outputable

import Ratio 		( numerator, denominator )
\end{code}


%************************************************************************
%*									*
\subsection{dsLet}
%*									*
%************************************************************************

@dsLet@ is a match-result transformer, taking the @MatchResult@ for the body
and transforming it into one for the let-bindings enclosing the body.

This may seem a bit odd, but (source) let bindings can contain unboxed
binds like
\begin{verbatim}
	C x# = e
\end{verbatim}
This must be transformed to a case expression and, if the type has
more than one constructor, may fail.

\begin{code}
dsLet :: TypecheckedHsBinds -> CoreExpr -> DsM CoreExpr

dsLet EmptyBinds body
  = returnDs body

dsLet (ThenBinds b1 b2) body
  = dsLet b2 body 	`thenDs` \ body' ->
    dsLet b1 body'
  
-- Special case for bindings which bind unlifted variables
-- Silently ignore INLINE pragmas...
dsLet (MonoBind (AbsBinds [] [] binder_triples inlines
                          (PatMonoBind pat grhss loc)) sigs is_rec) body
  | or [isUnLiftedType (idType g) | (_, g, l) <- binder_triples]
  = ASSERT (case is_rec of {NonRecursive -> True; other -> False})
    putSrcLocDs loc			$
    dsGuarded grhss 			`thenDs` \ rhs ->
    let
	body' = foldr bind body binder_triples
	bind (tyvars, g, l) body = ASSERT( null tyvars )
				   bindNonRec g (Var l) body
    in
    mkErrorAppDs iRREFUT_PAT_ERROR_ID result_ty (showSDoc (ppr pat))
    `thenDs` \ error_expr ->
    matchSimply rhs PatBindMatch pat body' error_expr
  where
    result_ty = exprType body

-- Ordinary case for bindings
dsLet (MonoBind binds sigs is_rec) body
  = dsMonoBinds NoSccs binds []  `thenDs` \ prs ->
    case is_rec of
      Recursive    -> returnDs (Let (Rec prs) body)
      NonRecursive -> returnDs (mkDsLets [NonRec b r | (b,r) <- prs] body)
\end{code}

%************************************************************************
%*									*
\subsection[DsExpr-vars-and-cons]{Variables and constructors}
%*									*
%************************************************************************

\begin{code}
dsExpr :: TypecheckedHsExpr -> DsM CoreExpr

dsExpr e@(HsVar var) = returnDs (Var var)
dsExpr e@(HsIPVar var) = returnDs (Var var)
\end{code}

%************************************************************************
%*									*
\subsection[DsExpr-literals]{Literals}
%*									*
%************************************************************************

We give int/float literals type @Integer@ and @Rational@, respectively.
The typechecker will (presumably) have put \tr{from{Integer,Rational}s}
around them.

ToDo: put in range checks for when converting ``@i@''
(or should that be in the typechecker?)

For numeric literals, we try to detect there use at a standard type
(@Int@, @Float@, etc.) are directly put in the right constructor.
[NB: down with the @App@ conversion.]

See also below where we look for @DictApps@ for \tr{plusInt}, etc.

\begin{code}
dsExpr (HsLitOut (HsString s) _)
  | _NULL_ s
  = returnDs (mkNilExpr charTy)

  | _LENGTH_ s == 1
  = let
	the_char = mkConApp charDataCon [mkLit (MachChar (_HEAD_ s))]
	the_nil  = mkNilExpr charTy
	the_cons = mkConsExpr charTy the_char the_nil
    in
    returnDs the_cons


-- "_" => build (\ c n -> c 'c' n)	-- LATER

dsExpr (HsLitOut (HsString str) _)
  = returnDs (mkStringLitFS str)

dsExpr (HsLitOut (HsLitLit str) ty)
  = ASSERT( maybeToBool maybe_ty )
    returnDs (wrap_fn (mkLit (MachLitLit str rep_ty)))
  where
    (maybe_ty, wrap_fn) = resultWrapper ty
    Just rep_ty 	= maybe_ty

dsExpr (HsLitOut (HsInt i) ty)
  = returnDs (mkIntegerLit i)


dsExpr (HsLitOut (HsFrac r) ty)
  = returnDs (mkConApp ratio_data_con [Type integer_ty,
				       mkIntegerLit (numerator r),
				       mkIntegerLit (denominator r)])
  where
    (ratio_data_con, integer_ty)
      = case (splitAlgTyConApp_maybe ty) of
	  Just (tycon, [i_ty], [con])
	    -> ASSERT(isIntegerTy i_ty && getUnique tycon == ratioTyConKey)
	       (con, i_ty)

	  _ -> (panic "ratio_data_con", panic "integer_ty")



-- others where we know what to do:

dsExpr (HsLitOut (HsIntPrim i) _) 
  = returnDs (mkIntLit i)

dsExpr (HsLitOut (HsFloatPrim f) _)
  = returnDs (mkLit (MachFloat f))

dsExpr (HsLitOut (HsDoublePrim d) _)
  = returnDs (mkLit (MachDouble d))
    -- ToDo: range checking needed!

dsExpr (HsLitOut (HsChar c) _)
  = returnDs ( mkConApp charDataCon [mkLit (MachChar c)] )

dsExpr (HsLitOut (HsCharPrim c) _)
  = returnDs (mkLit (MachChar c))

dsExpr (HsLitOut (HsStringPrim s) _)
  = returnDs (mkLit (MachStr s))

-- end of literals magic. --

dsExpr expr@(HsLam a_Match)
  = matchWrapper LambdaMatch [a_Match] "lambda"	`thenDs` \ (binders, matching_code) ->
    returnDs (mkLams binders matching_code)

dsExpr expr@(HsApp fun arg)      
  = dsExpr fun		`thenDs` \ core_fun ->
    dsExpr arg		`thenDs` \ core_arg ->
    returnDs (core_fun `App` core_arg)

\end{code}

Operator sections.  At first it looks as if we can convert
\begin{verbatim}
	(expr op)
\end{verbatim}
to
\begin{verbatim}
	\x -> op expr x
\end{verbatim}

But no!  expr might be a redex, and we can lose laziness badly this
way.  Consider
\begin{verbatim}
	map (expr op) xs
\end{verbatim}
for example.  So we convert instead to
\begin{verbatim}
	let y = expr in \x -> op y x
\end{verbatim}
If \tr{expr} is actually just a variable, say, then the simplifier
will sort it out.

\begin{code}
dsExpr (OpApp e1 op _ e2)
  = dsExpr op						`thenDs` \ core_op ->
    -- for the type of y, we need the type of op's 2nd argument
    dsExpr e1				`thenDs` \ x_core ->
    dsExpr e2				`thenDs` \ y_core ->
    returnDs (mkApps core_op [x_core, y_core])
    
dsExpr (SectionL expr op)
  = dsExpr op						`thenDs` \ core_op ->
    -- for the type of y, we need the type of op's 2nd argument
    let
	(x_ty:y_ty:_, _) = splitFunTys (exprType core_op)
    in
    dsExpr expr				`thenDs` \ x_core ->
    newSysLocalDs x_ty			`thenDs` \ x_id ->
    newSysLocalDs y_ty			`thenDs` \ y_id ->

    returnDs (bindNonRec x_id x_core $
	      Lam y_id (mkApps core_op [Var x_id, Var y_id]))

-- dsExpr (SectionR op expr)	-- \ x -> op x expr
dsExpr (SectionR op expr)
  = dsExpr op			`thenDs` \ core_op ->
    -- for the type of x, we need the type of op's 2nd argument
    let
	(x_ty:y_ty:_, _) = splitFunTys (exprType core_op)
    in
    dsExpr expr				`thenDs` \ y_core ->
    newSysLocalDs x_ty			`thenDs` \ x_id ->
    newSysLocalDs y_ty			`thenDs` \ y_id ->

    returnDs (bindNonRec y_id y_core $
	      Lam x_id (mkApps core_op [Var x_id, Var y_id]))

dsExpr (HsCCall lbl args may_gc is_asm result_ty)
  = mapDs dsExpr args		`thenDs` \ core_args ->
    dsCCall lbl core_args may_gc is_asm result_ty
	-- dsCCall does all the unboxification, etc.

dsExpr (HsSCC cc expr)
  = dsExpr expr			`thenDs` \ core_expr ->
    getModuleDs			`thenDs` \ mod_name ->
    returnDs (Note (SCC (mkUserCC cc mod_name)) core_expr)

-- special case to handle unboxed tuple patterns.

dsExpr (HsCase discrim matches src_loc)
 | all ubx_tuple_match matches
 =  putSrcLocDs src_loc $
    dsExpr discrim			  `thenDs` \ core_discrim ->
    matchWrapper CaseMatch matches "case" `thenDs` \ ([discrim_var], matching_code) ->
    case matching_code of
	Case (Var x) bndr alts | x == discrim_var -> 
		returnDs (Case core_discrim bndr alts)
	_ -> panic ("dsExpr: tuple pattern:\n" ++ showSDoc (ppr matching_code))
  where
    ubx_tuple_match (Match _ [TuplePat ps False{-unboxed-}] _ _) = True
    ubx_tuple_match _ = False

dsExpr (HsCase discrim matches src_loc)
  = putSrcLocDs src_loc $
    dsExpr discrim			  `thenDs` \ core_discrim ->
    matchWrapper CaseMatch matches "case" `thenDs` \ ([discrim_var], matching_code) ->
    returnDs (bindNonRec discrim_var core_discrim matching_code)

dsExpr (HsLet binds body)
  = dsExpr body		`thenDs` \ body' ->
    dsLet binds body'

dsExpr (HsWith expr binds)
  = dsExpr expr		`thenDs` \ expr' ->
    foldlDs dsIPBind expr' binds
    where
      dsIPBind body (n, e)
        = dsExpr e	`thenDs` \ e' ->
	  returnDs (Let (NonRec n e') body)

dsExpr (HsDoOut do_or_lc stmts return_id then_id fail_id result_ty src_loc)
  | maybeToBool maybe_list_comp
  =	-- Special case for list comprehensions
    putSrcLocDs src_loc $
    dsListComp stmts elt_ty

  | otherwise
  = putSrcLocDs src_loc $
    dsDo do_or_lc stmts return_id then_id fail_id result_ty
  where
    maybe_list_comp 
	= case (do_or_lc, splitTyConApp_maybe result_ty) of
	    (ListComp, Just (tycon, [elt_ty]))
		  | tycon == listTyCon
		 -> Just elt_ty
	    other -> Nothing
	-- We need the ListComp form to use deListComp (rather than the "do" form)
	-- because the "return" in a do block is a call to "PrelBase.return", and
	-- not a ReturnStmt.  Only the ListComp form has ReturnStmts

    Just elt_ty = maybe_list_comp

dsExpr (HsIf guard_expr then_expr else_expr src_loc)
  = putSrcLocDs src_loc $
    dsExpr guard_expr	`thenDs` \ core_guard ->
    dsExpr then_expr	`thenDs` \ core_then ->
    dsExpr else_expr	`thenDs` \ core_else ->
    returnDs (mkIfThenElse core_guard core_then core_else)
\end{code}


\noindent
\underline{\bf Type lambda and application}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
dsExpr (TyLam tyvars expr)
  = dsExpr expr `thenDs` \ core_expr ->
    returnDs (mkLams tyvars core_expr)

dsExpr (TyApp expr tys)
  = dsExpr expr		`thenDs` \ core_expr ->
    returnDs (mkTyApps core_expr tys)
\end{code}


\noindent
\underline{\bf Various data construction things}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
dsExpr (ExplicitListOut ty xs)
  = go xs
  where
    go []     = returnDs (mkNilExpr ty)
    go (x:xs) = dsExpr x				`thenDs` \ core_x ->
		go xs					`thenDs` \ core_xs ->
                ASSERT( isNotUsgTy ty )
		returnDs (mkConsExpr ty core_x core_xs)

dsExpr (ExplicitTuple expr_list boxed)
  = mapDs dsExpr expr_list	  `thenDs` \ core_exprs  ->
    returnDs (mkConApp ((if boxed 
			    then tupleCon 
			    else unboxedTupleCon) (length expr_list))
	    	(map (Type . unUsgTy . exprType) core_exprs ++ core_exprs))
                -- the above unUsgTy is *required* -- KSW 1999-04-07

dsExpr (ArithSeqOut expr (From from))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    returnDs (App expr2 from2)

dsExpr (ArithSeqOut expr (FromTo from two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr two		  `thenDs` \ two2 ->
    returnDs (mkApps expr2 [from2, two2])

dsExpr (ArithSeqOut expr (FromThen from thn))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr thn		  `thenDs` \ thn2 ->
    returnDs (mkApps expr2 [from2, thn2])

dsExpr (ArithSeqOut expr (FromThenTo from thn two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsExpr from		  `thenDs` \ from2 ->
    dsExpr thn		  `thenDs` \ thn2 ->
    dsExpr two		  `thenDs` \ two2 ->
    returnDs (mkApps expr2 [from2, thn2, two2])
\end{code}

\noindent
\underline{\bf Record construction and update}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For record construction we do this (assuming T has three arguments)
\begin{verbatim}
	T { op2 = e }
==>
	let err = /\a -> recConErr a 
	T (recConErr t1 "M.lhs/230/op1") 
	  e 
	  (recConErr t1 "M.lhs/230/op3")
\end{verbatim}
@recConErr@ then converts its arugment string into a proper message
before printing it as
\begin{verbatim}
	M.lhs, line 230: missing field op1 was evaluated
\end{verbatim}

We also handle @C{}@ as valid construction syntax for an unlabelled
constructor @C@, setting all of @C@'s fields to bottom.

\begin{code}
dsExpr (RecordConOut data_con con_expr rbinds)
  = dsExpr con_expr	`thenDs` \ con_expr' ->
    let
	(arg_tys, _) = splitFunTys (exprType con_expr')

	mk_arg (arg_ty, lbl)
	  = case [rhs | (sel_id,rhs,_) <- rbinds,
			lbl == recordSelectorFieldLabel sel_id] of
	      (rhs:rhss) -> ASSERT( null rhss )
		 	    dsExpr rhs
	      []         -> mkErrorAppDs rEC_CON_ERROR_ID arg_ty (showSDoc (ppr lbl))
	unlabelled_bottom arg_ty = mkErrorAppDs rEC_CON_ERROR_ID arg_ty ""

	labels = dataConFieldLabels data_con
    in

    (if null labels
	then mapDs unlabelled_bottom arg_tys
	else mapDs mk_arg (zipEqual "dsExpr:RecordCon" arg_tys labels))
	`thenDs` \ con_args ->

    returnDs (mkApps con_expr' con_args)
\end{code}

Record update is a little harder. Suppose we have the decl:
\begin{verbatim}
	data T = T1 {op1, op2, op3 :: Int}
	       | T2 {op4, op2 :: Int}
	       | T3
\end{verbatim}
Then we translate as follows:
\begin{verbatim}
	r { op2 = e }
===>
	let op2 = e in
	case r of
	  T1 op1 _ op3 -> T1 op1 op2 op3
	  T2 op4 _     -> T2 op4 op2
	  other	       -> recUpdError "M.lhs/230"
\end{verbatim}
It's important that we use the constructor Ids for @T1@, @T2@ etc on the
RHSs, and do not generate a Core constructor application directly, because the constructor
might do some argument-evaluation first; and may have to throw away some
dictionaries.

\begin{code}
dsExpr (RecordUpdOut record_expr record_out_ty dicts rbinds)
  = getSrcLocDs		`thenDs` \ src_loc ->
    dsExpr record_expr	 	`thenDs` \ record_expr' ->

	-- Desugar the rbinds, and generate let-bindings if
	-- necessary so that we don't lose sharing

    let
	record_in_ty	       = exprType record_expr'
	(_, in_inst_tys, cons) = splitAlgTyConApp record_in_ty
	(_, out_inst_tys, _)   = splitAlgTyConApp record_out_ty
	cons_to_upd  	       = filter has_all_fields cons

	mk_val_arg field old_arg_id 
	  = case [rhs | (sel_id, rhs, _) <- rbinds, 
			field == recordSelectorFieldLabel sel_id] of
	      (rhs:rest) -> ASSERT(null rest) rhs
	      []	 -> HsVar old_arg_id

	mk_alt con
	  = newSysLocalsDs (dataConArgTys con in_inst_tys)	`thenDs` \ arg_ids ->
		-- This call to dataConArgTys won't work for existentials
	    let 
		val_args = zipWithEqual "dsExpr:RecordUpd" mk_val_arg
					(dataConFieldLabels con) arg_ids
		rhs = foldl HsApp (DictApp (TyApp (HsVar (dataConWrapId con)) 
						  out_inst_tys)
					   dicts)
				  val_args
	    in
	    returnDs (mkSimpleMatch [ConPat con record_in_ty [] [] (map VarPat arg_ids)]
				    rhs
				    (Just record_out_ty)
				    src_loc)
    in
	-- Record stuff doesn't work for existentials
    ASSERT( all (not . isExistentialDataCon) cons )

	-- It's important to generate the match with matchWrapper,
	-- and the right hand sides with applications of the wrapper Id
	-- so that everything works when we are doing fancy unboxing on the
	-- constructor aguments.
    mapDs mk_alt cons_to_upd				`thenDs` \ alts ->
    matchWrapper RecUpdMatch alts "record update"	`thenDs` \ ([discrim_var], matching_code) ->

    returnDs (bindNonRec discrim_var record_expr' matching_code)

  where
    has_all_fields :: DataCon -> Bool
    has_all_fields con_id 
      = all ok rbinds
      where
	con_fields        = dataConFieldLabels con_id
	ok (sel_id, _, _) = recordSelectorFieldLabel sel_id `elem` con_fields
\end{code}


\noindent
\underline{\bf Dictionary lambda and application}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@DictLam@ and @DictApp@ turn into the regular old things.
(OLD:) @DictFunApp@ also becomes a curried application, albeit slightly more
complicated; reminiscent of fully-applied constructors.
\begin{code}
dsExpr (DictLam dictvars expr)
  = dsExpr expr `thenDs` \ core_expr ->
    returnDs (mkLams dictvars core_expr)

------------------

dsExpr (DictApp expr dicts)	-- becomes a curried application
  = dsExpr expr			`thenDs` \ core_expr ->
    returnDs (foldl (\f d -> f `App` (Var d)) core_expr dicts)
\end{code}

\begin{code}

#ifdef DEBUG
-- HsSyn constructs that just shouldn't be here:
dsExpr (HsDo _ _ _)	    = panic "dsExpr:HsDo"
dsExpr (ExplicitList _)	    = panic "dsExpr:ExplicitList"
dsExpr (ExprWithTySig _ _)  = panic "dsExpr:ExprWithTySig"
dsExpr (ArithSeqIn _)	    = panic "dsExpr:ArithSeqIn"
#endif

\end{code}

%--------------------------------------------------------------------

Basically does the translation given in the Haskell~1.3 report:

\begin{code}
dsDo	:: StmtCtxt
	-> [TypecheckedStmt]
	-> Id		-- id for: return m
	-> Id		-- id for: (>>=) m
	-> Id		-- id for: fail m
	-> Type		-- Element type; the whole expression has type (m t)
	-> DsM CoreExpr

dsDo do_or_lc stmts return_id then_id fail_id result_ty
  = let
	(_, b_ty) = splitAppTy result_ty	-- result_ty must be of the form (m b)
	
	go [ReturnStmt expr] 
	  = dsExpr expr			`thenDs` \ expr2 ->
	    returnDs (mkApps (Var return_id) [Type b_ty, expr2])
    
	go (GuardStmt expr locn : stmts)
	  = do_expr expr locn			`thenDs` \ expr2 ->
	    go stmts				`thenDs` \ rest ->
	    let msg = ASSERT( isNotUsgTy b_ty )
                 "Pattern match failure in do expression, " ++ showSDoc (ppr locn) in
	    returnDs (mkIfThenElse expr2 
				   rest 
				   (App (App (Var fail_id) 
					     (Type b_ty))
					     (mkStringLit msg)))
    
	go (ExprStmt expr locn : stmts)
	  = do_expr expr locn		`thenDs` \ expr2 ->
	    let
		(_, a_ty) = splitAppTy (exprType expr2)  -- Must be of form (m a)
	    in
	    if null stmts then
		returnDs expr2
	    else
		go stmts     		`thenDs` \ rest  ->
		newSysLocalDs a_ty		`thenDs` \ ignored_result_id ->
		returnDs (mkApps (Var then_id) [Type a_ty, Type b_ty, expr2, 
					        Lam ignored_result_id rest])
    
	go (LetStmt binds : stmts )
	  = go stmts 		`thenDs` \ rest   ->
	    dsLet binds	rest
	    
	go (BindStmt pat expr locn : stmts)
	  = putSrcLocDs locn $
	    dsExpr expr 	   `thenDs` \ expr2 ->
	    let
		(_, a_ty)  = splitAppTy (exprType expr2) -- Must be of form (m a)
		fail_expr  = HsApp (TyApp (HsVar fail_id) [b_ty])
                                   (HsLitOut (HsString (_PK_ msg)) stringTy)
	        msg = ASSERT2( isNotUsgTy a_ty, ppr a_ty )
                      ASSERT2( isNotUsgTy b_ty, ppr b_ty )
                      "Pattern match failure in do expression, " ++ showSDoc (ppr locn)
		main_match = mkSimpleMatch [pat] 
					   (HsDoOut do_or_lc stmts return_id then_id
                                                    fail_id result_ty locn)
					   (Just result_ty) locn
		the_matches
		  | failureFreePat pat = [main_match]
		  | otherwise	       =
		      [ main_match
		      , mkSimpleMatch [WildPat a_ty] fail_expr (Just result_ty) locn
		      ]
	    in
	    matchWrapper DoBindMatch the_matches match_msg
				`thenDs` \ (binders, matching_code) ->
	    returnDs (mkApps (Var then_id) [Type a_ty, Type b_ty, expr2,
				            mkLams binders matching_code])
    in
    go stmts

  where
    do_expr expr locn = putSrcLocDs locn (dsExpr expr)

    match_msg = case do_or_lc of
			DoStmt   -> "`do' statement"
			ListComp -> "comprehension"
\end{code}

\begin{code}
var_pat (WildPat _) = True
var_pat (VarPat _) = True
var_pat _ = False
\end{code}

\begin{code}
mkIntegerLit :: Integer -> CoreExpr
mkIntegerLit i
  | inIntRange i  	-- Small enough, so start from an Int
  = mkConApp smallIntegerDataCon [mkIntLit i]

  | otherwise 		-- Big, so start from a string
  = App (Var addr2IntegerId) (Lit (MachStr (_PK_ (show i))))
\end{code}

