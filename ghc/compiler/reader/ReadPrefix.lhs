%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section{Read parse tree built by Yacc parser}

\begin{code}
#include "HsVersions.h"

module ReadPrefix (
	rdModule,

	-- used over in ReadPragmas...
	wlkList, wlkMaybe, rdConDecl, wlkMonoType, rdMonoType
    )  where

import Ubiq{-uitous-}
import RdrLoop 		-- for paranoia checking

import UgenAll		-- all Yacc parser gumpff...
import PrefixSyn	-- and various syntaxen.
import HsSyn
import RdrHsSyn

-- friends:
import ReadPragmas
import PrefixToHs	-- reader utilities

-- others:
import FiniteMap	( elemFM, FiniteMap )
import MainMonad	( thenMn, MainIO(..) )
import PprStyle		( PprStyle(..) )
import Pretty
import ProtoName	( isConopPN, ProtoName(..) )
import Util		( nOfThem, pprError, panic )
\end{code}

%************************************************************************
%*									*
\subsection[ReadPrefix-help]{Help Functions}
%*									*
%************************************************************************

\begin{code}
wlkList :: (U_VOID_STAR -> UgnM a) -> U_list -> UgnM [a]

wlkList wlk_it U_lnil = returnUgn []

wlkList wlk_it (U_lcons hd tl)
  = wlk_it  hd		`thenUgn` \ hd_it ->
    wlkList wlk_it tl	`thenUgn` \ tl_it ->
    returnUgn (hd_it : tl_it)
\end{code}

\begin{code}
wlkMaybe :: (U_VOID_STAR -> UgnM a) -> U_maybe -> UgnM (Maybe a)

wlkMaybe wlk_it U_nothing  = returnUgn Nothing
wlkMaybe wlk_it (U_just x)
  = wlk_it  x		`thenUgn` \ it ->
    returnUgn (Just it)
\end{code}

\begin{code}
rdQid   :: ParseTree -> UgnM ProtoName
rdQid pt = rdU_qid pt `thenUgn` \ qid -> wlkQid qid

wlkQid	:: U_qid -> UgnM ProtoName
wlkQid (U_noqual name)
  = returnUgn (Unk name)
wlkQid (U_aqual  mod name)
  = returnUgn (Qunk mod name)
wlkQid (U_gid n name)
  = returnUgn (Unk name)
\end{code}

%************************************************************************
%*									*
\subsection[rdModule]{@rdModule@: reads in a Haskell module}
%*									*
%************************************************************************

\begin{code}
rdModule :: MainIO
	   (FAST_STRING,	   -- this module's name
	    (FAST_STRING -> Bool,  -- a function to chk if <x> is in the export list
	     FAST_STRING -> Bool), -- a function to chk if <M> is among the M..
				   -- ("dotdot") modules in the export list.
	    ProtoNameHsModule)	   -- the main goods

rdModule
  = _ccall_ hspmain `thenPrimIO` \ pt -> -- call the Yacc parser!
    let
	srcfile  = _packCString ``input_filename'' -- What A Great Hack! (TM)
    in
    initUgn srcfile (

    rdU_tree pt `thenUgn` \ (U_hmodule name himplist hexplist hfixlist hmodlist srcline) ->
    wlkList  rdFixOp		 hfixlist `thenUgn` \ fixities 	->
    wlkBinding			 hmodlist `thenUgn` \ binding	->
    wlkList  rdImportedInterface himplist `thenUgn` \ imports	->
    wlkMaybe rdEntities		 hexplist `thenUgn` \ exp_list	->
    mkSrcLocUgn srcline			  `thenUgn` \ src_loc	->

    case sepDeclsForTopBinds binding	  of {
      (tydecls, tysigs, classdecls, instdecls, instsigs, defaultdecls, binds) ->

    returnUgn (
     name,
     mk_export_list_chker exp_list,
     HsModule name
	      exp_list
	      imports
	      fixities
	      tydecls
	      tysigs
	      classdecls
	      instdecls
	      instsigs
	      defaultdecls
	      (cvSepdBinds srcfile cvValSig binds)
	      [{-no sigs-}]
	      src_loc
    ) } )
  where
    mk_export_list_chker = panic "ReadPrefix:mk_export_list_chker"
{- LATER:
    mk_export_list_chker exp_list
      = case (getExportees exp_list) of
	  Nothing -> ( \ n -> False, \ n -> False ) -- all suspicious
	  Just (entity_info, dotdot_modules) ->
	    ( \ n -> n `elemFM` entity_info,
	      \ n -> n `elemFM` dotdot_modules )
-}
\end{code}

%************************************************************************
%*									*
\subsection[wlkExprOrPat]{@wlkExpr@ and @wlkPat@}
%*									*
%************************************************************************

\begin{code}
rdExpr :: ParseTree -> UgnM ProtoNameHsExpr
rdPat  :: ParseTree -> UgnM ProtoNamePat

rdExpr pt = rdU_tree pt `thenUgn` \ tree -> wlkExpr tree
rdPat  pt = rdU_tree pt `thenUgn` \ tree -> wlkPat  tree

wlkExpr :: U_tree -> UgnM ProtoNameHsExpr
wlkPat  :: U_tree -> UgnM ProtoNamePat

wlkExpr expr
  = case expr of
      U_par expr -> -- parenthesised expr
	wlkExpr expr

      U_lsection lsexp lop -> -- left section
	wlkExpr lsexp	`thenUgn` \ expr ->
	wlkQid  lop	`thenUgn` \ op   ->
	returnUgn (SectionL expr (HsVar op))

      U_rsection rop rsexp -> -- right section
	wlkQid  rop	`thenUgn` \ op   ->
	wlkExpr rsexp	`thenUgn` \ expr ->
	returnUgn (SectionR (HsVar op) expr)

      U_ccall fun flavor ccargs -> -- ccall/casm
	wlkList rdExpr ccargs   `thenUgn` \ args ->
	let
	    tag = _HEAD_ flavor
	in
	returnUgn (CCall fun args
		    (tag == 'p' || tag == 'P') -- may invoke GC
		    (tag == 'N' || tag == 'P') -- really a "casm"
		    (panic "CCall:result_ty"))

      U_scc label sccexp -> -- scc (set-cost-centre) expression
	wlkExpr   sccexp	`thenUgn` \ expr  ->
	returnUgn (HsSCC label expr)

      U_lambda lampats lamexpr srcline -> -- lambda expression
	wlkList rdPat lampats	`thenUgn` \ pats ->
	wlkExpr       lamexpr	`thenUgn` \ body ->
	mkSrcLocUgn   srcline	`thenUgn` \ src_loc ->
	returnUgn (
	    HsLam (foldr PatMatch
			 (GRHSMatch (GRHSsAndBindsIn
				      [OtherwiseGRHS body src_loc]
				      EmptyBinds))
			 pats)
	)

      U_casee caseexpr casebody srcline ->	-- case expression
	wlkExpr	        caseexpr `thenUgn` \ expr ->
	wlkList rdMatch casebody `thenUgn` \ mats ->
	mkSrcLocUgn    srcline	 `thenUgn` \ src_loc ->
	getSrcFileUgn		 `thenUgn` \ sf ->
	let
	    matches = cvMatches sf True mats
	in
	returnUgn (HsCase expr matches src_loc)

      U_ife ifpred ifthen ifelse srcline ->	-- if expression
	wlkExpr ifpred		`thenUgn` \ e1 ->
	wlkExpr ifthen		`thenUgn` \ e2 ->
	wlkExpr ifelse		`thenUgn` \ e3 ->
	mkSrcLocUgn srcline	`thenUgn` \ src_loc ->
	returnUgn (HsIf e1 e2 e3 src_loc)

      U_let letvdefs letvexpr ->		-- let expression
	wlkBinding letvdefs	`thenUgn` \ binding ->
	wlkExpr    letvexpr	`thenUgn` \ expr    ->
	getSrcFileUgn		`thenUgn` \ sf	    ->
	let
	    binds = cvBinds sf cvValSig binding
	in
	returnUgn (HsLet binds expr)

      U_doe gdo srcline ->		-- do expression
	wlkList rd_stmt gdo	`thenUgn` \ stmts ->
	mkSrcLocUgn srcline	`thenUgn` \ src_loc ->
	returnUgn (HsDo stmts src_loc)
        where
	rd_stmt pt
	  = rdU_tree pt `thenUgn` \ bind ->
	    case bind of
	      U_doexp exp srcline ->
		wlkExpr exp 		`thenUgn` \ expr ->
		mkSrcLocUgn srcline	`thenUgn` \ src_loc ->
		returnUgn (ExprStmt expr src_loc)

	      U_dobind pat exp srcline ->
		wlkPat  pat		`thenUgn` \ patt ->
		wlkExpr exp 		`thenUgn` \ expr ->
		mkSrcLocUgn srcline	`thenUgn` \ src_loc ->
		returnUgn (BindStmt patt expr src_loc)

	      U_seqlet seqlet ->
		wlkBinding seqlet	`thenUgn` \ bs ->
		getSrcFileUgn		`thenUgn` \ sf ->
		let
		    binds = cvBinds sf cvValSig bs
		in
		returnUgn (LetStmt binds)

      U_comprh cexp cquals -> -- list comprehension
	wlkExpr cexp		`thenUgn` \ expr  ->
	wlkList rd_qual cquals	`thenUgn` \ quals ->
	returnUgn (ListComp expr quals)
	where
	  rd_qual pt
	    = rdU_tree pt	`thenUgn` \ qual ->
	      wlk_qual qual

	  wlk_qual qual
	    = case qual of
		U_guard exp ->
		  wlkExpr exp  	`thenUgn` \ expr ->
		  returnUgn (FilterQual expr)

		U_qual qpat qexp ->
		  wlkPat  qpat  `thenUgn` \ pat  ->
		  wlkExpr qexp  `thenUgn` \ expr ->
		  returnUgn (GeneratorQual pat expr)

		U_seqlet seqlet ->
		  wlkBinding seqlet	`thenUgn` \ bs ->
		  getSrcFileUgn		`thenUgn` \ sf ->
		  let
		      binds = cvBinds sf cvValSig bs
		  in
		  returnUgn (LetQual binds)

      U_eenum efrom estep eto -> -- arithmetic sequence
	wlkExpr efrom		`thenUgn` \ e1  ->
	wlkMaybe rdExpr estep	`thenUgn` \ es2 ->
	wlkMaybe rdExpr eto	`thenUgn` \ es3 ->
	returnUgn (cv_arith_seq e1 es2 es3)
	where
	   cv_arith_seq e1 Nothing   Nothing   = ArithSeqIn (From	e1)
	   cv_arith_seq e1 Nothing   (Just e3) = ArithSeqIn (FromTo     e1 e3)
	   cv_arith_seq e1 (Just e2) Nothing   = ArithSeqIn (FromThen   e1 e2)
	   cv_arith_seq e1 (Just e2) (Just e3) = ArithSeqIn (FromThenTo e1 e2 e3)

      U_restr restre restrt -> 	-- expression with type signature
	wlkExpr	    restre	`thenUgn` \ expr ->
	wlkPolyType restrt	`thenUgn` \ ty	 ->
	returnUgn (ExprWithTySig expr ty)

      --------------------------------------------------------------
      -- now the prefix items that can either be an expression or
      -- pattern, except we know they are *expressions* here
      -- (this code could be commoned up with the pattern version;
      -- but it probably isn't worth it)
      --------------------------------------------------------------
      U_lit lit ->
	wlkLiteral lit	`thenUgn` \ lit ->
	returnUgn (HsLit lit)

      U_ident n -> 			-- simple identifier
	wlkQid n	`thenUgn` \ var ->
	returnUgn (HsVar var)

      U_ap fun arg -> 			-- application
	wlkExpr fun	`thenUgn` \ expr1 ->
	wlkExpr arg	`thenUgn` \ expr2 ->
	returnUgn (HsApp expr1 expr2)

      U_infixap fun arg1 arg2 ->	-- infix application
	wlkQid  fun	`thenUgn` \ op    ->
	wlkExpr arg1	`thenUgn` \ expr1 ->
	wlkExpr arg2	`thenUgn` \ expr2 ->
	returnUgn (OpApp expr1 (HsVar op) expr2)

      U_negate nexp _ _ -> 		-- prefix negation
	wlkExpr nexp	`thenUgn` \ expr ->
	returnUgn (HsApp (HsVar (Unk SLIT("negate"))) expr)

      U_llist llist -> -- explicit list
	wlkList rdExpr llist `thenUgn` \ exprs ->
	returnUgn (ExplicitList exprs)

      U_tuple tuplelist -> -- explicit tuple
	wlkList rdExpr tuplelist `thenUgn` \ exprs ->
	returnUgn (ExplicitTuple exprs)

      U_record con rbinds -> -- record construction
	wlkQid  con		`thenUgn` \ rcon     ->
	wlkList rdRbind rbinds	`thenUgn` \ recbinds ->
	returnUgn (RecordCon (HsVar rcon) recbinds)

      U_rupdate updexp updbinds -> -- record update
	wlkExpr updexp		 `thenUgn` \ aexp ->
	wlkList rdRbind updbinds `thenUgn` \ recbinds ->
	returnUgn (RecordUpd aexp recbinds)

#ifdef DEBUG
      U_hmodule _ _ _ _ _ _ -> error "U_hmodule"
      U_as _ _ 		    -> error "U_as"
      U_lazyp _ 	    -> error "U_lazyp"
      U_wildp 		    -> error "U_wildp"
      U_qual _ _ 	    -> error "U_qual"
      U_guard _ 	    -> error "U_guard"
      U_seqlet _ 	    -> error "U_seqlet"
      U_dobind _ _ _ 	    -> error "U_dobind"
      U_doexp _ _	    -> error "U_doexp"
      U_rbind _ _	    -> error "U_rbind"
      U_fixop _ _ _	    -> error "U_fixop"
#endif

rdRbind pt
  = rdU_tree pt		`thenUgn` \ (U_rbind var exp) ->
    wlkQid   var	`thenUgn` \ rvar ->
    wlkMaybe rdExpr exp	`thenUgn` \ expr_maybe ->
    returnUgn (
      case expr_maybe of
	Nothing -> (rvar, HsVar rvar, True{-pun-})
	Just re -> (rvar, re,	      False)
    )
\end{code}

Patterns: just bear in mind that lists of patterns are represented as
a series of ``applications''.
\begin{code}
wlkPat pat
  = case pat of
      U_par pat ->  			-- parenthesised pattern
	wlkPat pat

      U_as avar as_pat -> 		-- "as" pattern
	wlkQid avar	`thenUgn` \ var ->
	wlkPat as_pat	`thenUgn` \ pat ->
	returnUgn (AsPatIn var pat)

      U_lazyp lazyp -> 			-- irrefutable ("twiddle") pattern
	wlkPat lazyp	`thenUgn` \ pat ->
	returnUgn (LazyPatIn pat)

      U_wildp -> returnUgn WildPatIn 	-- wildcard pattern

      --------------------------------------------------------------
      -- now the prefix items that can either be an expression or
      -- pattern, except we know they are *patterns* here.
      --------------------------------------------------------------
      U_negate nexp _ _ -> 		-- negated pattern: must be a literal
	wlkPat nexp	`thenUgn` \ lit_pat ->
	case lit_pat of
	  LitPatIn lit -> returnUgn (LitPatIn (negLiteral lit))
	  _	       -> panic "wlkPat: bad negated pattern!"

      U_lit lit ->			-- literal pattern
	wlkLiteral lit	`thenUgn` \ lit ->
	returnUgn (LitPatIn lit)

      U_ident nn ->			-- simple identifier
	wlkQid nn	`thenUgn` \ n ->
	returnUgn (
	  if isConopPN n
	  then ConPatIn n []
	  else VarPatIn n
	)

      U_ap l r ->	-- "application": there's a list of patterns lurking here!
	wlkPat r	  	`thenUgn` \ rpat	 ->
	collect_pats l [rpat] 	`thenUgn` \ (lpat,lpats) ->
	let
	    (n, arg_pats)
	      = case lpat of
		  VarPatIn x        -> (x,  lpats)
		  ConPatIn x []     -> (x,  lpats)
		  ConOpPatIn x op y -> (op, x:y:lpats)
		  _ -> -- sorry about the weedy msg; the parser missed this one
		       pprError "ERROR: an illegal `application' of a pattern to another one:"
			  (ppInterleave ppSP (map (ppr PprForUser) (lpat:lpats)))
	in
	returnUgn (ConPatIn n arg_pats)
	where
	  collect_pats pat acc
	    = case pat of
		U_ap l r ->
		  wlkPat r	`thenUgn` \ rpat  ->
		  collect_pats l (rpat:acc)
		other ->
		  wlkPat other	`thenUgn` \ pat ->
		  returnUgn (pat,acc)

      U_infixap fun arg1 arg2 ->
	wlkQid fun	`thenUgn` \ op   ->
	wlkPat arg1	`thenUgn` \ pat1 ->
	wlkPat arg2	`thenUgn` \ pat2 ->
	returnUgn (ConOpPatIn pat1 op pat2)

      U_llist llist -> 			-- explicit list
	wlkList rdPat llist 	`thenUgn` \ pats ->
	returnUgn (ListPatIn pats)

      U_tuple tuplelist -> 		-- explicit tuple
	wlkList rdPat tuplelist	`thenUgn` \ pats ->
	returnUgn (TuplePatIn pats)

      U_record con rpats -> 		-- record destruction
	wlkQid  con		`thenUgn` \ rcon     ->
	wlkList rdRpat rpats	`thenUgn` \ recpats ->
	returnUgn (RecPatIn rcon recpats)
	where
	  rdRpat pt
	    = rdU_tree pt	 `thenUgn` \ (U_rbind var pat) ->
    	      wlkQid   var	 `thenUgn` \ rvar ->
    	      wlkMaybe rdPat pat `thenUgn` \ pat_maybe ->
	      returnUgn (
		case pat_maybe of
		  Nothing -> (rvar, VarPatIn rvar, True{-pun-})
		  Just rp -> (rvar, rp,		   False)
	      )
\end{code}

\begin{code}
wlkLiteral :: U_literal -> UgnM HsLit

wlkLiteral ulit
  = returnUgn (
    case ulit of
      U_integer    s   -> HsInt	       (as_integer  s)
      U_floatr     s   -> HsFrac       (as_rational s)
      U_intprim    s   -> HsIntPrim    (as_integer  s)
      U_doubleprim s   -> HsDoublePrim (as_rational s)
      U_floatprim  s   -> HsFloatPrim  (as_rational s)
      U_charr	   s   -> HsChar       (as_char     s)
      U_charprim   s   -> HsCharPrim   (as_char     s)
      U_string     s   -> HsString     (as_string   s)
      U_stringprim s   -> HsStringPrim (as_string   s)
      U_clitlit    s _ -> HsLitLit     (as_string   s)
    )
  where
    as_char s     = _HEAD_ s
    as_integer s  = readInteger (_UNPK_ s)
    as_rational s = _readRational (_UNPK_ s) -- non-std
    as_string s   = s
\end{code}

%************************************************************************
%*									*
\subsection{wlkBinding}
%*									*
%************************************************************************

\begin{code}
wlkBinding :: U_binding -> UgnM RdrBinding

wlkBinding binding
  = case binding of
      U_nullbind -> -- null binding
	returnUgn RdrNullBind

      U_abind a b -> -- "and" binding (just glue, really)
	wlkBinding a    `thenUgn` \ binding1 ->
	wlkBinding b    `thenUgn` \ binding2 ->
	returnUgn (RdrAndBindings binding1 binding2)

      U_tbind tctxt ttype tcons tderivs srcline tpragma -> -- "data" declaration
	wlkContext	   tctxt    `thenUgn` \ ctxt	    ->
	wlkTyConAndTyVars  ttype    `thenUgn` \ (tycon, tyvars) ->
	wlkList rdConDecl  tcons    `thenUgn` \ cons	    ->
	wlkDerivings	   tderivs  `thenUgn` \ derivings   ->
	wlkDataPragma	   tpragma  `thenUgn` \ pragmas	    ->
	mkSrcLocUgn	   srcline  `thenUgn` \ src_loc	    ->
	returnUgn (RdrTyDecl (TyData ctxt tycon tyvars cons derivings pragmas src_loc))

      U_ntbind ntctxt nttype ntcon ntderivs srcline ntpragma -> -- "newtype" declaration
	wlkContext	   ntctxt   `thenUgn` \ ctxt	    ->
	wlkTyConAndTyVars  nttype   `thenUgn` \ (tycon, tyvars) ->
	wlkList rdConDecl  ntcon    `thenUgn` \ con	    ->
	wlkDerivings	   ntderivs `thenUgn` \ derivings   ->
	wlkDataPragma	   ntpragma `thenUgn` \ pragma	    ->
	mkSrcLocUgn	   srcline  `thenUgn` \ src_loc	    ->
	returnUgn (RdrTyDecl (TyNew ctxt tycon tyvars con derivings pragma src_loc))

      U_nbind nbindid nbindas srcline -> -- "type" declaration
	wlkTyConAndTyVars nbindid `thenUgn` \ (tycon, tyvars) ->
	wlkMonoType	  nbindas `thenUgn` \ expansion	    ->
	mkSrcLocUgn	  srcline `thenUgn` \ src_loc	    ->
	returnUgn (RdrTyDecl (TySynonym tycon tyvars expansion src_loc))

      U_fbind fbindl srcline -> -- function binding
	wlkList rdMatch fbindl	`thenUgn` \ matches ->
	mkSrcLocUgn     srcline	`thenUgn` \ src_loc ->
	returnUgn (RdrFunctionBinding srcline matches)

      U_pbind pbindl srcline ->  -- pattern binding
	wlkList rdMatch pbindl	`thenUgn` \ matches ->
	mkSrcLocUgn     srcline	`thenUgn` \ src_loc ->
	returnUgn (RdrPatternBinding srcline matches)

      U_cbind cbindc cbindid cbindw srcline cpragma ->	 	-- "class" declaration
	wlkContext	 cbindc	 `thenUgn` \ ctxt	  ->
	wlkClassAssertTy cbindid `thenUgn` \ (clas, tyvar)->
	wlkBinding	 cbindw	 `thenUgn` \ binding	  ->
	wlkClassPragma   cpragma `thenUgn` \ pragma	  ->
	mkSrcLocUgn	 srcline `thenUgn` \ src_loc	  ->
	getSrcFileUgn		 `thenUgn` \ sf		  ->
	let
	    (class_sigs, class_methods) = sepDeclsIntoSigsAndBinds binding

	    final_sigs    = concat (map cvClassOpSig class_sigs)
	    final_methods = cvMonoBinds sf class_methods
	in
	returnUgn (RdrClassDecl
	  (ClassDecl ctxt clas tyvar final_sigs final_methods pragma src_loc))

      U_ibind from_source orig_mod		 		-- "instance" declaration
	      ibindc iclas ibindi ibindw srcline ipragma ->
	wlkContext	ibindc	`thenUgn` \ ctxt    ->
	wlkQid		iclas	`thenUgn` \ clas    ->
	wlkMonoType	ibindi	`thenUgn` \ inst_ty ->
	wlkBinding	ibindw	`thenUgn` \ binding ->
	wlkInstPragma   ipragma	`thenUgn` \ pragma  ->
	mkSrcLocUgn	srcline	`thenUgn` \ src_loc ->
	getSrcFileUgn		`thenUgn` \ sf	    ->
	let
	    from_here = case from_source of { 0 -> False; 1 -> True }
	    (ss, bs)  = sepDeclsIntoSigsAndBinds binding
	    binds     = cvMonoBinds sf bs
	    uprags    = concat (map cvInstDeclSig ss)
	    ctxt_inst_ty = HsPreForAllTy ctxt inst_ty
	in
	returnUgn (RdrInstDecl
          (InstDecl clas ctxt_inst_ty binds from_here orig_mod uprags pragma src_loc))

      U_dbind dbindts srcline -> -- "default" declaration
	wlkList rdMonoType dbindts  `thenUgn` \ tys ->
	mkSrcLocUgn        srcline  `thenUgn` \ src_loc ->
	returnUgn (RdrDefaultDecl (DefaultDecl tys src_loc))

      U_mbind mod mbindimp srcline ->
	-- "import" declaration in an interface
	wlkList rdEntity   mbindimp	`thenUgn` \ entities  ->
	mkSrcLocUgn	   srcline	`thenUgn` \ src_loc   ->
	returnUgn (RdrIfaceImportDecl (IfaceImportDecl mod entities src_loc))

      U_mfbind fixes ->
	-- "infix" declarations in an interface
	wlkList rdFixOp fixes 		`thenUgn` \ fixities  ->
	returnUgn (RdrIfaceFixities fixities)

      a_sig_we_hope ->
	-- signature(-like) things, including user pragmas
	wlk_sig_thing a_sig_we_hope
\end{code}

\begin{code}
wlkDerivings :: U_maybe -> UgnM (Maybe [ProtoName])

wlkDerivings (U_nothing) = returnUgn Nothing
wlkDerivings (U_just pt)
  = rdU_list pt		 `thenUgn` \ ds	    ->
    wlkList rdQid ds	 `thenUgn` \ derivs ->
    returnUgn (Just derivs)
\end{code}

\begin{code}
wlk_sig_thing (U_sbind sbindids sbindid srcline spragma)  -- type signature
  = wlkList rdQid	sbindids `thenUgn` \ vars    ->
    wlkPolyType		sbindid  `thenUgn` \ poly_ty ->
    wlkTySigPragmas	spragma  `thenUgn` \ pragma  ->
    mkSrcLocUgn		srcline  `thenUgn` \ src_loc ->
    returnUgn (RdrTySig vars poly_ty pragma src_loc)

wlk_sig_thing (U_vspec_uprag uvar vspec_tys srcline) -- value specialisation user-pragma
  = wlkQid  uvar		    `thenUgn` \ var ->
    wlkList rd_ty_and_id vspec_tys  `thenUgn` \ tys_and_ids ->
    mkSrcLocUgn	         srcline    `thenUgn` \ src_loc ->
    returnUgn (RdrSpecValSig [SpecSig var ty using_id src_loc
			     | (ty, using_id) <- tys_and_ids ])
  where
    rd_ty_and_id :: ParseTree -> UgnM (ProtoNamePolyType, Maybe ProtoName)
    rd_ty_and_id pt
      = rdU_binding pt		`thenUgn` \ (U_vspec_ty_and_id vspec_ty vspec_id) ->
	wlkPolyType vspec_ty	`thenUgn` \ ty	     ->
	wlkMaybe rdQid vspec_id	`thenUgn` \ id_maybe ->
	returnUgn(ty, id_maybe)

wlk_sig_thing (U_ispec_uprag iclas ispec_ty srcline)-- instance specialisation user-pragma
  = wlkQid	iclas		`thenUgn` \ clas    ->
    wlkMonoType ispec_ty	`thenUgn` \ ty	    ->
    mkSrcLocUgn srcline		`thenUgn` \ src_loc ->
    returnUgn (RdrSpecInstSig (SpecInstSig clas ty src_loc))

wlk_sig_thing (U_inline_uprag ivar srcline) -- value inlining user-pragma
  = wlkQid	ivar		`thenUgn` \ var     ->
    mkSrcLocUgn	srcline      	`thenUgn` \ src_loc ->
    returnUgn (RdrInlineValSig (InlineSig var src_loc))

wlk_sig_thing (U_deforest_uprag ivar srcline) -- "deforest me" user-pragma
  = wlkQid	ivar		`thenUgn` \ var     ->
    mkSrcLocUgn srcline		`thenUgn` \ src_loc ->
    returnUgn (RdrDeforestSig (DeforestSig var src_loc))

wlk_sig_thing (U_magicuf_uprag ivar str srcline) -- "magic" unfolding user-pragma
  = wlkQid	ivar		`thenUgn` \ var     ->
    mkSrcLocUgn srcline		`thenUgn` \ src_loc ->
    returnUgn (RdrMagicUnfoldingSig (MagicUnfoldingSig var str src_loc))

wlk_sig_thing (U_dspec_uprag itycon dspec_tys srcline)
  = wlkQid	itycon		 `thenUgn` \ tycon   ->
    mkSrcLocUgn srcline		 `thenUgn` \ src_loc ->
    wlkList rdMonoType dspec_tys `thenUgn` \ tys     ->
    let
	spec_ty = MonoTyApp tycon tys
    in
    returnUgn (RdrSpecDataSig (SpecDataSig tycon spec_ty src_loc))
\end{code}

%************************************************************************
%*									*
\subsection[wlkTypes]{Reading in types in various forms (and data constructors)}
%*									*
%************************************************************************

\begin{code}
rdPolyType :: ParseTree -> UgnM ProtoNamePolyType
rdMonoType :: ParseTree -> UgnM ProtoNameMonoType

rdPolyType pt = rdU_ttype pt `thenUgn` \ ttype -> wlkPolyType ttype
rdMonoType pt = rdU_ttype pt `thenUgn` \ ttype -> wlkMonoType ttype

wlkPolyType :: U_ttype -> UgnM ProtoNamePolyType
wlkMonoType :: U_ttype -> UgnM ProtoNameMonoType

wlkPolyType ttype
  = case ttype of
{-LATER:
      U_uniforall utvs uty -> -- forall type (pragmas)
	wlkList rdU_unkId utvs  `thenUgn` \ tvs ->
	wlkMonoType       uty	`thenUgn` \ ty  ->
	returnUgn (HsForAllTy tvs ty)
-}

      U_context tcontextl tcontextt -> -- context
	wlkContext  tcontextl	`thenUgn` \ ctxt ->
	wlkMonoType tcontextt	`thenUgn` \ ty	 ->
	returnUgn (HsPreForAllTy ctxt ty)

      other -> -- something else
	wlkMonoType other   `thenUgn` \ ty ->
	returnUgn (HsPreForAllTy [{-no context-}] ty)

wlkMonoType ttype
  = case ttype of
      U_namedtvar tyvar -> -- type variable
	returnUgn (MonoTyVar tyvar)

      U_tname tcon -> -- type constructor
	wlkQid tcon	`thenUgn` \ tycon ->
	returnUgn (MonoTyApp tycon [])

      U_tapp t1 t2 ->
	wlkMonoType t2		`thenUgn` \ ty2 ->
	collect t1 [ty2]	`thenUgn` \ (tycon, tys) ->
	returnUgn (MonoTyApp tycon tys)
       where
	collect t acc
	  = case t of
	      U_tapp t1 t2 -> wlkMonoType t2	`thenUgn` \ ty2 ->
			      collect t1 (ty2:acc)
	      U_tname tcon -> wlkQid tcon	`thenUgn` \ tycon  ->
			      returnUgn (tycon, acc)
	      U_namedtvar tv -> returnUgn (tv, acc)
	      U_tllist _ -> panic "tlist"
	      U_ttuple _ -> panic "ttuple"
	      U_tfun _ _ -> panic "tfun"
	      U_tbang _ -> panic "tbang"
	      U_context _ _ -> panic "context"
	      _ -> panic "something else"
	      
      U_tllist tlist -> -- list type
	wlkMonoType tlist	`thenUgn` \ ty ->
	returnUgn (MonoListTy ty)

      U_ttuple ttuple ->
	wlkList rdMonoType ttuple `thenUgn` \ tys ->
	returnUgn (MonoTupleTy tys)

      U_tfun tfun targ ->
	wlkMonoType tfun	`thenUgn` \ ty1 ->
	wlkMonoType targ	`thenUgn` \ ty2 ->
	returnUgn (MonoFunTy ty1 ty2)

      U_unidict uclas t -> -- DictTy (pragmas)
	wlkQid uclas	`thenUgn` \ clas ->
	wlkMonoType t	`thenUgn` \ ty	 ->
	returnUgn (MonoDictTy clas ty)
\end{code}

\begin{code}
wlkTyConAndTyVars :: U_ttype -> UgnM (ProtoName, [ProtoName])
wlkContext   	  :: U_list  -> UgnM ProtoNameContext
wlkClassAssertTy  :: U_ttype -> UgnM (ProtoName, ProtoName)

wlkTyConAndTyVars ttype
  = wlkMonoType ttype	`thenUgn` \ (MonoTyApp tycon ty_args) ->
    let
	args = [ a | (MonoTyVar a) <- ty_args ]
    in
    returnUgn (tycon, args)

wlkContext list
  = wlkList rdMonoType list `thenUgn` \ tys ->
    returnUgn (map mk_class_assertion tys)

wlkClassAssertTy xs
  = wlkMonoType xs   `thenUgn` \ mono_ty ->
    returnUgn (mk_class_assertion mono_ty)

mk_class_assertion :: ProtoNameMonoType -> (ProtoName, ProtoName)

mk_class_assertion (MonoTyApp name [(MonoTyVar tyname)]) = (name, tyname)
mk_class_assertion other
  = pprError "ERROR: malformed type context: " (ppr PprForUser other)
    -- regrettably, the parser does let some junk past
    -- e.g., f :: Num {-nothing-} => a -> ...
\end{code}

\begin{code}
rdConDecl :: ParseTree -> UgnM ProtoNameConDecl
rdConDecl pt
  = rdU_constr pt    `thenUgn` \ blah ->
    wlkConDecl blah

wlkConDecl :: U_constr -> UgnM ProtoNameConDecl

wlkConDecl (U_constrpre ccon ctys srcline)
  = mkSrcLocUgn srcline		`thenUgn` \ src_loc ->
    wlkQid	ccon		`thenUgn` \ con	    ->
    wlkList     rdBangType ctys	`thenUgn` \ tys	    ->
    returnUgn (ConDecl con tys src_loc)

wlkConDecl (U_constrinf cty1 cop cty2 srcline)
  = mkSrcLocUgn srcline		`thenUgn` \ src_loc ->
    wlkBangType cty1		`thenUgn` \ ty1	    ->
    wlkQid	cop		`thenUgn` \ op	    ->
    wlkBangType cty2		`thenUgn` \ ty2	    ->
    returnUgn (ConOpDecl ty1 op ty2 src_loc)

wlkConDecl (U_constrnew ccon cty srcline)
  = mkSrcLocUgn srcline		`thenUgn` \ src_loc ->
    wlkQid	ccon		`thenUgn` \ con	    ->
    wlkMonoType cty		`thenUgn` \ ty	    ->
    returnUgn (NewConDecl con ty src_loc)

wlkConDecl (U_constrrec ccon cfields srcline)
  = mkSrcLocUgn srcline		`thenUgn` \ src_loc      ->
    wlkQid	ccon		`thenUgn` \ con		 ->
    wlkList rd_field cfields	`thenUgn` \ fields_lists ->
    returnUgn (RecConDecl con fields_lists src_loc)
  where
    rd_field :: ParseTree -> UgnM ([ProtoName], BangType ProtoName)
    rd_field pt
      = rdU_constr pt		`thenUgn` \ (U_field fvars fty) ->
	wlkList rdQid	fvars	`thenUgn` \ vars ->
	wlkBangType fty		`thenUgn` \ ty ->
	returnUgn (vars, ty)

-----------------
rdBangType pt = rdU_ttype pt `thenUgn` \ ty -> wlkBangType ty

wlkBangType :: U_ttype -> UgnM (BangType ProtoName)

wlkBangType (U_tbang bty) = wlkMonoType bty `thenUgn` \ ty -> returnUgn (Banged   ty)
wlkBangType uty		  = wlkMonoType uty `thenUgn` \ ty -> returnUgn (Unbanged ty)

\end{code}

%************************************************************************
%*									*
\subsection{Read a ``match''}
%*									*
%************************************************************************

\begin{code}
rdMatch :: ParseTree -> UgnM RdrMatch

rdMatch pt
  = rdU_pbinding pt `thenUgn` \ (U_pgrhs gpat gdexprs gbind gsrcfun srcline) ->

    wlkPat		gpat	`thenUgn` \ pat     ->
    wlkBinding		gbind	`thenUgn` \ binding ->
    wlkQid		gsrcfun	`thenUgn` \ srcfun  ->
    let
	wlk_guards (U_pnoguards exp)
	  = wlkExpr exp `thenUgn` \ expr ->
	    returnUgn (RdrMatch_NoGuard srcline srcfun pat expr binding)

	wlk_guards (U_pguards gs)
	  = wlkList rd_gd_expr gs   `thenUgn` \ gd_exps ->
	    returnUgn (RdrMatch_Guards  srcline srcfun pat gd_exps binding)
    in
    wlk_guards gdexprs
  where
    rd_gd_expr pt
      = rdU_pbinding pt `thenUgn` \ (U_pgdexp g e) ->
	wlkExpr      g  `thenUgn` \ guard ->
	wlkExpr	     e  `thenUgn` \ expr  ->
	returnUgn (guard, expr)
\end{code}

%************************************************************************
%*									*
\subsection[rdFixOp]{Read in a fixity declaration}
%*									*
%************************************************************************

\begin{code}
rdFixOp :: ParseTree -> UgnM ProtoNameFixityDecl
rdFixOp pt 
  = rdU_tree pt `thenUgn` \ fix ->
    case fix of
      U_fixop op (-1) prec -> returnUgn (InfixL op prec)
      U_fixop op   0  prec -> returnUgn (InfixN op prec)
      U_fixop op   1  prec -> returnUgn (InfixR op prec)
      _ -> error "ReadPrefix:rdFixOp"
\end{code}

%************************************************************************
%*									*
\subsection[rdImportedInterface]{Read an imported interface}
%*									*
%************************************************************************

\begin{code}
rdImportedInterface :: ParseTree
		    -> UgnM ProtoNameImportedInterface

rdImportedInterface pt
  = rdU_binding pt
	`thenUgn` \ (U_import ifname iffile binddef imod iqual ias ispec srcline) ->

    mkSrcLocUgn	srcline 		`thenUgn` \ src_loc   	->
    wlkMaybe rdU_stringId ias		`thenUgn` \ maybe_as	->
    wlkMaybe rd_spec ispec		`thenUgn` \ maybe_spec	->

    setSrcFileUgn iffile ( -- looking inside the .hi file...
	wlkBinding binddef
    )				`thenUgn` \ iface_bs  ->

    case (sepDeclsForInterface iface_bs) of {
	(tydecls,classdecls,instdecls,sigs,iimpdecls,ifixities) ->
    let
	cv_sigs  = concat (map cvValSig sigs)

	cv_iface = Interface ifname iimpdecls ifixities
			tydecls	classdecls instdecls cv_sigs
			src_loc

	cv_qual = case iqual of {0 -> False; 1 -> True}
    in
    returnUgn (ImportMod cv_iface cv_qual maybe_as maybe_spec)
    }
  where
    rd_spec pt = rdU_either pt 		`thenUgn` \ spec ->
      case spec of
	U_left pt  -> rdEntities pt	`thenUgn` \ ents ->
		      returnUgn (False, ents)
	U_right pt -> rdEntities pt 	`thenUgn` \ ents ->
		      returnUgn (True, ents)
\end{code}

\begin{code}
rdEntities pt
  = rdU_list pt		    `thenUgn` \ list ->
    wlkList rdEntity list

rdEntity :: ParseTree -> UgnM (IE ProtoName)

rdEntity pt
  = rdU_entidt pt `thenUgn` \ entity ->
    case entity of
      U_entid evar -> 		-- just a value
	wlkQid	evar		`thenUgn` \ var ->
	returnUgn (IEVar var)

      U_enttype x -> 		-- abstract type constructor/class
	wlkQid	x		`thenUgn` \ thing ->
	returnUgn (IEThingAbs thing)

      U_enttypeall x -> 	-- non-abstract type constructor/class
	wlkQid	x		`thenUgn` \ thing ->
	returnUgn (IEThingAll thing)

      U_enttypenamed x ns -> 	-- non-abstract type constructor/class
				-- with specified constrs/methods
	wlkQid	x		`thenUgn` \ thing ->
	wlkList rdQid ns	`thenUgn` \ names -> 
	returnUgn (IEThingAll thing)
	-- returnUgn (IEThingWith thing names)

      U_entmod mod -> -- everything provided by a module
	returnUgn (IEModuleContents mod)
\end{code}

