%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section{Read parse tree built by Yacc parser}

\begin{code}
module ReadPrefix ( rdModule )  where

#include "HsVersions.h"

import UgenAll		-- all Yacc parser gumpff...
import PrefixSyn	-- and various syntaxen.
import HsSyn
import HsTypes		( HsTyVar(..) )
import HsPragmas	( noDataPragmas, noClassPragmas, noInstancePragmas, noGenPragmas )
import RdrHsSyn         
import BasicTypes	( Fixity(..), FixityDirection(..), NewOrData(..), IfaceFlavour(..) )
import PrefixToHs
import CallConv

import CmdLineOpts      ( opt_NoImplicitPrelude )
import FiniteMap	( elemFM, FiniteMap )
import Name		( OccName(..), Module )
import Lex		( isLexConId )
import Outputable
import PrelMods		( pRELUDE )
import Util		( nOfThem )
import FastString	( mkFastCharString )
import IO 		( hPutStr, stderr )
import PrelRead		( readRational__ )
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
wlkTvId   = wlkQid TvOcc
wlkTCId   = wlkQid TCOcc
wlkVarId  = wlkQid VarOcc
wlkDataId = wlkQid VarOcc
wlkEntId = wlkQid (\occ -> if isLexConId occ
			   then TCOcc occ
			   else VarOcc occ)

wlkQid	:: (FAST_STRING -> OccName) -> U_qid -> UgnM RdrName

-- There are three kinds of qid:
--	qualified name (aqual)		A.x
--	unqualified name (noqual)	x
--	special name (gid)		[], (), ->, (,,,)
-- The special names always mean "Prelude.whatever"; that's why
-- they are distinct.  So if you write "()", it's just as if  you
-- had written "Prelude.()".  
-- NB: The (qualified) prelude is always in scope, so the renamer will find it.

-- EXCEPT: when we're compiling with -fno-implicit-prelude, in which
-- case we need to unqualify these things. -- SDM.

wlkQid mk_occ_name (U_noqual name)
  = returnUgn (Unqual (mk_occ_name name))
wlkQid mk_occ_name (U_aqual  mod name)
  = returnUgn (Qual mod (mk_occ_name name) HiFile)
wlkQid mk_occ_name (U_gid n name)
  | opt_NoImplicitPrelude 
	= returnUgn (Unqual (mk_occ_name name))
  | otherwise
	= returnUgn (Qual pRELUDE (mk_occ_name name) HiFile)

rdTCId  pt = rdU_qid pt `thenUgn` \ qid -> wlkTCId qid
rdVarId pt = rdU_qid pt `thenUgn` \ qid -> wlkVarId qid

cvFlag :: U_long -> Bool
cvFlag 0 = False
cvFlag 1 = True
\end{code}

%************************************************************************
%*									*
\subsection[rdModule]{@rdModule@: reads in a Haskell module}
%*									*
%************************************************************************

\begin{code}
rdModule :: IO (Module,		    -- this module's name
	        RdrNameHsModule)    -- the main goods

rdModule
  = _ccall_ hspmain 	>>= \ pt -> -- call the Yacc parser!
    let
	srcfile  = mkFastCharString ``input_filename'' -- What A Great Hack! (TM)
    in
    initUgn 		  $
    rdU_tree pt `thenUgn` \ (U_hmodule modname himplist hexplist hfixlist
				       hmodlist srciface_version srcline) ->

    setSrcFileUgn srcfile $
    setSrcModUgn  modname $
    mkSrcLocUgn srcline	  $		   \ src_loc	->

    wlkMaybe rdEntities	hexplist `thenUgn` \ exports	->
    wlkList  rdImport   himplist `thenUgn` \ imports	->
    wlkList  rdFixOp	hfixlist `thenUgn` \ fixities 	->
    wlkBinding		hmodlist `thenUgn` \ binding	->

    let
	val_decl    = ValD (cvBinds srcfile cvValSig binding)
	for_decls   = cvForeignDecls binding
	other_decls = cvOtherDecls binding
    in
    returnUgn (modname,
       	       HsModule modname
	      		  (case srciface_version of { 0 -> Nothing; n -> Just n })
	      		  exports
	      		  imports
	      		  fixities
			  (for_decls ++ val_decl: other_decls)
	      		  src_loc
       		)
\end{code}

%************************************************************************
%*									*
\subsection[wlkExprOrPat]{@wlkExpr@ and @wlkPat@}
%*									*
%************************************************************************

\begin{code}
rdExpr :: ParseTree -> UgnM RdrNameHsExpr
rdPat  :: ParseTree -> UgnM RdrNamePat

rdExpr pt = rdU_tree pt `thenUgn` \ tree -> wlkExpr tree
rdPat  pt = rdU_tree pt `thenUgn` \ tree -> wlkPat  tree

wlkExpr :: U_tree -> UgnM RdrNameHsExpr
wlkPat  :: U_tree -> UgnM RdrNamePat

wlkExpr expr
  = case expr of
      U_par pexpr -> -- parenthesised expr
	wlkExpr pexpr	`thenUgn` \ expr ->
	returnUgn (HsPar expr)

      U_lsection lsexp lop -> -- left section
	wlkExpr lsexp	`thenUgn` \ expr ->
	wlkVarId  lop	`thenUgn` \ op   ->
	returnUgn (SectionL expr (HsVar op))

      U_rsection rop rsexp -> -- right section
	wlkVarId  rop	`thenUgn` \ op   ->
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
	mkSrcLocUgn   srcline		$ \ src_loc ->
	wlkList rdPat lampats	`thenUgn` \ pats ->
	wlkExpr       lamexpr	`thenUgn` \ body ->
	returnUgn (
	    HsLam (foldr PatMatch
			 (GRHSMatch (GRHSsAndBindsIn
				      (unguardedRHS body src_loc)
				      EmptyBinds))
			 pats)
	)

      U_casee caseexpr casebody srcline ->	-- case expression
	mkSrcLocUgn srcline	 	 $ \ src_loc ->
	wlkExpr	        caseexpr `thenUgn` \ expr ->
	wlkList rdMatch casebody `thenUgn` \ mats ->
	getSrcFileUgn		 `thenUgn` \ sf ->
	let
	    matches = cvMatches sf True mats
	in
	returnUgn (HsCase expr matches src_loc)

      U_ife ifpred ifthen ifelse srcline ->	-- if expression
	mkSrcLocUgn srcline	        $ \ src_loc ->
	wlkExpr ifpred		`thenUgn` \ e1 ->
	wlkExpr ifthen		`thenUgn` \ e2 ->
	wlkExpr ifelse		`thenUgn` \ e3 ->
	returnUgn (HsIf e1 e2 e3 src_loc)

      U_let letvdefs letvexpr ->		-- let expression
	wlkBinding letvdefs	`thenUgn` \ binding ->
	wlkExpr    letvexpr	`thenUgn` \ expr    ->
	getSrcFileUgn		`thenUgn` \ sf	    ->
	let
	    binds = cvBinds sf cvValSig binding
	in
	returnUgn (HsLet binds expr)

      U_doe gdo srcline ->			-- do expression
	mkSrcLocUgn srcline		$ \ src_loc ->
	wlkList rd_stmt gdo	`thenUgn` \ stmts ->
	returnUgn (HsDo DoStmt stmts src_loc)
        where
	rd_stmt pt
	  = rdU_tree pt `thenUgn` \ bind ->
	    case bind of
	      U_doexp exp srcline ->
		mkSrcLocUgn srcline		$ \ src_loc ->
		wlkExpr exp 		`thenUgn` \ expr ->
		returnUgn (ExprStmt expr src_loc)

	      U_dobind pat exp srcline ->
		mkSrcLocUgn srcline		$ \ src_loc ->
		wlkPat  pat		`thenUgn` \ patt ->
		wlkExpr exp 		`thenUgn` \ expr ->
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
	wlkQuals cquals 	`thenUgn` \ quals ->
	getSrcLocUgn 		`thenUgn` \ loc ->
	returnUgn (HsDo ListComp (quals ++ [ReturnStmt expr]) loc)

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
	wlkHsType restrt	`thenUgn` \ ty	 ->
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
	wlkVarId n	`thenUgn` \ var ->
	returnUgn (HsVar var)

      U_ap fun arg -> 			-- application
	wlkExpr fun	`thenUgn` \ expr1 ->
	wlkExpr arg	`thenUgn` \ expr2 ->
	returnUgn (HsApp expr1 expr2)

      U_infixap fun arg1 arg2 ->	-- infix application
	wlkVarId  fun	`thenUgn` \ op    ->
	wlkExpr arg1	`thenUgn` \ expr1 ->
	wlkExpr arg2	`thenUgn` \ expr2 ->
	returnUgn (mkOpApp expr1 op expr2)

      U_negate nexp ->	 		-- prefix negation
	wlkExpr nexp	`thenUgn` \ expr ->
	returnUgn (NegApp expr (HsVar dummyRdrVarName))

      U_llist llist -> -- explicit list
	wlkList rdExpr llist `thenUgn` \ exprs ->
	returnUgn (ExplicitList exprs)

      U_tuple tuplelist -> -- explicit tuple
	wlkList rdExpr tuplelist `thenUgn` \ exprs ->
	returnUgn (ExplicitTuple exprs)

      U_record con rbinds -> -- record construction
	wlkDataId  con		`thenUgn` \ rcon     ->
	wlkList rdRbind rbinds	`thenUgn` \ recbinds ->
	returnUgn (RecordCon rcon (HsVar rcon) recbinds)

      U_rupdate updexp updbinds -> -- record update
	wlkExpr updexp		 `thenUgn` \ aexp ->
	wlkList rdRbind updbinds `thenUgn` \ recbinds ->
	returnUgn (RecordUpd aexp recbinds)

#ifdef DEBUG
      U_hmodule _ _ _ _ _ _ _ -> error "U_hmodule"
      U_as _ _ 		      -> error "U_as"
      U_lazyp _ 	      -> error "U_lazyp"
      U_wildp 		      -> error "U_wildp"
      U_qual _ _ 	      -> error "U_qual"
      U_guard _ 	      -> error "U_guard"
      U_seqlet _ 	      -> error "U_seqlet"
      U_dobind _ _ _ 	      -> error "U_dobind"
      U_doexp _ _	      -> error "U_doexp"
      U_rbind _ _	      -> error "U_rbind"
      U_fixop _ _ _ _	      -> error "U_fixop"
#endif

rdRbind pt
  = rdU_tree pt		`thenUgn` \ (U_rbind var exp) ->
    wlkVarId   var	`thenUgn` \ rvar ->
    wlkMaybe rdExpr exp	`thenUgn` \ expr_maybe ->
    returnUgn (
      case expr_maybe of
	Nothing -> (rvar, HsVar rvar, True{-pun-})
	Just re -> (rvar, re,	      False)
    )

wlkQuals cquals
  = wlkList rd_qual cquals
  where
	  rd_qual pt
	    = rdU_tree pt	`thenUgn` \ qual ->
	      wlk_qual qual

	  wlk_qual qual
	    = case qual of
		U_guard exp ->
		  wlkExpr exp  	`thenUgn` \ expr ->
		  getSrcLocUgn 	`thenUgn` \ loc ->
		  returnUgn (GuardStmt expr loc)

		U_qual qpat qexp ->
		  wlkPat  qpat  `thenUgn` \ pat  ->
		  wlkExpr qexp  `thenUgn` \ expr ->
		  getSrcLocUgn 	`thenUgn` \ loc ->
		  returnUgn (BindStmt pat expr loc)

		U_seqlet seqlet ->
		  wlkBinding seqlet	`thenUgn` \ bs ->
		  getSrcFileUgn		`thenUgn` \ sf ->
		  let
		      binds = cvBinds sf cvValSig bs
		  in
		  returnUgn (LetStmt binds)
	        U_let letvdefs letvexpr -> 
		    wlkBinding letvdefs	`thenUgn` \ binding ->
		    wlkExpr    letvexpr	`thenUgn` \ expr    ->
		    getSrcLocUgn 	`thenUgn` \ loc ->
		    getSrcFileUgn	`thenUgn` \ sf	    ->
		    let
		     binds = cvBinds sf cvValSig binding
		    in
		    returnUgn (GuardStmt (HsLet binds expr) loc)
\end{code}

Patterns: just bear in mind that lists of patterns are represented as
a series of ``applications''.
\begin{code}
wlkPat pat
  = case pat of
      U_par ppat -> 			-- parenthesised pattern
	wlkPat ppat	`thenUgn` \ pat ->
	-- tidy things up a little:
	returnUgn (
	case pat of
	  VarPatIn _ -> pat
	  WildPatIn  -> pat
	  other	     -> ParPatIn pat
	)

      U_as avar as_pat -> 		-- "as" pattern
	wlkVarId avar	`thenUgn` \ var ->
	wlkPat as_pat	`thenUgn` \ pat ->
	returnUgn (AsPatIn var pat)

      U_lazyp lazyp -> 			-- irrefutable ("twiddle") pattern
	wlkPat lazyp	`thenUgn` \ pat ->
	returnUgn (LazyPatIn pat)

      U_plusp avar lit ->
	wlkVarId avar	`thenUgn` \ var ->
	wlkLiteral lit	`thenUgn` \ lit ->
	returnUgn (NPlusKPatIn var lit)

      U_wildp -> returnUgn WildPatIn 	-- wildcard pattern

      U_lit lit ->			-- literal pattern
	wlkLiteral lit	`thenUgn` \ lit ->
	returnUgn (LitPatIn lit)

      U_ident nn ->			-- simple identifier
	wlkVarId nn	`thenUgn` \ n ->
	returnUgn (
	  case rdrNameOcc n of
		VarOcc occ | isLexConId occ -> ConPatIn n []
		other     		    -> VarPatIn n
	)

      U_ap l r ->	-- "application": there's a list of patterns lurking here!
	wlkPat r	  	`thenUgn` \ rpat	 ->
	collect_pats l [rpat] 	`thenUgn` \ (lpat,lpats) ->
	(case lpat of
	    VarPatIn x          -> returnUgn (x,  lpats)
	    ConPatIn x []       -> returnUgn (x,  lpats)
	    ConOpPatIn x op _ y -> returnUgn (op, x:y:lpats)
	    _ -> getSrcLocUgn 	`thenUgn` \ loc ->
		 pprPanic "Illegal pattern `application'"
			  (ppr loc <> colon <+> hsep (map ppr (lpat:lpats)))

	)			`thenUgn` \ (n, arg_pats) ->
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

      U_infixap fun arg1 arg2 ->	-- infix pattern
	wlkVarId fun	`thenUgn` \ op   ->
	wlkPat arg1	`thenUgn` \ pat1 ->
	wlkPat arg2	`thenUgn` \ pat2 ->
	returnUgn (ConOpPatIn pat1 op (error "ConOpPatIn:fixity") pat2)

      U_negate npat ->	 		-- negated pattern
	wlkPat npat	`thenUgn` \ pat ->
        returnUgn (NegPatIn pat)

      U_llist llist -> 			-- explicit list
	wlkList rdPat llist 	`thenUgn` \ pats ->
	returnUgn (ListPatIn pats)

      U_tuple tuplelist -> 		-- explicit tuple
	wlkList rdPat tuplelist	`thenUgn` \ pats ->
	returnUgn (TuplePatIn pats)

      U_record con rpats -> 		-- record destruction
	wlkDataId  con		`thenUgn` \ rcon     ->
	wlkList rdRpat rpats	`thenUgn` \ recpats ->
	returnUgn (RecPatIn rcon recpats)
	where
	  rdRpat pt
	    = rdU_tree pt	 `thenUgn` \ (U_rbind var pat) ->
    	      wlkVarId   var	 `thenUgn` \ rvar ->
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
      U_integer    s -> HsInt	     (as_integer  s)
      U_floatr     s -> HsFrac       (as_rational s)
      U_intprim    s -> HsIntPrim    (as_integer  s)
      U_doubleprim s -> HsDoublePrim (as_rational s)
      U_floatprim  s -> HsFloatPrim  (as_rational s)
      U_charr	   s -> HsChar       (as_char     s)
      U_charprim   s -> HsCharPrim   (as_char     s)
      U_string     s -> HsString     (as_string   s)
      U_stringprim s -> HsStringPrim (as_string   s)
      U_clitlit    s -> HsLitLit     (as_string   s)
    )
  where
    as_char s     = _HEAD_ s
    as_integer s  = readInteger (_UNPK_ s)
    as_rational s = readRational__ (_UNPK_ s) -- use non-std readRational__ 
					      -- to handle rationals with leading '-'
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
	-- null binding
      U_nullbind ->
	returnUgn RdrNullBind

	-- "and" binding (just glue, really)
      U_abind a b ->
	wlkBinding a    `thenUgn` \ binding1 ->
	wlkBinding b    `thenUgn` \ binding2 ->
	returnUgn (RdrAndBindings binding1 binding2)

	-- "data" declaration
      U_tbind tctxt ttype tcons tderivs srcline ->
	mkSrcLocUgn	   srcline  	    $ \ src_loc	    ->
	wlkContext	   tctxt    `thenUgn` \ ctxt	    ->
	wlkConAndTyVars    ttype    `thenUgn` \ (tycon, tyvars) ->
	wlkList rdConDecl  tcons    `thenUgn` \ cons	    ->
	wlkDerivings	   tderivs  `thenUgn` \ derivings   ->
	returnUgn (RdrTyDecl (TyData DataType ctxt tycon tyvars cons derivings noDataPragmas src_loc))

	-- "newtype" declaration
      U_ntbind ntctxt nttype ntcon ntderivs srcline ->
	mkSrcLocUgn	   srcline  	    $ \ src_loc	    ->
	wlkContext	   ntctxt   `thenUgn` \ ctxt	    ->
	wlkConAndTyVars    nttype   `thenUgn` \ (tycon, tyvars) ->
	wlkList rdConDecl  ntcon    `thenUgn` \ cons	    ->
	wlkDerivings	   ntderivs `thenUgn` \ derivings   ->
	returnUgn (RdrTyDecl (TyData NewType ctxt tycon tyvars cons derivings noDataPragmas src_loc))

	-- "type" declaration
      U_nbind nbindid nbindas srcline -> 		
	mkSrcLocUgn	  srcline 	  $ \ src_loc	    ->
	wlkConAndTyVars   nbindid `thenUgn` \ (tycon, tyvars) ->
	wlkMonoType	  nbindas `thenUgn` \ expansion	    ->
	returnUgn (RdrTyDecl (TySynonym tycon tyvars expansion src_loc))

	-- function binding
      U_fbind fbindl srcline ->
	mkSrcLocUgn     srcline		$ \ src_loc ->
	wlkList rdMatch fbindl	`thenUgn` \ matches ->
	returnUgn (RdrFunctionBinding srcline matches)

	-- pattern binding
      U_pbind pbindl srcline ->
	mkSrcLocUgn     srcline		$ \ src_loc ->
	wlkList rdMatch pbindl	`thenUgn` \ matches ->
	returnUgn (RdrPatternBinding srcline matches)

 	-- "class" declaration
      U_cbind cbindc cbindid cbindw srcline ->
	mkSrcLocUgn	 srcline 	$ \ src_loc	    ->
	wlkContext	 cbindc	 `thenUgn` \ ctxt	    ->
	wlkConAndTyVars  cbindid `thenUgn` \ (clas, tyvars) ->
	wlkBinding	 cbindw	 `thenUgn` \ binding	    ->
	getSrcFileUgn		 `thenUgn` \ sf		    ->
	let
	    (final_methods, final_sigs) = cvMonoBindsAndSigs sf cvClassOpSig binding
	in
	returnUgn (RdrClassDecl
	  (mkClassDecl ctxt clas tyvars final_sigs final_methods noClassPragmas src_loc))

	-- "instance" declaration
      U_ibind ty ibindw srcline ->
	-- The "ty" contains the instance context too
	-- So for "instance Eq a => Eq [a]" the type will be
	--	Eq a => Eq [a]
	mkSrcLocUgn	srcline		$ \ src_loc ->
	wlkInstType       ty		`thenUgn` \ inst_ty    ->
	wlkBinding	ibindw		`thenUgn` \ binding ->
	getSrcModUgn			`thenUgn` \ modname ->
	getSrcFileUgn			`thenUgn` \ sf	    ->
	let
	    (binds,uprags) = cvMonoBindsAndSigs sf cvInstDeclSig binding
	in
	returnUgn (RdrInstDecl
          (InstDecl inst_ty binds uprags Nothing {- No dfun id -} src_loc))

	-- "default" declaration
      U_dbind dbindts srcline ->
	mkSrcLocUgn        srcline  	$ \ src_loc ->
	wlkList rdMonoType dbindts  `thenUgn` \ tys ->
	returnUgn (RdrDefaultDecl (DefaultDecl tys src_loc))

        -- "foreign" declaration
      U_fobind id ty ext_name unsafe_flag cconv imp_exp srcline ->
	  mkSrcLocUgn        srcline		   $ \ src_loc ->
	  wlkVarId id				   `thenUgn` \ h_id ->
	  wlkHsType ty				   `thenUgn` \ h_ty ->
	  wlkExtName ext_name			   `thenUgn` \ h_ext_name ->
	  rdCallConv cconv		           `thenUgn` \ h_cconv ->
	  rdImpExp imp_exp (cvFlag unsafe_flag)    `thenUgn` \ h_imp_exp ->
 	  returnUgn (RdrForeignDecl (ForeignDecl h_id h_imp_exp h_ty h_ext_name h_cconv src_loc))

      a_sig_we_hope ->
	-- signature(-like) things, including user pragmas
	wlk_sig_thing a_sig_we_hope
\end{code}

\begin{code}
wlkDerivings :: U_maybe -> UgnM (Maybe [RdrName])

wlkDerivings (U_nothing) = returnUgn Nothing
wlkDerivings (U_just pt)
  = rdU_list pt		 `thenUgn` \ ds	    ->
    wlkList rdTCId ds	 `thenUgn` \ derivs ->
    returnUgn (Just derivs)
\end{code}

\begin{code}
	-- type signature
wlk_sig_thing (U_sbind sbindids sbindid srcline)
  = mkSrcLocUgn		srcline  	$ \ src_loc ->
    wlkList rdVarId	sbindids `thenUgn` \ vars    ->
    wlkHsType		sbindid  `thenUgn` \ poly_ty ->
    returnUgn (foldr1 RdrAndBindings [RdrSig (Sig var poly_ty src_loc) | var <- vars])

	-- value specialisation user-pragma
wlk_sig_thing (U_vspec_uprag uvar vspec_tys srcline)
  = mkSrcLocUgn	srcline		    	    $ \ src_loc ->
    wlkVarId  uvar		    `thenUgn` \ var ->
    wlkList rd_ty_and_id vspec_tys  `thenUgn` \ tys_and_ids ->
    returnUgn (foldr1 RdrAndBindings [ RdrSig (SpecSig var ty using_id src_loc)
				     | (ty, using_id) <- tys_and_ids ])
  where
    rd_ty_and_id :: ParseTree -> UgnM (RdrNameHsType, Maybe RdrName)
    rd_ty_and_id pt
      = rdU_binding pt		`thenUgn` \ (U_vspec_ty_and_id vspec_ty vspec_id) ->
	wlkHsType vspec_ty	`thenUgn` \ ty	     ->
	wlkMaybe rdVarId vspec_id	`thenUgn` \ id_maybe ->
	returnUgn(ty, id_maybe)

	-- instance specialisation user-pragma
wlk_sig_thing (U_ispec_uprag iclas ispec_ty srcline)
  = mkSrcLocUgn srcline		$ \ src_loc ->
    wlkHsType ispec_ty		`thenUgn` \ ty	    ->
    returnUgn (RdrSig (SpecInstSig ty src_loc))

	-- value inlining user-pragma
wlk_sig_thing (U_inline_uprag ivar srcline)
  = mkSrcLocUgn	srcline      		$ \ src_loc ->
    wlkVarId	ivar		`thenUgn` \ var     ->
    returnUgn (RdrSig (InlineSig var src_loc))

wlk_sig_thing (U_noinline_uprag ivar srcline)
  = mkSrcLocUgn	srcline      		$ \ src_loc ->
    wlkVarId	ivar		`thenUgn` \ var     ->
    returnUgn (RdrSig (NoInlineSig var src_loc))
\end{code}

%************************************************************************
%*									*
\subsection[wlkTypes]{Reading in types in various forms (and data constructors)}
%*									*
%************************************************************************

\begin{code}
rdHsType :: ParseTree -> UgnM RdrNameHsType
rdMonoType :: ParseTree -> UgnM RdrNameHsType

rdHsType pt = rdU_ttype pt `thenUgn` \ ttype -> wlkHsType ttype
rdMonoType pt = rdU_ttype pt `thenUgn` \ ttype -> wlkMonoType ttype

wlkHsType :: U_ttype -> UgnM RdrNameHsType
wlkMonoType :: U_ttype -> UgnM RdrNameHsType

wlkHsType ttype
  = case ttype of
      U_context tcontextl tcontextt -> -- context
	wlkContext  tcontextl	`thenUgn` \ ctxt ->
	wlkMonoType tcontextt	`thenUgn` \ ty	 ->
	returnUgn (HsPreForAllTy ctxt ty)

      other -> -- something else
	wlkMonoType other   `thenUgn` \ ty ->
	returnUgn (HsPreForAllTy [{-no context-}] ty)

wlkMonoType ttype
  = case ttype of
		-- Glasgow extension: nested polymorhism
      U_context tcontextl tcontextt -> -- context
	wlkContext  tcontextl	`thenUgn` \ ctxt ->
	wlkMonoType tcontextt	`thenUgn` \ ty	 ->
	returnUgn (HsPreForAllTy ctxt ty)

      U_namedtvar tv -> -- type variable
	wlkTvId tv	`thenUgn` \ tyvar ->
	returnUgn (MonoTyVar tyvar)

      U_tname tcon -> -- type constructor
	wlkTCId tcon	`thenUgn` \ tycon ->
	returnUgn (MonoTyVar tycon)

      U_tapp t1 t2 ->
	wlkMonoType t1		`thenUgn` \ ty1 ->
	wlkMonoType t2		`thenUgn` \ ty2 ->
	returnUgn (MonoTyApp ty1 ty2)
	      
      U_tllist tlist -> -- list type
	wlkMonoType tlist	`thenUgn` \ ty ->
	returnUgn (MonoListTy dummyRdrTcName ty)

      U_ttuple ttuple ->
	wlkList rdMonoType ttuple `thenUgn` \ tys ->
	returnUgn (MonoTupleTy dummyRdrTcName tys)

      U_tfun tfun targ ->
	wlkMonoType tfun	`thenUgn` \ ty1 ->
	wlkMonoType targ	`thenUgn` \ ty2 ->
	returnUgn (MonoFunTy ty1 ty2)

wlkInstType ttype
  = case ttype of
      U_context tcontextl tcontextt -> -- context
	wlkContext  tcontextl	`thenUgn` \ ctxt ->
	wlkConAndTys tcontextt	`thenUgn` \ (clas, tys)	 ->
	returnUgn (HsPreForAllTy ctxt (MonoDictTy clas tys))

      other -> -- something else
	wlkConAndTys other   `thenUgn` \ (clas, tys) ->
	returnUgn (HsPreForAllTy [{-no context-}] (MonoDictTy clas tys))
\end{code}

\begin{code}
wlkConAndTyVars :: U_ttype   -> UgnM (RdrName, [HsTyVar RdrName])
wlkConAndTyVars ttype
  = wlkMonoType ttype	`thenUgn` \ ty ->
    let
	split (MonoTyApp fun (MonoTyVar arg)) args = split fun (UserTyVar arg : args)
	split (MonoTyVar tycon)		      args = (tycon,args)
	split other			      args = pprPanic "ERROR: malformed type: "
						     (ppr other)
    in
    returnUgn (split ty [])


wlkContext   :: U_list  -> UgnM RdrNameContext
rdConAndTys  :: ParseTree -> UgnM (RdrName, [HsType RdrName])

wlkContext list = wlkList rdConAndTys list

rdConAndTys pt
  = rdU_ttype pt `thenUgn` \ ttype -> 
    wlkConAndTys ttype

wlkConAndTys ttype
  = wlkMonoType ttype	`thenUgn` \ ty ->
    let
	split (MonoTyApp fun ty) tys = split fun (ty : tys)
	split (MonoTyVar tycon)  tys = (tycon, tys)
	split other		 tys = pprPanic "ERROR: malformed type: "
					     (ppr other)
    in
    returnUgn (split ty [])
\end{code}

\begin{code}
rdConDecl :: ParseTree -> UgnM RdrNameConDecl
rdConDecl pt
  = rdU_constr pt    `thenUgn` \ blah ->
    wlkConDecl blah

wlkConDecl :: U_constr -> UgnM RdrNameConDecl

wlkConDecl (U_constrcxt ccxt ccdecl)
  = wlkContext ccxt		`thenUgn` \ theta ->
    wlkConDecl ccdecl		`thenUgn` \ (ConDecl con _ details loc) ->
    returnUgn (ConDecl con theta details loc)

wlkConDecl (U_constrpre ccon ctys srcline)
  = mkSrcLocUgn srcline			$ \ src_loc ->
    wlkDataId	ccon		`thenUgn` \ con	    ->
    wlkList     rdBangType ctys	`thenUgn` \ tys	    ->
    returnUgn (ConDecl con [] (VanillaCon tys) src_loc)

wlkConDecl (U_constrinf cty1 cop cty2 srcline)
  = mkSrcLocUgn srcline			$ \ src_loc ->
    wlkBangType cty1		`thenUgn` \ ty1	    ->
    wlkDataId	cop		`thenUgn` \ op	    ->
    wlkBangType cty2		`thenUgn` \ ty2	    ->
    returnUgn (ConDecl op [] (InfixCon ty1 ty2) src_loc)

wlkConDecl (U_constrnew ccon cty srcline)
  = mkSrcLocUgn srcline			$ \ src_loc ->
    wlkDataId	ccon		`thenUgn` \ con	    ->
    wlkMonoType cty		`thenUgn` \ ty	    ->
    returnUgn (ConDecl con [] (NewCon ty) src_loc)

wlkConDecl (U_constrrec ccon cfields srcline)
  = mkSrcLocUgn srcline			$ \ src_loc      ->
    wlkDataId	ccon		`thenUgn` \ con		 ->
    wlkList rd_field cfields	`thenUgn` \ fields_lists ->
    returnUgn (ConDecl con [] (RecCon fields_lists) src_loc)
  where
    rd_field :: ParseTree -> UgnM ([RdrName], BangType RdrName)
    rd_field pt
      = rdU_constr pt		`thenUgn` \ (U_field fvars fty) ->
	wlkList rdVarId	fvars	`thenUgn` \ vars ->
	wlkBangType fty		`thenUgn` \ ty ->
	returnUgn (vars, ty)

-----------------
rdBangType pt = rdU_ttype pt `thenUgn` \ ty -> wlkBangType ty

wlkBangType :: U_ttype -> UgnM (BangType RdrName)

wlkBangType (U_tbang bty) = wlkMonoType bty `thenUgn` \ ty ->
			    returnUgn (Banged   ty)
wlkBangType uty		  = wlkMonoType uty `thenUgn` \ ty ->
			    returnUgn (Unbanged ty)
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
    mkSrcLocUgn srcline			$ \ src_loc      ->
    wlkPat	gpat		`thenUgn` \ pat     ->
    wlkBinding	gbind		`thenUgn` \ binding ->
    wlkVarId	gsrcfun		`thenUgn` \ srcfun  ->
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
	wlkQuals     g  `thenUgn` \ guard ->
	wlkExpr	     e  `thenUgn` \ expr  ->
	returnUgn (guard, expr)
\end{code}

%************************************************************************
%*									*
\subsection[rdFixOp]{Read in a fixity declaration}
%*									*
%************************************************************************

\begin{code}
rdFixOp :: ParseTree -> UgnM RdrNameFixityDecl
rdFixOp pt 
  = rdU_tree pt `thenUgn` \ fix ->
    case fix of
      U_fixop op dir_n prec srcline -> wlkVarId op 		`thenUgn` \ op ->
				       mkSrcLocUgn srcline	$ \ src_loc ->
				       returnUgn (FixityDecl op (Fixity prec dir) src_loc)
			    where
			      dir = case dir_n of
					(-1) -> InfixL
					0    -> InfixN
					1    -> InfixR
      _ -> error "ReadPrefix:rdFixOp"
\end{code}

%************************************************************************
%*									*
\subsection[rdImport]{Read an import decl}
%*									*
%************************************************************************

\begin{code}
rdImport :: ParseTree
	 -> UgnM RdrNameImportDecl

rdImport pt
  = rdU_binding pt `thenUgn` \ (U_import imod iqual ias ispec isrc srcline) ->
    mkSrcLocUgn srcline				$ \ src_loc      ->
    wlkMaybe rdU_stringId ias		`thenUgn` \ maybe_as	->
    wlkMaybe rd_spec ispec		`thenUgn` \ maybe_spec	->
    returnUgn (ImportDecl imod (cvFlag iqual) (cvIfaceFlavour isrc) maybe_as maybe_spec src_loc)
  where
    rd_spec pt = rdU_either pt 		`thenUgn` \ spec ->
      case spec of
	U_left pt  -> rdEntities pt	`thenUgn` \ ents ->
		      returnUgn (False, ents)
	U_right pt -> rdEntities pt 	`thenUgn` \ ents ->
		      returnUgn (True, ents)

cvIfaceFlavour 0 = HiFile	-- No pragam
cvIfaceFlavour 1 = HiBootFile	-- {-# SOURCE #-}
\end{code}

\begin{code}
rdEntities pt
  = rdU_list pt		    `thenUgn` \ list ->
    wlkList rdEntity list

rdEntity :: ParseTree -> UgnM (IE RdrName)

rdEntity pt
  = rdU_entidt pt `thenUgn` \ entity ->
    case entity of
      U_entid evar -> 		-- just a value
	wlkEntId	evar		`thenUgn` \ var ->
	returnUgn (IEVar var)

      U_enttype x -> 		-- abstract type constructor/class
	wlkTCId	x		`thenUgn` \ thing ->
	returnUgn (IEThingAbs thing)

      U_enttypeall x -> 	-- non-abstract type constructor/class
	wlkTCId	x		`thenUgn` \ thing ->
	returnUgn (IEThingAll thing)

      U_enttypenamed x ns -> 	-- non-abstract type constructor/class
				-- with specified constrs/methods
	wlkTCId	x		`thenUgn` \ thing ->
	wlkList rdVarId ns	`thenUgn` \ names -> 
	returnUgn (IEThingWith thing names)

      U_entmod mod -> 		-- everything provided unqualified by a module
	returnUgn (IEModuleContents mod)
\end{code}


%************************************************************************
%*									*
\subsection[rdExtName]{Read an external name}
%*									*
%************************************************************************

\begin{code}
wlkExtName :: U_maybe -> UgnM ExtName
wlkExtName (U_nothing) = returnUgn Dynamic
wlkExtName (U_just pt)
  = rdU_list pt		    `thenUgn` \ ds ->
    wlkList rdU_hstring ds  `thenUgn` \ ss ->
    case ss of
      [nm]     -> returnUgn (ExtName nm Nothing)
      [mod,nm] -> returnUgn (ExtName nm (Just mod))

rdCallConv :: Int -> UgnM CallConv
rdCallConv x = returnUgn x

rdImpExp :: Int -> Bool -> UgnM (Maybe Bool)
rdImpExp 0 isUnsafe = -- foreign import
  returnUgn (Just isUnsafe)
rdImpExp 1 _ = -- foreign export
  returnUgn Nothing
\end{code}
