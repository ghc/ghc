%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section{Read parse tree built by Yacc parser}

\begin{code}
module ReadPrefix ( rdModule )  where

#include "HsVersions.h"

import UgenAll		-- all Yacc parser gumpff...
import PrefixSyn	-- and various syntaxen.
import HsSyn
import HsTypes		( HsTyVar(..) )
import HsPragmas	( noDataPragmas, noClassPragmas )
import RdrHsSyn         
import BasicTypes	( Fixity(..), FixityDirection(..), NewOrData(..), IfaceFlavour(..) )
import PrefixToHs
import CallConv

import CmdLineOpts      ( opt_NoImplicitPrelude, opt_GlasgowExts )
import Name		( OccName, srcTvOcc, srcVarOcc, srcTCOcc, 
			  Module, mkModuleFS,
			  isConOcc, isLexConId, isWildCardOcc
			)
import Outputable
import SrcLoc		( SrcLoc )
import PrelMods		( pRELUDE )
import FastString	( mkFastCharString )
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
wlkTCId   = wlkQid srcTCOcc
wlkVarId  = wlkQid srcVarOcc
wlkDataId = wlkQid srcVarOcc
wlkEntId = wlkQid (\occ -> if isLexConId occ
			   then srcTCOcc occ
			   else srcVarOcc occ)

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
  = returnUgn (Qual (mkModuleFS mod) (mk_occ_name name) HiFile)
wlkQid mk_occ_name (U_gid n name)
  | opt_NoImplicitPrelude 
	= returnUgn (Unqual (mk_occ_name name))
  | otherwise
	= returnUgn (Qual pRELUDE (mk_occ_name name) HiFile)


rdTCId  pt = rdU_qid pt `thenUgn` wlkTCId
rdVarId pt = rdU_qid pt `thenUgn` wlkVarId

rdTvId  pt = rdU_stringId pt `thenUgn` \ string -> wlkTvId string
wlkTvId string = returnUgn (Unqual (srcTvOcc string))

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
    rdU_tree pt `thenUgn` \ (U_hmodule mod_fs himplist hexplist
				       hmodlist srciface_version srcline) ->
    let
	mod_name = mkModuleFS mod_fs
    in

    setSrcFileUgn srcfile 		$
    setSrcModUgn  mod_name		$
    mkSrcLocUgn srcline			$ \ src_loc	->

    wlkMaybe rdEntities	hexplist `thenUgn` \ exports	->
    wlkList  rdImport   himplist `thenUgn` \ imports	->
    wlkBinding		hmodlist `thenUgn` \ binding	->

    let
	top_decls = cvTopDecls srcfile binding
    in
    returnUgn (mod_name,
       	       HsModule mod_name
	      		  (case srciface_version of { 0 -> Nothing; n -> Just n })
	      		  exports
	      		  imports
			  top_decls
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

rdExpr pt = rdU_tree pt `thenUgn` wlkExpr
rdPat  pt = rdU_tree pt `thenUgn` wlkPat

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

      U_lambda match -> -- lambda expression
	wlkMatch match		`thenUgn` \ match' -> 
	returnUgn (HsLam match')

      U_casee caseexpr casebody srcline ->	-- case expression
	mkSrcLocUgn srcline	 	 $ \ src_loc ->
	wlkExpr	        caseexpr `thenUgn` \ expr ->
	wlkList rdMatch casebody `thenUgn` \ mats ->
	returnUgn (HsCase expr mats src_loc)

      U_ife ifpred ifthen ifelse srcline ->	-- if expression
	mkSrcLocUgn srcline	        $ \ src_loc ->
	wlkExpr ifpred		`thenUgn` \ e1 ->
	wlkExpr ifthen		`thenUgn` \ e2 ->
	wlkExpr ifelse		`thenUgn` \ e3 ->
	returnUgn (HsIf e1 e2 e3 src_loc)

      U_let letvdefs letvexpr ->		-- let expression
	wlkLocalBinding letvdefs	`thenUgn` \ binding ->
	wlkExpr    letvexpr		`thenUgn` \ expr    ->
	returnUgn (HsLet binding expr)

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
		wlkLocalBinding seqlet	`thenUgn` \ binds ->
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
	wlkHsSigType restrt	`thenUgn` \ ty	 ->
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
	returnUgn (ExplicitTuple exprs True)

      U_utuple tuplelist -> -- explicit tuple
	wlkList rdExpr tuplelist `thenUgn` \ exprs ->
	returnUgn (ExplicitTuple exprs False)

      U_record con rbinds -> -- record construction
	wlkDataId  con		`thenUgn` \ rcon     ->
	wlkList rdRbind rbinds	`thenUgn` \ recbinds ->
	returnUgn (RecordCon rcon recbinds)

      U_rupdate updexp updbinds -> -- record update
	wlkExpr updexp		 `thenUgn` \ aexp ->
	wlkList rdRbind updbinds `thenUgn` \ recbinds ->
	returnUgn (RecordUpd aexp recbinds)

#ifdef DEBUG
      U_hmodule _ _ _ _ _ _   -> error "U_hmodule"
      U_as _ _ 		      -> error "U_as"
      U_lazyp _ 	      -> error "U_lazyp"
      U_qual _ _ 	      -> error "U_qual"
      U_guard _ 	      -> error "U_guard"
      U_seqlet _ 	      -> error "U_seqlet"
      U_dobind _ _ _ 	      -> error "U_dobind"
      U_doexp _ _	      -> error "U_doexp"
      U_rbind _ _	      -> error "U_rbind"
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
		  wlkLocalBinding seqlet	`thenUgn` \ binds ->
		  returnUgn (LetStmt binds)

	        U_let letvdefs letvexpr -> 
		    wlkLocalBinding letvdefs	`thenUgn` \ binds ->
		    wlkExpr    letvexpr		`thenUgn` \ expr    ->
		    getSrcLocUgn 		`thenUgn` \ loc ->
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

      U_restr pat ty ->
	wlkPat pat	`thenUgn` \ pat' ->
	wlkHsType ty	`thenUgn` \ ty' ->
	returnUgn (SigPatIn pat' ty')

      U_lazyp lazyp -> 			-- irrefutable ("twiddle") pattern
	wlkPat lazyp	`thenUgn` \ pat ->
	returnUgn (LazyPatIn pat)

      U_plusp avar lit ->
	wlkVarId avar	`thenUgn` \ var ->
	wlkLiteral lit	`thenUgn` \ lit ->
	returnUgn (NPlusKPatIn var lit)

      U_lit lit ->			-- literal pattern
	wlkLiteral lit	`thenUgn` \ lit ->
	returnUgn (LitPatIn lit)

      U_ident nn ->			-- simple identifier
	wlkVarId nn	`thenUgn` \ n ->
	let occ = rdrNameOcc n in
	returnUgn (
	  if isConOcc occ then
		ConPatIn n []
	  else
		if (isWildCardOcc occ) then WildPatIn else (VarPatIn n)
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
		U_par l ->
		  collect_pats l acc
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
	returnUgn (TuplePatIn pats True)

      U_utuple tuplelist -> 		-- explicit tuple
	wlkList rdPat tuplelist	`thenUgn` \ pats ->
	returnUgn (TuplePatIn pats False)

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
wlkLocalBinding bind
  = wlkBinding bind	`thenUgn` \ bind' ->
    getSrcFileUgn	`thenUgn` \ sf	  ->
    returnUgn (cvBinds sf cvValSig bind')

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

	-- fixity declaration
      U_fixd op dir_n prec srcline ->
	let
	      dir = case dir_n of
			(-1) -> InfixL
			0    -> InfixN
			1    -> InfixR
	in
	wlkVarId op 		`thenUgn` \ op ->
	mkSrcLocUgn srcline	$ \ src_loc ->
	returnUgn (RdrSig (FixSig (FixitySig op (Fixity prec dir) src_loc)))


	-- "data" declaration
      U_tbind tctxt ttype tcons tderivs srcline ->
	mkSrcLocUgn	   srcline  	    $ \ src_loc	    ->
	wlkContext	   tctxt    `thenUgn` \ ctxt	    ->
	wlkConAndTyVars    ttype    `thenUgn` \ (tycon, tyvars) ->
	wlkList rdConDecl  tcons    `thenUgn` \ cons	    ->
	wlkDerivings	   tderivs  `thenUgn` \ derivings   ->
	returnUgn (RdrTyClDecl (TyData DataType ctxt tycon tyvars cons derivings noDataPragmas src_loc))

	-- "newtype" declaration
      U_ntbind ntctxt nttype ntcon ntderivs srcline ->
	mkSrcLocUgn	   srcline  	    $ \ src_loc	    ->
	wlkContext	   ntctxt   `thenUgn` \ ctxt	    ->
	wlkConAndTyVars    nttype   `thenUgn` \ (tycon, tyvars) ->
	wlkList rdConDecl  ntcon    `thenUgn` \ cons	    ->
	wlkDerivings	   ntderivs `thenUgn` \ derivings   ->
	returnUgn (RdrTyClDecl (TyData NewType ctxt tycon tyvars cons derivings noDataPragmas src_loc))

	-- "type" declaration
      U_nbind nbindid nbindas srcline -> 		
	mkSrcLocUgn	  srcline 	  $ \ src_loc	    ->
	wlkConAndTyVars   nbindid `thenUgn` \ (tycon, tyvars) ->
	wlkHsType	  nbindas `thenUgn` \ expansion	    ->
	returnUgn (RdrTyClDecl (TySynonym tycon tyvars expansion src_loc))

	-- function binding
      U_fbind fbindm srcline ->
	mkSrcLocUgn     srcline		$ \ src_loc ->
	wlkList rdMatch fbindm		`thenUgn` \ matches ->
	returnUgn (RdrValBinding (mkRdrFunctionBinding matches src_loc))

	-- pattern binding
      U_pbind pbindl pbindr srcline ->
	mkSrcLocUgn srcline		$ \ src_loc ->
	rdPat pbindl			`thenUgn` \ pat ->
	rdGRHSs pbindr			`thenUgn` \ grhss ->
	returnUgn (RdrValBinding (PatMonoBind pat grhss src_loc))

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
	returnUgn (RdrTyClDecl
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
	rdForKind imp_exp (cvFlag unsafe_flag)    `thenUgn` \ h_imp_exp ->
 	returnUgn (RdrForeignDecl (ForeignDecl h_id h_imp_exp h_ty h_ext_name h_cconv src_loc))

      U_sbind sbindids sbindid srcline ->
	-- Type signature
	mkSrcLocUgn srcline		$ \ src_loc ->
	wlkList rdVarId	sbindids	`thenUgn` \ vars    ->
	wlkHsSigType	sbindid		`thenUgn` \ poly_ty ->
	returnUgn (foldr1 RdrAndBindings [RdrSig (Sig var poly_ty src_loc) | var <- vars])

      U_vspec_uprag uvar vspec_tys srcline ->
	-- value specialisation user-pragma
	mkSrcLocUgn srcline		$ \ src_loc ->
	wlkVarId uvar		    	`thenUgn` \ var ->
	wlkList rd_ty_and_id vspec_tys  `thenUgn` \ tys_and_ids ->
	returnUgn (foldr1 RdrAndBindings [ RdrSig (SpecSig var ty using_id src_loc)
				         | (ty, using_id) <- tys_and_ids ])
	where
	  rd_ty_and_id :: ParseTree -> UgnM (RdrNameHsType, Maybe RdrName)
          rd_ty_and_id pt
	      = rdU_binding pt			`thenUgn` \ (U_vspec_ty_and_id vspec_ty vspec_id) ->
		wlkHsSigType vspec_ty		`thenUgn` \ ty	     ->
		wlkMaybe rdVarId vspec_id	`thenUgn` \ id_maybe ->
		returnUgn(ty, id_maybe)

      U_ispec_uprag iclas ispec_ty srcline ->
	-- instance specialisation user-pragma
	mkSrcLocUgn srcline		$ \ src_loc ->
	wlkHsSigType ispec_ty		`thenUgn` \ ty	    ->
	returnUgn (RdrSig (SpecInstSig ty src_loc))

      U_inline_uprag ivar srcline ->
	-- value inlining user-pragma
	mkSrcLocUgn	srcline    	$ \ src_loc ->
	wlkVarId	ivar		`thenUgn` \ var     ->
	returnUgn (RdrSig (InlineSig var src_loc))

      U_noinline_uprag ivar srcline ->
	-- No-inline pragma
	mkSrcLocUgn	srcline      	$ \ src_loc ->
	wlkVarId	ivar		`thenUgn` \ var     ->
	returnUgn (RdrSig (NoInlineSig var src_loc))


mkRdrFunctionBinding :: [RdrNameMatch] -> SrcLoc -> RdrNameMonoBinds
mkRdrFunctionBinding fun_matches src_loc
  = FunMonoBind (head fns) (head infs) matches src_loc
  where
    (fns, infs, matches) = unzip3 (map de_fun_match fun_matches)

    de_fun_match (Match _ [ConPatIn fn pats]      sig grhss) = (fn, False, Match [] pats    sig grhss)
    de_fun_match (Match _ [ConOpPatIn p1 fn _ p2] sig grhss) = (fn, True,  Match [] [p1,p2] sig grhss)


rdGRHSs :: ParseTree -> UgnM RdrNameGRHSs
rdGRHSs pt = rdU_grhsb pt `thenUgn` wlkGRHSs

wlkGRHSs :: U_grhsb -> UgnM RdrNameGRHSs
wlkGRHSs (U_pguards rhss bind)
  = wlkList rdGdExp rhss	`thenUgn` \ gdexps ->
    wlkLocalBinding bind	`thenUgn` \ bind' ->
    returnUgn (GRHSs gdexps bind' Nothing)
wlkGRHSs (U_pnoguards srcline rhs bind)
  = mkSrcLocUgn srcline  	$ \ src_loc ->
    rdExpr rhs			`thenUgn` \ rhs' ->
    wlkLocalBinding bind	`thenUgn` \ bind' ->
    returnUgn (GRHSs (unguardedRHS rhs' src_loc) bind' Nothing)


rdGdExp :: ParseTree -> UgnM RdrNameGRHS
rdGdExp pt = rdU_gdexp pt		`thenUgn` \ (U_pgdexp guards srcline rhs) ->
	     wlkQuals guards		`thenUgn` \ guards' ->
	     mkSrcLocUgn srcline  	$ \ src_loc ->
    	     wlkExpr rhs		`thenUgn` \ expr'  ->
	     returnUgn (GRHS (guards' ++ [ExprStmt expr' src_loc]) src_loc)
\end{code}

\begin{code}
wlkDerivings :: U_maybe -> UgnM (Maybe [RdrName])

wlkDerivings (U_nothing) = returnUgn Nothing
wlkDerivings (U_just pt)
  = rdU_list pt		 `thenUgn` \ ds	    ->
    wlkList rdTCId ds	 `thenUgn` \ derivs ->
    returnUgn (Just derivs)
\end{code}

%************************************************************************
%*									*
\subsection[wlkTypes]{Reading in types in various forms (and data constructors)}
%*									*
%************************************************************************

\begin{code}
rdHsType :: ParseTree -> UgnM RdrNameHsType
rdMonoType :: ParseTree -> UgnM RdrNameHsType

rdHsType   pt = rdU_ttype pt `thenUgn` wlkHsType
rdMonoType pt = rdU_ttype pt `thenUgn` wlkHsType

wlkHsConstrArgType ttype
	-- Used for the argument types of contructors
	-- Only an implicit quantification point if -fglasgow-exts
  | opt_GlasgowExts = wlkHsSigType ttype
  | otherwise       = wlkHsType    ttype

	-- wlkHsSigType is used for type signatures: any place there
	-- should be *implicit* quantification
wlkHsSigType ttype
  = wlkHsType ttype	`thenUgn` \ ty ->
	-- This is an implicit quantification point, so
	-- make sure it starts with a ForAll
    case ty of
	HsForAllTy _ _ _ -> returnUgn ty
	other		 -> returnUgn (HsForAllTy [] [] ty)

wlkHsType :: U_ttype -> UgnM RdrNameHsType
wlkHsType ttype
  = case ttype of
      U_forall u_tyvars u_theta u_ty -> -- context
	wlkList rdTvId u_tyvars		`thenUgn` \ tyvars -> 
	wlkContext u_theta		`thenUgn` \ theta ->
	wlkHsType u_ty			`thenUgn` \ ty	 ->
	returnUgn (HsForAllTy (map UserTyVar tyvars) theta ty)

      U_namedtvar tv -> -- type variable
	wlkTvId tv	`thenUgn` \ tyvar ->
	returnUgn (MonoTyVar tyvar)

      U_tname tcon -> -- type constructor
	wlkTCId tcon	`thenUgn` \ tycon ->
	returnUgn (MonoTyVar tycon)

      U_tapp t1 t2 ->
	wlkHsType t1		`thenUgn` \ ty1 ->
	wlkHsType t2		`thenUgn` \ ty2 ->
	returnUgn (MonoTyApp ty1 ty2)
	      
      U_tllist tlist -> -- list type
	wlkHsType tlist	`thenUgn` \ ty ->
	returnUgn (MonoListTy ty)

      U_ttuple ttuple ->
	wlkList rdMonoType ttuple `thenUgn` \ tys ->
	returnUgn (MonoTupleTy tys True)

      U_tutuple ttuple ->
	wlkList rdMonoType ttuple `thenUgn` \ tys ->
	returnUgn (MonoTupleTy tys False)

      U_tfun tfun targ ->
	wlkHsType tfun	`thenUgn` \ ty1 ->
	wlkHsType targ	`thenUgn` \ ty2 ->
	returnUgn (MonoFunTy ty1 ty2)

wlkInstType ttype
  = case ttype of
      U_forall u_tyvars u_theta inst_head ->
	wlkList rdTvId u_tyvars		`thenUgn` \ tyvars -> 
	wlkContext  u_theta		`thenUgn` \ theta ->
	wlkConAndTys inst_head		`thenUgn` \ (clas, tys)	 ->
	returnUgn (HsForAllTy (map UserTyVar tyvars) theta (MonoDictTy clas tys))

      other -> -- something else
	wlkConAndTys other   `thenUgn` \ (clas, tys) ->
	returnUgn (HsForAllTy [] [] (MonoDictTy clas tys))
\end{code}

\begin{code}
wlkConAndTyVars :: U_ttype -> UgnM (RdrName, [HsTyVar RdrName])
wlkConAndTyVars ttype
  = wlkHsType ttype	`thenUgn` \ ty ->
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

rdConAndTys pt = rdU_ttype pt `thenUgn` wlkConAndTys

wlkConAndTys ttype
  = wlkHsType ttype	`thenUgn` \ ty ->
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
rdConDecl pt = rdU_constr pt    `thenUgn` wlkConDecl

wlkConDecl :: U_constr -> UgnM RdrNameConDecl

wlkConDecl (U_constrex u_tvs ccxt ccdecl)
  = wlkList rdTvId u_tvs	`thenUgn` \ tyvars -> 
    wlkContext ccxt		`thenUgn` \ theta ->
    wlkConDecl ccdecl		`thenUgn` \ (ConDecl con _ _ details loc) ->
    returnUgn (ConDecl con (map UserTyVar tyvars) theta details loc)

wlkConDecl (U_constrpre ccon ctys srcline)
  = mkSrcLocUgn srcline			$ \ src_loc ->
    wlkDataId	ccon		`thenUgn` \ con	    ->
    wlkList     rdBangType ctys	`thenUgn` \ tys	    ->
    returnUgn (ConDecl con [] [] (VanillaCon tys) src_loc)

wlkConDecl (U_constrinf cty1 cop cty2 srcline)
  = mkSrcLocUgn srcline			$ \ src_loc ->
    wlkBangType cty1		`thenUgn` \ ty1	    ->
    wlkDataId	cop		`thenUgn` \ op	    ->
    wlkBangType cty2		`thenUgn` \ ty2	    ->
    returnUgn (ConDecl op [] [] (InfixCon ty1 ty2) src_loc)

wlkConDecl (U_constrnew ccon cty mb_lab srcline)
  = mkSrcLocUgn srcline			 $ \ src_loc ->
    wlkDataId	ccon		 `thenUgn` \ con	    ->
    wlkHsSigType cty		 `thenUgn` \ ty	    ->
    wlkMaybe     rdVarId  mb_lab `thenUgn` \ mb_lab  ->
    returnUgn (ConDecl con [] [] (NewCon ty mb_lab) src_loc)

wlkConDecl (U_constrrec ccon cfields srcline)
  = mkSrcLocUgn srcline			$ \ src_loc      ->
    wlkDataId	ccon		`thenUgn` \ con		 ->
    wlkList rd_field cfields	`thenUgn` \ fields_lists ->
    returnUgn (ConDecl con [] [] (RecCon fields_lists) src_loc)
   where
    rd_field :: ParseTree -> UgnM ([RdrName], BangType RdrName)
    rd_field pt =
      rdU_constr pt		`thenUgn` \ (U_field fvars fty) ->
      wlkList rdVarId	fvars	`thenUgn` \ vars ->
      wlkBangType fty		`thenUgn` \ ty ->
      returnUgn (vars, ty)

-----------------
rdBangType pt = rdU_ttype pt `thenUgn` wlkBangType

wlkBangType :: U_ttype -> UgnM (BangType RdrName)

wlkBangType (U_tbang bty) = wlkHsConstrArgType bty	`thenUgn` \ ty ->
			    returnUgn (Banged   ty)
wlkBangType uty		  = wlkHsConstrArgType uty	`thenUgn` \ ty ->
			    returnUgn (Unbanged ty)
\end{code}

%************************************************************************
%*									*
\subsection{Read a ``match''}
%*									*
%************************************************************************

\begin{code}
rdMatch :: ParseTree -> UgnM RdrNameMatch
rdMatch pt = rdU_match pt `thenUgn` wlkMatch 

wlkMatch :: U_match -> UgnM RdrNameMatch
wlkMatch (U_pmatch pats sig grhsb)
  = wlkList rdPat pats		`thenUgn` \ pats'   ->
    wlkMaybe rdHsType sig	`thenUgn` \ maybe_ty ->
    wlkGRHSs grhsb		`thenUgn` \ grhss' ->
    returnUgn (Match [] pats' maybe_ty grhss')
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
    returnUgn (ImportDecl (mkModuleFS imod) 
			  (cvFlag iqual) 
			  (cvIfaceFlavour isrc) 
			  (case maybe_as of { Just m -> Just (mkModuleFS m); Nothing -> Nothing })
			  maybe_spec src_loc)
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
rdEntities pt = rdU_list pt `thenUgn` wlkList rdEntity

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
	returnUgn (IEModuleContents (mkModuleFS mod))
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
rdCallConv x = 
   -- this tracks the #defines in parser/utils.h
  case x of
    (-1) -> -- no calling convention specified, use default.
          returnUgn defaultCallConv
    _    -> returnUgn x

rdForKind :: Int -> Bool -> UgnM ForKind
rdForKind 0 isUnsafe = -- foreign import
  returnUgn (FoImport isUnsafe)
rdForKind 1 _ = -- foreign export
  returnUgn FoExport
rdForKind 2 _ = -- foreign label
  returnUgn FoLabel

\end{code}
