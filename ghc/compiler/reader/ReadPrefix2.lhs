%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[ReadPrefix2]{Read parse tree built by Yacc parser}

Comments?

\begin{code}
#include "HsVersions.h"

module ReadPrefix2 (
	rdModule,

	-- used over in ReadPragmas2...
	wlkList, rdConDecl, wlkMonoType
    )  where

IMPORT_Trace		-- ToDo: rm (debugging)
import Pretty

import UgenAll

import AbsSyn
import HsCore		-- ****** NEED TO SEE CONSTRUCTORS ******
import HsPragmas	-- ****** NEED TO SEE CONSTRUCTORS ******
import FiniteMap
import IdInfo		( UnfoldingGuidance(..) )
import MainMonad
import Maybes		( Maybe(..) )
import PrefixToHs
import PrefixSyn
import ProtoName
import Outputable
import ReadPragmas2
import Util
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

%************************************************************************
%*									*
\subsection[rdModule]{@rdModule@: reads in a Haskell module}
%*									*
%************************************************************************

\begin{code}
rdModule :: MainIO
	    (FAST_STRING,			-- this module's name
	     (FAST_STRING -> Bool,	-- a function to chk if <x> is in the export list
	      FAST_STRING -> Bool),	-- a function to chk if <M> is among the M..
					-- ("dotdot") modules in the export list.
	     ProtoNameModule)		-- the main goods

rdModule
  = _ccall_ hspmain `thenMn` \ pt -> -- call the Yacc parser!
    let
	srcfile  = _packCString ``input_filename'' -- What A Great Hack! (TM)
    in
    initUgn srcfile (

    rdU_tree pt `thenUgn` \ (U_hmodule name himplist hexplist hmodlist srcline) ->
    rdFixities	`thenUgn` \ fixities ->
    wlkBinding			hmodlist `thenUgn` \ binding	->
    wlkList rdImportedInterface himplist `thenUgn` \ imports	->
    wlkList rdEntity		hexplist `thenUgn` \ export_list->
    mkSrcLocUgn srcline			 `thenUgn` \ src_loc	->

    case sepDeclsForTopBinds binding	  of {
      (tydecls, tysigs, classdecls, instdecls, instsigs, defaultdecls, binds) ->
      -- ToDo: bad for laziness??

    returnUgn (
     name,
     mk_export_list_chker export_list,
     Module name
	    export_list
	    imports
	    fixities
	    tydecls
	    tysigs
	    classdecls
	    (cvInstDecls True name name instdecls) -- True indicates not imported
	    instsigs
	    defaultdecls
	    (cvSepdBinds srcfile cvValSig binds)
	    [{-no sigs-}]
	    src_loc
    ) } )
  where
    mk_export_list_chker exp_list
      = case (getIEStrings exp_list) of { (entity_info, dotdot_modules) ->
	( \ n -> n `elemFM` entity_info,
	  \ n -> n `elemFM` dotdot_modules )
    	}
\end{code}

Convert fixities table:
\begin{code}
rdFixities :: UgnM [ProtoNameFixityDecl]

rdFixities
  = ioToUgnM (_ccall_ nfixes)	`thenUgn` \ num_fixities@(I# _) ->
    let
	rd i acc
	  | i >= num_fixities
	  = returnUgn acc

	  | otherwise
	  = ioToUgnM (_ccall_ fixtype i) `thenUgn` \ fix_ty@(A# _) ->
	    if fix_ty == ``NULL'' then
		rd (i+1) acc
	    else
		ioToUgnM (_ccall_ fixop      i) `thenUgn` \ fix_op@(A# _) ->
		ioToUgnM (_ccall_ precedence i) `thenUgn` \ precedence@(I# _) ->
		let
		    op = Unk (_packCString fix_op)

		    associativity
		      = _UNPK_ (_packCString fix_ty)

		    new_fix
		      = case associativity of
			  "infix"  -> InfixN op precedence
			  "infixl" -> InfixL op precedence
			  "infixr" -> InfixR op precedence
		in
		rd (i+1) (new_fix : acc)
    in
    rd 0 []
\end{code}

%************************************************************************
%*									*
\subsection[wlkExprOrPat]{@wlkExpr@ and @wlkPat@}
%*									*
%************************************************************************

\begin{code}
rdExpr :: ParseTree -> UgnM ProtoNameExpr
rdPat  :: ParseTree -> UgnM ProtoNamePat

rdExpr pt = rdU_tree pt `thenUgn` \ tree -> wlkExpr tree
rdPat  pt = rdU_tree pt `thenUgn` \ tree -> wlkPat  tree

wlkExpr :: U_tree -> UgnM ProtoNameExpr
wlkPat  :: U_tree -> UgnM ProtoNamePat

wlkExpr expr
  = case expr of
      U_par expr -> -- parenthesised expr
	wlkExpr expr

      U_lsection lsexp op ->	-- left section
        wlkExpr lsexp	`thenUgn` \ expr ->
	returnUgn (SectionL expr (Var op))

      U_rsection op rsexp -> -- right section
	wlkExpr rsexp	`thenUgn` \ expr ->
	returnUgn (SectionR (Var op) expr)

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
	returnUgn (SCC label expr)

      U_lambda lampats lamexpr srcline -> -- lambda expression
        wlkList rdPat lampats	`thenUgn` \ pats ->
	wlkExpr       lamexpr	`thenUgn` \ body ->
	mkSrcLocUgn   srcline	`thenUgn` \ src_loc ->
	returnUgn (
	    Lam (foldr PatMatch
		       (GRHSMatch (GRHSsAndBindsIn
				    [OtherwiseGRHS body src_loc]
				    EmptyBinds))
		       pats)
	)

      U_casee caseexpr casebody -> -- case expression
        wlkExpr	       caseexpr	 `thenUgn` \ expr ->
	wlkList rdMatch casebody `thenUgn` \ mats ->
	getSrcFileUgn		 `thenUgn` \ sf ->
	let
	    matches = cvMatches sf True mats
	in
	returnUgn (Case expr matches)

      U_ife ifpred ifthen ifelse -> -- if expression
        wlkExpr ifpred	`thenUgn` \ e1 ->
	wlkExpr ifthen	`thenUgn` \ e2 ->
	wlkExpr ifelse	`thenUgn` \ e3 ->
	returnUgn (If e1 e2 e3)

      U_let letvdeflist letvexpr -> -- let expression
        wlkBinding letvdeflist	`thenUgn` \ binding ->
	wlkExpr    letvexpr	`thenUgn` \ expr    ->
	getSrcFileUgn		`thenUgn` \ sf	    ->
	let
	    binds = cvBinds sf cvValSig binding
	in
	returnUgn (Let binds expr)

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
		U_par expr -> wlk_qual expr -- overkill? (ToDo?)

	        U_qual qpat qexp ->
		  wlkPat  qpat  `thenUgn` \ pat  ->
		  wlkExpr qexp  `thenUgn` \ expr ->
		  returnUgn (GeneratorQual pat expr)

		U_guard gexp ->
		  wlkExpr gexp  `thenUgn` \ expr ->
		  returnUgn (FilterQual expr)

      U_eenum efrom estep eto -> -- arithmetic sequence
        wlkExpr efrom		`thenUgn` \ e1  ->
	wlkList rdExpr estep	`thenUgn` \ es2 ->
	wlkList rdExpr eto	`thenUgn` \ es3 ->
	returnUgn (cv_arith_seq e1 es2 es3)
	where -- ToDo: use Maybe type
	   cv_arith_seq e1 []   []   = ArithSeqIn (From	      e1)
	   cv_arith_seq e1 []   [e3] = ArithSeqIn (FromTo     e1 e3)
	   cv_arith_seq e1 [e2] []   = ArithSeqIn (FromThen   e1 e2)
	   cv_arith_seq e1 [e2] [e3] = ArithSeqIn (FromThenTo e1 e2 e3)

      U_restr restre restrt -> -- expression with type signature
        wlkExpr	    restre	`thenUgn` \ expr ->
	wlkPolyType restrt	`thenUgn` \ ty	 ->
	returnUgn (ExprWithTySig expr ty)

      U_negate nexp -> -- negated expression
        wlkExpr nexp		`thenUgn` \ expr ->
	returnUgn (App (Var (Unk SLIT("negate"))) expr)

      -- ToDo: DPH stuff

      --------------------------------------------------------------
      -- now the prefix items that can either be an expression or
      -- pattern, except we know they are *expressions* here
      -- (this code could be commoned up with the pattern version;
      -- but it probably isn't worth it)
      --------------------------------------------------------------
      U_lit lit ->
        wlkLiteral lit	`thenUgn` \ lit ->
	returnUgn (Lit lit)

      U_ident n -> -- simple identifier
	returnUgn (Var n)

      U_ap fun arg -> -- application
        wlkExpr fun	`thenUgn` \ expr1 ->
	wlkExpr arg	`thenUgn` \ expr2 ->
	returnUgn (App expr1 expr2)

      U_tinfixop (op, arg1, arg2) ->
        wlkExpr arg1	`thenUgn` \ expr1 ->
	wlkExpr arg2	`thenUgn` \ expr2 ->
	returnUgn (OpApp expr1 (Var op) expr2)

      U_llist llist -> -- explicit list
        wlkList rdExpr llist `thenUgn` \ exprs ->
	returnUgn (ExplicitList exprs)

      U_tuple tuplelist -> -- explicit tuple
        wlkList rdExpr tuplelist `thenUgn` \ exprs ->
	returnUgn (ExplicitTuple exprs)

#ifdef DEBUG
      U_hmodule _ _ _ _ _ -> error "U_hmodule"
      U_as _ _ -> error "U_as"
      U_lazyp _ -> error "U_lazyp"
      U_plusp _ _ -> error "U_plusp"
      U_wildp -> error "U_wildp"
      U_qual _ _ -> error "U_qual"
      U_guard _ -> error "U_guard"
      U_def _ -> error "U_def"
#endif

-- ToDo: DPH stuff
\end{code}

Patterns: just bear in mind that lists of patterns are represented as
a series of ``applications''.
\begin{code}
wlkPat pat
  = case pat of
      U_par pat ->  -- parenthesised pattern
	wlkPat pat

      U_as var as_pat -> -- "as" pattern
	wlkPat as_pat	`thenUgn` \ pat ->
	returnUgn (AsPatIn var pat)

      U_lazyp lazyp -> -- irrefutable ("twiddle") pattern
        wlkPat lazyp	`thenUgn` \ pat ->
	returnUgn (LazyPatIn pat)

      U_plusp plusn plusk -> -- n+k pattern
        wlkPat     plusn    `thenUgn` \ pat ->
	wlkLiteral plusk    `thenUgn` \ lit ->
	let
	    n = case pat of
		  VarPatIn n -> n
		  WildPatIn  -> error "ERROR: wlkPat: GHC can't handle _+k patterns\n"
	in
	returnUgn (NPlusKPatIn n lit)

      U_wildp -> returnUgn WildPatIn -- wildcard pattern

      --------------------------------------------------------------
      -- now the prefix items that can either be an expression or
      -- pattern, except we know they are *patterns* here.
      --------------------------------------------------------------
      U_negate nexp ->	-- negated pattern: negatee must be a literal
        wlkPat nexp	`thenUgn` \ lit_pat ->
	case lit_pat of
	  LitPatIn lit -> returnUgn (LitPatIn (negLiteral lit))
	  _	       -> panic "wlkPat: bad negated pattern!"

      U_lit lit ->
        wlkLiteral lit	`thenUgn` \ lit ->
	returnUgn (LitPatIn lit)

      U_ident n -> -- simple identifier
	returnUgn (
	  if isConopPN n
	  then ConPatIn n []
	  else VarPatIn n
	)

      U_ap l r -> -- "application": there's a list of patterns lurking here!
        wlk_curried_pats l `thenUgn` \ (lpat:lpats) ->
	wlkPat		 r `thenUgn` \ rpat	    ->
	let
	    (n, llpats)
	      = case lpat of
		  VarPatIn x        -> (x, [])
		  ConPatIn x []     -> (x, [])
		  ConOpPatIn x op y -> (op, [x, y])
		  _ -> -- sorry about the weedy msg; the parser missed this one
		       error (ppShow 100 (ppCat [ppStr "ERROR: an illegal `application' of a pattern to another one:", ppInterleave ppSP (map (ppr PprForUser) bad_app)]))

	    arg_pats = llpats ++ lpats ++ [rpat]
	    bad_app  = (lpat:lpats) ++ [rpat]
	in
	returnUgn (ConPatIn n arg_pats)
	where
	  wlk_curried_pats pat
	    = case pat of
	        U_ap l r ->
		  wlk_curried_pats l	`thenUgn` \ lpats ->
		  wlkPat	   r	`thenUgn` \ rpat  ->
		  returnUgn (lpats ++ [rpat])
		other ->
		  wlkPat other		`thenUgn` \ pat ->
		  returnUgn [pat]

      U_tinfixop (op, arg1, arg2) ->
	wlkPat arg1	`thenUgn` \ pat1 ->
	wlkPat arg2	`thenUgn` \ pat2 ->
	returnUgn (ConOpPatIn pat1 op pat2)

      U_llist llist -> -- explicit list
        wlkList rdPat llist `thenUgn` \ pats ->
	returnUgn (ListPatIn pats)

      U_tuple tuplelist -> -- explicit tuple
        wlkList rdPat tuplelist	`thenUgn` \ pats ->
	returnUgn (TuplePatIn pats)

      -- ToDo: DPH
\end{code}

OLD, MISPLACED NOTE: The extra DPH syntax above is defined such that
to the left of a \tr{<<-} or \tr{<<=} there has to be a processor (no
expressions).  Therefore in the pattern matching below we are taking
this into consideration to create the @DrawGen@ whose fields are the
\tr{K} patterns, pat and the exp right of the generator.

\begin{code}
wlkLiteral :: U_literal -> UgnM Literal

wlkLiteral ulit
  = returnUgn (
    case ulit of
      U_integer    s   -> IntLit	(as_integer  s)
      U_floatr     s   -> FracLit	(as_rational s)
      U_intprim    s   -> IntPrimLit    (as_integer  s)
      U_doubleprim s   -> DoublePrimLit (as_rational s)
      U_floatprim  s   -> FloatPrimLit  (as_rational s)
      U_charr	   s   -> CharLit	(as_char     s)
      U_charprim   s   -> CharPrimLit   (as_char     s)
      U_string     s   -> StringLit     (as_string   s)
      U_stringprim s   -> StringPrimLit (as_string   s)
      U_clitlit    s _ -> LitLitLitIn   (as_string   s)
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

      U_tbind tbindc tbindid tbindl tbindd srcline tpragma -> -- "data" declaration
        wlkContext	   tbindc  `thenUgn` \ ctxt	    ->
	wlkList rdU_unkId  tbindd  `thenUgn` \ derivings    ->
	wlkTyConAndTyVars  tbindid `thenUgn` \ (tycon, tyvars) ->
	wlkList rdConDecl  tbindl  `thenUgn` \ cons	    ->
	wlkDataPragma	   tpragma `thenUgn` \ pragma	    ->
	mkSrcLocUgn	   srcline `thenUgn` \ src_loc	    ->
	returnUgn (RdrTyData (TyData ctxt tycon tyvars cons derivings pragma src_loc))

      U_nbind nbindid nbindas srcline npragma -> -- "type" declaration
        wlkTyConAndTyVars nbindid `thenUgn` \ (tycon, tyvars) ->
	wlkMonoType	  nbindas `thenUgn` \ expansion	    ->
	wlkTypePragma	  npragma `thenUgn` \ pragma	    ->
	mkSrcLocUgn	  srcline `thenUgn` \ src_loc	    ->
	returnUgn (RdrTySynonym (TySynonym tycon tyvars expansion pragma src_loc))

      U_fbind fbindl srcline -> -- function binding
        wlkList rdMatch fbindl	`thenUgn` \ matches ->
	mkSrcLocUgn     srcline	`thenUgn` \ src_loc ->
	returnUgn (RdrFunctionBinding srcline matches)

      U_pbind pbindl srcline ->  -- pattern binding
        wlkList rdMatch pbindl	`thenUgn` \ matches ->
	mkSrcLocUgn     srcline	`thenUgn` \ src_loc ->
	returnUgn (RdrPatternBinding srcline matches)

      U_cbind cbindc cbindid cbindw srcline cpragma -> -- "class" declaration
        wlkContext	 cbindc	 `thenUgn` \ ctxt	  ->
	wlkClassAssertTy cbindid `thenUgn` \ (clas, tyvar) ->
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

      U_ibind ibindc clas ibindi ibindw srcline ipragma -> -- "instance" declaration
        wlkContext	ibindc	`thenUgn` \ ctxt    ->
	wlkMonoType	ibindi	`thenUgn` \ inst_ty ->
	wlkBinding	ibindw	`thenUgn` \ binding ->
	wlkInstPragma   ipragma	`thenUgn` \ (modname_maybe, pragma) ->
	mkSrcLocUgn	srcline	`thenUgn` \ src_loc ->
	getSrcFileUgn		`thenUgn` \ sf	    ->
	let
	    (ss, bs) = sepDeclsIntoSigsAndBinds binding
	    binds    = cvMonoBinds sf bs
	    uprags   = concat (map cvInstDeclSig ss)
	in
	returnUgn (
	case modname_maybe of {
	  Nothing ->
	    RdrInstDecl (\ orig_mod infor_mod here ->
		  InstDecl ctxt clas inst_ty binds here orig_mod infor_mod uprags pragma src_loc);
	  Just orig_mod ->
	    RdrInstDecl (\ _ infor_mod here ->
		  InstDecl ctxt clas inst_ty binds here orig_mod infor_mod uprags pragma src_loc)
	})

      U_dbind dbindts srcline -> -- "default" declaration
        wlkList rdMonoType dbindts  `thenUgn` \ tys ->
	mkSrcLocUgn        srcline  `thenUgn` \ src_loc ->
	returnUgn (RdrDefaultDecl (DefaultDecl tys src_loc))

      U_mbind mod mbindimp mbindren srcline ->
        -- "import" declaration in an interface
	wlkList rdEntity   mbindimp	`thenUgn` \ entities  ->
	wlkList rdRenaming mbindren	`thenUgn` \ renamings ->
	mkSrcLocUgn	   srcline	`thenUgn` \ src_loc   ->
	returnUgn (RdrIfaceImportDecl (IfaceImportDecl mod entities renamings src_loc))

      a_sig_we_hope ->
        -- signature(-like) things, including user pragmas
	wlk_sig_thing a_sig_we_hope
\end{code}

ToDo: really needed as separate?
\begin{code}
wlk_sig_thing (U_sbind sbindids sbindid srcline spragma)  -- type signature
  = wlkList rdU_unkId	sbindids `thenUgn` \ vars    ->
    wlkPolyType		sbindid  `thenUgn` \ poly_ty ->
    wlkTySigPragmas	spragma  `thenUgn` \ pragma  ->
    mkSrcLocUgn		srcline  `thenUgn` \ src_loc ->
    returnUgn (RdrTySig vars poly_ty pragma src_loc)

wlk_sig_thing (U_vspec_uprag var vspec_tys srcline) -- value specialisation user-pragma
  = wlkList rd_ty_and_id vspec_tys `thenUgn` \ tys_and_ids ->
    mkSrcLocUgn	         srcline   `thenUgn` \ src_loc ->
    returnUgn (RdrSpecValSig [SpecSig var ty using_id src_loc
			     | (ty, using_id) <- tys_and_ids ])
  where
    rd_ty_and_id :: ParseTree -> UgnM (ProtoNamePolyType, Maybe ProtoName)
    rd_ty_and_id pt
      = rdU_binding pt			`thenUgn` \ (U_vspec_ty_and_id vspec_ty vspec_id) ->
	wlkPolyType vspec_ty		`thenUgn` \ ty	    ->
	wlkList rdU_unkId vspec_id	`thenUgn` \ id_list ->
	returnUgn(ty, case id_list of { []  -> Nothing; [x] -> Just x })

wlk_sig_thing (U_ispec_uprag clas ispec_ty srcline)-- instance specialisation user-pragma
  = wlkMonoType	    ispec_ty	`thenUgn` \ ty	    ->
    mkSrcLocUgn	    srcline	`thenUgn` \ src_loc ->
    returnUgn (RdrSpecInstSig (InstSpecSig clas ty src_loc))

wlk_sig_thing (U_inline_uprag var inline_howto srcline) -- value inlining user-pragma
  = wlkList rdU_stringId inline_howto `thenUgn` \ howto	 ->
    mkSrcLocUgn		 srcline      `thenUgn` \ src_loc ->
    let
	guidance -- ToDo: use Maybe type
	  = (case howto of {
	      []  -> id;
	      [x] -> trace "ignoring unfold howto" }) UnfoldAlways
    in
    returnUgn (RdrInlineValSig (InlineSig var guidance src_loc))

wlk_sig_thing (U_deforest_uprag var srcline) -- "deforest me" user-pragma
  = mkSrcLocUgn srcline	     `thenUgn` \ src_loc ->
    returnUgn (RdrDeforestSig (DeforestSig var src_loc))

wlk_sig_thing (U_magicuf_uprag var str srcline) -- "magic" unfolding user-pragma
  = mkSrcLocUgn srcline	     `thenUgn` \ src_loc ->
    returnUgn (RdrMagicUnfoldingSig (MagicUnfoldingSig var str src_loc))

wlk_sig_thing (U_abstract_uprag tycon srcline) -- abstract-type-synonym user-pragma
  = mkSrcLocUgn srcline	     `thenUgn` \ src_loc ->
    returnUgn (RdrAbstractTypeSig (AbstractTypeSig tycon src_loc))

wlk_sig_thing (U_dspec_uprag tycon dspec_tys srcline)
  = mkSrcLocUgn srcline		 `thenUgn` \ src_loc ->
    wlkList rdMonoType dspec_tys `thenUgn` \ tys ->
    let
	spec_ty = MonoTyCon tycon tys
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
      U_context tcontextl tcontextt -> -- context
	wlkContext  tcontextl	`thenUgn` \ ctxt ->
	wlkMonoType tcontextt	`thenUgn` \ ty	 ->
	returnUgn (OverloadedTy ctxt ty)

      U_uniforall utvs uty -> -- forall type (pragmas)
        wlkList rdU_unkId utvs  `thenUgn` \ tvs ->
	wlkMonoType       uty	`thenUgn` \ ty  ->
	returnUgn (ForAllTy tvs ty)

      other -> -- something else
        wlkMonoType other   `thenUgn` \ ty ->
	returnUgn (UnoverloadedTy ty)

wlkMonoType ttype
  = case ttype of
      U_tname tycon typel -> -- tycon
	wlkList rdMonoType typel `thenUgn` \ tys ->
	returnUgn (MonoTyCon tycon tys)

      U_tllist tlist -> -- list type
        wlkMonoType tlist	`thenUgn` \ ty ->
	returnUgn (ListMonoTy ty)

      U_ttuple ttuple ->
        wlkList rdPolyType ttuple `thenUgn` \ tys ->
	returnUgn (TupleMonoTy tys)

      U_tfun tfun targ ->
        wlkMonoType tfun	`thenUgn` \ ty1 ->
	wlkMonoType targ	`thenUgn` \ ty2 ->
	returnUgn (FunMonoTy ty1 ty2)

      U_namedtvar tyvar -> -- type variable
	returnUgn (MonoTyVar tyvar)

      U_unidict clas t -> -- UniDict (pragmas)
	wlkMonoType t	`thenUgn` \ ty	 ->
	returnUgn (MonoDict clas ty)

      U_unityvartemplate tv_tmpl -> -- pragmas only
	returnUgn (MonoTyVarTemplate tv_tmpl)

#ifdef DPH
wlkMonoType ('v' : xs)
  = wlkMonoType xs	    `thenUgn` \ (ty, xs1) ->
    returnUgn (RdrExplicitPodTy ty, xs1)
    BEND

wlkMonoType ('u' : xs)
  = wlkList rdMonoType xs `thenUgn` \ (tys, xs1) ->
    wlkMonoType xs1	`thenUgn` \ (ty,  xs2)  ->
    returnUgn (RdrExplicitProcessorTy tys ty, xs2)
    BEND BEND
#endif {- Data Parallel Haskell -}

--wlkMonoType oops = panic ("wlkMonoType:"++oops)
\end{code}

\begin{code}
wlkTyConAndTyVars :: U_ttype -> UgnM (ProtoName, [ProtoName])
wlkContext   	  :: U_list  -> UgnM ProtoNameContext
wlkClassAssertTy  :: U_ttype -> UgnM (ProtoName, ProtoName)

wlkTyConAndTyVars ttype
  = wlkMonoType ttype	`thenUgn` \ (MonoTyCon tycon ty_args) ->
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

mk_class_assertion (MonoTyCon name [(MonoTyVar tyname)]) = (name, tyname)
mk_class_assertion other
  = error ("ERROR: malformed type context: "++ppShow 80 (ppr PprForUser other)++"\n")
    -- regrettably, the parser does let some junk past
    -- e.g., f :: Num {-nothing-} => a -> ...
\end{code}

\begin{code}
rdConDecl :: ParseTree -> UgnM ProtoNameConDecl

rdConDecl pt
  = rdU_atype pt    `thenUgn` \ (U_atc con atctypel srcline) ->

    mkSrcLocUgn srcline		`thenUgn` \ src_loc ->
    wlkList rdMonoType atctypel	`thenUgn` \ tys	    ->
    returnUgn (ConDecl con tys src_loc)
\end{code}

%************************************************************************
%*									*
\subsection{Read a ``match''}
%*									*
%************************************************************************

\begin{code}
rdMatch :: ParseTree -> UgnM RdrMatch

rdMatch pt
  = rdU_pbinding pt	`thenUgn` \ (U_pgrhs gpat gdexprs gbind srcfun srcline) ->

    mkSrcLocUgn		srcline	`thenUgn` \ src_loc ->
    wlkPat		gpat	`thenUgn` \ pat     ->
    wlkList rd_guarded	gdexprs	`thenUgn` \ grhss   ->
    wlkBinding		gbind	`thenUgn` \ binding ->

    returnUgn (RdrMatch srcline srcfun pat grhss binding)
  where
    rd_guarded pt
      = rdU_list pt	    `thenUgn` \ list ->
	wlkList rdExpr list `thenUgn` \ [g,e] ->
	returnUgn (g, e)
\end{code}

%************************************************************************
%*									*
\subsection[wlkFixity]{Read in a fixity declaration}
%*									*
%************************************************************************

\begin{code}
{-
wlkFixity :: ParseTree -> UgnM ProtoNameFixityDecl

wlkFixity pt
  = wlkId	   xs	`thenUgn` \ (op,	     xs1) ->
    wlkIdString xs1	`thenUgn` \ (associativity, xs2) ->
    wlkIdString xs2	`thenUgn` \ (prec_str,	     xs3) ->
    let
	precedence = read prec_str
    in
    case associativity of {
      "infix"  -> returnUgn (InfixN op precedence, xs3);
      "infixl" -> returnUgn (InfixL op precedence, xs3);
      "infixr" -> returnUgn (InfixR op precedence, xs3)
    } BEND BEND BEND
-}
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
  = grab_pieces pt  `thenUgn`
	\ (expose_or_hide,
	   modname,
	   bindexp,
	   bindren,
	   binddef,
	   bindfile,
	   srcline) ->

    mkSrcLocUgn		srcline `thenUgn` \ src_loc   ->
    wlkList rdEntity	bindexp `thenUgn` \ imports   ->
    wlkList rdRenaming	bindren `thenUgn` \ renamings ->

    setSrcFileUgn bindfile ( -- OK, we're now looking inside the .hi file...
	wlkBinding binddef
    )				`thenUgn` \ iface_bs  ->

    case (sepDeclsForInterface iface_bs) of {
		(tydecls,classdecls,instdecls,sigs,iimpdecls) ->
    let
	cv_iface
	  = MkInterface	modname
		iimpdecls
		[{-fixity decls-}]  -- can't get fixity decls in here yet (ToDo)
		tydecls
		classdecls
		(cvInstDecls False SLIT(""){-probably superceded by modname < pragmas-}
				   modname instdecls)
			    -- False indicates imported
		(concat (map cvValSig sigs))
		src_loc -- OLD: (mkSrcLoc importing_srcfile srcline)
    in
    returnUgn (
     if null imports then
	ImportAll cv_iface renamings
     else
	expose_or_hide cv_iface imports renamings
    )}
  where
    grab_pieces pt
      = rdU_binding pt `thenUgn` \ binding ->
	returnUgn (
        case binding of
	  U_import a b c d e f -> (ImportSome,    a, b, c, d, e, f)
	  U_hiding a b c d e f -> (ImportButHide, a, b, c, d, e, f)
	)
\end{code}

\begin{code}
rdRenaming :: ParseTree -> UgnM Renaming

rdRenaming pt
  = rdU_list 		 pt	`thenUgn` \ list ->
    wlkList rdU_stringId list	`thenUgn` \ [id1, id2] ->
    returnUgn (MkRenaming id1 id2)
\end{code}

\begin{code}
rdEntity :: ParseTree -> UgnM IE

rdEntity pt
  = rdU_entidt pt   `thenUgn` \ entity ->
    case entity of
      U_entid var -> -- just a value
	returnUgn (IEVar var)

      U_enttype thing -> -- abstract type constructor/class
	returnUgn (IEThingAbs thing)

      U_enttypeall thing -> -- non-abstract type constructor/class
	returnUgn (IEThingAll thing)

      U_enttypecons tycon ctentcons -> -- type con w/ data cons listed
	wlkList rdU_stringId   ctentcons   `thenUgn` \ cons  ->
	returnUgn (IEConWithCons tycon cons)

      U_entclass clas centops -> -- class with ops listed
	wlkList rdU_stringId   centops	`thenUgn` \ ops ->
	returnUgn (IEClsWithOps clas ops)

      U_entmod mod -> -- everything provided by a module
	returnUgn (IEModuleContents mod)
\end{code}
