%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[ReadPragmas2]{Read pragmatic interface info, including Core}

\begin{code}
#include "HsVersions.h"

module ReadPragmas2 (
	ProtoUfBinder(..),

	wlkClassPragma,
	wlkDataPragma,
	wlkInstPragma,
	wlkTySigPragmas,
	wlkTypePragma
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Pretty

import UgenAll

import AbsPrel		( nilDataCon, readUnfoldingPrimOp, PrimOp(..)
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import PrimKind		( guessPrimKind, PrimKind )
import AbsSyn
import BasicLit		( mkMachInt, BasicLit(..) )
import HsCore		-- ****** NEED TO SEE CONSTRUCTORS ******
import HsPragmas	-- ****** NEED TO SEE CONSTRUCTORS ******
import Id		( mkTupleCon )
import IdInfo		-- ( UnfoldingGuidance(..) )
import Maybes		( Maybe(..) )
import PrefixToHs
import PrefixSyn
import ProtoName
import Outputable
import ReadPrefix2	( wlkList, rdConDecl, wlkMonoType )
import Util
\end{code}

\begin{code}
wlkDataPragma :: U_hpragma -> UgnM ProtoNameDataPragmas

wlkDataPragma pragma
  = case pragma of
      U_no_pragma    -> returnUgn (DataPragmas [] [])
      U_idata_pragma cs ss ->
	wlkList rdConDecl cs `thenUgn` \ cons  ->
	wlkList rd_spec   ss `thenUgn` \ specs ->
	returnUgn (DataPragmas cons specs)
  where
    rd_spec pt
      = rdU_hpragma pt  `thenUgn` \ stuff ->
	case stuff of { U_idata_pragma_4s ss ->

	wlkList rdMonoTypeMaybe ss `thenUgn` \ specs ->
	returnUgn specs }
\end{code}

\begin{code}
wlkTypePragma :: U_hpragma -> UgnM TypePragmas

wlkTypePragma pragma
  = case pragma of
      U_no_pragma    -> returnUgn NoTypePragmas
      U_itype_pragma -> returnUgn AbstractTySynonym
\end{code}

\begin{code}
wlkClassPragma :: U_hpragma -> UgnM ProtoNameClassPragmas

wlkClassPragma pragma
  = case pragma of
      U_no_pragma    -> returnUgn NoClassPragmas
      U_iclas_pragma gens ->
	wlkList rdGenPragma gens `thenUgn` \ gen_pragmas ->
	ASSERT(not (null gen_pragmas))
	returnUgn (SuperDictPragmas gen_pragmas)
\end{code}

\begin{code}
wlkInstPragma :: U_hpragma -> UgnM (Maybe FAST_STRING, ProtoNameInstancePragmas)

wlkInstPragma pragma
  = case pragma of
      U_no_pragma    -> returnUgn (Nothing, NoInstancePragmas)

      U_iinst_simpl_pragma modname dfun_gen ->
	wlkGenPragma dfun_gen	`thenUgn` \ gen_pragmas ->
	returnUgn (Just modname, SimpleInstancePragma gen_pragmas)

      U_iinst_const_pragma modname dfun_gen constm_stuff ->
	wlkGenPragma	  dfun_gen     `thenUgn` \ gen_pragma	 ->
	wlkList rd_constm constm_stuff `thenUgn` \ constm_pragmas ->
	returnUgn (Just modname, ConstantInstancePragma gen_pragma constm_pragmas)

rd_constm pt
  = rdU_hpragma pt  `thenUgn` \ stuff ->
    case stuff of { U_iname_pragma_pr name gen ->

    wlkGenPragma gen `thenUgn` \ prag ->
    returnUgn (name, prag) }
\end{code}

\begin{code}
rdGenPragma :: ParseTree -> UgnM ProtoNameGenPragmas

rdGenPragma pt = rdU_hpragma pt `thenUgn` \ prag -> wlkGenPragma prag

wlkGenPragma :: U_hpragma -> UgnM ProtoNameGenPragmas

wlkGenPragma pragma
  = case pragma of
      U_no_pragma -> returnUgn NoGenPragmas

      U_igen_pragma aritee update deforest strct uf speccs ->
	wlk_arity	aritee	 `thenUgn` \ arity   ->
	wlk_update	update	 `thenUgn` \ upd     ->
	wlk_deforest	deforest `thenUgn` \ def     ->
	wlk_strict	strct	 `thenUgn` \ strict  ->
	wlk_unfold	uf	 `thenUgn` \ unfold  ->
	wlkList rd_spec	speccs	 `thenUgn` \ specs   ->
	returnUgn (GenPragmas arity upd def strict unfold specs)
  where
    wlk_arity stuff
      = case stuff of
	  U_no_pragma -> returnUgn Nothing
	  U_iarity_pragma arity ->
	    returnUgn (Just arity)

    ------------
    wlk_update stuff
      = case stuff of
	  U_no_pragma -> returnUgn Nothing
	  U_iupdate_pragma upd_spec ->
	    returnUgn (Just ((read (_UNPK_ upd_spec))::UpdateInfo))

    ------------
    wlk_deforest stuff
      = case stuff of
	  U_no_pragma -> returnUgn Don'tDeforest
	  U_ideforest_pragma -> returnUgn DoDeforest

    ------------
    wlk_unfold stuff
      = case stuff of
	  U_no_pragma -> returnUgn NoImpUnfolding

	  U_imagic_unfolding_pragma magic ->
	    returnUgn (ImpMagicUnfolding magic)

	  U_iunfolding_pragma guide core ->
	    wlkGuidance guide	`thenUgn` \ guidance ->
	    wlkCoreExpr core	`thenUgn` \ coresyn  ->
	    returnUgn (ImpUnfolding guidance coresyn)

    ------------
    wlk_strict stuff
      = case stuff of
	  U_no_pragma -> returnUgn NoImpStrictness

	  U_istrictness_pragma strict_spec wrkr_stuff ->
	    wlkGenPragma wrkr_stuff  `thenUgn` \ wrkr_pragma ->
	    let
		strict_spec_str = _UNPK_ strict_spec
		(is_bot, ww_strict_info)
		  = if (strict_spec_str == "B")
		    then (True,  [])
		    else (False, (read strict_spec_str)::[Demand])
	    in
	    returnUgn (ImpStrictness is_bot ww_strict_info wrkr_pragma)

    ------------
    rd_spec pt
      = rdU_hpragma pt	`thenUgn` \ stuff ->
        case stuff of { U_itype_pragma_pr maybe_tys num_dicts prag ->

        wlkList rdMonoTypeMaybe	maybe_tys `thenUgn` \ mono_tys_maybe ->
	wlkGenPragma		prag	  `thenUgn` \ gen_prag	     ->
	returnUgn (mono_tys_maybe, num_dicts, gen_prag) }
\end{code}

The only tricky case is pragmas on signatures; we have no way of
knowing whether it is a @GenPragma@ or a @ClassOp@ pragma.  So we read
whatever comes, store it in a @RdrTySigPragmas@ structure, and someone
will sort it out later.
\begin{code}
wlkTySigPragmas :: U_hpragma -> UgnM RdrTySigPragmas

wlkTySigPragmas pragma
  = case pragma of
      U_no_pragma -> returnUgn RdrNoPragma

      U_iclasop_pragma dsel defm ->
        wlkGenPragma dsel   `thenUgn` \ dsel_pragma ->
	wlkGenPragma defm   `thenUgn` \ defm_pragma ->
	returnUgn (RdrClassOpPragmas (ClassOpPragmas dsel_pragma defm_pragma))

      other -> 
	wlkGenPragma other  `thenUgn` \ gen_pragmas ->
	returnUgn (RdrGenPragmas gen_pragmas)
\end{code}

\begin{code}
wlkGuidance guide
  = case guide of
      U_iunfold_always -> returnUgn UnfoldAlways

      U_iunfold_if_args num_ty_args num_val_args con_arg_spec size ->
	let
	    con_arg_info = take num_val_args (map cvt (_UNPK_ con_arg_spec))
	    -- if there were 0 args, we want to throw away
	    -- any dummy con_arg_spec stuff...
	in
	returnUgn (UnfoldIfGoodArgs num_ty_args num_val_args
		    con_arg_info size)
	where
	  cvt 'C' = True  -- want a constructor in this arg position
	  cvt _   = False
\end{code}

\begin{code}
wlkCoreExpr :: U_coresyn -> UgnM ProtoNameUnfoldingCoreExpr

wlkCoreExpr core_expr
  = case core_expr of
      U_covar v ->
        wlkCoreId  v	`thenUgn` \ var ->
	returnUgn (UfCoVar var)

      U_coliteral l ->
        wlkBasicLit l	`thenUgn` \ lit ->
	returnUgn (UfCoLit lit)

      U_cocon c ts as ->
        wlkCoreId c		`thenUgn` \ (BoringUfId con) ->
	wlkList rdCoreType ts	`thenUgn` \ tys ->
	wlkList rdCoreAtom as	`thenUgn` \ vs  ->
	returnUgn (UfCoCon con tys vs)

      U_coprim o ts as ->
        wlk_primop	   o	`thenUgn` \ op  ->
	wlkList rdCoreType ts   `thenUgn` \ tys ->
	wlkList rdCoreAtom as	`thenUgn` \ vs  ->
	let
	    fixed_vs = case op of { UfOtherOp pop -> fixup pop vs ; _ -> vs }
	in
	returnUgn (UfCoPrim op tys fixed_vs)
       where

	-- Question: why did ccall once panic if you looked at the
	-- maygc flag?  Was this just laziness or is it not needed?
	-- In that case, modify the stuff that writes them to pragmas
	-- so that it never adds the _GC_ tag. ADR

	wlk_primop op
	  = case op of
	      U_co_primop op_str ->
		returnUgn (UfOtherOp (readUnfoldingPrimOp op_str))

	      U_co_ccall fun_str may_gc a_tys r_ty ->
		wlkList rdCoreType a_tys `thenUgn` \ arg_tys ->
		wlkCoreType	   r_ty	 `thenUgn` \ res_ty  ->
		returnUgn (UfCCallOp fun_str False (is_T_or_F may_gc) arg_tys res_ty)

	      U_co_casm litlit may_gc a_tys r_ty ->
	        wlkBasicLit	    litlit  `thenUgn` \ (MachLitLit casm_str _) ->
		wlkList rdCoreType  a_tys   `thenUgn` \ arg_tys	    ->
		wlkCoreType	    r_ty    `thenUgn` \ res_ty	    ->
		returnUgn (UfCCallOp casm_str True (is_T_or_F may_gc) arg_tys res_ty)
	  where
	    is_T_or_F 0 = False
	    is_T_or_F _ = True

	-- Now *this* is a hack: we can't distinguish Int# literals
	-- from Word# literals as they come in; this is only likely
	-- to bite on the args of certain PrimOps (shifts, etc); so
	-- we look for those and fix things up!!! (WDP 95/05)

	fixup AndOp    [a1, a2] = [fixarg a1, fixarg a2]
	fixup OrOp     [a1, a2] = [fixarg a1, fixarg a2]
	fixup NotOp    [a1]     = [fixarg a1]
	fixup SllOp    [a1, a2] = [fixarg a1, a2]
	fixup SraOp    [a1, a2] = [fixarg a1, a2]
	fixup SrlOp    [a1, a2] = [fixarg a1, a2]
	fixup WordGtOp [a1, a2] = [fixarg a1, fixarg a2]
	fixup WordGeOp [a1, a2] = [fixarg a1, fixarg a2]
	fixup WordLtOp [a1, a2] = [fixarg a1, fixarg a2]
	fixup WordLeOp [a1, a2] = [fixarg a1, fixarg a2]
	fixup WordEqOp [a1, a2] = [fixarg a1, fixarg a2]
	fixup WordNeOp [a1, a2] = [fixarg a1, fixarg a2]
	fixup _	       as	= as

	fixarg (UfCoLitAtom (MachInt i _)) = UfCoLitAtom (MachInt i False{-unsigned-})
	fixarg arg			   = arg

      U_colam vars expr ->
        wlkList rdCoreBinder vars   `thenUgn` \ bs   ->
	wlkCoreExpr	     expr   `thenUgn` \ body ->
	returnUgn (UfCoLam bs body)

      U_cotylam vars expr ->
        wlkList rdU_unkId   vars    `thenUgn` \ tvs  ->
	wlkCoreExpr	    expr    `thenUgn` \ body ->
	returnUgn (foldr UfCoTyLam body tvs)

      U_coapp f as ->
        wlkCoreExpr	   f	`thenUgn` \ fun  ->
	wlkList rdCoreAtom as	`thenUgn` \ args ->
	returnUgn (foldl UfCoApp fun args)

      U_cotyapp e t ->
        wlkCoreExpr e	    `thenUgn` \ expr ->
	wlkCoreType t	    `thenUgn` \ ty	 ->
	returnUgn (UfCoTyApp expr ty)

      U_cocase s as ->
        wlkCoreExpr s	    `thenUgn` \ scrut ->
	wlk_alts    as	    `thenUgn` \ alts  ->
	returnUgn (UfCoCase scrut alts)
       where
	wlk_alts (U_coalg_alts as d)
	  = wlkList rd_alg_alt as   `thenUgn` \ alts  ->
	    wlk_deflt	       d    `thenUgn` \ deflt ->
	    returnUgn (UfCoAlgAlts alts deflt)
	  where
	    rd_alg_alt pt
	      = rdU_coresyn pt	`thenUgn` \ (U_coalg_alt c bs exp) ->

		wlkCoreId	     c   `thenUgn` \ (BoringUfId con) ->
		wlkList rdCoreBinder bs  `thenUgn` \ params	      ->
		wlkCoreExpr	     exp `thenUgn` \ rhs	      ->
		returnUgn (con, params, rhs)

	wlk_alts (U_coprim_alts as d)
	  = wlkList rd_prim_alt as  `thenUgn` \ alts  ->
	    wlk_deflt	    	d   `thenUgn` \ deflt ->
	    returnUgn (UfCoPrimAlts alts deflt)
	  where
	    rd_prim_alt pt
	      = rdU_coresyn pt	`thenUgn` \ (U_coprim_alt l exp) ->

		wlkBasicLit l   `thenUgn` \ lit ->
		wlkCoreExpr exp `thenUgn` \ rhs ->
		returnUgn (lit, rhs)

	wlk_deflt U_conodeflt = returnUgn UfCoNoDefault
	wlk_deflt (U_cobinddeflt v exp)
	  = wlkCoreBinder v	`thenUgn` \ b   ->  
	    wlkCoreExpr   exp	`thenUgn` \ rhs ->
	    returnUgn (UfCoBindDefault b rhs)

      U_colet b expr ->
        wlk_bind    b    `thenUgn` \ bind ->
	wlkCoreExpr expr `thenUgn` \ body ->
	returnUgn (UfCoLet bind body)
       where
	wlk_bind (U_cononrec v expr)
	  = wlkCoreBinder v	`thenUgn` \ b	->
	    wlkCoreExpr   expr	`thenUgn` \ rhs ->
	    returnUgn (UfCoNonRec b rhs)

	wlk_bind (U_corec prs)
	  = wlkList rd_pair prs `thenUgn` \ pairs ->
	    returnUgn (UfCoRec pairs)
	  where
	    rd_pair pt
	      = rdU_coresyn pt	`thenUgn` \ (U_corec_pair v expr) ->

		wlkCoreBinder v    `thenUgn` \ b   ->
		wlkCoreExpr   expr `thenUgn` \ rhs ->
		returnUgn (b, rhs)

      U_coscc c expr ->
        wlk_cc	    c    `thenUgn` \ cc   ->
	wlkCoreExpr expr `thenUgn` \ body ->
	returnUgn (UfCoSCC cc body)
      where
	wlk_cc (U_co_preludedictscc dupd)
	  = wlk_dupd dupd	`thenUgn` \ is_dupd ->
	    returnUgn (UfPreludeDictsCC is_dupd)

	wlk_cc (U_co_alldictscc m g dupd)
	  = wlk_dupd dupd	`thenUgn` \ is_dupd ->
	    returnUgn (UfAllDictsCC m g is_dupd)

	wlk_cc (U_co_usercc n m g dupd cafd)
	  = wlk_dupd dupd	`thenUgn` \ is_dupd ->
	    wlk_cafd cafd	`thenUgn` \ is_cafd ->
	    returnUgn (UfUserCC n m g is_dupd is_cafd)

	wlk_cc (U_co_autocc id m g dupd cafd)
	  = wlkCoreId id	`thenUgn` \ i	    ->
	    wlk_dupd  dupd	`thenUgn` \ is_dupd ->
	    wlk_cafd  cafd	`thenUgn` \ is_cafd ->
	    returnUgn (UfAutoCC i m g is_dupd is_cafd)

	wlk_cc (U_co_dictcc id m g dupd cafd)
	  = wlkCoreId id	`thenUgn` \ i	    ->
	    wlk_dupd  dupd	`thenUgn` \ is_dupd ->
	    wlk_cafd  cafd	`thenUgn` \ is_cafd ->
	    returnUgn (UfDictCC i m g is_dupd is_cafd)

	------
	wlk_cafd U_co_scc_noncaf  = returnUgn False
	wlk_cafd U_co_scc_caf	  = returnUgn True

	wlk_dupd U_co_scc_nondupd = returnUgn False
	wlk_dupd U_co_scc_dupd	  = returnUgn True
\end{code}

\begin{code}
type ProtoUfBinder = (ProtoName, PolyType ProtoName)

rdCoreBinder :: ParseTree -> UgnM ProtoUfBinder

rdCoreBinder pt = rdU_coresyn pt `thenUgn` \ x -> wlkCoreBinder x

wlkCoreBinder :: U_coresyn -> UgnM ProtoUfBinder

wlkCoreBinder (U_cobinder b t)
  = wlkCoreType	t   `thenUgn` \ ty ->
    returnUgn (b, ty)

rdCoreAtom pt
  = rdU_coresyn pt `thenUgn` \ atom ->
    case atom of
      U_colit l ->
        wlkBasicLit l	`thenUgn` \ lit ->
	returnUgn (UfCoLitAtom lit)

      U_colocal var ->
        wlkCoreId var	`thenUgn` \ v ->
	returnUgn (UfCoVarAtom v)
\end{code}

\begin{code}
rdCoreType :: ParseTree -> UgnM ProtoNamePolyType

rdCoreType pt = rdU_ttype pt `thenUgn` \ ttype -> wlkCoreType ttype

wlkCoreType :: U_ttype -> UgnM ProtoNamePolyType

wlkCoreType (U_uniforall ts t)
  = wlkList rdU_unkId ts    `thenUgn` \ tvs ->
    wlkMonoType       t	    `thenUgn` \ ty  ->
    returnUgn (ForAllTy tvs ty)

wlkCoreType other
  = wlkMonoType other 	`thenUgn` \ ty ->
    returnUgn (UnoverloadedTy ty)
\end{code}

\begin{code}
{- OLD???
wlkCoreTypeMaybe :: ParseTree -> RETN_TYPE(Maybe ProtoNamePolyType, FAST_STRING)

wlkCoreTypeMaybe ('2' : 'D' : xs) = RETN(Nothing, xs)
wlkCoreTypeMaybe ('2' : 'E' : xs)
  = wlkCoreType xs)    `thenUgn` \ (ty, xs1) ->
    RETN(Just ty, xs1)
    BEND
-}

rdMonoTypeMaybe pt
  = rdU_ttype pt `thenUgn` \ ty ->
    case ty of
      U_ty_maybe_nothing -> returnUgn Nothing

      U_ty_maybe_just t ->
        wlkMonoType t	`thenUgn` \ mono_ty ->
	returnUgn (Just mono_ty)
\end{code}

\begin{code}
wlkCoreId :: U_coresyn -> UgnM (UfId ProtoName)

wlkCoreId (U_co_id v)
  = returnUgn (BoringUfId (cvt_IdString v))

wlkCoreId (U_co_orig_id mod nm)
  = returnUgn (BoringUfId (Imp mod nm [mod]{-dubious, but doesn't matter-} nm))

wlkCoreId (U_co_sdselid clas super_clas)
  = returnUgn (SuperDictSelUfId clas super_clas)

wlkCoreId (U_co_classopid clas method)
  = returnUgn (ClassOpUfId clas method)

wlkCoreId (U_co_defmid clas method)
  = returnUgn (DefaultMethodUfId clas method)

wlkCoreId (U_co_dfunid clas t)
  = wlkCoreType t   `thenUgn` \ ty ->
    returnUgn (DictFunUfId clas ty)

wlkCoreId (U_co_constmid clas op t)
  = wlkCoreType t   `thenUgn` \ ty ->
    returnUgn (ConstMethodUfId clas op ty)

wlkCoreId (U_co_specid id tys)
  = wlkCoreId		    id	`thenUgn` \ unspec    ->
    wlkList rdMonoTypeMaybe tys	`thenUgn` \ ty_maybes ->
    returnUgn (SpecUfId unspec ty_maybes)

wlkCoreId (U_co_wrkrid un)
  = wlkCoreId un	`thenUgn` \ unwrkr ->
    returnUgn (WorkerUfId unwrkr)

------------
cvt_IdString :: FAST_STRING -> ProtoName

cvt_IdString s
  = if (_HEAD_ s /= '_') then
--	trace (show s++(show (_HEAD_ s /= '_'))++(_HEAD_ s):'_':"/*0*/\n") (
	boring
--	)
    else if (sub_s == SLIT("NIL_")) then
--	trace (show s++"/*1*/"++show sub_s++"/"++show (_SUBSTR_ s 5 99999)++"\n") (
    	Prel (WiredInVal nilDataCon)
--	)
    else if (sub_s == SLIT("TUP_")) then
--	trace (show s++"/*2*/"++show sub_s++"/"++show (_SUBSTR_ s 5 99999)++"\n") (
	Prel (WiredInVal (mkTupleCon arity))
--	)
    else
--	trace (show s++"/*3*/"++show sub_s++"/"++show (_SUBSTR_ s 5 99999)++"\n") (
	boring
--	)
  where
    boring = Unk s
    sub_s  = _SUBSTR_ s 1 4	-- chars 1--4 (0-origin)
    arity  = read (_UNPK_ (_SUBSTR_ s 5 999999))
				-- chars 5 onwards give the arity
\end{code}

\begin{code}
wlkBasicLit :: U_literal -> UgnM BasicLit

wlkBasicLit (U_norepr n d)
  = let
	num = ((read (_UNPK_ n)) :: Integer)
	den = ((read (_UNPK_ d)) :: Integer)
    in
    returnUgn (NoRepRational (num % den))

wlkBasicLit other
  = returnUgn (
    case other of
      U_intprim    s -> mkMachInt   (as_integer  s)
      U_doubleprim s -> MachDouble  (as_rational s)
      U_floatprim  s -> MachFloat   (as_rational s)
      U_charprim   s -> MachChar    (as_char     s)
      U_stringprim s -> MachStr	    (as_string   s)

      U_clitlit    s k -> MachLitLit (as_string  s) (guessPrimKind (_UNPK_ k))

      U_norepi	   s -> NoRepInteger (as_integer s)
      U_noreps	   s -> NoRepStr     (as_string  s)
    )
  where
    as_char s	  = _HEAD_ s
    as_integer s  = readInteger (_UNPK_ s)
    as_rational s = _readRational (_UNPK_ s) -- non-std
    as_string s	  = s
\end{code}
