%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[ReadPragmas]{Read pragmatic interface info, including Core}

\begin{code}
-- HBC does not have stack stubbing; you get a space leak w/
-- default defns from HsVersions.h.

-- GHC may be overly slow to compile w/ the defaults...

#define BIND {--}
#define _TO_ `thenLft` ( \ {--}
#define BEND )
#define RETN returnLft
#define RETN_TYPE LiftM

#include "HsVersions.h"
\end{code}

\begin{code}
module ReadPragmas where

IMPORT_Trace		-- ToDo: rm (debugging)
import Pretty

import AbsPrel		( nilDataCon, readUnfoldingPrimOp, PrimOp(..), PrimKind
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsSyn
import BasicLit		( mkMachInt, BasicLit(..) )
import HsCore		-- ****** NEED TO SEE CONSTRUCTORS ******
import HsPragmas	-- ****** NEED TO SEE CONSTRUCTORS ******
import Id		( mkTupleCon )
import IdInfo		-- ( UnfoldingGuidance(..) )
import LiftMonad
import Maybes		( Maybe(..) )
import PrefixToHs
import PrefixSyn
import ProtoName
import Outputable
import ReadPrefix	( rdList, rdId, rdIdString, rdString, rdConDecl, rdMonoType )
import Util
\end{code}

\begin{code}
rdDataPragma :: String -> RETN_TYPE (ProtoNameDataPragmas, String)

rdDataPragma ('P' : 'N' : xs) = RETN (DataPragmas [] [], xs)

rdDataPragma ('P' : 'd' : xs)
  = BIND (rdList (rdConDecl srcfile) xs)  _TO_ (cons, xs1) ->
    BIND (rdList rd_spec xs1)  		  _TO_ (specs, xs2) ->
    RETN (DataPragmas cons specs, xs2)
    BEND BEND
  where
    srcfile = SLIT("<pragma>")

    rd_spec ('P' : '4' : xs)
      = BIND (rdList rdMonoTypeMaybe xs)  _TO_ (spec, xs1) ->
	RETN (spec, xs1)
	BEND
\end{code}

\begin{code}
rdTypePragma :: String -> RETN_TYPE (TypePragmas, String)

rdTypePragma ('P' : 'N' : xs) = RETN (NoTypePragmas, xs)
rdTypePragma ('P' : 't' : xs) = RETN (AbstractTySynonym, xs)
\end{code}

\begin{code}
rdClassPragma :: String -> RETN_TYPE (ProtoNameClassPragmas, String)

rdClassPragma ('P' : 'N' : xs) = RETN (NoClassPragmas, xs)
rdClassPragma ('P' : 'c' : xs)
  = BIND (rdList rdGenPragma xs)   _TO_ (gen_pragmas, xs1) ->
    ASSERT(not (null gen_pragmas))
    RETN (SuperDictPragmas gen_pragmas, xs1)
    BEND
\end{code}

\begin{code}
rdInstPragma :: String -> RETN_TYPE (Maybe FAST_STRING, ProtoNameInstancePragmas, String)

rdInstPragma ('P' : 'N' : xs) = RETN (Nothing, NoInstancePragmas, xs)

rdInstPragma ('P' : 'i' : 's' : xs)
  = BIND (rdIdString  xs)   	_TO_ (modname,	   xs1) ->
    BIND (rdGenPragma xs1)	_TO_ (gen_pragmas, xs2) ->
    RETN (Just modname, SimpleInstancePragma gen_pragmas, xs2)
    BEND BEND

rdInstPragma ('P' : 'i' : 'c' : xs)
  = BIND (rdIdString	     xs)  _TO_ (modname,	xs1) ->
    BIND (rdGenPragma	     xs1) _TO_ (gen_pragma,	xs2) ->
    BIND (rdList rd_constm   xs2) _TO_ (constm_pragmas, xs3) ->
    RETN (Just modname, ConstantInstancePragma gen_pragma constm_pragmas, xs3)
    BEND BEND BEND

rd_constm ('P' : '1' : xs)
  = BIND (rdId	xs)  _TO_ (name, xs1) ->
    BIND (rdGenPragma  xs1) _TO_ (prag, xs2) ->
    RETN ((name, prag), xs2)
    BEND BEND
\end{code}

\begin{code}
rdGenPragma :: String -> RETN_TYPE (ProtoNameGenPragmas, String)

rdGenPragma ('P' : 'N' : xs) = RETN (NoGenPragmas, xs)

rdGenPragma ('P': 'g' : xs)
  = BIND (rd_arity  xs)	      _TO_ (arity,  xs1) ->
    BIND (rd_update xs1)      _TO_ (upd,    xs2) ->
    BIND (rd_strict xs2)      _TO_ (strict, xs3) ->
    BIND (rd_unfold xs3)      _TO_ (unfold, xs4) ->
    BIND (rdList rd_spec xs4) _TO_ (specs,  xs5) ->
ToDo: do something for DeforestInfo
    RETN (GenPragmas arity upd strict unfold specs, xs5)
    BEND BEND BEND BEND BEND
  where
    rd_arity ('P' : 'N' : xs) = RETN (Nothing, xs)
    rd_arity ('P' : 'A' : xs)
      = BIND (rdIdString xs)	_TO_ (a_str, xs1) ->
	RETN (Just ((read (_UNPK_ a_str))::Int), xs1)
	BEND

    rd_update ('P' : 'N' : xs) = RETN (Nothing, xs)
    rd_update ('P' : 'u' : xs)
      = BIND (rdIdString xs)	_TO_ (upd_spec, xs1) ->
	RETN (Just ((read (_UNPK_ upd_spec))::UpdateInfo), xs1)
	BEND

    rd_unfold ('P' : 'N' : xs) = RETN (NoImpUnfolding, xs)

    rd_unfold ('P' : 'M' : xs)
      = BIND (rdIdString xs)	_TO_ (str, xs1) ->
	RETN (ImpMagicUnfolding str, xs1)
	BEND

    rd_unfold ('P' : 'U' : xs)
      = BIND (rdGuidance xs)	_TO_ (guidance, xs1) ->
	BIND (rdCoreExpr xs1)	_TO_ (core, 	xs2) ->
	RETN (ImpUnfolding guidance core, xs2)
	BEND BEND

    rd_strict ('P' : 'N' : xs) = RETN (NoImpStrictness, xs)
    rd_strict ('P' : 'S' : xs)
      = BIND (rdString    xs)	_TO_ (strict_spec, xs1) ->
	BIND (rdGenPragma xs1)	_TO_ (wrkr_pragma, xs2) ->
	let
	    ww_strict_info = (read (_UNPK_ strict_spec))::[Demand]
	in
	RETN (ImpStrictness (trace "ImpStrictness" False) ww_strict_info wrkr_pragma, xs2)
	BEND BEND

    rd_spec ('P' : '2' : xs)
      = BIND (rdList rdMonoTypeMaybe xs)  _TO_ (mono_tys_maybe, xs1) ->
	BIND (rdIdString    	     xs1) _TO_ (num_dicts,  	xs2) ->
	BIND (rdGenPragma            xs2) _TO_ (gen_prag, 	xs3) ->
	RETN ((mono_tys_maybe, ((read (_UNPK_ num_dicts))::Int), gen_prag), xs3)
	BEND BEND BEND
\end{code}

The only tricky case is pragmas on signatures; we have no way of
knowing whether it is a @GenPragma@ or a @ClassOp@ pragma.  So we read
whatever comes, store it in a @RdrTySigPragmas@ structure, and someone
will sort it out later.
\begin{code}
rdTySigPragmas :: String -> RETN_TYPE (RdrTySigPragmas, String)

rdTySigPragmas ('P' : 'N' : xs) = RETN (RdrNoPragma, xs)

rdTySigPragmas ('P' : 'o' : xs)
  = BIND (rdGenPragma xs)   _TO_ (dsel_pragma, xs1) ->
    BIND (rdGenPragma xs1)  _TO_ (defm_pragma, xs2) ->
    RETN (RdrClassOpPragmas (ClassOpPragmas dsel_pragma defm_pragma), xs2)
    BEND BEND

rdTySigPragmas xs
  = BIND (rdGenPragma 	 xs)  _TO_ (gen_pragmas, xs1) ->
    RETN (RdrGenPragmas gen_pragmas, xs1)
    BEND
\end{code}

\begin{code}
rdGuidance ('P' : 'x' : xs) = RETN (UnfoldAlways, xs)

-- EssentialUnfolding should never appear in interfaces, so we
-- don't have any way to read them.

rdGuidance ('P' : 'y' : xs)
  = BIND (rdIdString xs)	_TO_ (m_ty_args,    xs1) ->
    BIND (rdIdString xs1)	_TO_ (n_val_args,   xs2) ->
    BIND (rdIdString xs2)	_TO_ (con_arg_spec, xs3) ->
    BIND (rdIdString xs3)   	_TO_ (size_str,	    xs4) ->
    let
	num_val_args = ((read (_UNPK_ n_val_args)) :: Int)
	con_arg_info = take num_val_args (map cvt (_UNPK_ con_arg_spec))
	-- if there were 0 args, we want to throw away
	-- any dummy con_arg_spec stuff...
    in
    RETN (UnfoldIfGoodArgs (read (_UNPK_ m_ty_args)) num_val_args
		con_arg_info (read (_UNPK_ size_str)), xs4)
    BEND BEND BEND BEND
  where
    cvt 'C' = True  -- want a constructor in this arg position
    cvt _   = False

{- OLD:
rdGuidance ('P' : 'z' : xs)
  = BIND (rdIdString xs)	_TO_ (m_ty_args, xs1) ->
    BIND (rdIdString xs1)	_TO_ (size,	 xs2) ->
    RETN (trace "read:UnfoldIsCon" UnfoldNever, xs2) -- ToDo: rm
    BEND BEND
-}
\end{code}

\begin{code}
rdCoreExpr :: String -> RETN_TYPE (ProtoNameUnfoldingCoreExpr, String)

rdCoreExpr ('F' : 'g' : xs)
  = BIND (rdCoreId   xs)	_TO_ (var, xs1) ->
    RETN (UfCoVar var, xs1)
    BEND

rdCoreExpr ('F' : 'h' : xs)
  = BIND (rdBasicLit xs)	_TO_ (lit, xs1) ->
    RETN (UfCoLit lit, xs1)
    BEND

rdCoreExpr ('F' : 'i' : xs)
  = BIND (rdCoreId xs)		    _TO_ (BoringUfId con, xs1) ->
    BIND (rdList rdCoreType xs1)    _TO_ (tys, xs2) ->
    BIND (rdList rdCoreAtom xs2)    _TO_ (vs,  xs3) ->
    RETN (UfCoCon con tys vs, xs3)
    BEND BEND BEND

rdCoreExpr ('F' : 'j' : xs)
  = BIND (rd_primop xs)		    _TO_ (op,  xs1) ->
    BIND (rdList rdCoreType xs1)    _TO_ (tys, xs2) ->
    BIND (rdList rdCoreAtom xs2)    _TO_ (vs,  xs3) ->
    RETN (UfCoPrim op tys vs, xs3)
    BEND BEND BEND
  where

-- Question: why did ccall once panic if you looked at the maygc flag?
-- Was this just laziness or is it not needed?  In that case, modify
-- the stuff that writes them to pragmas so that it never adds the _GC_
-- tag. ADR

    rd_primop ('F' : 'w' : xs)
      = BIND (rdIdString xs)	_TO_ (op_str, xs1) ->
	RETN (UfOtherOp (readUnfoldingPrimOp op_str), xs1)
	BEND
    rd_primop ('F' : 'x' : t_or_f : xs)
      = BIND (rdIdString    	xs)  _TO_ (fun_str, xs1) ->
	BIND (rdList rdCoreType xs1) _TO_ (arg_tys, xs2) ->
	BIND (rdCoreType    	xs2) _TO_ (res_ty,  xs3) ->
	RETN (UfCCallOp fun_str False (is_T_or_F t_or_f) arg_tys res_ty, xs3)
	BEND BEND BEND
    rd_primop ('F' : 'y' : t_or_f : xs)
      = BIND (rdBasicLit    	xs)  _TO_ (casm_litlit, xs1) ->
	BIND (rdList rdCoreType xs1) _TO_ (arg_tys, xs2) ->
	BIND (rdCoreType    	xs2) _TO_ (res_ty,  xs3) ->
	let
	    (MachLitLit casm_str _) = casm_litlit
	in
	RETN (UfCCallOp casm_str True (is_T_or_F t_or_f) arg_tys res_ty, xs3)
	BEND BEND BEND

    is_T_or_F 'T' = True
    is_T_or_F 'F' = False

rdCoreExpr ('F' : 'k' : xs)
  = BIND (rdList rdCoreBinder xs)   _TO_ (bs,   xs1) ->
    BIND (rdCoreExpr	      xs1)  _TO_ (body, xs2) ->
    RETN (UfCoLam bs body, xs2)
    BEND BEND

rdCoreExpr ('F' : 'l' : xs)
  = BIND (rdList rdId	xs)	    _TO_ (tvs,  xs1) ->
    BIND (rdCoreExpr	xs1)	    _TO_ (body, xs2) ->
    RETN (foldr UfCoTyLam body tvs, xs2)
    BEND BEND

rdCoreExpr ('F' : 'm' : xs)
  = BIND (rdCoreExpr	    xs)	    _TO_ (fun,  xs1) ->
    BIND (rdList rdCoreAtom xs1)    _TO_ (args, xs2) ->
    RETN (foldl UfCoApp fun args, xs2)
    BEND BEND


rdCoreExpr ('F' : 'n' : xs)
  = BIND (rdCoreExpr	xs)	    _TO_ (expr, xs1) ->
    BIND (rdCoreType	xs1)	    _TO_ (ty,   xs2) ->
    RETN (UfCoTyApp expr ty, xs2)
    BEND BEND

rdCoreExpr ('F' : 'o' : xs)
  = BIND (rdCoreExpr	xs)	    _TO_ (scrut, xs1) ->
    BIND (rd_alts	xs1)	    _TO_ (alts,  xs2) ->
    RETN (UfCoCase scrut alts, xs2)
    BEND BEND
  where
    rd_alts ('F' : 'q' : xs)
      = BIND (rdList rd_alg_alt xs)	_TO_ (alts,  xs1) ->
	BIND (rd_deflt	    	xs1)	_TO_ (deflt, xs2) ->
	RETN (UfCoAlgAlts alts deflt, xs2)
	BEND BEND
      where
	rd_alg_alt ('F' : 'r' : xs)
	  = BIND (rdCoreId	      xs)   _TO_ (BoringUfId con, xs1) ->
	    BIND (rdList rdCoreBinder xs1)  _TO_ (params,   	  xs2) ->
	    BIND (rdCoreExpr	      xs2)  _TO_ (rhs,	    	  xs3) ->
	    RETN ((con, params, rhs), xs3)
	    BEND BEND BEND

    rd_alts ('F' : 's' : xs)
      = BIND (rdList rd_prim_alt xs)	_TO_ (alts,  xs1) ->
	BIND (rd_deflt	    	 xs1)	_TO_ (deflt, xs2) ->
	RETN (UfCoPrimAlts alts deflt, xs2)
	BEND BEND
      where
	rd_prim_alt ('F' : 't' : xs)
	  = BIND (rdBasicLit	xs)   _TO_ (lit, xs1) ->
	    BIND (rdCoreExpr	xs1)  _TO_ (rhs, xs2) ->
	    RETN ((lit, rhs), xs2)
	    BEND BEND

    rd_deflt ('F' : 'u' : xs) = RETN (UfCoNoDefault, xs)
    rd_deflt ('F' : 'v' : xs)
      = BIND (rdCoreBinder xs)	_TO_ (b,   xs1) ->
        BIND (rdCoreExpr   xs1)	_TO_ (rhs, xs2) ->
	RETN (UfCoBindDefault b rhs, xs2)
	BEND BEND

rdCoreExpr ('F' : 'p' : xs)
  = BIND (rd_bind    xs)  _TO_ (bind, xs1) ->
    BIND (rdCoreExpr xs1) _TO_ (body, xs2) ->
    RETN (UfCoLet bind body, xs2)
    BEND BEND
  where
    rd_bind ('F' : 'd' : xs)
      = BIND (rdCoreBinder xs)	_TO_ (b,   xs1) ->
        BIND (rdCoreExpr   xs1) _TO_ (rhs, xs2) ->
	RETN (UfCoNonRec b rhs, xs2)
	BEND BEND

    rd_bind ('F' : 'e' : xs)
      = BIND (rdList rd_pair xs) _TO_ (pairs, xs1) ->
        RETN (UfCoRec pairs, xs1)
        BEND
      where
	rd_pair ('F' : 'f' : xs)
	  = BIND (rdCoreBinder xs)  _TO_ (b,   xs1) ->
	    BIND (rdCoreExpr   xs1) _TO_ (rhs, xs2) ->
	    RETN ((b, rhs), xs2)
	    BEND BEND

rdCoreExpr ('F' : 'z' : xs)
  = BIND (rd_cc	     xs)  _TO_ (cc,   xs1) ->
    BIND (rdCoreExpr xs1) _TO_ (body, xs2) ->
    RETN (UfCoSCC cc body, xs2)
    BEND BEND
  where
    rd_cc ('F' : '?' : 'a' : xs)
      = BIND (rd_dupd xs)	_TO_ (is_dupd, xs1) ->
        RETN (UfPreludeDictsCC is_dupd, xs1)
	BEND

    rd_cc ('F' : '?' : 'b' : xs)
      = BIND (rdString xs)	_TO_ (m,       xs1) ->
	BIND (rdString xs1)	_TO_ (g,       xs2) ->
        BIND (rd_dupd  xs2)	_TO_ (is_dupd, xs3) ->
	RETN (UfAllDictsCC m g is_dupd, xs3)
	BEND BEND BEND

    rd_cc ('F' : '?' : 'c' : xs)
      = BIND (rdString xs)	_TO_ (n, xs1) ->
	BIND (rdString xs1)	_TO_ (m, xs2) ->
	BIND (rdString xs2)	_TO_ (g, xs3) ->
	BIND (rd_dupd  xs3)	_TO_ (is_dupd, xs4) ->
	BIND (rd_cafd  xs4)	_TO_ (is_cafd, xs5) ->
	RETN (UfUserCC n m g is_dupd is_cafd, xs5)
	BEND BEND BEND BEND BEND

    rd_cc ('F' : '?' : 'd' : xs)
      = BIND (rdCoreId	 xs)	_TO_ (i, xs1) ->
	BIND (rdString xs1)	_TO_ (m, xs2) ->
	BIND (rdString xs2)	_TO_ (g, xs3) ->
	BIND (rd_dupd  xs3)	_TO_ (is_dupd, xs4) ->
	BIND (rd_cafd  xs4)	_TO_ (is_cafd, xs5) ->
	RETN (UfAutoCC i m g is_dupd is_cafd, xs5)
	BEND BEND BEND BEND BEND

    rd_cc ('F' : '?' : 'e' : xs)
      = BIND (rdCoreId	 xs)	_TO_ (i, xs1) ->
	BIND (rdString xs1)	_TO_ (m, xs2) ->
	BIND (rdString xs2)	_TO_ (g, xs3) ->
	BIND (rd_dupd  xs3)	_TO_ (is_dupd, xs4) ->
	BIND (rd_cafd  xs4)	_TO_ (is_cafd, xs5) ->
	RETN (UfDictCC i m g is_dupd is_cafd, xs5)
	BEND BEND BEND BEND BEND

    ------
    rd_cafd ('F' : '?' : 'f' : xs) = RETN (False, xs)
    rd_cafd ('F' : '?' : 'g' : xs) = RETN (True,  xs)
--  rd_cafd xs = panic ("rd_cafd:\n"++xs)
    
    rd_dupd ('F' : '?' : 'h' : xs) = RETN (False, xs)
    rd_dupd ('F' : '?' : 'i' : xs) = RETN (True,  xs)
\end{code}

\begin{code}
rdCoreBinder ('F' : 'a' : xs)
  = BIND (rdId		xs)	_TO_ (b,  xs1) ->
    BIND (rdCoreType	xs1)	_TO_ (ty, xs2) ->
    RETN ((b, ty), xs2)
    BEND BEND

rdCoreAtom ('F' : 'b' : xs)
  = BIND (rdBasicLit xs) _TO_ (lit, xs1) ->
    RETN (UfCoLitAtom lit, xs1)
    BEND

rdCoreAtom ('F' : 'c' : xs)
  = BIND (rdCoreId xs)	 _TO_ (v,   xs1) ->
    RETN (UfCoVarAtom v, xs1)
    BEND
\end{code}

\begin{code}
rdCoreType :: String -> RETN_TYPE (ProtoNamePolyType, String)

rdCoreType ('2' : 'C' : xs)
  = BIND (rdList rdId xs)   	_TO_ (tvs, xs1) ->
    BIND (rdMonoType  xs1)  	_TO_ (ty,  xs2) ->
    RETN (ForAllTy tvs ty, xs2)
    BEND BEND

rdCoreType other
  = BIND (rdMonoType other) 	_TO_ (ty, xs1) ->
    RETN (UnoverloadedTy ty, xs1)
    BEND
\end{code}

\begin{code}
rdCoreTypeMaybe :: String -> RETN_TYPE(Maybe ProtoNamePolyType, String)

rdCoreTypeMaybe ('2' : 'D' : xs) = RETN(Nothing, xs)
rdCoreTypeMaybe ('2' : 'E' : xs)
  = BIND (rdCoreType xs)    _TO_ (ty, xs1) ->
    RETN(Just ty, xs1)
    BEND

rdMonoTypeMaybe ('2' : 'D' : xs) = RETN (Nothing, xs)

rdMonoTypeMaybe ('2' : 'E' : xs)
  = BIND (rdMonoType xs) _TO_ (mono_ty, xs1) ->
    RETN (Just mono_ty, xs1)
    BEND
\end{code}

\begin{code}
rdCoreId :: String -> RETN_TYPE (UfId ProtoName, String)

rdCoreId ('F' : '1' : xs)
  = BIND (rdIdString xs)	_TO_ (v, xs1) ->
    RETN (BoringUfId (cvt_IdString v), xs1)
    BEND
rdCoreId ('F' : '9' : xs)
  = BIND (rdIdString xs)	_TO_ (mod, xs1) ->
    BIND (rdIdString xs1)	_TO_ (nm,  xs2) ->
    RETN (BoringUfId (Imp mod nm [mod]{-dubious, but doesn't matter-} nm), xs2)
    BEND BEND
rdCoreId ('F' : '2' : xs)
  = BIND (rdId xs)		_TO_ (clas, 	  xs1) ->
    BIND (rdId xs1)		_TO_ (super_clas, xs2) ->
    RETN (SuperDictSelUfId clas super_clas, xs2)
    BEND BEND
rdCoreId ('F' : '3' : xs)
  = BIND (rdId xs)		_TO_ (clas,   xs1) ->
    BIND (rdId xs1)		_TO_ (method, xs2) ->
    RETN (ClassOpUfId clas method, xs2)
    BEND BEND
rdCoreId ('F' : '4' : xs)
  = BIND (rdId xs)		_TO_ (clas,   xs1) ->
    BIND (rdId xs1)		_TO_ (method, xs2) ->
    RETN (DefaultMethodUfId clas method, xs2)
    BEND BEND
rdCoreId ('F' : '5' : xs)
  = BIND (rdId 	     xs)	_TO_ (clas, xs1) ->
    BIND (rdCoreType xs1)	_TO_ (ty,   xs2) ->
    RETN (DictFunUfId clas ty, xs2)
    BEND BEND
rdCoreId ('F' : '6' : xs)
  = BIND (rdId 	     xs)	_TO_ (clas, xs1) ->
    BIND (rdId	     xs1)	_TO_ (op,   xs2) ->
    BIND (rdCoreType xs2)	_TO_ (ty,   xs3) ->
    RETN (ConstMethodUfId clas op ty, xs3)
    BEND BEND BEND
rdCoreId ('F' : '7' : xs)
  = BIND (rdCoreId xs)	    	    	_TO_ (unspec,    xs1) ->
    BIND (rdList rdMonoTypeMaybe xs1)	_TO_ (ty_maybes, xs2) ->
    RETN (SpecUfId unspec ty_maybes, xs2)
    BEND BEND
rdCoreId ('F' : '8' : xs)
  = BIND (rdCoreId xs)	    	    	_TO_ (unwrkr,    xs1) ->
    RETN (WorkerUfId unwrkr, xs1)
    BEND

cvt_IdString :: FAST_STRING -> ProtoName

cvt_IdString s
  = if (_HEAD_ s /= '_') then
	boring
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
rdBasicLit :: String -> RETN_TYPE (BasicLit, String)

rdBasicLit ('R' : xs)
  = BIND (rdString xs)  _TO_ (n, xs1) ->
    BIND (rdString xs1) _TO_ (d, xs2) ->
    let
	num = ((read (_UNPK_ n)) :: Integer)
	den = ((read (_UNPK_ d)) :: Integer)
    in
    RETN (NoRepRational (num % den), xs2)
    BEND BEND

rdBasicLit ( tag : xs)
  = BIND (rdString xs) _TO_ (x, zs) ->
    let
	s = _UNPK_ x

	as_char	    = chr ((read s) :: Int)
	    -- a char comes in as a number string
	    -- representing its ASCII code
	as_integer  = readInteger s
#ifdef __GLASGOW_HASKELL__
	as_rational = _readRational s -- non-std
#else
	as_rational = ((read s)::Rational)
#endif
	as_double   = ((read s) :: Double)
    in
    case tag of {
     'H' -> RETN (mkMachInt 	as_integer, zs);
     'J' -> RETN (MachDouble 	as_rational,zs);
     'K' -> RETN (MachFloat 	as_rational,zs);
     'P' -> RETN (MachChar 	as_char,    zs);
     'V' -> RETN (MachStr 	x,	    zs);
     'Y' -> BIND (rdString zs) _TO_ (k, zs2) ->
	    RETN (MachLitLit 	x (guessPrimKind k), zs2)
	    BEND;
     'I' -> RETN (NoRepInteger	as_integer, zs);
     's' -> RETN (NoRepStr	x,  	    zs)
    } BEND
\end{code}
