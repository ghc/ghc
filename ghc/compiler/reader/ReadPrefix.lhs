%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[ReadPrefix]{Read prefix-form input}

This module contains a function, @rdModule@, which reads a Haskell
module in `prefix form' emitted by the Lex/Yacc parser.

The prefix form string is converted into an algebraic data type
defined in @PrefixSyn@.

Identifier names are converted into the @ProtoName@ data type.

@sf@ is used consistently to mean ``source file'' (name).

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
module ReadPrefix (
	rdModule,

	rdList, rdId, rdIdString, rdString, rdConDecl, rdMonoType
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Pretty

import AbsSyn
import HsCore		-- ****** NEED TO SEE CONSTRUCTORS ******
import HsPragmas	-- ****** NEED TO SEE CONSTRUCTORS ******
import IdInfo		( UnfoldingGuidance(..) )
import LiftMonad
import Maybes		( Maybe(..) )
import PrefixToHs
import PrefixSyn
import ProtoName
import Outputable
import ReadPragmas
import SrcLoc		( mkSrcLoc )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[ReadPrefix-help]{Help Functions}
%*									*
%************************************************************************

\begin{code}
rdList :: (String -> RETN_TYPE (a, String)) -> String -> RETN_TYPE ([a], String)

rdList rd_it ('N':xs) = RETN ([], xs)
rdList rd_it ('L':xs)
  = BIND (rd_it xs)		_TO_ (hd_it, xs1) ->
    BIND (rdList rd_it xs1)	_TO_ (tl_it, xs2) ->
    RETN (hd_it : tl_it, xs2)
    BEND BEND
rdList rd_it junk = panic ("ReadPrefix.rdList:"++junk)

rdString, rdIdString :: String -> RETN_TYPE (FAST_STRING, String)
rdId :: String -> RETN_TYPE (ProtoName, String)

rdString ('#':xs) = BIND (split_at_tab xs) _TO_ (str, rest) ->
		    RETN (_PK_ (de_escape str), rest)
		    BEND
  where
    -- partain: tabs and backslashes are escaped
    de_escape []		= []
    de_escape ('\\':'\\':xs)	= '\\' : (de_escape xs)
    de_escape ('\\':'t':xs)	= '\t' : (de_escape xs)
    de_escape (x:xs)		= x    : (de_escape xs)

rdString xs = panic ("ReadPrefix.rdString:"++xs)

rdIdString ('#':xs) = BIND (split_at_tab xs) _TO_ (stuff,rest) -> -- no de-escaping...
		      RETN (_PK_ stuff, rest)
		      BEND
rdIdString other    = panic ("rdIdString:"++other)

 -- no need to de-escape it...
rdId ('#':xs) = BIND (split_at_tab xs) _TO_ (str, rest) ->
		RETN (Unk (_PK_ str), rest)
		BEND

split_at_tab :: String -> RETN_TYPE (String, String) -- a la Lennart
split_at_tab xs
  = split_me [] xs
  where
    split_me acc ('\t' : ys) = BIND (my_rev acc []) _TO_ reversed ->
			       RETN (reversed, ys)
			       BEND
    split_me acc (y    : ys) = split_me (y:acc) ys

    my_rev ""	  acc = RETN acc -- instead of reverse, so can see on heap-profiles
    my_rev (x:xs) acc = my_rev xs (x:acc)
\end{code}

%************************************************************************
%*									*
\subsection[rdModule]{@rdModule@: reads in a Haskell module}
%*									*
%************************************************************************

\begin{code}
rdModule :: String
	 -> (FAST_STRING,		-- this module's name
	     (FAST_STRING -> Bool,	-- a function to chk if <x> is in the export list
	      FAST_STRING -> Bool),	-- a function to chk if <M> is among the M..
				-- ("dotdot") modules in the export list.
	     ProtoNameModule)	-- the main goods

rdModule (next_char:xs)
  = case next_char of { 'M' ->

    BIND (rdString		     	       xs)  _TO_ (srcline,  xs1) ->
    BIND (rdIdString		     	       xs1) _TO_ (name,	  xs2) ->
    BIND (rdString		     	       xs2) _TO_ (srcfile,  xs3) ->
    BIND (rdBinding srcfile	     	       xs3) _TO_ (binding,  xs4) ->
    BIND (rdList rdFixity	     	       xs4) _TO_ (fixities, xs5) ->
    BIND (rdList (rdImportedInterface srcfile) xs5) _TO_ (imports,  xs6) ->
    BIND (rdList rdEntity	     	       xs6) _TO_ (export_list, _) ->

    case sepDeclsForTopBinds binding	  of {
      (tydecls, tysigs, classdecls, instdecls, instsigs, defaultdecls, binds) ->

    (name,
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
	    (mkSrcLoc srcfile srcline)
    )
    } BEND BEND BEND BEND BEND BEND BEND
    }
  where
    mk_export_list_chker exp_list
      = case (getIEStrings exp_list) of { (entity_info, dotdot_modules) ->
	( \ n -> n `elemFM` just_the_strings,
	  \ n -> n `elemFM` dotdot_modules )
    	}
\end{code}

%************************************************************************
%*									*
\subsection[rdExprOrPat]{@rdExpr@ and @rdPat@}
%*									*
%************************************************************************

\begin{code}
rdExpr	 :: SrcFile -> String -> RETN_TYPE (ProtoNameExpr, String)
rdPat	 :: SrcFile -> String -> RETN_TYPE (ProtoNamePat,  String)

rdExpr sf (next_char:xs)
  = case next_char of
     '(' -> -- left section
	    BIND (rdExpr sf xs)	   _TO_ (expr,xs1) ->
	    BIND (rdId	    xs1)   _TO_ (id,  xs2) ->
	    RETN (SectionL expr (Var id), xs2)
	    BEND BEND

     ')' -> -- right section
	    BIND (rdId	    xs)	   _TO_ (id,  xs1) ->
	    BIND (rdExpr sf xs1)   _TO_ (expr,xs2) ->
	    RETN (SectionR (Var id) expr, xs2)
	    BEND BEND

     'j' -> -- ccall/casm
	    BIND (rdString	     xs)  _TO_ (fun,	 xs1) ->
	    BIND (rdString	     xs1) _TO_ (flavor,	 xs2) ->
	    BIND (rdList (rdExpr sf) xs2) _TO_ (args,	 xs3) ->
	    RETN (CCall fun args
			(flavor == SLIT("p") || flavor == SLIT("P")) -- may invoke GC
			(flavor == SLIT("N") || flavor == SLIT("P")) -- really a "casm"
			(panic "CCall:result_ty"),
		  xs3)
	    BEND BEND BEND

     'k' -> -- scc (set-cost-centre) expression
	    BIND (rdString	xs)	_TO_ (label, xs1) ->
	    BIND (rdExpr sf	xs1)	_TO_ (expr,  xs2) ->
	    RETN (SCC label expr, xs2)
	    BEND BEND

     'l' -> -- lambda expression
	    BIND (rdString	    xs)	  _TO_ (srcline, xs1) ->
	    BIND (rdList (rdPat sf) xs1)  _TO_ (pats,	 xs2) ->
	    BIND (rdExpr sf	    xs2)  _TO_ (body,	 xs3) ->
	    let
		src_loc = mkSrcLoc sf srcline
	    in
	    RETN (Lam (foldr PatMatch
			     (GRHSMatch (GRHSsAndBindsIn
					  [OtherwiseGRHS body src_loc]
					  EmptyBinds))
			     pats
		      ),
		 xs3)
	    BEND BEND BEND

     'c' -> -- case expression
	    BIND (rdExpr sf	      xs)  _TO_ (expr, xs1) ->
	    BIND (rdList (rdMatch sf) xs1) _TO_ (mats, xs2) ->
	    let
		matches = cvMatches sf True mats
	    in
	    RETN (Case expr matches, xs2)
	    BEND BEND

     'b' -> -- if expression
	    BIND (rdExpr sf xs)	   _TO_ (e1, xs1) ->
	    BIND (rdExpr sf xs1)   _TO_ (e2, xs2) ->
	    BIND (rdExpr sf xs2)   _TO_ (e3, xs3) ->
	    RETN (If e1 e2 e3, xs3)
	    BEND BEND BEND

     'E' -> -- let expression
	    BIND (rdBinding sf xs)  _TO_ (binding,xs1) ->
	    BIND (rdExpr sf    xs1) _TO_ (expr,	  xs2) ->
	    let
		binds = cvBinds sf cvValSig binding
	    in
	    RETN (Let binds expr, xs2)
	    BEND BEND

     'Z' -> -- list comprehension
	    BIND (rdExpr sf	 xs)	_TO_ (expr,  xs1) ->
	    BIND (rdList rd_qual xs1)	_TO_ (quals, xs2) ->
	    RETN (ListComp expr quals, xs2)
	    BEND BEND
	    where
	       rd_qual ('G':xs)
		 = BIND (rdPat	sf xs)	_TO_ (pat, xs1) ->
		   BIND (rdExpr sf xs1) _TO_ (expr,xs2) ->
		   RETN (GeneratorQual pat expr, xs2)
		   BEND BEND

	       rd_qual ('g':xs)
		 = BIND (rdExpr sf xs)	_TO_ (expr,xs1) ->
		   RETN (FilterQual expr, xs1)
		   BEND

     '.' -> -- arithmetic sequence
	    BIND (rdExpr sf		xs)	_TO_ (e1,  xs1) ->
	    BIND (rdList (rdExpr sf)	xs1)	_TO_ (es2, xs2) ->
	    BIND (rdList (rdExpr sf)	xs2)	_TO_ (es3, xs3) ->
	    RETN (cv_arith_seq e1 es2 es3, xs3)
	    BEND BEND BEND
	    where
	       cv_arith_seq e1 []   []	 = ArithSeqIn (From	  e1)
	       cv_arith_seq e1 []   [e3] = ArithSeqIn (FromTo	  e1 e3)
	       cv_arith_seq e1 [e2] []	 = ArithSeqIn (FromThen	  e1 e2)
	       cv_arith_seq e1 [e2] [e3] = ArithSeqIn (FromThenTo e1 e2 e3)

     'R' -> -- expression with type signature
	    BIND (rdExpr   sf xs)    _TO_ (expr,xs1) ->
	    BIND (rdPolyType  xs1)   _TO_ (ty,	xs2) ->
	    RETN (ExprWithTySig expr ty, xs2)
	    BEND BEND

     '-' -> -- negated expression
	    BIND (rdExpr sf  xs)   _TO_ (expr,xs1) ->
	    RETN (App (Var (Unk SLIT("negate"))) expr, xs1)
	    BEND
#ifdef DPH
     '5' -> -- parallel ZF expression
	    BIND (rdExpr sf xs)		_TO_ (expr,	 xs1) ->
	    BIND (rdList (rd_par_qual sf) xs1) _TO_ (qual_list, xs2) ->
	    let
		quals = foldr1 AndParQuals qual_list
	    in
	    RETN (RdrParallelZF expr quals, xs2)
	    BEND BEND
	    where
	      rdParQual sf inp
		= case inp of
		-- ToDo:DPH: I have kawunkled your RdrExplicitProcessor hack
		    '0':xs -> BIND (rdExPat sf xs)  _TO_ (RdrExplicitProcessor pats pat, xs1) ->
			      BIND (rdExpr  sf xs1) _TO_ (expr, xs2) ->
			      RETN (DrawnGenIn pats pat expr, xs2)
			      BEND BEND

		    'w':xs -> BIND (rdExPat sf xs)  _TO_ (RdrExplicitProcessor exprs pat, xs1) ->
			      BIND (rdExpr  sf xs1) _TO_ (expr, xs2) ->
			      RETN (IndexGen exprs pat expr, xs2)
			      BEND BEND

		    'I':xs -> BIND (rdExpr sf xs)	_TO_ (expr,xs1) ->
			      RETN (ParFilter expr, xs1)
			      BEND

     '6' -> -- explicitPod expression
	    BIND (rdList (rdExpr sf) xs)  _TO_ (exprs,xs1) ->
	    RETN (RdrExplicitPod exprs,xs1)
	    BEND
#endif {- Data Parallel Haskell -}

    --------------------------------------------------------------
    -- now the prefix items that can either be an expression or
    -- pattern, except we know they are *expressions* here
    -- (this code could be commoned up with the pattern version;
    -- but it probably isn't worth it)
    --------------------------------------------------------------
     'C' -> BIND (rdLiteral xs)	_TO_ (lit, xs1) ->
	    RETN (Lit lit, xs1)
	    BEND

     'i' -> -- simple identifier
	    BIND (rdId xs) _TO_ (str,xs1) ->
	    RETN (Var str, xs1)
	    BEND

     'a' -> -- application
	    BIND (rdExpr sf xs)	 _TO_ (expr1, xs1) ->
	    BIND (rdExpr sf xs1) _TO_ (expr2, xs2) ->
	    RETN (App expr1 expr2, xs2)
	    BEND BEND

     '@' -> -- operator application
	    BIND (rdExpr sf xs)	  _TO_ (expr1, xs1) ->
	    BIND (rdId	    xs1)  _TO_ (op,    xs2) ->
	    BIND (rdExpr sf xs2)  _TO_ (expr2, xs3) ->
	    RETN (OpApp expr1 (Var op) expr2, xs3)
	    BEND BEND BEND

     ':' -> -- explicit list
	    BIND (rdList (rdExpr sf) xs) _TO_ (exprs, xs1) ->
	    RETN (ExplicitList exprs, xs1)
	    BEND

     ',' -> -- explicit tuple
	    BIND (rdList (rdExpr sf) xs) _TO_ (exprs, xs1) ->
	    RETN (ExplicitTuple exprs, xs1)
	    BEND

#ifdef DPH
     'O' -> -- explicitProcessor expression
	    BIND (rdList (rdExpr sf) xs)  _TO_ (exprs,xs1) ->
	    BIND (rdExpr sf xs1)	    _TO_ (expr, xs2) ->
	    RETN (ExplicitProcessor exprs expr, xs2)
	    BEND BEND
#endif {- Data Parallel Haskell -}

     huh -> panic ("ReadPrefix.rdExpr:"++(next_char:xs))
\end{code}

Patterns: just bear in mind that lists of patterns are represented as
a series of ``applications''.
\begin{code}
rdPat sf (next_char:xs)
  = case next_char of
     's' -> -- "as" pattern
	    BIND (rdId	   xs)	_TO_ (id, xs1) ->
	    BIND (rdPat sf xs1) _TO_ (pat,xs2) ->
	    RETN (AsPatIn id pat, xs2)
	    BEND BEND

     '~' -> -- irrefutable ("twiddle") pattern
	    BIND (rdPat sf xs)	_TO_ (pat,xs1) ->
	    RETN (LazyPatIn pat, xs1)
	    BEND

     '+' -> -- n+k pattern
	    BIND (rdPat	    sf xs)  _TO_ (pat, xs1) ->
	    BIND (rdLiteral    xs1) _TO_ (lit, xs2) ->
	    let
		n = case pat of
		      VarPatIn n -> n
		      WildPatIn	 -> error "ERROR: rdPat: GHC can't handle _+k patterns yet"
	    in
	    RETN (NPlusKPatIn n lit, xs2)
	    BEND BEND

     '_' -> -- wildcard pattern
	    RETN (WildPatIn, xs)

    --------------------------------------------------------------
    -- now the prefix items that can either be an expression or
    -- pattern, except we know they are *patterns* here.
    --------------------------------------------------------------
     '-' -> BIND (rdPat sf xs)	_TO_ (lit_pat, xs1) ->
	    case lit_pat of
	      LitPatIn lit -> RETN (LitPatIn (negLiteral lit), xs1)
	      _	    	   -> panic "rdPat: bad negated pattern!"
	    BEND

     'C' -> BIND (rdLiteral xs) _TO_ (lit, xs1) ->
	    RETN (LitPatIn lit, xs1)
	    BEND

     'i' -> -- simple identifier
	    BIND (rdIdString xs) _TO_ (str, xs1) ->
	    RETN (if isConop str then
		     ConPatIn (Unk str) []
		  else
		     VarPatIn (Unk str),
		  xs1)
	    BEND

     'a' -> -- "application": there's a list of patterns lurking here!
	    BIND (rd_curried_pats    xs)  _TO_ (lpat:lpats, xs1) ->
	    BIND (rdPat		  sf xs1) _TO_ (rpat,	    xs2) ->
	    let
		(n, llpats)
		  = case lpat of
		      VarPatIn x    -> (x, [])
		      ConPatIn x [] -> (x, [])
		      ConOpPatIn x op y -> (op, [x, y])
		      other -> -- sorry about the weedy msg; the parser missed this one
		        error (ppShow 100 (ppCat [ppStr "ERROR: an illegal `application' of a pattern to another one:", ppInterleave ppSP (map (ppr PprForUser) bad_app)]))

		arg_pats = llpats ++ lpats ++ [rpat]
		bad_app  = (lpat:lpats) ++ [rpat]
	    in
	    RETN (ConPatIn n arg_pats, xs2)
	    BEND BEND
	    where
	      rd_curried_pats ('a' : ys)
		= BIND (rd_curried_pats ys)  _TO_ (lpats, ys1) ->
		  BIND (rdPat	     sf ys1) _TO_ (rpat,  ys2) ->
		  RETN (lpats ++ [rpat], ys2)
		  BEND BEND
	      rd_curried_pats ys
		= BIND (rdPat sf ys) _TO_ (pat,	 ys1) ->
		  RETN ([pat], ys1)
		  BEND

     '@' -> -- operator application
	    BIND (rdPat sf xs)	 _TO_ (pat1, xs1) ->
	    BIND (rdId	   xs1)	 _TO_ (op,   xs2) ->
	    BIND (rdPat sf xs2)	 _TO_ (pat2, xs3) ->
	    RETN (ConOpPatIn pat1 op pat2, xs3)
	    BEND BEND BEND

     ':' -> -- explicit list
	    BIND (rdList (rdPat sf) xs) _TO_ (pats, xs1) ->
	    RETN (ListPatIn pats, xs1)
	    BEND

     ',' -> -- explicit tuple
	    BIND (rdList (rdPat sf) xs) _TO_ (pats, xs1) ->
	    RETN (TuplePatIn pats, xs1)
	    BEND

#ifdef DPH
     'O' -> -- explicitProcessor pattern
	    BIND (rdList (rdPat sf) xs) _TO_ (pats, xs1) ->
	    BIND (rdPat sf xs1)		_TO_ (pat,  xs2) ->
	    RETN (ProcessorPatIn pats pat, xs2)
	    BEND BEND
#endif {- Data Parallel Haskell -}

     huh -> panic ("ReadPrefix.rdPat:"++(next_char:xs))
\end{code}

OLD, MISPLACED NOTE: The extra DPH syntax above is defined such that
to the left of a \tr{<<-} or \tr{<<=} there has to be a processor (no
expressions).  Therefore in the pattern matching below we are taking
this into consideration to create the @DrawGen@ whose fields are the
\tr{K} patterns, pat and the exp right of the generator.

\begin{code}
rdLiteral :: String -> RETN_TYPE (Literal, String)

rdLiteral (tag : xs)
  = BIND (rdString xs)	_TO_ (x, zs) ->
    let
	s = _UNPK_ x

	as_char	    = chr ((read s) :: Int)
	    -- a char comes in as a number string
	    -- representing its ASCII code
	as_integer  = readInteger s
#if __GLASGOW_HASKELL__ <= 22
	as_rational = toRational ((read s)::Double)
#else
#ifdef __GLASGOW_HASKELL__
	as_rational = _readRational s -- non-std
#else
	as_rational = ((read s)::Rational)
#endif
#endif
	as_double   = ((read s) :: Double)
    in
    case tag of {
     '4' -> RETN (IntLit as_integer,	  zs);
     'F' -> RETN (FracLit as_rational,	  zs);
     'H' -> RETN (IntPrimLit as_integer,  zs);
#if __GLASGOW_HASKELL__ <= 22
     'J' -> RETN (DoublePrimLit as_double,zs);
     'K' -> RETN (FloatPrimLit as_double, zs);
#else
     'J' -> RETN (DoublePrimLit as_rational,zs);
     'K' -> RETN (FloatPrimLit as_rational, zs);
#endif
     'C' -> RETN (CharLit as_char,	  zs);
     'P' -> RETN (CharPrimLit as_char,	  zs);
     'S' -> RETN (StringLit x,		  zs);
     'V' -> RETN (StringPrimLit x,	  zs);
     'Y' -> RETN (LitLitLitIn x,	  zs)
    } BEND
\end{code}

%************************************************************************
%*									*
\subsection[rdBinding]{rdBinding}
%*									*
%************************************************************************

\begin{code}
rdBinding :: SrcFile -> String -> RETN_TYPE (RdrBinding, String)

rdBinding sf (next_char:xs)
  = case next_char of
     'B' -> -- null binding
	    RETN (RdrNullBind, xs)

     'A' -> -- "and" binding (just glue, really)
	    BIND (rdBinding sf xs)  _TO_ (binding1, xs1) ->
	    BIND (rdBinding sf xs1) _TO_ (binding2, xs2) ->
	    RETN (RdrAndBindings binding1 binding2, xs2)
	    BEND BEND

     't' -> -- "data" declaration
	    BIND (rdString		xs)  _TO_ (srcline,	    xs1) ->
	    BIND (rdContext		xs1) _TO_ (ctxt,	    xs2) ->
	    BIND (rdList rdId		xs2) _TO_ (derivings,	    xs3) ->
	    BIND (rdTyConAndTyVars	xs3) _TO_ ((tycon, tyvars), xs4) ->
	    BIND (rdList (rdConDecl sf) xs4) _TO_ (cons,	    xs5) ->
	    BIND (rdDataPragma		xs5) _TO_ (pragma,	    xs6) ->
	    let
		src_loc = mkSrcLoc sf srcline
	    in
	    RETN (RdrTyData (TyData ctxt tycon tyvars cons derivings pragma src_loc),
		  xs6)
	    BEND BEND BEND BEND BEND BEND

     'n' -> -- "type" declaration
	    BIND (rdString	   xs)	_TO_ (srcline,	       xs1) ->
	    BIND (rdTyConAndTyVars xs1) _TO_ ((tycon, tyvars), xs2) ->
	    BIND (rdMonoType	   xs2) _TO_ (expansion,       xs3) ->
	    BIND (rdTypePragma	   xs3) _TO_ (pragma,	       xs4) ->
	    let
		src_loc = mkSrcLoc sf srcline
	    in
	    RETN (RdrTySynonym (TySynonym tycon tyvars expansion pragma src_loc),
		  xs4)
	    BEND BEND BEND BEND

     'f' -> -- function binding
	    BIND (rdString		xs) _TO_ (srcline, xs1) ->
	    BIND (rdList (rdMatch sf) xs1)  _TO_ (matches, xs2) ->
	    RETN (RdrFunctionBinding (read (_UNPK_ srcline)) matches, xs2)
	    BEND BEND

     'p' -> -- pattern binding
	    BIND (rdString		xs)  _TO_ (srcline, xs1) ->
	    BIND (rdList (rdMatch sf) xs1) _TO_ (matches, xs2) ->
	    RETN (RdrPatternBinding (read (_UNPK_ srcline)) matches, xs2)
	    BEND BEND

     '$' -> -- "class" declaration
	    BIND (rdString	  xs)	_TO_ (srcline,	     xs1) ->
	    BIND (rdContext	  xs1)	_TO_ (ctxt,	     xs2) ->
	    BIND (rdClassAssertTy xs2)	_TO_ ((clas, tyvar), xs3) ->
	    BIND (rdBinding sf	  xs3)	_TO_ (binding,	     xs4) ->
	    BIND (rdClassPragma	  xs4)	_TO_ (pragma,	     xs5) ->
	    let
		(class_sigs, class_methods) = sepDeclsIntoSigsAndBinds binding

		final_sigs    = concat (map cvClassOpSig class_sigs)
		final_methods = cvMonoBinds sf class_methods

		src_loc = mkSrcLoc sf srcline
	    in
	    RETN (RdrClassDecl
		  (ClassDecl ctxt clas tyvar final_sigs final_methods pragma src_loc),
		  xs5)
	    BEND BEND BEND BEND BEND

     '%' -> -- "instance" declaration
	    BIND (rdString     xs)	_TO_ (srcline,	xs1) ->
	    BIND (rdContext    xs1)	_TO_ (ctxt,	xs2) ->
	    BIND (rdId		xs2)	_TO_ (clas,	xs3) ->
	    BIND (rdMonoType   xs3)	_TO_ (inst_ty,	xs4) ->
	    BIND (rdBinding sf xs4)	_TO_ (binding,	xs5) ->
	    BIND (rdInstPragma xs5)	_TO_ (modname_maybe, pragma, xs6) ->
	    let
		(ss, bs)   = sepDeclsIntoSigsAndBinds binding
		binds	   = cvMonoBinds sf bs
	    	uprags	   = concat (map cvInstDeclSig ss)
		src_loc	   = mkSrcLoc sf srcline
	    in
	    case modname_maybe of {
	      Nothing ->
		RETN (RdrInstDecl (\ orig_mod infor_mod here ->
		      InstDecl ctxt clas inst_ty binds here orig_mod infor_mod uprags pragma src_loc),
		      xs6);
	      Just orig_mod ->
		RETN (RdrInstDecl (\ _ infor_mod here ->
		      InstDecl ctxt clas inst_ty binds here orig_mod infor_mod uprags pragma src_loc),
		      xs6)
	    }
	    BEND BEND BEND BEND BEND BEND

     'D' -> -- "default" declaration
	    BIND (rdString	    xs)	  _TO_ (srcline,xs1) ->
	    BIND (rdList rdMonoType xs1)  _TO_ (tys,	xs2) ->

	    RETN (RdrDefaultDecl (DefaultDecl tys (mkSrcLoc sf srcline)),
		  xs2)
	    BEND BEND

     '7' -> -- "import" declaration in an interface
	    BIND (rdString	    xs)	 _TO_ (srcline,	  xs1) ->
	    BIND (rdIdString	    xs1) _TO_ (mod,	  xs2) ->
	    BIND (rdList rdEntity   xs2) _TO_ (entities,  xs3) ->
	    BIND (rdList rdRenaming xs3) _TO_ (renamings, xs4) ->
	    let
		src_loc = mkSrcLoc sf srcline
	    in
	    RETN (RdrIfaceImportDecl (IfaceImportDecl mod entities renamings src_loc),
		  xs4)
	    BEND BEND BEND BEND

     'S' -> -- signature(-like) things, including user pragmas
	    rd_sig_thing sf xs
\end{code}

\begin{code}
rd_sig_thing sf (next_char:xs)
  = case next_char of
     't' -> -- type signature
	    BIND (rdString	 xs)  _TO_ (srcline, xs1) ->
	    BIND (rdList rdId	 xs1) _TO_ (vars,    xs2) ->
	    BIND (rdPolyType	 xs2) _TO_ (poly_ty, xs3) ->
	    BIND (rdTySigPragmas xs3) _TO_ (pragma,  xs4) ->
	    let
		src_loc = mkSrcLoc sf srcline
	    in
	    RETN (RdrTySig vars poly_ty pragma src_loc, xs4)
	    BEND BEND BEND BEND

     's' -> -- value specialisation user-pragma
	    BIND (rdString	    xs)  _TO_ (srcline, xs1) ->
	    BIND (rdId		    xs1) _TO_ (var,     xs2) ->
	    BIND (rdList rdPolyType xs2) _TO_ (tys,	xs3) ->
	    let
		src_loc = mkSrcLoc sf srcline
	    in
	    RETN (RdrSpecValSig [SpecSig var ty Nothing{-ToDo: using...s-} src_loc | ty <- tys], xs3)
	    BEND BEND BEND

     'S' -> -- instance specialisation user-pragma
	    BIND (rdString	    xs)  _TO_ (srcline, xs1) ->
	    BIND (rdId	    	    xs1) _TO_ (clas,    xs2) ->
	    BIND (rdMonoType	    xs2) _TO_ (ty,	xs3) ->
	    let
		src_loc = mkSrcLoc sf srcline
	    in
	    RETN (RdrSpecInstSig (InstSpecSig clas ty src_loc), xs3)
	    BEND BEND BEND

     'i' -> -- value inlining user-pragma
	    BIND (rdString	    xs)  _TO_ (srcline, xs1) ->
	    BIND (rdId	    	    xs1) _TO_ (var,     xs2) ->
	    BIND (rdList rdIdString xs2) _TO_ (howto,   xs3) ->
	    let
		src_loc = mkSrcLoc sf srcline

		guidance
		  = (case howto of {
		      []  -> id;
		      [x] -> trace "ignoring unfold howto" }) UnfoldAlways
	    in
	    RETN (RdrInlineValSig (InlineSig var guidance src_loc), xs3)
	    BEND BEND BEND

     'd' -> -- value deforest user-pragma
            BIND (rdString       xs)  _TO_ (srcline, xs1) ->
            BIND (rdId           xs1) _TO_ (var, xs2) ->
            let
                src_loc = mkSrcLoc sf srcline
            in
            RETN (RdrDeforestSig (DeforestSig var src_loc), xs2)
            BEND BEND

     'u' -> -- value magic-unfolding user-pragma
	    BIND (rdString	 xs)  _TO_ (srcline, xs1) ->
	    BIND (rdId	    	 xs1) _TO_ (var,     xs2) ->
	    BIND (rdIdString   	 xs2) _TO_ (str,     xs3) ->
	    let
		src_loc = mkSrcLoc sf srcline
	    in
	    RETN (RdrMagicUnfoldingSig (MagicUnfoldingSig var str src_loc), xs3)
	    BEND BEND BEND

     'a' -> -- abstract-type-synonym user-pragma
	    BIND (rdString	 xs)  _TO_ (srcline, xs1) ->
	    BIND (rdId	    	 xs1) _TO_ (tycon,   xs2) ->
	    let
		src_loc = mkSrcLoc sf srcline
	    in
	    RETN (RdrAbstractTypeSig (AbstractTypeSig tycon src_loc), xs2)
	    BEND BEND

     'd' -> -- data specialisation user-pragma
	    BIND (rdString	    xs)  _TO_ (srcline, xs1) ->
	    BIND (rdId		    xs1) _TO_ (tycon,   xs2) ->
	    BIND (rdList rdMonoType xs2) _TO_ (tys,	xs3) ->
	    let
		src_loc = mkSrcLoc sf srcline
		spec_ty = MonoTyCon tycon tys
	    in
	    RETN (RdrSpecDataSig (SpecDataSig tycon spec_ty src_loc), xs3)
	    BEND BEND BEND
\end{code}

%************************************************************************
%*									*
\subsection[rdTypes]{Reading in types in various forms (and data constructors)}
%*									*
%************************************************************************

\begin{code}
rdPolyType :: String -> RETN_TYPE (ProtoNamePolyType, String)
rdMonoType :: String -> RETN_TYPE (ProtoNameMonoType, String)

rdPolyType ('3' : xs)
  = BIND (rdContext	 xs)	_TO_ (ctxt, xs1) ->
    BIND (rdMonoType xs1)	_TO_ (ty,   xs2) ->
    RETN (OverloadedTy ctxt ty, xs2)
    BEND BEND

rdPolyType ('2' : 'C' : xs)
  = BIND (rdList rdId xs)   	_TO_ (tvs, xs1) ->
    BIND (rdMonoType  xs1)  	_TO_ (ty,  xs2) ->
    RETN (ForAllTy tvs ty, xs2)
    BEND BEND

rdPolyType other
  = BIND (rdMonoType other) 	    _TO_ (ty, xs1) ->
    RETN (UnoverloadedTy ty, xs1)
    BEND

rdMonoType ('T' : xs)
  = BIND (rdId		xs)  	    _TO_ (tycon, xs1) ->
    BIND (rdList rdMonoType xs1)    _TO_ (tys,	 xs2) ->
    RETN (MonoTyCon tycon tys, xs2)
    BEND BEND

rdMonoType (':' : xs)
  = BIND (rdMonoType xs)	    _TO_ (ty, xs1) ->
    RETN (ListMonoTy ty, xs1)
    BEND

rdMonoType (',' : xs)
  = BIND (rdList rdPolyType xs)	    _TO_ (tys, xs1) ->
    RETN (TupleMonoTy tys, xs1)
    BEND

rdMonoType ('>' : xs)
  = BIND (rdMonoType xs)	_TO_ (ty1, xs1) ->
    BIND (rdMonoType xs1)	_TO_ (ty2, xs2) ->
    RETN (FunMonoTy ty1 ty2, xs2)
    BEND BEND

rdMonoType ('y' : xs)
  = BIND (rdId xs)		_TO_ (tyvar, xs1) ->
    RETN (MonoTyVar tyvar, xs1)
    BEND

rdMonoType ('2' : 'A' : xs)
  = BIND (rdId	     xs)	_TO_ (clas, xs1) ->
    BIND (rdMonoType xs1)	_TO_ (ty,   xs2) ->
    RETN (MonoDict clas ty, xs2)
    BEND BEND

rdMonoType ('2' : 'B' : xs)
  = BIND (rdId xs)  	    	_TO_ (tv_tmpl, xs1) ->
    RETN (MonoTyVarTemplate tv_tmpl, xs1)
    BEND

#ifdef DPH
rdMonoType ('v' : xs)
  = BIND (rdMonoType xs)	    _TO_ (ty, xs1) ->
    RETN (RdrExplicitPodTy ty, xs1)
    BEND

rdMonoType ('u' : xs)
  = BIND (rdList rdMonoType xs) _TO_ (tys, xs1) ->
    BIND (rdMonoType xs1)	_TO_ (ty,  xs2)  ->
    RETN (RdrExplicitProcessorTy tys ty, xs2)
    BEND BEND
#endif {- Data Parallel Haskell -}

rdMonoType oops = panic ("rdMonoType:"++oops)
\end{code}

\begin{code}
rdTyConAndTyVars :: String -> RETN_TYPE ((ProtoName, [ProtoName]), String)
rdContext   	 :: String -> RETN_TYPE (ProtoNameContext, String)
rdClassAssertTy  :: String -> RETN_TYPE ((ProtoName, ProtoName), String)

rdTyConAndTyVars xs
  = BIND (rdMonoType xs)   _TO_ (MonoTyCon tycon ty_args, xs1) ->
    let
	args = [ a | (MonoTyVar a) <- ty_args ]
    in
    RETN ((tycon, args), xs1)
    BEND

rdContext xs
  = BIND (rdList rdMonoType xs)	_TO_ (tys, xs1) ->
    RETN (map mk_class_assertion tys, xs1)
    BEND

rdClassAssertTy xs
  = BIND (rdMonoType xs)   _TO_ (mono_ty, xs1) ->
    RETN (mk_class_assertion mono_ty, xs1)
    BEND

mk_class_assertion :: ProtoNameMonoType -> (ProtoName, ProtoName)

mk_class_assertion (MonoTyCon name [(MonoTyVar tyname)]) = (name, tyname)
mk_class_assertion other
  = error ("ERROR: malformed type context: "++ppShow 80 (ppr PprForUser other)++"\n")
    -- regrettably, the parser does let some junk past
    -- e.g., f :: Num {-nothing-} => a -> ...
\end{code}

\begin{code}
rdConDecl :: SrcFile -> String -> RETN_TYPE (ProtoNameConDecl, String)

rdConDecl sf ('1':xs)
  = BIND (rdString	    xs)	 _TO_ (srcline,	  xs1) ->
    BIND (rdId		    xs1) _TO_ (id,	  xs2) ->
    BIND (rdList rdMonoType xs2) _TO_ (tys,	  xs3) ->
    RETN (ConDecl id tys (mkSrcLoc sf srcline), xs3)
    BEND BEND BEND
\end{code}

%************************************************************************
%*									*
\subsection[rdMatch]{Read a ``match''}
%*									*
%************************************************************************

\begin{code}
rdMatch :: SrcFile -> String -> RETN_TYPE (RdrMatch, String)

rdMatch sf ('W':xs)
  = BIND (rdString	    xs)	 _TO_ (srcline, xs1) ->
    BIND (rdIdString	    xs1) _TO_ (srcfun,	xs2) ->
    BIND (rdPat sf	    xs2) _TO_ (pat,	xs3) ->
    BIND (rdList rd_guarded xs3) _TO_ (grhss,	xs4) ->
    BIND (rdBinding sf	    xs4) _TO_ (binding, xs5) ->

    RETN (RdrMatch (read (_UNPK_ srcline)) srcfun pat grhss binding, xs5)
    BEND BEND BEND BEND BEND
  where
    rd_guarded xs
      = BIND (rdExpr sf xs)	_TO_ (g, xs1) ->
	BIND (rdExpr sf xs1)	_TO_ (e, xs2) ->
	RETN ((g, e), xs2)
	BEND BEND
\end{code}

%************************************************************************
%*									*
\subsection[rdFixity]{Read in a fixity declaration}
%*									*
%************************************************************************

\begin{code}
rdFixity :: String -> RETN_TYPE (ProtoNameFixityDecl, String)
rdFixity xs
  = BIND (rdId	   xs)	_TO_ (op,	     xs1) ->
    BIND (rdString xs1)	_TO_ (associativity, xs2) ->
    BIND (rdString xs2)	_TO_ (prec_str,	     xs3) ->
    let
	precedence = read (_UNPK_ prec_str)
    in
    case (_UNPK_ associativity) of {
      "infix"  -> RETN (InfixN op precedence, xs3);
      "infixl" -> RETN (InfixL op precedence, xs3);
      "infixr" -> RETN (InfixR op precedence, xs3)
    } BEND BEND BEND
\end{code}

%************************************************************************
%*									*
\subsection[rdImportedInterface]{Read an imported interface}
%*									*
%************************************************************************

\begin{code}
rdImportedInterface :: FAST_STRING -> String
		    -> RETN_TYPE (ProtoNameImportedInterface, String)

rdImportedInterface importing_srcfile (x:xs)
  = BIND (rdString	    xs)	 _TO_ (srcline,	 xs1) ->
    BIND (rdString	    xs1) _TO_ (srcfile,	 xs2) ->
    BIND (rdIdString	    xs2) _TO_ (modname,	 xs3) ->
    BIND (rdList rdEntity   xs3) _TO_ (imports,	 xs4) ->
    BIND (rdList rdRenaming xs4) _TO_ (renamings,xs5) ->
    BIND (rdBinding srcfile xs5) _TO_ (iface_bs, xs6) ->

    case (sepDeclsForInterface iface_bs) of {
		(tydecls,classdecls,instdecls,sigs,iimpdecls) ->
    let
	expose_or_hide = case x of { 'e' -> ImportSome; 'h' -> ImportButHide }

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
		(mkSrcLoc importing_srcfile srcline)
    in
    RETN (
    (if null imports then
	ImportAll cv_iface renamings
     else
	expose_or_hide cv_iface imports renamings
    , xs6))
    } BEND BEND BEND BEND BEND BEND
\end{code}

\begin{code}
rdRenaming :: String -> RETN_TYPE (Renaming, String)

rdRenaming xs
  = BIND (rdIdString xs)    _TO_ (id1, xs1) ->
    BIND (rdIdString xs1)   _TO_ (id2, xs2) ->
    RETN (MkRenaming id1 id2, xs2)
    BEND BEND
\end{code}

\begin{code}
rdEntity :: String -> RETN_TYPE (IE, String)

rdEntity inp
  = case inp of
      'x':xs -> BIND (rdIdString xs)	_TO_ (var, xs1) ->
		RETN (IEVar var, xs1)
		BEND

      'X':xs -> BIND (rdIdString xs)	_TO_ (thing, xs1) ->
		RETN (IEThingAbs thing, xs1)
		BEND

      'z':xs -> BIND (rdIdString xs)	_TO_ (thing, xs1) ->
		RETN (IEThingAll thing, xs1)
		BEND

      '8':xs -> BIND (rdIdString	xs)  _TO_ (tycon, xs1) ->
		BIND (rdList rdString	xs1) _TO_ (cons,  xs2) ->
		RETN (IEConWithCons tycon cons, xs2)
		BEND BEND

      '9':xs -> BIND (rdIdString	xs)  _TO_ (c,	xs1) ->
		BIND (rdList rdString	xs1) _TO_ (ops, xs2) ->
		RETN (IEClsWithOps c ops, xs2)
		BEND BEND

      'm':xs -> BIND (rdIdString xs)	_TO_ (m, xs1) ->
		RETN (IEModuleContents m, xs1)
		BEND
\end{code}
