%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[StgSyn]{Shared term graph (STG) syntax for spineless-tagless code generation}

This data type represents programs just before code generation
(conversion to @AbstractC@): basically, what we have is a stylised
form of @CoreSyntax@, the style being one that happens to be ideally
suited to spineless tagless code generation.

\begin{code}
#include "HsVersions.h"

module StgSyn (
	GenStgArg(..),
	GenStgLiveVars(..),

	GenStgBinding(..), GenStgExpr(..), GenStgRhs(..),
	GenStgCaseAlts(..), GenStgCaseDefault(..),

	UpdateFlag(..),

	StgBinderInfo(..),
	stgArgOcc, stgUnsatOcc, stgStdHeapOcc, stgNoUpdHeapOcc,
	stgNormalOcc, stgFakeFunAppOcc,
	combineStgBinderInfo,

	-- a set of synonyms for the most common (only :-) parameterisation
	StgArg(..), StgLiveVars(..),
	StgBinding(..), StgExpr(..), StgRhs(..),
	StgCaseAlts(..), StgCaseDefault(..),

	pprPlainStgBinding,
	getArgPrimRep,
	isLitLitArg,
	stgArity,
	collectExportedStgBinders

	-- and to make the interface self-sufficient...
    ) where

import Ubiq{-uitous-}

import CostCentre	( showCostCentre )
import Id		( idPrimRep, GenId{-instance NamedThing-} )
import Literal		( literalPrimRep, isLitLitLit, Literal{-instance Outputable-} )
import Name		( isExported, isOpLexeme )
import Outputable	( ifPprDebug, interppSP, interpp'SP,
			  Outputable(..){-instance * Bool-}
			)
import PprStyle		( PprStyle(..) )
import PprType		( GenType{-instance Outputable-} )
import Pretty		-- all of it
import PrimOp		( PrimOp{-instance Outputable-} )
import Unique		( pprUnique )
import UniqSet		( isEmptyUniqSet, uniqSetToList, UniqSet(..) )
import Util		( panic )
\end{code}

%************************************************************************
%*									*
\subsection{@GenStgBinding@}
%*									*
%************************************************************************

As usual, expressions are interesting; other things are boring.  Here
are the boring things [except note the @GenStgRhs@], parameterised
with respect to binder and occurrence information (just as in
@CoreSyn@):

\begin{code}
data GenStgBinding bndr occ
  = StgNonRec	bndr (GenStgRhs bndr occ)
  | StgRec	[(bndr, GenStgRhs bndr occ)]
\end{code}

%************************************************************************
%*									*
\subsection{@GenStgArg@}
%*									*
%************************************************************************

\begin{code}
data GenStgArg occ
  = StgVarArg	occ
  | StgLitArg	Literal
\end{code}

\begin{code}
getArgPrimRep (StgVarArg  local) = idPrimRep local
getArgPrimRep (StgLitArg  lit)	 = literalPrimRep lit

isLitLitArg (StgLitArg x) = isLitLitLit x
isLitLitArg _		  = False
\end{code}

%************************************************************************
%*									*
\subsection{STG expressions}
%*									*
%************************************************************************

The @GenStgExpr@ data type is parameterised on binder and occurrence
info, as before.

%************************************************************************
%*									*
\subsubsection{@GenStgExpr@ application}
%*									*
%************************************************************************

An application is of a function to a list of atoms [not expressions].
Operationally, we want to push the arguments on the stack and call the
function.  (If the arguments were expressions, we would have to build
their closures first.)

There is no constructor for a lone variable; it would appear as
@StgApp var [] _@.
\begin{code}
type GenStgLiveVars occ = UniqSet occ

data GenStgExpr bndr occ
  = StgApp
	(GenStgArg occ)	-- function
	[GenStgArg occ]	-- arguments
	(GenStgLiveVars occ)	-- Live vars in continuation; ie not
				-- including the function and args

    -- NB: a literal is: StgApp <lit-atom> [] ...
\end{code}

%************************************************************************
%*									*
\subsubsection{@StgCon@ and @StgPrim@---saturated applications}
%*									*
%************************************************************************

There are two specialised forms of application, for
constructors and primitives.
\begin{code}
  | StgCon			-- always saturated
	Id -- data constructor
	[GenStgArg occ]
	(GenStgLiveVars occ)	-- Live vars in continuation; ie not
				-- including the constr and args

  | StgPrim			-- always saturated
	PrimOp
	[GenStgArg occ]
	(GenStgLiveVars occ)	-- Live vars in continuation; ie not
				-- including the op and args
\end{code}
These forms are to do ``inline versions,'' as it were.
An example might be: @f x = x:[]@.

%************************************************************************
%*									*
\subsubsection{@GenStgExpr@: case-expressions}
%*									*
%************************************************************************

This has the same boxed/unboxed business as Core case expressions.
\begin{code}
  | StgCase
	(GenStgExpr bndr occ)
			-- the thing to examine

	(GenStgLiveVars occ) -- Live vars of whole case
			-- expression; i.e., those which mustn't be
			-- overwritten

	(GenStgLiveVars occ) -- Live vars of RHSs;
			-- i.e., those which must be saved before eval.
			--
			-- note that an alt's constructor's
			-- binder-variables are NOT counted in the
			-- free vars for the alt's RHS

	Unique		-- Occasionally needed to compile case
			-- statements, as the uniq for a local
			-- variable to hold the tag of a primop with
			-- algebraic result

	(GenStgCaseAlts bndr occ)
\end{code}

%************************************************************************
%*									*
\subsubsection{@GenStgExpr@:  @let(rec)@-expressions}
%*									*
%************************************************************************

The various forms of let(rec)-expression encode most of the
interesting things we want to do.
\begin{enumerate}
\item
\begin{verbatim}
let-closure x = [free-vars] expr [args]
in e
\end{verbatim}
is equivalent to
\begin{verbatim}
let x = (\free-vars -> \args -> expr) free-vars
\end{verbatim}
\tr{args} may be empty (and is for most closures).  It isn't under
circumstances like this:
\begin{verbatim}
let x = (\y -> y+z)
\end{verbatim}
This gets mangled to
\begin{verbatim}
let-closure x = [z] [y] (y+z)
\end{verbatim}
The idea is that we compile code for @(y+z)@ in an environment in which
@z@ is bound to an offset from \tr{Node}, and @y@ is bound to an
offset from the stack pointer.

(A let-closure is an @StgLet@ with a @StgRhsClosure@ RHS.)

\item
\begin{verbatim}
let-constructor x = Constructor [args]
in e
\end{verbatim}

(A let-constructor is an @StgLet@ with a @StgRhsCon@ RHS.)

\item
Letrec-expressions are essentially the same deal as
let-closure/let-constructor, so we use a common structure and
distinguish between them with an @is_recursive@ boolean flag.

\item
\begin{verbatim}
let-unboxed u = an arbitrary arithmetic expression in unboxed values
in e
\end{verbatim}
All the stuff on the RHS must be fully evaluated.  No function calls either!

(We've backed away from this toward case-expressions with
suitably-magical alts ...)

\item
~[Advanced stuff here!  Not to start with, but makes pattern matching
generate more efficient code.]

\begin{verbatim}
let-escapes-not fail = expr
in e'
\end{verbatim}
Here the idea is that @e'@ guarantees not to put @fail@ in a data structure,
or pass it to another function.  All @e'@ will ever do is tail-call @fail@.
Rather than build a closure for @fail@, all we need do is to record the stack
level at the moment of the @let-escapes-not@; then entering @fail@ is just
a matter of adjusting the stack pointer back down to that point and entering
the code for it.

Another example:
\begin{verbatim}
f x y = let z = huge-expression in
	if y==1 then z else
	if y==2 then z else
	1
\end{verbatim}

(A let-escapes-not is an @StgLetNoEscape@.)

\item
We may eventually want:
\begin{verbatim}
let-literal x = Literal
in e
\end{verbatim}

(ToDo: is this obsolete?)
\end{enumerate}

And so the code for let(rec)-things:
\begin{code}
  | StgLet
	(GenStgBinding bndr occ)	-- right hand sides (see below)
	(GenStgExpr bndr occ)		-- body

  | StgLetNoEscape			-- remember: ``advanced stuff''
	(GenStgLiveVars occ)		-- Live in the whole let-expression
					-- Mustn't overwrite these stack slots
    	    	    	    	    	-- *Doesn't* include binders of the let(rec).

	(GenStgLiveVars occ)		-- Live in the right hand sides (only)
					-- These are the ones which must be saved on
					-- the stack if they aren't there already
    	    	    	    	    	-- *Does* include binders of the let(rec) if recursive.

	(GenStgBinding bndr occ)	-- right hand sides (see below)
	(GenStgExpr bndr occ)		-- body
\end{code}

%************************************************************************
%*									*
\subsubsection{@GenStgExpr@: @scc@ expressions}
%*									*
%************************************************************************

Finally for @scc@ expressions we introduce a new STG construct.

\begin{code}
  | StgSCC
	Type			-- the type of the body
	CostCentre		-- label of SCC expression
	(GenStgExpr bndr occ)	-- scc expression
  -- end of GenStgExpr
\end{code}

%************************************************************************
%*									*
\subsection{STG right-hand sides}
%*									*
%************************************************************************

Here's the rest of the interesting stuff for @StgLet@s; the first
flavour is for closures:
\begin{code}
data GenStgRhs bndr occ
  = StgRhsClosure
	CostCentre		-- cost centre to be attached (default is CCC)
	StgBinderInfo		-- Info about how this binder is used (see below)
	[occ]			-- non-global free vars; a list, rather than
				-- a set, because order is important
	UpdateFlag		-- ReEntrant | Updatable | SingleEntry
	[bndr]			-- arguments; if empty, then not a function;
				-- as above, order is important
	(GenStgExpr bndr occ)	-- body
\end{code}
An example may be in order.  Consider:
\begin{verbatim}
let t = \x -> \y -> ... x ... y ... p ... q in e
\end{verbatim}
Pulling out the free vars and stylising somewhat, we get the equivalent:
\begin{verbatim}
let t = (\[p,q] -> \[x,y] -> ... x ... y ... p ...q) p q
\end{verbatim}
Stg-operationally, the @[x,y]@ are on the stack, the @[p,q]@ are
offsets from @Node@ into the closure, and the code ptr for the closure
will be exactly that in parentheses above.

The second flavour of right-hand-side is for constructors (simple but important):
\begin{code}
  | StgRhsCon
	CostCentre		-- Cost centre to be attached (default is CCC).
				-- Top-level (static) ones will end up with
				-- DontCareCC, because we don't count static
				-- data in heap profiles, and we don't set CCC
				-- from static closure.
	Id			-- constructor
	[GenStgArg occ]	-- args
\end{code}

Here's the @StgBinderInfo@ type, and its combining op:
\begin{code}
data StgBinderInfo
  = NoStgBinderInfo
  | StgBinderInfo
	Bool		-- At least one occurrence as an argument

	Bool		-- At least one occurrence in an unsaturated application

	Bool		-- This thing (f) has at least occurrence of the form:
			--    x = [..] \u [] -> f a b c
			-- where the application is saturated

	Bool		-- Ditto for non-updatable x.

	Bool		-- At least one fake application occurrence, that is
			-- an StgApp f args where args is an empty list
			-- This is due to the fact that we do not have a
			-- StgVar constructor.
			-- Used by the lambda lifter.
			-- True => "at least one unsat app" is True too

stgArgOcc        = StgBinderInfo True  False False False False
stgUnsatOcc      = StgBinderInfo False True  False False False
stgStdHeapOcc    = StgBinderInfo False False True  False False
stgNoUpdHeapOcc  = StgBinderInfo False False False True  False
stgNormalOcc     = StgBinderInfo False False False False False
-- [Andre] can't think of a good name for the last one.
stgFakeFunAppOcc = StgBinderInfo False True  False False True

combineStgBinderInfo :: StgBinderInfo -> StgBinderInfo -> StgBinderInfo

combineStgBinderInfo NoStgBinderInfo info2 = info2
combineStgBinderInfo info1 NoStgBinderInfo = info1
combineStgBinderInfo (StgBinderInfo arg1 unsat1 std_heap1 upd_heap1 fkap1)
		     (StgBinderInfo arg2 unsat2 std_heap2 upd_heap2 fkap2)
  = StgBinderInfo (arg1      || arg2)
		  (unsat1    || unsat2)
		  (std_heap1 || std_heap2)
		  (upd_heap1 || upd_heap2)
		  (fkap1     || fkap2)
\end{code}

%************************************************************************
%*									*
\subsection[Stg-case-alternatives]{STG case alternatives}
%*									*
%************************************************************************

Just like in @CoreSyntax@ (except no type-world stuff).

\begin{code}
data GenStgCaseAlts bndr occ
  = StgAlgAlts	Type	-- so we can find out things about constructor family
		[(Id,				-- alts: data constructor,
		  [bndr],			-- constructor's parameters,
		  [Bool],			-- "use mask", same length as
						-- parameters; a True in a
						-- param's position if it is
						-- used in the ...
		  GenStgExpr bndr occ)]	-- ...right-hand side.
		(GenStgCaseDefault bndr occ)
  | StgPrimAlts	Type	-- so we can find out things about constructor family
		[(Literal,			-- alts: unboxed literal,
		  GenStgExpr bndr occ)]	-- rhs.
		(GenStgCaseDefault bndr occ)

data GenStgCaseDefault bndr occ
  = StgNoDefault				-- small con family: all
						-- constructor accounted for
  | StgBindDefault  bndr			-- form: var -> expr
		    Bool			-- True <=> var is used in rhs
						-- i.e., False <=> "_ -> expr"
		    (GenStgExpr bndr occ)
\end{code}

%************************************************************************
%*									*
\subsection[Stg]{The Plain STG parameterisation}
%*									*
%************************************************************************

This happens to be the only one we use at the moment.

\begin{code}
type StgBinding     = GenStgBinding	Id Id
type StgArg         = GenStgArg		Id
type StgLiveVars    = GenStgLiveVars	Id
type StgExpr        = GenStgExpr	Id Id
type StgRhs         = GenStgRhs		Id Id
type StgCaseAlts    = GenStgCaseAlts	Id Id
type StgCaseDefault = GenStgCaseDefault	Id Id
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection[UpdateFlag-datatype]{@UpdateFlag@}
%*                                                                      *
%************************************************************************

This is also used in @LambdaFormInfo@ in the @ClosureInfo@ module.

\begin{code}
data UpdateFlag = ReEntrant | Updatable | SingleEntry

instance Outputable UpdateFlag where
    ppr sty u
      = ppChar (case u of { ReEntrant -> 'r';  Updatable -> 'u';  SingleEntry -> 's' })
\end{code}

%************************************************************************
%*									*
\subsection[Stg-utility-functions]{Utility functions}
%*									*
%************************************************************************


For doing interfaces, we want the exported top-level Ids from the
final pre-codegen STG code, so as to be sure we have the
latest/greatest pragma info.

\begin{code}
collectExportedStgBinders
	:: [StgBinding]	-- input program
	-> [Id]			-- exported top-level Ids

collectExportedStgBinders binds
  = ex [] binds
  where
    ex es [] = es

    ex es ((StgNonRec b _) : binds)
      = if not (isExported b) then
	    ex es binds
	else
	    ex (b:es) binds

    ex es ((StgRec []) : binds) = ex es binds

    ex es ((StgRec ((b, rhs) : pairs)) : binds)
      = ex es (StgNonRec b rhs : (StgRec pairs : binds))
	    -- OK, a total hack; laziness rules
\end{code}

%************************************************************************
%*									*
\subsection[Stg-pretty-printing]{Pretty-printing}
%*									*
%************************************************************************

Robin Popplestone asked for semi-colon separators on STG binds; here's
hoping he likes terminators instead...  Ditto for case alternatives.

\begin{code}
pprStgBinding :: (Outputable bndr, Outputable bdee, Ord bdee) =>
		PprStyle -> GenStgBinding bndr bdee -> Pretty

pprStgBinding sty (StgNonRec bndr rhs)
  = ppHang (ppCat [ppr sty bndr, ppEquals])
    	 4 (ppBeside (ppr sty rhs) ppSemi)

pprStgBinding sty (StgRec pairs)
  = ppAboves ((ifPprDebug sty (ppStr "{- StgRec -}")) :
	      (map (ppr_bind sty) pairs))
  where
    ppr_bind sty (bndr, expr)
      = ppHang (ppCat [ppr sty bndr, ppEquals])
	     4 (ppBeside (ppr sty expr) ppSemi)

pprPlainStgBinding :: PprStyle -> StgBinding -> Pretty
pprPlainStgBinding sty b = pprStgBinding sty b
\end{code}

\begin{code}
instance (Outputable bdee) => Outputable (GenStgArg bdee) where
    ppr = pprStgArg

instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (GenStgBinding bndr bdee) where
    ppr = pprStgBinding

instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (GenStgExpr bndr bdee) where
    ppr = pprStgExpr

instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (GenStgRhs bndr bdee) where
    ppr sty rhs = pprStgRhs sty rhs
\end{code}

\begin{code}
pprStgArg :: (Outputable bdee) => PprStyle -> GenStgArg bdee -> Pretty

pprStgArg sty (StgVarArg var) = ppr sty var
pprStgArg sty (StgLitArg lit) = ppr sty lit
\end{code}

\begin{code}
pprStgExpr :: (Outputable bndr, Outputable bdee, Ord bdee) =>
		PprStyle -> GenStgExpr bndr bdee -> Pretty
-- special case
pprStgExpr sty (StgApp func [] lvs)
  = ppBeside (ppr sty func) (pprStgLVs sty lvs)

-- general case
pprStgExpr sty (StgApp func args lvs)
  = ppHang (ppBeside (ppr sty func) (pprStgLVs sty lvs))
	 4 (ppSep (map (ppr sty) args))
\end{code}

\begin{code}
pprStgExpr sty (StgCon con args lvs)
  = ppBesides [ ppBeside (ppr sty con) (pprStgLVs sty lvs),
		ppStr "! [", interppSP sty args, ppStr "]" ]

pprStgExpr sty (StgPrim op args lvs)
  = ppBesides [ ppr sty op, ppChar '#', pprStgLVs sty lvs,
		ppStr " [", interppSP sty args, ppStr "]" ]
\end{code}

\begin{code}
-- special case: let v = <very specific thing>
--		 in
--		 let ...
--		 in
--		 ...
--
-- Very special!  Suspicious! (SLPJ)

pprStgExpr sty (StgLet (StgNonRec bndr (StgRhsClosure cc bi free_vars upd_flag args rhs))
		    	expr@(StgLet _ _))
  = ppAbove
      (ppHang (ppBesides [ppStr "let { ", ppr sty bndr, ppStr " = ",
			  ppStr (showCostCentre sty True{-as string-} cc),
			  pp_binder_info sty bi,
			  ppStr " [", ifPprDebug sty (interppSP sty free_vars), ppStr "] \\",
			  ppr sty upd_flag, ppStr " [",
			  interppSP sty args, ppStr "]"])
	    8 (ppSep [ppCat [ppr sty rhs, ppStr "} in"]]))
      (ppr sty expr)

-- special case: let ... in let ...

pprStgExpr sty (StgLet bind expr@(StgLet _ _))
  = ppAbove
      (ppSep [ppHang (ppStr "let {") 2 (ppCat [pprStgBinding sty bind, ppStr "} in"])])
      (ppr sty expr)

-- general case
pprStgExpr sty (StgLet bind expr)
  = ppSep [ppHang (ppStr "let {") 2 (pprStgBinding sty bind),
	   ppHang (ppStr "} in ") 2 (ppr sty expr)]

pprStgExpr sty (StgLetNoEscape lvs_whole lvs_rhss bind expr)
  = ppSep [ppHang (ppStr "let-no-escape {")
	    	2 (pprStgBinding sty bind),
	   ppHang (ppBeside (ppStr "} in ")
		   (ifPprDebug sty (
		    ppNest 4 (
		      ppBesides [ppStr  "-- lvs: [", interppSP sty (uniqSetToList lvs_whole),
			     ppStr "]; rhs lvs: [", interppSP sty (uniqSetToList lvs_rhss),
			     ppStr "]"]))))
		2 (ppr sty expr)]
\end{code}

\begin{code}
pprStgExpr sty (StgSCC ty cc expr)
  = ppSep [ ppCat [ppStr "_scc_", ppStr (showCostCentre sty True{-as string-} cc)],
	    pprStgExpr sty expr ]
\end{code}

\begin{code}
pprStgExpr sty (StgCase expr lvs_whole lvs_rhss uniq alts)
  = ppSep [ppSep [ppStr "case",
	   ppNest 4 (ppCat [pprStgExpr sty expr,
	     ifPprDebug sty (ppBeside (ppStr "::") (pp_ty alts))]),
	   ppStr "of {"],
	   ifPprDebug sty (
	   ppNest 4 (
	     ppBesides [ppStr  "-- lvs: [", interppSP sty (uniqSetToList lvs_whole),
		    ppStr "]; rhs lvs: [", interppSP sty (uniqSetToList lvs_rhss),
		    ppStr "]; uniq: ", pprUnique uniq])),
	   ppNest 2 (ppr_alts sty alts),
	   ppStr "}"]
  where
    pp_ty (StgAlgAlts  ty _ _) = ppr sty ty
    pp_ty (StgPrimAlts ty _ _) = ppr sty ty

    ppr_alts sty (StgAlgAlts ty alts deflt)
      = ppAboves [ ppAboves (map (ppr_bxd_alt sty) alts),
		   ppr_default sty deflt ]
      where
	ppr_bxd_alt sty (con, params, use_mask, expr)
	  = ppHang (ppCat [ppr_con sty con, interppSP sty params, ppStr "->"])
		   4 (ppBeside (ppr sty expr) ppSemi)
	  where
	    ppr_con sty con
	      = if isOpLexeme con
		then ppBesides [ppLparen, ppr sty con, ppRparen]
		else ppr sty con

    ppr_alts sty (StgPrimAlts ty alts deflt)
      = ppAboves [ ppAboves (map (ppr_ubxd_alt sty) alts),
		   ppr_default sty deflt ]
      where
	ppr_ubxd_alt sty (lit, expr)
	  = ppHang (ppCat [ppr sty lit, ppStr "->"])
		 4 (ppBeside (ppr sty expr) ppSemi)

    ppr_default sty StgNoDefault = ppNil
    ppr_default sty (StgBindDefault bndr used expr)
      = ppHang (ppCat [pp_binder, ppStr "->"]) 4 (ppr sty expr)
      where
    	pp_binder = if used then ppr sty bndr else ppChar '_'
\end{code}

\begin{code}
-- pprStgLVs :: PprStyle -> GenStgLiveVars occ -> Pretty

pprStgLVs PprForUser lvs = ppNil

pprStgLVs sty lvs
  = if isEmptyUniqSet lvs then
	ppNil
    else
	ppBesides [ppStr "{-lvs:", interpp'SP sty (uniqSetToList lvs), ppStr "-}"]
\end{code}

\begin{code}
pprStgRhs :: (Outputable bndr, Outputable bdee, Ord bdee) =>
		PprStyle -> GenStgRhs bndr bdee -> Pretty

-- special case
pprStgRhs sty (StgRhsClosure cc bi [free_var] upd_flag [{-no args-}] (StgApp func [] lvs))
  = ppBesides [ ppStr (showCostCentre sty True{-as String-} cc),
		pp_binder_info sty bi,
		ppStr " [", ifPprDebug sty (ppr sty free_var),
	    ppStr "] \\", ppr sty upd_flag, ppStr " [] ", ppr sty func ]
-- general case
pprStgRhs sty (StgRhsClosure cc bi free_vars upd_flag args body)
  = ppHang (ppBesides [ ppStr (showCostCentre sty True{-as String-} cc),
		pp_binder_info sty bi,
		ppStr " [", ifPprDebug sty (interppSP sty free_vars),
		ppStr "] \\", ppr sty upd_flag, ppStr " [", interppSP sty args, ppStr "]"])
	 4 (ppr sty body)

pprStgRhs sty (StgRhsCon cc con args)
  = ppBesides [ ppStr (showCostCentre sty True{-as String-} cc),
		ppSP, ppr sty con, ppStr " [", interppSP sty args, ppStr "]" ]

--------------
pp_binder_info PprForUser _ = ppNil

pp_binder_info sty NoStgBinderInfo = ppNil

-- cases so boring that we print nothing
pp_binder_info sty (StgBinderInfo True b c d e) = ppNil

-- general case
pp_binder_info sty (StgBinderInfo a b c d e)
  = ppBesides [ppChar '(', ppInterleave ppComma (map pp_bool [a,b,c,d,e]), ppChar ')']
  where
    pp_bool x = ppr (panic "pp_bool") x
\end{code}

Collect @IdInfo@ stuff that is most easily just snaffled straight
from the STG bindings.

\begin{code}
stgArity :: StgRhs -> Int

stgArity (StgRhsCon _ _ _)	         = 0 -- it's a constructor, fully applied
stgArity (StgRhsClosure _ _ _ _ args _ ) = length args
\end{code}
