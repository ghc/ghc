%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[StgSyn]{Shared term graph (STG) syntax for spineless-tagless code generation}

This data type represents programs just before code generation
(conversion to @AbstractC@): basically, what we have is a stylised
form of @CoreSyntax@, the style being one that happens to be ideally
suited to spineless tagless code generation.

\begin{code}
#include "HsVersions.h"

module StgSyn (
	StgAtom(..),
	StgLiveVars(..),

	StgBinding(..), StgExpr(..), StgRhs(..),
	StgCaseAlternatives(..), StgCaseDefault(..),
#ifdef DPH
	StgParCommunicate(..),
#endif {- Data Parallel Haskell -}

	UpdateFlag(..),

	StgBinderInfo(..),
	stgArgOcc, stgUnsatOcc, stgStdHeapOcc, stgNoUpdHeapOcc,
	stgNormalOcc, stgFakeFunAppOcc,
	combineStgBinderInfo,

	-- a set of synonyms for the most common (only :-) parameterisation
	PlainStgAtom(..), PlainStgLiveVars(..), PlainStgProgram(..),
	PlainStgBinding(..), PlainStgExpr(..), PlainStgRhs(..),
	PlainStgCaseAlternatives(..), PlainStgCaseDefault(..),

	pprPlainStgBinding,
--UNUSED:	fvsFromAtoms,
	getAtomKind,
	isLitLitStgAtom,
	stgArity,
	collectExportedStgBinders,

	-- and to make the interface self-sufficient...
	Outputable(..), NamedThing(..), Pretty(..),
	Unique, ExportFlag, SrcLoc, PprStyle, PrettyRep,

	BasicLit, Class, ClassOp, 
	
	Binds, Expr, GRHS, GRHSsAndBinds, InPat,

	Id, IdInfo, Maybe, Name, FullName, ShortName,
	PrimKind, PrimOp, CostCentre, TyCon, TyVar,
	UniqSet(..), UniqFM, Bag,
	TyVarTemplate, UniType, TauType(..),
	ThetaType(..), SigmaType(..),
	TyVarEnv(..), IdEnv(..)

	IF_ATTACK_PRAGMAS(COMMA isLitLitLit)
	IF_ATTACK_PRAGMAS(COMMA cmpTyCon COMMA cmpTyVar COMMA cmpClass)
	IF_ATTACK_PRAGMAS(COMMA cmpUniType)
    ) where

import AbsPrel		( getPrimOpResultInfo, PrimOpResultInfo(..),
			  PrimOp, PrimKind
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsSyn		( Binds, Expr, GRHS, GRHSsAndBinds, InPat )
import AbsUniType
import BasicLit		( typeOfBasicLit, kindOfBasicLit, isLitLitLit,
			  BasicLit(..) -- (..) for pragmas
			)
import Id		( getIdUniType, getIdKind, toplevelishId,
			  isTopLevId, Id, IdInfo
			)
import Maybes		( Maybe(..), catMaybes )
import Outputable
import Pretty
import PrimKind		( PrimKind )
import CostCentre	( showCostCentre, CostCentre )
import UniqSet
import Unique
import Util
\end{code}

%************************************************************************
%*									*
\subsection[StgBinding]{@StgBinding@}
%*									*
%************************************************************************

As usual, expressions are interesting; other things are boring.  Here
are the boring things [except note the @StgRhs@], parameterised with
respect to binder and bindee information (just as in @CoreSyntax@):
\begin{code}
data StgBinding binder bindee
  = StgNonRec	binder (StgRhs binder bindee)
  | StgRec	[(binder, StgRhs binder bindee)]
\end{code}

An @StgProgram@ is just a list of @StgBindings@; the
properties/restrictions-on this list are the same as for a
@CoreProgram@ (a list of @CoreBindings@).
\begin{code}
--type StgProgram binder bindee = [StgBinding binder bindee]
\end{code}

%************************************************************************
%*									*
\subsection[StgAtom]{@StgAtom@}
%*									*
%************************************************************************

\begin{code}
data StgAtom bindee
  = StgVarAtom	bindee
  | StgLitAtom	BasicLit
\end{code}

\begin{code}
getAtomKind (StgVarAtom  local) = getIdKind local
getAtomKind (StgLitAtom  lit)	= kindOfBasicLit lit

{- UNUSED happily
fvsFromAtoms :: [PlainStgAtom] -> (UniqSet Id)	-- ToDo: this looks like a HACK to me (WDP)
fvsFromAtoms as = mkUniqSet [ id | (StgVarAtom id) <- as, not (toplevelishId id) ]
-}

isLitLitStgAtom (StgLitAtom x) = isLitLitLit x
isLitLitStgAtom _   	       = False
\end{code}

%************************************************************************
%*									*
\subsection[StgExpr]{STG expressions}
%*									*
%************************************************************************

The @StgExpr@ data type is parameterised on binder and bindee info, as
before.

%************************************************************************
%*									*
\subsubsection[StgExpr-application]{@StgExpr@ application}
%*									*
%************************************************************************

An application is of a function to a list of atoms [not expressions].
Operationally, we want to push the arguments on the stack and call the
function.  (If the arguments were expressions, we would have to build
their closures first.)

There is no constructor for a lone variable; it would appear as
@StgApp var [] _@.
\begin{code}
type StgLiveVars bindee = UniqSet bindee

data StgExpr binder bindee
  = StgApp	
	(StgAtom bindee)	-- function
	[StgAtom bindee]	-- arguments
	(StgLiveVars bindee)	-- Live vars in continuation; ie not
				-- including the function and args

    -- NB: a literal is: StgApp <lit-atom> [] ...
\end{code}

%************************************************************************
%*									*
\subsubsection[StgExpr-apps]{@StgConApp@ and @StgPrimApp@---saturated applications}
%*									*
%************************************************************************

There are two specialised forms of application, for
constructors and primitives.
\begin{code}
  | StgConApp			-- always saturated
	Id -- data constructor
	[StgAtom bindee]
	(StgLiveVars bindee)	-- Live vars in continuation; ie not
				-- including the constr and args

  | StgPrimApp			-- always saturated
	PrimOp
	[StgAtom bindee]
	(StgLiveVars bindee)	-- Live vars in continuation; ie not
				-- including the op and args
\end{code}
These forms are to do ``inline versions,'' as it were.
An example might be: @f x = x:[]@.

%************************************************************************
%*									*
\subsubsection[StgExpr-case]{@StgExpr@: case-expressions}
%*									*
%************************************************************************

This has the same boxed/unboxed business as Core case expressions.
\begin{code}
  | StgCase
	(StgExpr binder bindee)
			-- the thing to examine

	(StgLiveVars bindee) -- Live vars of whole case
			-- expression; i.e., those which mustn't be
			-- overwritten

	(StgLiveVars bindee) -- Live vars of RHSs;
			-- i.e., those which must be saved before eval.
			--
			-- note that an alt's constructor's
			-- binder-variables are NOT counted in the
			-- free vars for the alt's RHS

	Unique		-- Occasionally needed to compile case
			-- statements, as the uniq for a local
			-- variable to hold the tag of a primop with
			-- algebraic result

	(StgCaseAlternatives binder bindee)
\end{code}

%************************************************************************
%*									*
\subsubsection[StgExpr-lets]{@StgExpr@:  @let(rec)@-expressions}
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
let-literal x = BasicLit
in e
\end{verbatim}

(ToDo: is this obsolete?)
\end{enumerate}

And so the code for let(rec)-things:
\begin{code}
  | StgLet
	(StgBinding binder bindee)	-- right hand sides (see below)
	(StgExpr binder bindee)		-- body

  | StgLetNoEscape			-- remember: ``advanced stuff''
	(StgLiveVars bindee)		-- Live in the whole let-expression
					-- Mustn't overwrite these stack slots
    	    	    	    	    	-- *Doesn't* include binders of the let(rec).

	(StgLiveVars bindee)		-- Live in the right hand sides (only)
					-- These are the ones which must be saved on
					-- the stack if they aren't there already
    	    	    	    	    	-- *Does* include binders of the let(rec) if recursive.

	(StgBinding binder bindee)	-- right hand sides (see below)
	(StgExpr binder bindee)		-- body
\end{code}

%************************************************************************
%*									*
\subsubsection[StgExpr-scc]{@StgExpr@: @scc@ expressions}
%*									*
%************************************************************************

Finally for @scc@ expressions we introduce a new STG construct.

\begin{code}
  | StgSCC
	UniType			-- the type of the body
	CostCentre		-- label of SCC expression
	(StgExpr binder bindee)	-- scc expression
\end{code}

%************************************************************************
%*									*
\subsection[DataParallel]{Data parallel extensions to STG syntax}
%*									*
%************************************************************************

\begin{code}
#ifdef DPH
  | StgParConApp			-- saturated parallel constructor
        Id
	Int				-- What parallel context
	[StgAtom bindee]
	(StgLiveVars bindee)

  | StgParComm
	Int
	(StgExpr binder bindee)		-- The thing we are communicating
	(StgParCommunicate binder bindee)
#endif {- Data Parallel Haskell -}
  -- end of StgExpr
\end{code}

%************************************************************************
%*									*
\subsection[StgRhs]{STG right-hand sides}
%*									*
%************************************************************************

Here's the rest of the interesting stuff for @StgLet@s; the first
flavour is for closures:
\begin{code}
data StgRhs binder bindee
  = StgRhsClosure
	CostCentre		-- cost centre to be attached (default is CCC)
	StgBinderInfo		-- Info about how this binder is used (see below)
	[bindee]		-- non-global free vars; a list, rather than
				-- a set, because order is important
	UpdateFlag		-- ReEntrant | Updatable | SingleEntry
	[binder]		-- arguments; if empty, then not a function;
				-- as above, order is important
	(StgExpr binder bindee)	-- body
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
	[StgAtom bindee]	-- args
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
data StgCaseAlternatives binder bindee
  = StgAlgAlts	UniType	-- so we can find out things about constructor family
		[(Id,				-- alts: data constructor,
		  [binder],			-- constructor's parameters,
		  [Bool],			-- "use mask", same length as
						-- parameters; a True in a
						-- param's position if it is
						-- used in the ...
		  StgExpr binder bindee)]	-- ...right-hand side.
		(StgCaseDefault binder bindee)
  | StgPrimAlts	UniType	-- so we can find out things about constructor family
		[(BasicLit,			-- alts: unboxed literal,
		  StgExpr binder bindee)]	-- rhs.
		(StgCaseDefault binder bindee)
#ifdef DPH
  | StgParAlgAlts	
		UniType	
		Int				-- What context we are in
		[binder]			
		[(Id,StgExpr binder bindee)]	
		(StgCaseDefault binder bindee)
  | StgParPrimAlts	UniType
		Int				-- What context we are in
		[(BasicLit,			-- alts: unboxed literal,
		  StgExpr binder bindee)]	-- rhs.
		(StgCaseDefault binder bindee)
#endif {- Data Parallel Haskell -}

data StgCaseDefault binder bindee
  = StgNoDefault				-- small con family: all
						-- constructor accounted for
  | StgBindDefault  binder			-- form: var -> expr
		    Bool			-- True <=> var is used in rhs
						-- i.e., False <=> "_ -> expr"
		    (StgExpr binder bindee)
\end{code}

%************************************************************************
%*									*
\subsection[Stg-parComummunicate]{Communication operations}
%*									*
%************************************************************************

\begin{code}
#ifdef DPH
data StgParCommunicate binder bindee
  = StgParSend 
	[StgAtom bindee]	-- Sending PODs

  | StgParFetch 
	[StgAtom bindee]	-- Fetching PODs

  | StgToPodized		-- Convert a POD to the podized form

  | StgFromPodized		-- Convert a POD from the podized form
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[PlainStg]{The Plain STG parameterisation}
%*									*
%************************************************************************

This happens to be the only one we use at the moment.

\begin{code}
type PlainStgProgram = [StgBinding Id Id]
type PlainStgBinding = StgBinding Id Id
type PlainStgAtom    = StgAtom    Id
type PlainStgLiveVars= UniqSet Id
type PlainStgExpr    = StgExpr    Id Id
type PlainStgRhs     = StgRhs     Id Id
type PlainStgCaseAlternatives = StgCaseAlternatives Id Id
type PlainStgCaseDefault      = StgCaseDefault      Id Id
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
	:: [PlainStgBinding]	-- input: PlainStgProgram
	-> [Id]			-- exported top-level Ids

collectExportedStgBinders binds
  = exported_from_here [] binds
  where
    exported_from_here es [] = es

    exported_from_here es ((StgNonRec b _) : binds)
      = if not (isExported b) then
	    exported_from_here es binds
	else
	    exported_from_here (b:es) binds

    exported_from_here es ((StgRec []) : binds)
      = exported_from_here es binds

    exported_from_here es ((StgRec ((b, rhs) : pairs)) : binds)
      = exported_from_here
	  es
	  (StgNonRec b rhs : (StgRec pairs : binds))
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
		PprStyle -> StgBinding bndr bdee -> Pretty

pprStgBinding sty (StgNonRec binder rhs)
  = ppHang (ppCat [ppr sty binder, ppEquals])
    	 4 (ppBeside (ppr sty rhs) ppSemi)

pprStgBinding sty (StgRec pairs)
  = ppAboves ((ifPprDebug sty (ppStr "{- StgRec -}")) :
	      (map (ppr_bind sty) pairs))
  where
    ppr_bind sty (binder, expr)
      = ppHang (ppCat [ppr sty binder, ppEquals])
	     4 (ppBeside (ppr sty expr) ppSemi)

pprPlainStgBinding :: PprStyle -> PlainStgBinding -> Pretty
pprPlainStgBinding sty b = pprStgBinding sty b
\end{code}

\begin{code}
instance (Outputable bdee) => Outputable (StgAtom bdee) where
    ppr = pprStgAtom

instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (StgBinding bndr bdee) where
    ppr = pprStgBinding

instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (StgExpr bndr bdee) where
    ppr = pprStgExpr

{- OLD:
instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (StgCaseDefault bndr bdee) where
    ppr sty deflt = panic "ppr:StgCaseDefault"
-}

instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (StgRhs bndr bdee) where
    ppr sty rhs = pprStgRhs sty rhs
\end{code}

\begin{code}
pprStgAtom :: (Outputable bdee) => PprStyle -> StgAtom bdee -> Pretty

pprStgAtom sty (StgVarAtom var) = ppr sty var
pprStgAtom sty (StgLitAtom lit) = ppr sty lit
\end{code}

\begin{code}
pprStgExpr :: (Outputable bndr, Outputable bdee, Ord bdee) =>
		PprStyle -> StgExpr bndr bdee -> Pretty
-- special case
pprStgExpr sty (StgApp func [] lvs)
  = ppBeside (ppr sty func) (pprStgLVs sty lvs)

-- general case
pprStgExpr sty (StgApp func args lvs)
  = ppHang (ppBeside (ppr sty func) (pprStgLVs sty lvs))
	 4 (ppSep (map (ppr sty) args))
\end{code}

\begin{code}
pprStgExpr sty (StgConApp con args lvs)
  = ppBesides [ ppBeside (ppr sty con) (pprStgLVs sty lvs),
		ppStr "! [", interppSP sty args, ppStr "]" ]

pprStgExpr sty (StgPrimApp op args lvs)
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

pprStgExpr sty (StgLet (StgNonRec binder (StgRhsClosure cc bi free_vars upd_flag args rhs))
		    	expr@(StgLet _ _))
  = ppAbove
      (ppHang (ppBesides [ppStr "let { ", ppr sty binder, ppStr " = ",
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

#ifdef DPH
    ppr_alts sty (StgParAlgAlts ty dim params alts deflt)
      = ppAboves [ ppBeside (ppCat (map (ppr sty) params))
                        (ppCat [ppStr "|" , ppr sty dim , ppStr "|"]),
		   ppAboves (map (ppr_bxd_alt sty) alts),
		   ppr_default sty deflt ]
      where
	ppr_bxd_alt sty (con, expr)
	  = ppHang (ppCat [ppStr "\\/", ppr_con sty con, ppStr "->"])
		   4 (ppr sty expr)
	  where
	    ppr_con sty con
	      = if isOpLexeme con
		then ppBesides [ppLparen, ppr sty con, ppRparen]
		else ppr sty con

    ppr_alts sty (StgParPrimAlts ty dim alts deflt)
      = ppAboves [ ifPprShowAll sty (ppr sty ty),
		   ppCat [ppStr "|" , ppr sty dim , ppStr "|"],
		   ppAboves (map (ppr_ubxd_alt sty) alts),
		   ppr_default sty deflt ]
      where
	ppr_ubxd_alt sty (lit, expr)
	  = ppHang (ppCat [ppStr "\\/", ppr sty lit, ppStr "->"]) 4 (ppr sty expr)
#endif {- Data Parallel Haskell -}

    ppr_default sty StgNoDefault = ppNil
    ppr_default sty (StgBindDefault binder used expr)
      = ppHang (ppCat [pp_binder, ppStr "->"]) 4 (ppr sty expr)
      where
    	pp_binder = if used then ppr sty binder else ppChar '_' 
\end{code}

\begin{code}
#ifdef DPH
pprStgExpr sty (StgParConApp con dim args lvs)
  = ppBesides [ppr sty con, pprStgLVs sty lvs, ppStr "!<<" ,ppr sty dim , 
	       ppStr ">> [", interppSP sty args, ppStr "]" ]

pprStgExpr sty (StgParComm dim expr comm)
  = ppSep [ppSep [ppStr "COMM ",
                  ppNest 2 (pprStgExpr sty expr),ppStr "{"],
           ppNest 2 (ppr_comm sty comm),
           ppStr "}"]
  where
    ppr_comm sty (StgParSend args)
      = ppSep [ppStr "SEND [",interppSP sty args, ppStr "]" ]
    ppr_comm sty (StgParFetch args)
      = ppSep [ppStr "FETCH [",interppSP sty args, ppStr "]" ]
    ppr_comm sty (StgToPodized)
      = ppStr "ToPodized"
    ppr_comm sty (StgFromPodized)
      = ppStr "FromPodized"
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
-- pprStgLVs :: PprStyle -> StgLiveVars bindee -> Pretty

pprStgLVs PprForUser lvs = ppNil

pprStgLVs sty lvs
  = if isEmptyUniqSet lvs then
	ppNil
    else
	ppBesides [ppStr "{-lvs:", interpp'SP sty (uniqSetToList lvs), ppStr "-}"]
\end{code}

\begin{code}
pprStgRhs :: (Outputable bndr, Outputable bdee, Ord bdee) =>
		PprStyle -> StgRhs bndr bdee -> Pretty

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
stgArity :: PlainStgRhs -> Int

stgArity (StgRhsCon _ _ _)	         = 0 -- it's a constructor, fully applied
stgArity (StgRhsClosure _ _ _ _ args _ ) = length args
\end{code}
