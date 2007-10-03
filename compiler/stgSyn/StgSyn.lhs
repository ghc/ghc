%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[StgSyn]{Shared term graph (STG) syntax for spineless-tagless code generation}

This data type represents programs just before code generation
(conversion to @AbstractC@): basically, what we have is a stylised
form of @CoreSyntax@, the style being one that happens to be ideally
suited to spineless tagless code generation.

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module StgSyn (
	GenStgArg(..), 
	GenStgLiveVars,

	GenStgBinding(..), GenStgExpr(..), GenStgRhs(..),
	GenStgAlt, AltType(..),

	UpdateFlag(..), isUpdatable,

	StgBinderInfo,
	noBinderInfo, stgSatOcc, stgUnsatOcc, satCallsOnly,
	combineStgBinderInfo,

	-- a set of synonyms for the most common (only :-) parameterisation
	StgArg, StgLiveVars,
	StgBinding, StgExpr, StgRhs, StgAlt, 

	-- StgOp
	StgOp(..),

	-- SRTs
	SRT(..),

	-- utils
	stgBindHasCafRefs, stgArgHasCafRefs, stgRhsArity,
	isDllConApp, isStgTypeArg,
	stgArgType,

	pprStgBinding, pprStgBindings, pprStgBindingsWithSRTs

#ifdef DEBUG
	, pprStgLVs
#endif
    ) where

#include "HsVersions.h"

import CostCentre	( CostCentreStack, CostCentre )
import VarSet		( IdSet, isEmptyVarSet )
import Var		( isId )
import Id		( Id, idName, idType, idCafInfo )
import IdInfo		( mayHaveCafRefs )
import Packages		( isDllName )
import Literal		( Literal, literalType )
import ForeignCall	( ForeignCall )
import DataCon		( DataCon, dataConName )
import CoreSyn		( AltCon )
import PprCore		( {- instances -} )
import PrimOp		( PrimOp )
import Outputable
import Util             ( count )
import Type             ( Type )
import TyCon            ( TyCon )
import UniqSet		( isEmptyUniqSet, uniqSetToList, UniqSet )
import Unique		( Unique )
import Bitmap
import StaticFlags	( opt_SccProfilingOn )
import Module
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

There is one SRT for each group of bindings.

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
  | StgLitArg   Literal
  | StgTypeArg  Type		-- For when we want to preserve all type info
\end{code}

\begin{code}
isStgTypeArg (StgTypeArg _) = True
isStgTypeArg other	    = False

isDllArg :: PackageId -> StgArg -> Bool
	-- Does this argument refer to something in a different DLL?
isDllArg this_pkg (StgTypeArg v)  = False
isDllArg this_pkg (StgVarArg v)   = isDllName this_pkg (idName v)
isDllArg this_pkg (StgLitArg lit) = False

isDllConApp :: PackageId -> DataCon -> [StgArg] -> Bool
	-- Does this constructor application refer to 
	-- anything in a different DLL?
	-- If so, we can't allocate it statically
isDllConApp this_pkg con args
   = isDllName this_pkg (dataConName con) || any (isDllArg this_pkg) args

stgArgType :: StgArg -> Type
	-- Very half baked becase we have lost the type arguments
stgArgType (StgVarArg v)   = idType v
stgArgType (StgLitArg lit) = literalType lit
stgArgType (StgTypeArg lit) = panic "stgArgType called on stgTypeArg"
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
	occ		-- function
	[GenStgArg occ]	-- arguments; may be empty
\end{code}

%************************************************************************
%*									*
\subsubsection{@StgConApp@ and @StgPrimApp@---saturated applications}
%*									*
%************************************************************************

There are a specialised forms of application, for
constructors, primitives, and literals.
\begin{code}
  | StgLit	Literal
  
	-- StgConApp is vital for returning unboxed tuples
	-- which can't be let-bound first
  | StgConApp	DataCon
		[GenStgArg occ]	-- Saturated

  | StgOpApp	StgOp		-- Primitive op or foreign call
		[GenStgArg occ]	-- Saturated
		Type		-- Result type
				-- We need to know this so that we can 
				-- assign result registers
\end{code}

%************************************************************************
%*									*
\subsubsection{@StgLam@}
%*									*
%************************************************************************

StgLam is used *only* during CoreToStg's work.  Before CoreToStg has finished
it encodes (\x -> e) as (let f = \x -> e in f)

\begin{code}
  | StgLam
	Type		-- Type of whole lambda (useful when making a binder for it)
	[bndr]
	StgExpr		-- Body of lambda
\end{code}


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

	(GenStgLiveVars occ) -- Live vars of whole case expression, 
			-- plus everything that happens after the case
			-- i.e., those which mustn't be overwritten

	(GenStgLiveVars occ) -- Live vars of RHSs (plus what happens afterwards)
			-- i.e., those which must be saved before eval.
			--
			-- note that an alt's constructor's
			-- binder-variables are NOT counted in the
			-- free vars for the alt's RHS

	bndr		-- binds the result of evaluating the scrutinee

	SRT		-- The SRT for the continuation

	AltType 

	[GenStgAlt bndr occ]	-- The DEFAULT case is always *first* 
				-- if it is there at all
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
    	    	    	    	    	--  *Doesn't* include binders of the let(rec).

	(GenStgLiveVars occ)		-- Live in the right hand sides (only)
					-- These are the ones which must be saved on
					-- the stack if they aren't there already
    	    	    	    	    	--  *Does* include binders of the let(rec) if recursive.

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
	CostCentre		-- label of SCC expression
	(GenStgExpr bndr occ)	-- scc expression
\end{code}

%************************************************************************
%*									*
\subsubsection{@GenStgExpr@: @hpc@ expressions}
%*									*
%************************************************************************

Finally for @scc@ expressions we introduce a new STG construct.

\begin{code}
  | StgTick
    Module			-- the module of the source of this tick
    Int				-- tick number
    (GenStgExpr bndr occ)	-- sub expression
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
	CostCentreStack		-- CCS to be attached (default is CurrentCCS)
	StgBinderInfo		-- Info about how this binder is used (see below)
	[occ]			-- non-global free vars; a list, rather than
				-- a set, because order is important
	!UpdateFlag		-- ReEntrant | Updatable | SingleEntry
	SRT 			-- The SRT reference
	[bndr]			-- arguments; if empty, then not a function;
				-- as above, order is important.
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
	CostCentreStack		-- CCS to be attached (default is CurrentCCS).
				-- Top-level (static) ones will end up with
				-- DontCareCCS, because we don't count static
				-- data in heap profiles, and we don't set CCCS
				-- from static closure.
	DataCon			-- constructor
	[GenStgArg occ]	-- args
\end{code}

\begin{code}
stgRhsArity :: StgRhs -> Int
stgRhsArity (StgRhsClosure _ _ _ _ _ bndrs _) = count isId bndrs
  -- The arity never includes type parameters, so
  -- when keeping type arguments and binders in the Stg syntax 
  -- (opt_RuntimeTypes) we have to fliter out the type binders.
stgRhsArity (StgRhsCon _ _ _) = 0
\end{code}

\begin{code}
stgBindHasCafRefs :: GenStgBinding bndr Id -> Bool
stgBindHasCafRefs (StgNonRec _ rhs) = rhsHasCafRefs rhs
stgBindHasCafRefs (StgRec binds)    = any rhsHasCafRefs (map snd binds)

rhsHasCafRefs (StgRhsClosure _ _ _ upd srt _ _) 
  = isUpdatable upd || nonEmptySRT srt
rhsHasCafRefs (StgRhsCon _ _ args)
  = any stgArgHasCafRefs args

stgArgHasCafRefs (StgVarArg id) = mayHaveCafRefs (idCafInfo id)
stgArgHasCafRefs _ = False
\end{code}

Here's the @StgBinderInfo@ type, and its combining op:
\begin{code}
data StgBinderInfo
  = NoStgBinderInfo
  | SatCallsOnly	-- All occurrences are *saturated* *function* calls
			-- This means we don't need to build an info table and 
			-- slow entry code for the thing
			-- Thunks never get this value

noBinderInfo = NoStgBinderInfo
stgUnsatOcc  = NoStgBinderInfo
stgSatOcc    = SatCallsOnly

satCallsOnly :: StgBinderInfo -> Bool
satCallsOnly SatCallsOnly    = True
satCallsOnly NoStgBinderInfo = False

combineStgBinderInfo :: StgBinderInfo -> StgBinderInfo -> StgBinderInfo
combineStgBinderInfo SatCallsOnly SatCallsOnly = SatCallsOnly
combineStgBinderInfo info1 info2 	       = NoStgBinderInfo

--------------
pp_binder_info NoStgBinderInfo = empty
pp_binder_info SatCallsOnly    = ptext SLIT("sat-only")
\end{code}

%************************************************************************
%*									*
\subsection[Stg-case-alternatives]{STG case alternatives}
%*									*
%************************************************************************

Very like in @CoreSyntax@ (except no type-world stuff).

The type constructor is guaranteed not to be abstract; that is, we can
see its representation.  This is important because the code generator
uses it to determine return conventions etc.  But it's not trivial
where there's a moduule loop involved, because some versions of a type
constructor might not have all the constructors visible.  So
mkStgAlgAlts (in CoreToStg) ensures that it gets the TyCon from the
constructors or literals (which are guaranteed to have the Real McCoy)
rather than from the scrutinee type.

\begin{code}
type GenStgAlt bndr occ
  = (AltCon,		-- alts: data constructor,
     [bndr],		-- constructor's parameters,
     [Bool],		-- "use mask", same length as
			-- parameters; a True in a
			-- param's position if it is
			-- used in the ...
     GenStgExpr bndr occ)	-- ...right-hand side.

data AltType
  = PolyAlt		-- Polymorphic (a type variable)
  | UbxTupAlt TyCon	-- Unboxed tuple
  | AlgAlt    TyCon	-- Algebraic data type; the AltCons will be DataAlts
  | PrimAlt   TyCon	-- Primitive data type; the AltCons will be LitAlts
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
type StgAlt	    = GenStgAlt		Id Id
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection[UpdateFlag-datatype]{@UpdateFlag@}
%*                                                                      *
%************************************************************************

This is also used in @LambdaFormInfo@ in the @ClosureInfo@ module.

A @ReEntrant@ closure may be entered multiple times, but should not be
updated or blackholed.  An @Updatable@ closure should be updated after
evaluation (and may be blackholed during evaluation).  A @SingleEntry@
closure will only be entered once, and so need not be updated but may
safely be blackholed.

\begin{code}
data UpdateFlag = ReEntrant | Updatable | SingleEntry

instance Outputable UpdateFlag where
    ppr u
      = char (case u of { ReEntrant -> 'r';  Updatable -> 'u';  SingleEntry -> 's' })

isUpdatable ReEntrant   = False
isUpdatable SingleEntry = False
isUpdatable Updatable   = True
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection{StgOp}
%*                                                                      *
%************************************************************************

An StgOp allows us to group together PrimOps and ForeignCalls.
It's quite useful to move these around together, notably
in StgOpApp and COpStmt.

\begin{code}
data StgOp = StgPrimOp  PrimOp

	   | StgFCallOp ForeignCall Unique
		-- The Unique is occasionally needed by the C pretty-printer
		-- (which lacks a unique supply), notably when generating a
		-- typedef for foreign-export-dynamic
\end{code}


%************************************************************************
%*                                                                      *
\subsubsection[Static Reference Tables]{@SRT@}
%*                                                                      *
%************************************************************************

There is one SRT per top-level function group.  Each local binding and
case expression within this binding group has a subrange of the whole
SRT, expressed as an offset and length.

In CoreToStg we collect the list of CafRefs at each SRT site, which is later 
converted into the length and offset form by the SRT pass.

\begin{code}
data SRT = NoSRT
	 | SRTEntries IdSet
		-- generated by CoreToStg
         | SRT !Int{-offset-} !Int{-length-} !Bitmap{-bitmap-}
		-- generated by computeSRTs

noSRT :: SRT
noSRT = NoSRT

nonEmptySRT NoSRT           = False
nonEmptySRT (SRTEntries vs) = not (isEmptyVarSet vs)
nonEmptySRT _               = True

pprSRT (NoSRT) = ptext SLIT("_no_srt_")
pprSRT (SRTEntries ids) = text "SRT:" <> ppr ids
pprSRT (SRT off length bitmap) = parens (ppr off <> comma <> text "*bitmap*")
\end{code}

%************************************************************************
%*									*
\subsection[Stg-pretty-printing]{Pretty-printing}
%*									*
%************************************************************************

Robin Popplestone asked for semi-colon separators on STG binds; here's
hoping he likes terminators instead...  Ditto for case alternatives.

\begin{code}
pprGenStgBinding :: (Outputable bndr, Outputable bdee, Ord bdee)
		 => GenStgBinding bndr bdee -> SDoc

pprGenStgBinding (StgNonRec bndr rhs)
  = hang (hsep [ppr bndr, equals])
    	4 ((<>) (ppr rhs) semi)

pprGenStgBinding (StgRec pairs)
  = vcat ((ifPprDebug (ptext SLIT("{- StgRec (begin) -}"))) :
	   (map (ppr_bind) pairs) ++ [(ifPprDebug (ptext SLIT("{- StgRec (end) -}")))])
  where
    ppr_bind (bndr, expr)
      = hang (hsep [ppr bndr, equals])
	     4 ((<>) (ppr expr) semi)

pprStgBinding  :: StgBinding -> SDoc
pprStgBinding  bind  = pprGenStgBinding bind

pprStgBindings :: [StgBinding] -> SDoc
pprStgBindings binds = vcat (map pprGenStgBinding binds)

pprGenStgBindingWithSRT	 
	:: (Outputable bndr, Outputable bdee, Ord bdee) 
	=> (GenStgBinding bndr bdee,[(Id,[Id])]) -> SDoc

pprGenStgBindingWithSRT (bind,srts)
  = vcat (pprGenStgBinding bind : map pprSRT srts)
  where pprSRT (id,srt) = 
	   ptext SLIT("SRT") <> parens (ppr id) <> ptext SLIT(": ") <> ppr srt

pprStgBindingsWithSRTs :: [(StgBinding,[(Id,[Id])])] -> SDoc
pprStgBindingsWithSRTs binds = vcat (map pprGenStgBindingWithSRT binds)
\end{code}

\begin{code}
instance (Outputable bdee) => Outputable (GenStgArg bdee) where
    ppr = pprStgArg

instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (GenStgBinding bndr bdee) where
    ppr = pprGenStgBinding

instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (GenStgExpr bndr bdee) where
    ppr = pprStgExpr

instance (Outputable bndr, Outputable bdee, Ord bdee)
		=> Outputable (GenStgRhs bndr bdee) where
    ppr rhs = pprStgRhs rhs
\end{code}

\begin{code}
pprStgArg :: (Outputable bdee) => GenStgArg bdee -> SDoc

pprStgArg (StgVarArg var) = ppr var
pprStgArg (StgLitArg con) = ppr con
pprStgArg (StgTypeArg ty) = char '@' <+> ppr ty
\end{code}

\begin{code}
pprStgExpr :: (Outputable bndr, Outputable bdee, Ord bdee)
	   => GenStgExpr bndr bdee -> SDoc
-- special case
pprStgExpr (StgLit lit)     = ppr lit

-- general case
pprStgExpr (StgApp func args)
  = hang (ppr func)
	 4 (sep (map (ppr) args))
\end{code}

\begin{code}
pprStgExpr (StgConApp con args)
  = hsep [ ppr con, brackets (interppSP args)]

pprStgExpr (StgOpApp op args _)
  = hsep [ pprStgOp op, brackets (interppSP args)]

pprStgExpr (StgLam _ bndrs body)
  =sep [ char '\\' <+> ppr bndrs <+> ptext SLIT("->"),
	 pprStgExpr body ]
\end{code}

\begin{code}
-- special case: let v = <very specific thing>
--		 in
--		 let ...
--		 in
--		 ...
--
-- Very special!  Suspicious! (SLPJ)

{-
pprStgExpr (StgLet srt (StgNonRec bndr (StgRhsClosure cc bi free_vars upd_flag args rhs))
		    	expr@(StgLet _ _))
  = ($$)
      (hang (hcat [ptext SLIT("let { "), ppr bndr, ptext SLIT(" = "),
			  ppr cc,
			  pp_binder_info bi,
			  ptext SLIT(" ["), ifPprDebug (interppSP free_vars), ptext SLIT("] \\"),
			  ppr upd_flag, ptext SLIT(" ["),
			  interppSP args, char ']'])
	    8 (sep [hsep [ppr rhs, ptext SLIT("} in")]]))
      (ppr expr)
-}

-- special case: let ... in let ...

pprStgExpr (StgLet bind expr@(StgLet _ _))
  = ($$)
      (sep [hang (ptext SLIT("let {"))
	 	2 (hsep [pprGenStgBinding bind, ptext SLIT("} in")])])
      (ppr expr)

-- general case
pprStgExpr (StgLet bind expr)
  = sep [hang (ptext SLIT("let {")) 2 (pprGenStgBinding bind),
	   hang (ptext SLIT("} in ")) 2 (ppr expr)]

pprStgExpr (StgLetNoEscape lvs_whole lvs_rhss bind expr)
  = sep [hang (ptext SLIT("let-no-escape {"))
	    	2 (pprGenStgBinding bind),
	   hang ((<>) (ptext SLIT("} in "))
		   (ifPprDebug (
		    nest 4 (
		      hcat [ptext  SLIT("-- lvs: ["), interppSP (uniqSetToList lvs_whole),
			     ptext SLIT("]; rhs lvs: ["), interppSP (uniqSetToList lvs_rhss),
			     char ']']))))
		2 (ppr expr)]

pprStgExpr (StgSCC cc expr)
  = sep [ hsep [ptext SLIT("_scc_"), ppr cc],
	  pprStgExpr expr ]

pprStgExpr (StgTick m n expr)
  = sep [ hsep [ptext SLIT("_tick_"),  pprModule m,text (show n)],
	  pprStgExpr expr ]

pprStgExpr (StgCase expr lvs_whole lvs_rhss bndr srt alt_type alts)
  = sep [sep [ptext SLIT("case"),
	   nest 4 (hsep [pprStgExpr expr,
	     ifPprDebug (dcolon <+> ppr alt_type)]),
	   ptext SLIT("of"), ppr bndr, char '{'],
	   ifPprDebug (
	   nest 4 (
	     hcat [ptext  SLIT("-- lvs: ["), interppSP (uniqSetToList lvs_whole),
		    ptext SLIT("]; rhs lvs: ["), interppSP (uniqSetToList lvs_rhss),
		    ptext SLIT("]; "),
		    pprMaybeSRT srt])),
	   nest 2 (vcat (map pprStgAlt alts)),
	   char '}']

pprStgAlt (con, params, use_mask, expr)
  = hang (hsep [ppr con, interppSP params, ptext SLIT("->")])
	 4 (ppr expr <> semi)

pprStgOp (StgPrimOp  op)   = ppr op
pprStgOp (StgFCallOp op _) = ppr op

instance Outputable AltType where
  ppr PolyAlt 	     = ptext SLIT("Polymorphic")
  ppr (UbxTupAlt tc) = ptext SLIT("UbxTup") <+> ppr tc
  ppr (AlgAlt tc)    = ptext SLIT("Alg")    <+> ppr tc
  ppr (PrimAlt tc)   = ptext SLIT("Prim")   <+> ppr tc
\end{code}

\begin{code}
pprStgLVs :: Outputable occ => GenStgLiveVars occ -> SDoc
pprStgLVs lvs
  = getPprStyle $ \ sty ->
    if userStyle sty || isEmptyUniqSet lvs then
	empty
    else
	hcat [text "{-lvs:", interpp'SP (uniqSetToList lvs), text "-}"]
\end{code}

\begin{code}
pprStgRhs :: (Outputable bndr, Outputable bdee, Ord bdee)
	  => GenStgRhs bndr bdee -> SDoc

-- special case
pprStgRhs (StgRhsClosure cc bi [free_var] upd_flag srt [{-no args-}] (StgApp func []))
  = hcat [ ppr cc,
	   pp_binder_info bi,
	   brackets (ifPprDebug (ppr free_var)),
	   ptext SLIT(" \\"), ppr upd_flag, pprMaybeSRT srt, ptext SLIT(" [] "), ppr func ]

-- general case
pprStgRhs (StgRhsClosure cc bi free_vars upd_flag srt args body)
  = hang (hsep [if opt_SccProfilingOn then ppr cc else empty,
		pp_binder_info bi,
		ifPprDebug (brackets (interppSP free_vars)),
		char '\\' <> ppr upd_flag, pprMaybeSRT srt, brackets (interppSP args)])
	 4 (ppr body)

pprStgRhs (StgRhsCon cc con args)
  = hcat [ ppr cc,
	   space, ppr con, ptext SLIT("! "), brackets (interppSP args)]

pprMaybeSRT (NoSRT) = empty
pprMaybeSRT srt     = ptext SLIT("srt:") <> pprSRT srt
\end{code}
