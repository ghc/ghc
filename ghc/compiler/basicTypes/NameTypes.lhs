%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
%************************************************************************
%*									*
\section[NameTypes]{@NameTypes@: The flavours of names that we stick on things}
%*									*
%************************************************************************

\begin{code}
#include "HsVersions.h"

module NameTypes (
	ShortName, FullName,	-- abstract types
	Provenance(..),

	fromPrelude,

	mkShortName,

	mkFullName, mkPrivateFullName, mkPreludeCoreName,

	invisibleFullName,

	unlocaliseFullName, unlocaliseShortName,

#ifdef DPH
	isInventedFullName,
#endif {- Data Parallel Haskell -}

	-- and to make the interface self-sufficient....
	ExportFlag, Unique, SrcLoc
    ) where

import CLabelInfo	( identToC, cSEP )
import Outputable
import PrelFuns		( pRELUDE, pRELUDE_CORE ) -- NB: naughty import
import Pretty
import SrcLoc		( SrcLoc, mkBuiltinSrcLoc )
import Unique		( showUnique, Unique )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[NameTypes-flavours]{Datatypes for names}
%*									*
%************************************************************************

Here are the types; see the notes that follow.
\begin{code}
data ShortName
  = ShortName	    FAST_STRING -- entity's name in this module
		    SrcLoc	-- defining location (only one possible)

data FullName
  = FullName	    FAST_STRING	-- original module name
		    FAST_STRING	-- entity's name in original module
		    Provenance 	-- where this thing came from
				-- (also records its local name, if any)
		    ExportFlag	-- where this thing is going (from here)
		    Bool	-- True <=> invisible to the user
		    SrcLoc	-- defining location (just one)
\end{code}
(@FullNames@ don't have fast-comparison keys; the things with
@FullNames@ do.)

\begin{description}
%----------------------------------------------------------------------
\item[@ShortName@:]

These are used for entities local to the module being compiled; for
example, function parameters, where- and let-bound things.  These are
@TyVars@ (ToDo: what if imported???) and local @Ids@.  They have
@Uniques@ for fast comparison.

%----------------------------------------------------------------------
\item[@FullName@:]
These are used for things that either have, or may be required to
have, full-blown original names.  All @Classes@ and @TyCons@ have full
names.  All data-constructor and top-level @Ids@ (things that were
top-level in the original source) have fullnames.
\end{description}

%************************************************************************
%*									*
\subsection[NameTypes-Provenance]{Where a name(d thing) came from}
%*									*
%************************************************************************

The ``provenance'' of a name says something about where it came from.
This is used:
\begin{itemize}
\item
to decide whether to generate the code fragments for constructors
(only done for @ThisModule@).
\item
to detect when a thing is from @PreludeCore@, in which case we
use shorter target-code names.
\end{itemize}

\begin{code}
data Provenance
  = ThisModule

  | InventedInThisModule	-- for workers/wrappers, specialized
				-- versions, etc: anything "conjured up"
				-- on the compiler's initiative.

  | ExportedByPreludeCore	-- these are the immutable, unrenamable
				-- things the compiler knows about

  | OtherPrelude    FAST_STRING	-- the FullName gave the *original*
				-- name; this says what it was renamed
				-- to (if anything); really just for
				-- pretty-printing

  | OtherModule	    FAST_STRING	-- as for OtherPrelude, just the occurrence
				-- name
		    [FAST_STRING]-- The modules from whose interface we
				-- got the information about this thing

  | HereInPreludeCore		-- used when compiling PreludeCore bits:
				-- == ThisModule + ExportedByPreludeCore

  | OtherInstance		-- For imported instances.
		    FAST_STRING	-- The module where this instance supposedly
				-- was declared; "" if we don't know.
		    [FAST_STRING] -- The modules whose interface told us about
				-- this instance.
\end{code}

%************************************************************************
%*									*
\subsection[NameTypes-access-fns]{Access functions for names}
%*									*
%************************************************************************

Things to make 'em:
\begin{code}
mkShortName = ShortName

mkFullName m n p e l = FullName m n p e False{-not invisible-} l

mkPrivateFullName m n p e l = FullName m n p e True{-invisible-} l

mkPreludeCoreName mod name
  = FullName mod name ExportedByPreludeCore ExportAll False mkBuiltinSrcLoc
    -- Mark them as Exported; mkInterface may decide against it
    -- later.  (Easier than marking them NotExported, then later
    -- deciding it would be a good idea...)
\end{code}

\begin{code}
#ifdef DPH
isInventedFullName (FullName _ _ p _ _ _)
  = case p of
      InventedInThisModule -> True
      _			   -> False

#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
unlocaliseShortName :: FAST_STRING -> Unique -> ShortName -> FullName

{- We now elucidate Simon's favourite piece of code:

   When we are told to "unlocalise" a ShortName, we really really want
   the resulting monster to be unique (across the entire universe).
   We can't count on the module name being printed (for Prelude
   things, it isn't), so we brutally force the module-name into the
   regular-name component.

   We change the provenance to InventedInThisModule, because
   that's what it is.
-}
unlocaliseShortName mod u (ShortName nm loc)
  = FullName mod
	    (mod _APPEND_ nm _APPEND_ (showUnique u))
	    InventedInThisModule
	    ExportAll False loc

-- FullNames really can't be mangled; someone out there
-- *expects* the thing to have this name.
-- We only change the export status.

unlocaliseFullName (FullName m n p _ i l)
  = FullName m n p ExportAll i l
\end{code}

%************************************************************************
%*									*
\subsection[NameTypes-instances]{Instance declarations for various names}
%*									*
%************************************************************************

We don't have equality and ordering; that's defined for the things
that have @ShortNames@ and @FullNames@ in them.

\begin{code}
instance NamedThing ShortName where
    getExportFlag a		      = NotExported
    isLocallyDefined a		      = True
    getOrigName (ShortName s l)       = (panic "NamedThing.ShortName.getOrigName", s)
    getOccurrenceName (ShortName s l) = s
    getSrcLoc	(ShortName s l)       = l
    fromPreludeCore _		      = False
#ifdef DEBUG
    getTheUnique (ShortName s l)      = panic "NamedThing.ShortName.getTheUnique" 
    getInformingModules a	      = panic "NamedThing.ShortName.getInformingModule"
    hasType a			      = panic "NamedThing.ShortName.hasType"
    getType a			      = panic "NamedThing.ShortName.getType"
#endif
\end{code}

\begin{code}
instance NamedThing FullName where

    getExportFlag     (FullName m s p e i l) = e
    getOrigName	      (FullName m s p e i l) = (m, s)
    getSrcLoc	      (FullName m s p e i l) = l

    isLocallyDefined  (FullName m s p e i l)
      = case p of
	  ThisModule	       -> True
	  InventedInThisModule -> True
	  HereInPreludeCore    -> True
	  _		       -> False

    getOccurrenceName (FullName _ s p _ _ _)
      = case p of
	  OtherPrelude o   -> o
	  OtherModule  o _ -> o
	  _	           -> s

    fromPreludeCore (FullName _ _ p _ _ _)
      = case p of
	  ExportedByPreludeCore -> True
	  HereInPreludeCore	-> True
	  _			-> False

    getInformingModules (FullName _ _ p _ _ _)
      = case p of
	  ThisModule		-> []	-- Urgh.  ToDo
	  InventedInThisModule	-> []
	  OtherModule   _ ms	-> ms
	  OtherInstance _ ms	-> ms
	  ExportedByPreludeCore	-> [pRELUDE_CORE]
	  HereInPreludeCore	-> [pRELUDE_CORE]
	  OtherPrelude _	-> [pRELUDE]

#ifdef DEBUG
    getTheUnique = panic "NamedThing.FullName.getTheUnique"
    hasType = panic "NamedThing.FullName.hasType"
    getType = panic "NamedThing.FullName.getType"
#endif
\end{code}

A hack (ToDo?):
\begin{code}
fromPrelude :: FAST_STRING -> Bool

fromPrelude s = (_SUBSTR_ s 0 6 == SLIT("Prelude"))

invisibleFullName (FullName m s p e i l) = i
\end{code}

Forcing and printing:
\begin{code}
instance Outputable ShortName where
    ppr sty (ShortName s loc) = ppPStr s

instance Outputable FullName where
    ppr sty name@(FullName m s p e i l)
      = let pp_name =
	      ppBeside (if fromPreludeCore name
			then ppNil
			else case sty of
			      PprForUser     -> ppNil
			      PprDebug	     -> ppNil
			      PprInterface _ -> ppNil
			      PprUnfolding _ -> ppNil	-- ToDo: something diff later?
			      PprForC _ -> ppBeside (identToC m) (ppPStr cSEP)
			      PprForAsm _ False _ -> ppBeside (identToC m) (ppPStr cSEP)
			      PprForAsm _ True _ -> ppBesides [ppPStr cSEP, identToC m, ppPStr cSEP]
			      _	        -> ppBeside (ppPStr m) (ppChar '.'))
		       (if codeStyle sty
		        then identToC s
			else case sty of
			       PprInterface _ -> pp_local_name s p
			       PprForUser     -> pp_local_name s p
			       _	      -> ppPStr s)

	    pp_debug = ppBeside pp_name (pp_occur_name s p)
	in
        case sty of
	  PprShowAll 	 -> ppBesides [pp_debug, pp_exp e] -- (ppr sty loc)
	  PprDebug   	 -> pp_debug
	  PprUnfolding _ -> pp_debug
	  _	     	 -> pp_name
      where
	pp_exp NotExported = ppNil
	pp_exp ExportAll   = ppPStr SLIT("/EXP(..)")
	pp_exp ExportAbs   = ppPStr SLIT("/EXP")

-- little utility gizmos...
pp_occur_name, pp_local_name :: FAST_STRING -> Provenance -> Pretty

pp_occur_name s (OtherPrelude o)  | s /= o = ppBesides [ppChar '{', ppPStr o, ppChar '}']
pp_occur_name s (OtherModule o ms)| s /= o = ppBesides [ppChar '{', ppPStr o, ppChar '}']
	-- ToDo: print the "informant modules"?
pp_occur_name _ _			   = ppNil

pp_local_name s (OtherPrelude o)  | s /= o = ppPStr o
pp_local_name s (OtherModule o ms)| s /= o = ppPStr o
pp_local_name s _			   = ppPStr s
\end{code}
