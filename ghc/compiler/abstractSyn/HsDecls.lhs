%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[HsDecls]{Abstract syntax: global declarations}

Definitions for: @FixityDecl@, @TyDecl@ and @ConDecl@, @ClassDecl@,
@InstDecl@, @DefaultDecl@.

\begin{code}
#include "HsVersions.h"

module HsDecls where

import HsBinds		( nullMonoBinds, ProtoNameMonoBinds(..),
			  MonoBinds, Sig
			)
import HsPat		( ProtoNamePat(..), RenamedPat(..), InPat )
import HsPragmas	( DataPragmas, TypePragmas, ClassPragmas,
			  InstancePragmas, ClassOpPragmas
			)
import HsTypes
import Id		( Id )
import Name		( Name )
import Outputable
import Pretty
import ProtoName	( cmpProtoName, ProtoName(..) ) -- .. for pragmas only
import SrcLoc		( SrcLoc )
import Unique		( Unique )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[FixityDecl]{A fixity declaration}
%*									*
%************************************************************************

These are only used in generating interfaces at the moment.  They are
not used in pretty-printing.

\begin{code}
data FixityDecl name
  = InfixL	    name Int
  | InfixR	    name Int
  | InfixN	    name Int

type ProtoNameFixityDecl = FixityDecl ProtoName
type RenamedFixityDecl   = FixityDecl Name
\end{code}

\begin{code}
instance (NamedThing name, Outputable name)
     => Outputable (FixityDecl name) where
    ppr sty (InfixL var prec)	= ppCat [ppStr "infixl", ppInt prec, pprOp sty var]
    ppr sty (InfixR var prec)	= ppCat [ppStr "infixr", ppInt prec, pprOp sty var]
    ppr sty (InfixN var prec)	= ppCat [ppStr "infix ", ppInt prec, pprOp sty var]
\end{code}

%************************************************************************
%*									*
\subsection[TyDecl]{An algebraic datatype or type-synonym declaration (plus @DataTypeSig@...)}
%*									*
%************************************************************************

\begin{code}
data TyDecl name
  = TyData	(Context name)	-- context (not used yet)
		name		-- type constructor
		[name]		-- type variables
		[ConDecl name]	-- data constructors
		[name]		-- derivings
		(DataPragmas name)
		SrcLoc

  | TySynonym	name		-- type constructor
		[name]		-- type variables
		(MonoType name)	-- synonym expansion
		TypePragmas
		SrcLoc

type ProtoNameTyDecl = TyDecl ProtoName
type RenamedTyDecl   = TyDecl Name
\end{code}

\begin{code}
instance (NamedThing name, Outputable name)
	      => Outputable (TyDecl name) where

    ppr sty (TyData context tycon tyvars condecls derivings pragmas src_loc)
     = ppAbove (ifPprShowAll sty (ppr sty src_loc)) -- ToDo: pragmas
	(ppHang (ppCat [ppStr "data", pprContext sty context, ppr sty tycon, interppSP sty tyvars])
	       4
	       (ppSep [
		ppr sty condecls,
		if (null derivings) then
		    ppNil
		else
		    ppBesides [ppStr "deriving (", interpp'SP sty derivings, ppStr ")"]]))

    ppr sty (TySynonym tycon tyvars mono_ty pragmas src_loc)
     = ppHang (ppCat [ppStr "type", ppr sty tycon, interppSP sty tyvars])
	      4 (ppCat [ppEquals, ppr sty mono_ty,
			ifPprShowAll sty (ppr sty src_loc)]) -- ToDo: pragmas
\end{code}

A type for recording what type synonyms the user wants treated as {\em
abstract} types.  It's called a ``Sig'' because it's sort of like a
``type signature'' for an synonym declaration.

Note: the Right Way to do this abstraction game is for the language to
support it.
\begin{code}
data DataTypeSig name
  = AbstractTypeSig name	-- tycon to abstract-ify
		    SrcLoc
  | SpecDataSig name		-- tycon to specialise
		(MonoType name)
		SrcLoc
		

type ProtoNameDataTypeSig = DataTypeSig ProtoName
type RenamedDataTypeSig   = DataTypeSig Name

instance (NamedThing name, Outputable name)
	      => Outputable (DataTypeSig name) where

    ppr sty (AbstractTypeSig tycon _)
      = ppCat [ppStr "{-# ABSTRACT", ppr sty tycon, ppStr "#-}"]

    ppr sty (SpecDataSig tycon ty _)
      = ppCat [ppStr "{-# SPECIALSIE data", ppr sty ty, ppStr "#-}"]
\end{code}

%************************************************************************
%*									*
\subsection[ConDecl]{A data-constructor declaration}
%*									*
%************************************************************************

A data constructor for an algebraic data type.

\begin{code}
data ConDecl name = ConDecl name [MonoType name] SrcLoc

type ProtoNameConDecl = ConDecl ProtoName
type RenamedConDecl   = ConDecl Name
\end{code}

In checking interfaces, we need to ``compare'' @ConDecls@.  Use with care!
\begin{code}
eqConDecls cons1 cons2
  = case (cmpList cmp cons1 cons2) of { EQ_ -> True; _ -> False }
  where
    cmp (ConDecl n1 tys1 _) (ConDecl n2 tys2 _)
      = case cmpProtoName n1 n2 of
	  EQ_ -> cmpList (cmpMonoType cmpProtoName) tys1 tys2
	  xxx -> xxx
\end{code}

\begin{code}
instance (NamedThing name, Outputable name) => Outputable (ConDecl name) where

    ppr sty (ConDecl con mono_tys src_loc)
      = ppCat [pprNonOp sty con,
	       ppInterleave ppNil (map (pprParendMonoType sty) mono_tys)]
\end{code}

%************************************************************************
%*									*
\subsection[ClassDecl]{A class declaration}
%*									*
%************************************************************************

\begin{code}
data ClassDecl name pat
  = ClassDecl	(Context name)	    	-- context...
		name		    	-- name of the class
		name		    	-- the class type variable
		[Sig name]		-- methods' signatures
		(MonoBinds name pat)	-- default methods
		(ClassPragmas name)
		SrcLoc

type ProtoNameClassDecl = ClassDecl ProtoName ProtoNamePat
type RenamedClassDecl   = ClassDecl Name      RenamedPat
\end{code}

\begin{code}
instance (NamedThing name, Outputable name,
	  NamedThing pat, Outputable pat)
		=> Outputable (ClassDecl name pat) where

    ppr sty (ClassDecl context clas tyvar sigs methods pragmas src_loc)
     = ppAboves [ppCat [ppStr "class", pprContext sty context, ppr sty clas,
			ppr sty tyvar, ppStr "where"],
			-- ToDo: really shouldn't print "where" unless there are sigs
		 ppNest 4 (ppAboves (map (ppr sty) sigs)),
		 ppNest 4 (ppr sty methods),
		 ppNest 4 (ppr sty pragmas)]
\end{code}

%************************************************************************
%*									*
\subsection[InstDecl]{An instance declaration (also, @SpecialisedInstanceSig@)}
%*									*
%************************************************************************

\begin{code}
data InstDecl name pat
  = InstDecl	(Context name)
		name		     -- class
		(MonoType name)
		(MonoBinds name pat)
		Bool	-- True <=> This instance decl is from the
			-- module being compiled; False <=> It is from
			-- an imported interface.

		FAST_STRING{-ModuleName-}
			-- The module where the instance decl
			-- originally came from; easy enough if it's
			-- the module being compiled; otherwise, the
			-- info comes from a pragma.

		FAST_STRING
			-- Name of the module who told us about this
			-- inst decl (the `informer') ToDo: listify???

		[Sig name]		-- actually user-supplied pragmatic info
		(InstancePragmas name)	-- interface-supplied pragmatic info
		SrcLoc

type ProtoNameInstDecl = InstDecl ProtoName ProtoNamePat
type RenamedInstDecl   = InstDecl Name      RenamedPat
\end{code}

\begin{code}
instance (NamedThing name, Outputable name,
	  NamedThing pat, Outputable pat)
	      => Outputable (InstDecl name pat) where

    ppr sty (InstDecl context clas ty binds local modname imod uprags pragmas src_loc)
      = let
	    top_matter = ppCat [ppStr "instance", pprContext sty context, ppr sty clas, ppr sty ty]
	in
	if nullMonoBinds binds && null uprags then
	    ppAbove top_matter (ppNest 4 (ppr sty pragmas))
	else
	    ppAboves [
	      ppCat [top_matter, ppStr "where"],
	      ppNest 4 (ppr sty uprags),
	      ppNest 4 (ppr sty binds),
	      ppNest 4 (ppr sty pragmas) ]
\end{code}

A type for recording what instances the user wants to specialise;
called a ``Sig'' because it's sort of like a ``type signature'' for an
instance.
\begin{code}
data SpecialisedInstanceSig name
  = InstSpecSig  name		    -- class
		 (MonoType name)    -- type to specialise to
		 SrcLoc

type ProtoNameSpecialisedInstanceSig = SpecialisedInstanceSig ProtoName
type RenamedSpecialisedInstanceSig   = SpecialisedInstanceSig Name

instance (NamedThing name, Outputable name)
	      => Outputable (SpecialisedInstanceSig name) where

    ppr sty (InstSpecSig clas ty _)
      = ppCat [ppStr "{-# SPECIALIZE instance", ppr sty clas, ppr sty ty, ppStr "#-}"]
\end{code}

%************************************************************************
%*									*
\subsection[DefaultDecl]{A @default@ declaration}
%*									*
%************************************************************************

There can only be one default declaration per module, but it is hard
for the parser to check that; we pass them all through in the abstract
syntax, and that restriction must be checked in the front end.

\begin{code}
data DefaultDecl name
  = DefaultDecl	[MonoType name]
		SrcLoc

type ProtoNameDefaultDecl = DefaultDecl ProtoName
type RenamedDefaultDecl   = DefaultDecl Name
\end{code}

\begin{code}
instance (NamedThing name, Outputable name)
	      => Outputable (DefaultDecl name) where

    ppr sty (DefaultDecl tys src_loc)
      = ppBesides [ppStr "default (", interpp'SP sty tys, ppStr ")"]
\end{code}
