%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsDecls]{Abstract syntax: global declarations}

Definitions for: @FixityDecl@, @TyDecl@ and @ConDecl@, @ClassDecl@,
@InstDecl@, @DefaultDecl@.

\begin{code}
#include "HsVersions.h"

module HsDecls where

IMP_Ubiq()

-- friends:
IMPORT_DELOOPER(HsLoop)		( nullMonoBinds, MonoBinds, Sig )
import HsPragmas	( DataPragmas, ClassPragmas,
			  InstancePragmas, ClassOpPragmas
			)
import HsTypes

-- others:
import Name		( pprSym, pprNonSym )
import Outputable	( interppSP, interpp'SP,
			  Outputable(..){-instance * []-}
			)
import Pretty
import SrcLoc		( SrcLoc )
import Util		( panic#{-ToDo:rm eventually-} )
\end{code}

%************************************************************************
%*									*
\subsection[FixityDecl]{A fixity declaration}
%*									*
%************************************************************************

\begin{code}
data FixityDecl name
  = InfixL	name Int
  | InfixR	name Int
  | InfixN	name Int
\end{code}

\begin{code}
instance (NamedThing name, Outputable name)
     => Outputable (FixityDecl name) where
    ppr sty (InfixL var prec)	= print_it sty "l" prec var
    ppr sty (InfixR var prec)	= print_it sty "r" prec var
    ppr sty (InfixN var prec)	= print_it sty ""  prec var

print_it sty suff prec var
  = ppBesides [ppStr "infix", ppStr suff, ppSP, ppInt prec, ppSP, pprSym sty var]
\end{code}

%************************************************************************
%*									*
\subsection[TyDecl]{@data@, @newtype@ or @type@ (synonym) type declaration}
%*									*
%************************************************************************

\begin{code}
data TyDecl name
  = TyData	(Context name)	-- context
		name		-- type constructor
		[name]		-- type variables
		[ConDecl name]	-- data constructors (empty if abstract)
		(Maybe [name])	-- derivings; Nothing => not specified
				-- (i.e., derive default); Just [] => derive
				-- *nothing*; Just <list> => as you would
				-- expect...
		(DataPragmas name)
		SrcLoc

  | TyNew	(Context name)	-- context
		name		-- type constructor
		[name]		-- type variables
		[ConDecl name]	-- data constructor (empty if abstract)
		(Maybe [name])	-- derivings; as above
		(DataPragmas name)
		SrcLoc

  | TySynonym	name		-- type constructor
		[name]		-- type variables
		(MonoType name)	-- synonym expansion
		SrcLoc

\end{code}

\begin{code}
instance (NamedThing name, Outputable name)
	      => Outputable (TyDecl name) where

    ppr sty (TySynonym tycon tyvars mono_ty src_loc)
      = ppHang (pp_decl_head sty SLIT("type") ppNil tycon tyvars)
	     4 (ppCat [ppEquals, ppr sty mono_ty])

    ppr sty (TyData context tycon tyvars condecls derivings pragmas src_loc)
      = pp_tydecl sty
		  (pp_decl_head sty SLIT("data") (pprContext sty context) tycon tyvars)
		  (pp_condecls sty condecls)
		  derivings

    ppr sty (TyNew context tycon tyvars condecl derivings pragmas src_loc)
      = pp_tydecl sty
		  (pp_decl_head sty SLIT("newtype") (pprContext sty context) tycon tyvars)
		  (pp_condecls sty condecl)
		  derivings

pp_decl_head sty str pp_context tycon tyvars
  = ppCat [ppPStr str, pp_context, ppr sty tycon, interppSP sty tyvars]

pp_condecls sty [] = ppNil -- abstract datatype
pp_condecls sty (c:cs)
  = ppSep (ppBeside (ppStr "= ") (ppr sty c)
	   : map (\ x -> ppBeside (ppStr "| ") (ppr sty x)) cs)

pp_tydecl sty pp_head pp_decl_rhs derivings
  = ppHang pp_head 4 (ppSep [
	pp_decl_rhs,
	case derivings of
	  Nothing -> ppNil
	  Just ds -> ppBeside (ppPStr SLIT("deriving "))
			(ppParens (ppInterleave ppComma (map (ppr sty) ds)))])
\end{code}

A type for recording what types a datatype should be specialised to.
It's called a ``Sig'' because it's sort of like a ``type signature''
for an datatype declaration.

\begin{code}
data SpecDataSig name
  = SpecDataSig name		-- tycon to specialise
		(MonoType name)
		SrcLoc

instance (NamedThing name, Outputable name)
	      => Outputable (SpecDataSig name) where

    ppr sty (SpecDataSig tycon ty _)
      = ppCat [ppStr "{-# SPECIALIZE data", ppr sty ty, ppStr "#-}"]
\end{code}

%************************************************************************
%*									*
\subsection[ConDecl]{A data-constructor declaration}
%*									*
%************************************************************************

\begin{code}
data ConDecl name
  = ConDecl	name		-- prefix-style con decl
		[BangType name]
		SrcLoc

  | ConOpDecl	(BangType name)	-- infix-style con decl
		name
		(BangType name)
		SrcLoc

  | RecConDecl	name
		[([name], BangType name)]	-- list of "fields"
		SrcLoc

  | NewConDecl  name		-- newtype con decl
		(MonoType name)
		SrcLoc

data BangType name
  = Banged   (PolyType name)	-- PolyType: to allow Haskell extensions
  | Unbanged (PolyType name)	-- (MonoType only needed for straight Haskell)
\end{code}

\begin{code}
instance (NamedThing name, Outputable name) => Outputable (ConDecl name) where

    ppr sty (ConDecl con tys _)
      = ppCat [pprNonSym sty con, ppInterleave ppNil (map (ppr_bang sty) tys)]
    ppr sty (ConOpDecl ty1 op ty2 _)
      = ppCat [ppr_bang sty ty1, pprSym sty op, ppr_bang sty ty2]
    ppr sty (NewConDecl con ty _)
      = ppCat [pprNonSym sty con, pprParendMonoType sty ty]
    ppr sty (RecConDecl con fields _)
      = ppCat [pprNonSym sty con, ppChar '{',
	       ppInterleave pp'SP (map pp_field fields), ppChar '}']
      where
	pp_field (n, ty) = ppCat [ppr sty n, ppPStr SLIT("::"), ppr_bang sty ty]

ppr_bang sty (Banged   ty) = ppBeside (ppChar '!') (pprParendPolyType sty ty)
ppr_bang sty (Unbanged ty) = pprParendPolyType sty ty
\end{code}

%************************************************************************
%*									*
\subsection[ClassDecl]{A class declaration}
%*									*
%************************************************************************

\begin{code}
data ClassDecl tyvar uvar name pat
  = ClassDecl	(Context name)	    		-- context...
		name		    		-- name of the class
		name		    		-- the class type variable
		[Sig name]			-- methods' signatures
		(MonoBinds tyvar uvar name pat)	-- default methods
		(ClassPragmas name)
		SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
		=> Outputable (ClassDecl tyvar uvar name pat) where

    ppr sty (ClassDecl context clas tyvar sigs methods pragmas src_loc)
     = let 
           top_matter = ppCat [ppStr "class", pprContext sty context,
                               ppr sty clas, ppr sty tyvar]
       in
       if null sigs && nullMonoBinds methods then
	   ppAbove top_matter (ppNest 4 (ppr sty pragmas))
       else
	   ppAboves [ppCat [top_matter, ppStr "where"],
		     ppNest 4 (ppAboves (map (ppr sty) sigs)),
		     ppNest 4 (ppr sty methods),
		     ppNest 4 (ppr sty pragmas) ]
\end{code}

%************************************************************************
%*									*
\subsection[InstDecl]{An instance declaration (also, @SpecInstSig@)}
%*									*
%************************************************************************

\begin{code}
data InstDecl tyvar uvar name pat
  = InstDecl	name		-- Class

		(PolyType name)	-- Context => Instance-type
				-- Using a polytype means that the renamer conveniently
				-- figures out the quantified type variables for us.

		(MonoBinds tyvar uvar name pat)

		Bool		-- True <=> This instance decl is from the
				-- module being compiled; False <=> It is from
				-- an imported interface.

		(Maybe Module)	-- The name of the module where the instance decl
				-- originally came from; Nothing => Prelude

		[Sig name]		-- actually user-supplied pragmatic info
		(InstancePragmas name)	-- interface-supplied pragmatic info
		SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
	      => Outputable (InstDecl tyvar uvar name pat) where

    ppr sty (InstDecl clas ty binds local modname uprags pragmas src_loc)
      = let
	    (context, inst_ty)
	      = case ty of
		  HsPreForAllTy c t -> (c, t)
		  HsForAllTy  _ c t -> (c, t)

	    top_matter = ppCat [ppStr "instance", pprContext sty context,
			        ppr sty clas, pprParendMonoType sty inst_ty]
	in
	if nullMonoBinds binds && null uprags then
	    ppAbove top_matter (ppNest 4 (ppr sty pragmas))
	else
	    ppAboves [ppCat [top_matter, ppStr "where"],
	              if null uprags then ppNil else ppNest 4 (ppr sty uprags),
	              ppNest 4 (ppr sty binds),
	              ppNest 4 (ppr sty pragmas) ]
\end{code}

A type for recording what instances the user wants to specialise;
called a ``Sig'' because it's sort of like a ``type signature'' for an
instance.
\begin{code}
data SpecInstSig name
  = SpecInstSig  name		    -- class
		 (MonoType name)    -- type to specialise to
		 SrcLoc

instance (NamedThing name, Outputable name)
	      => Outputable (SpecInstSig name) where

    ppr sty (SpecInstSig clas ty _)
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

instance (NamedThing name, Outputable name)
	      => Outputable (DefaultDecl name) where

    ppr sty (DefaultDecl tys src_loc)
      = ppBeside (ppPStr SLIT("default ")) (ppParens (interpp'SP sty tys))
\end{code}
