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
import HsBinds		( HsBinds, MonoBinds, Sig, nullMonoBinds )
import HsPragmas	( DataPragmas, ClassPragmas,
			  InstancePragmas, ClassOpPragmas
			)
import HsTypes
import IdInfo
import SpecEnv		( SpecEnv )
import HsCore		( UfExpr )
import HsBasic		( Fixity )

-- others:
import Name		( pprSym, pprNonSym, getOccName, OccName )
import Outputable	( interppSP, interpp'SP,
			  Outputable(..){-instance * []-}
			)
import Pretty
import SrcLoc		( SrcLoc )
import PprStyle		( PprStyle(..), ifaceStyle )
\end{code}


%************************************************************************
%*									*
\subsection[HsDecl]{Declarations}
%*									*
%************************************************************************

\begin{code}
data HsDecl tyvar uvar name pat
  = TyD		(TyDecl name)
  | ClD		(ClassDecl tyvar uvar name pat)
  | InstD	(InstDecl  tyvar uvar name pat)
  | DefD	(DefaultDecl name)
  | ValD	(HsBinds tyvar uvar name pat)
  | SigD	(IfaceSig name)
\end{code}

\begin{code}
hsDeclName (TyD (TyData _ name _ _ _ _ _))    = name
hsDeclName (TyD (TyNew  _ name _ _ _ _ _))    = name
hsDeclName (TyD (TySynonym name _ _ _))       = name
hsDeclName (ClD (ClassDecl _ name _ _ _ _ _)) = name
hsDeclName (SigD (IfaceSig name _ _ _))	      = name
-- Others don't make sense
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
	=> Outputable (HsDecl tyvar uvar name pat) where

    ppr sty (TyD td)     = ppr sty td
    ppr sty (ClD cd)     = ppr sty cd
    ppr sty (SigD sig)   = ppr sty sig
    ppr sty (ValD binds) = ppr sty binds
    ppr sty (DefD def)   = ppr sty def
    ppr sty (InstD inst) = ppr sty inst

-- In interfaces, top-level binders are printed without their "Module." prefix
ppr_top_binder sty bndr | ifaceStyle sty = ppr sty (getOccName bndr)
	  	        | otherwise	 = ppr sty bndr
\end{code}


%************************************************************************
%*									*
\subsection[FixityDecl]{A fixity declaration}
%*									*
%************************************************************************

\begin{code}
data FixityDecl name  = FixityDecl name Fixity SrcLoc

instance Outputable name => Outputable (FixityDecl name) where
  ppr sty (FixityDecl name fixity loc) = ppSep [ppr sty fixity, ppr sty name]
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
		[HsTyVar name]	-- type variables
		[ConDecl name]	-- data constructors (empty if abstract)
		(Maybe [name])	-- derivings; Nothing => not specified
				-- (i.e., derive default); Just [] => derive
				-- *nothing*; Just <list> => as you would
				-- expect...
		(DataPragmas name)
		SrcLoc

  | TyNew	(Context name)	-- context
		name		-- type constructor
		[HsTyVar name]	-- type variables
		(ConDecl name)	-- data constructor
		(Maybe [name])	-- derivings; as above
		(DataPragmas name)
		SrcLoc

  | TySynonym	name		-- type constructor
		[HsTyVar name]	-- type variables
		(HsType name)	-- synonym expansion
		SrcLoc

\end{code}

\begin{code}
instance (NamedThing name, Outputable name)
	      => Outputable (TyDecl name) where

    ppr sty (TySynonym tycon tyvars mono_ty src_loc)
      = ppHang (pp_decl_head sty SLIT("type") ppNil tycon tyvars)
	     4 (ppr sty mono_ty)

    ppr sty (TyData context tycon tyvars condecls derivings pragmas src_loc)
      = pp_tydecl sty
		  (pp_decl_head sty SLIT("data") (pp_context_and_arrow sty context) tycon tyvars)
		  (pp_condecls sty condecls)
		  derivings

    ppr sty (TyNew context tycon tyvars condecl derivings pragmas src_loc)
      = pp_tydecl sty
		  (pp_decl_head sty SLIT("newtype") (pp_context_and_arrow sty context) tycon tyvars)
		  (ppr sty condecl)
		  derivings

pp_decl_head sty str pp_context tycon tyvars
  = ppCat [ppPStr str, pp_context, ppr_top_binder sty tycon,
	   interppSP sty tyvars, ppPStr SLIT("=")]

pp_condecls sty [] = ppNil		-- Curious!
pp_condecls sty (c:cs)
  = ppSep (ppr sty c : map (\ c -> ppBeside (ppPStr SLIT("| ")) (ppr sty c)) cs)

pp_tydecl sty pp_head pp_decl_rhs derivings
  = ppHang pp_head 4 (ppSep [
	pp_decl_rhs,
	case (derivings, sty) of
	  (Nothing,_) 	   -> ppNil
	  (_,PprInterface) -> ppNil	-- No derivings in interfaces
	  (Just ds,_)	   -> ppCat [ppPStr SLIT("deriving"), ppParens (interpp'SP sty ds)]
    ])

pp_context_and_arrow :: Outputable name => PprStyle -> Context name -> Pretty
pp_context_and_arrow sty [] = ppNil
pp_context_and_arrow sty theta = ppCat [pprContext sty theta, ppPStr SLIT("=>")]
\end{code}

A type for recording what types a datatype should be specialised to.
It's called a ``Sig'' because it's sort of like a ``type signature''
for an datatype declaration.

\begin{code}
data SpecDataSig name
  = SpecDataSig name		-- tycon to specialise
		(HsType name)
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
		(HsType name)
		SrcLoc

data BangType name
  = Banged   (HsType name)	-- HsType: to allow Haskell extensions
  | Unbanged (HsType name)	-- (MonoType only needed for straight Haskell)
\end{code}

\begin{code}
instance (NamedThing name, Outputable name) => Outputable (ConDecl name) where

    ppr sty (ConDecl con tys _)
      = ppCat [ppr_top_binder sty con, ppInterleave ppNil (map (ppr_bang sty) tys)]

	-- We print ConOpDecls in prefix form in interface files
    ppr sty (ConOpDecl ty1 op ty2 _)
      | ifaceStyle sty
      = ppCat [ppr_top_binder sty op, ppr_bang sty ty1, ppr_bang sty ty2]
      | otherwise
      = ppCat [ppr_bang sty ty1, ppr_top_binder sty op, ppr_bang sty ty2]

    ppr sty (NewConDecl con ty _)
      = ppCat [ppr_top_binder sty con, pprParendHsType sty ty]
    ppr sty (RecConDecl con fields _)
      = ppCat [ppr_top_binder sty con,
	       ppCurlies (ppInterleave pp'SP (map pp_field fields))
	      ]
      where
	pp_field (ns, ty) = ppCat [ppCat (map (ppr_top_binder sty) ns), 
				   ppPStr SLIT("::"), ppr_bang sty ty]

ppr_bang sty (Banged   ty) = ppBeside (ppPStr SLIT("! ")) (pprParendHsType sty ty)
				-- The extra space helps the lexical analyser that lexes
				-- interface files; it doesn't make the rigid operator/identifier
				-- distinction, so "!a" is a valid identifier so far as it is concerned
ppr_bang sty (Unbanged ty) = pprParendHsType sty ty
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
		(HsTyVar name)	    		-- the class type variable
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
      | null sigs	-- No "where" part
      = top_matter

      | iface_style	-- All on one line (for now at least)
      = ppCat [top_matter, ppPStr SLIT("where"), 
	       ppCurlies (ppInterleave (ppPStr SLIT("; ")) pp_sigs)]

      | otherwise	-- Laid out
      = ppSep [ppCat [top_matter, ppPStr SLIT("where {")],
	       ppNest 4 ((ppIntersperse ppSemi pp_sigs `ppAbove` pp_methods)
			 `ppBeside` ppChar '}')]
      where
        top_matter = ppCat [ppPStr SLIT("class"), pp_context_and_arrow sty context,
                            ppr_top_binder sty clas, ppr sty tyvar]
	pp_sigs     = map (ppr sty) sigs 
	pp_methods  = ppr sty methods
	iface_style = case sty of {PprInterface -> True; other -> False}
\end{code}

%************************************************************************
%*									*
\subsection[InstDecl]{An instance declaration (also, @SpecInstSig@)}
%*									*
%************************************************************************

\begin{code}
data InstDecl tyvar uvar name pat
  = InstDecl	(HsType name)	-- Context => Class Instance-type
				-- Using a polytype means that the renamer conveniently
				-- figures out the quantified type variables for us.

		(MonoBinds tyvar uvar name pat)

		[Sig name]		-- User-supplied pragmatic info

		(Maybe name)		-- Name for the dictionary function

		SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
	      => Outputable (InstDecl tyvar uvar name pat) where

    ppr sty (InstDecl inst_ty binds uprags dfun_name src_loc)
      | case sty of { PprInterface -> True; other -> False} ||
	nullMonoBinds binds && null uprags
      = ppCat [ppPStr SLIT("instance"), ppr sty inst_ty]

      | otherwise
      =	ppAboves [ppCat [ppPStr SLIT("instance"), ppr sty inst_ty, ppPStr SLIT("where")],
	          ppNest 4 (ppr sty uprags),
	          ppNest 4 (ppr sty binds) ]
\end{code}

A type for recording what instances the user wants to specialise;
called a ``Sig'' because it's sort of like a ``type signature'' for an
instance.
\begin{code}
data SpecInstSig name
  = SpecInstSig  name		    -- class
		 (HsType name)    -- type to specialise to
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
  = DefaultDecl	[HsType name]
		SrcLoc

instance (NamedThing name, Outputable name)
	      => Outputable (DefaultDecl name) where

    ppr sty (DefaultDecl tys src_loc)
      = ppBeside (ppPStr SLIT("default ")) (ppParens (interpp'SP sty tys))
\end{code}

%************************************************************************
%*									*
\subsection{Signatures in interface files}
%*									*
%************************************************************************

\begin{code}
data IfaceSig name
  = IfaceSig	name
		(HsType name)
		[HsIdInfo name]
		SrcLoc

instance (NamedThing name, Outputable name) => Outputable (IfaceSig name) where
    ppr sty (IfaceSig var ty _ _)
      = ppHang (ppCat [ppr_top_binder sty var, ppPStr SLIT("::")])
	     4 (ppr sty ty)

data HsIdInfo name
  = HsArity		ArityInfo
  | HsStrictness	(StrictnessInfo name)
  | HsUnfold		(UfExpr name)
  | HsUpdate		UpdateInfo
  | HsDeforest		DeforestInfo
  | HsArgUsage		ArgUsageInfo
  | HsFBType		FBTypeInfo
	-- ToDo: specialisations
\end{code}
