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
import BasicTypes	( Fixity, NewOrData(..) )

-- others:
import Name		( getOccName, OccName, NamedThing(..) )
import Outputable	( interppSP, interpp'SP,
			  PprStyle(..), Outputable(..){-instance * []-}
			)
import Pretty
import SrcLoc		( SrcLoc )
import Util
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
#ifdef DEBUG
hsDeclName :: (NamedThing name, Outputable name, Outputable pat,
	       Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
	   => HsDecl tyvar uvar name pat -> name
#endif
hsDeclName (TyD (TyData _ _ name _ _ _ _ _))  	  = name
hsDeclName (TyD (TySynonym name _ _ _))       	  = name
hsDeclName (ClD (ClassDecl _ name _ _ _ _ _)) 	  = name
hsDeclName (SigD (IfaceSig name _ _ _))	      	  = name
hsDeclName (InstD (InstDecl _ _ _ (Just name) _)) = name
-- Others don't make sense
#ifdef DEBUG
hsDeclName x				      = pprPanic "HsDecls.hsDeclName" (ppr PprDebug x)
#endif
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

#ifdef DEBUG
instance (Ord3 name, Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar,
	  NamedThing name, Outputable name, Outputable pat) => 
	  Ord3 (HsDecl tyvar uvar name pat) where
#else
instance (Ord3 name) => Ord3 (HsDecl tyvar uvar name pat) where
#endif
  d1 `cmp` d2 = hsDeclName d1 `cmp` hsDeclName d2
\end{code}


%************************************************************************
%*									*
\subsection[FixityDecl]{A fixity declaration}
%*									*
%************************************************************************

\begin{code}
data FixityDecl name  = FixityDecl name Fixity SrcLoc

instance Outputable name => Outputable (FixityDecl name) where
  ppr sty (FixityDecl name fixity loc) = sep [ppr sty fixity, ppr sty name]
\end{code}


%************************************************************************
%*									*
\subsection[TyDecl]{@data@, @newtype@ or @type@ (synonym) type declaration}
%*									*
%************************************************************************

\begin{code}
data TyDecl name
  = TyData	NewOrData
		(Context name)	-- context
		name		-- type constructor
		[HsTyVar name]	-- type variables
		[ConDecl name]	-- data constructors (empty if abstract)
		(Maybe [name])	-- derivings; Nothing => not specified
				-- (i.e., derive default); Just [] => derive
				-- *nothing*; Just <list> => as you would
				-- expect...
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
      = hang (pp_decl_head sty SLIT("type") empty tycon tyvars)
	     4 (ppr sty mono_ty)

    ppr sty (TyData new_or_data context tycon tyvars condecls derivings pragmas src_loc)
      = pp_tydecl sty
		  (pp_decl_head sty keyword (pp_context_and_arrow sty context) tycon tyvars)
		  (pp_condecls sty condecls)
		  derivings
      where
	keyword = case new_or_data of
			NewType  -> SLIT("newtype")
			DataType -> SLIT("data")

pp_decl_head sty str pp_context tycon tyvars
  = hsep [ptext str, pp_context, ppr sty tycon,
	   interppSP sty tyvars, ptext SLIT("=")]

pp_condecls sty [] = empty		-- Curious!
pp_condecls sty (c:cs)
  = sep (ppr sty c : map (\ c -> (<>) (ptext SLIT("| ")) (ppr sty c)) cs)

pp_tydecl sty pp_head pp_decl_rhs derivings
  = hang pp_head 4 (sep [
	pp_decl_rhs,
	case (derivings, sty) of
	  (Nothing,_) 	   -> empty
	  (_,PprInterface) -> empty	-- No derivings in interfaces
	  (Just ds,_)	   -> hsep [ptext SLIT("deriving"), parens (interpp'SP sty ds)]
    ])

pp_context_and_arrow :: Outputable name => PprStyle -> Context name -> Doc
pp_context_and_arrow sty [] = empty
pp_context_and_arrow sty theta = hsep [pprContext sty theta, ptext SLIT("=>")]
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
      = hsep [text "{-# SPECIALIZE data", ppr sty ty, text "#-}"]
\end{code}

%************************************************************************
%*									*
\subsection[ConDecl]{A data-constructor declaration}
%*									*
%************************************************************************

\begin{code}
data ConDecl name
  = ConDecl 	name			-- Constructor name
		(Context name)		-- Existential context for this constructor
		(ConDetails name)
		SrcLoc

data ConDetails name
  = VanillaCon			-- prefix-style con decl
		[BangType name]

  | InfixCon			-- infix-style con decl
		(BangType name)
		(BangType name)

  | RecCon			-- record-style con decl
		[([name], BangType name)]	-- list of "fields"

  | NewCon	 		-- newtype con decl
		(HsType name)

data BangType name
  = Banged   (HsType name)	-- HsType: to allow Haskell extensions
  | Unbanged (HsType name)	-- (MonoType only needed for straight Haskell)
\end{code}

\begin{code}
instance (NamedThing name, Outputable name) => Outputable (ConDecl name) where
    ppr sty (ConDecl con cxt con_details  loc)
      = pp_context_and_arrow sty cxt <+> ppr_con_details sty con con_details

ppr_con_details sty con (InfixCon ty1 ty2)
  = hsep [ppr_bang sty ty1, ppr sty con, ppr_bang sty ty2]

ppr_con_details sty con (VanillaCon tys)
  = ppr sty con <+> hsep (map (ppr_bang sty) tys)

ppr_con_details sty con (NewCon ty)
  = ppr sty con <+> pprParendHsType sty ty

ppr_con_details sty con (RecCon fields)
  = ppr sty con <+> braces (hsep (punctuate comma (map ppr_field fields)))
  where
    ppr_field (ns, ty) = hsep (map (ppr sty) ns) <+> 
			 ptext SLIT("::") <+>
			 ppr_bang sty ty

ppr_bang sty (Banged   ty) = ptext SLIT("!") <> pprParendHsType sty ty
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

      | otherwise	-- Laid out
      = sep [hsep [top_matter, ptext SLIT("where {")],
	       nest 4 (vcat [sep (map ppr_sig sigs),
				   ppr sty methods,
				   char '}'])]
      where
        top_matter = hsep [ptext SLIT("class"), pp_context_and_arrow sty context,
                            ppr sty clas, ppr sty tyvar]
	ppr_sig sig = ppr sty sig <> semi
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
      = hsep [ptext SLIT("instance"), ppr sty inst_ty]

      | otherwise
      =	vcat [hsep [ptext SLIT("instance"), ppr sty inst_ty, ptext SLIT("where")],
	          nest 4 (ppr sty uprags),
	          nest 4 (ppr sty binds) ]
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
      = hsep [text "{-# SPECIALIZE instance", ppr sty clas, ppr sty ty, text "#-}"]
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
      = (<>) (ptext SLIT("default ")) (parens (interpp'SP sty tys))
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
      = hang (hsep [ppr sty var, ptext SLIT("::")])
	     4 (ppr sty ty)

data HsIdInfo name
  = HsArity		ArityInfo
  | HsStrictness	(HsStrictnessInfo name)
  | HsUnfold		Bool (UfExpr name)	-- True <=> INLINE pragma
  | HsUpdate		UpdateInfo
  | HsArgUsage		ArgUsageInfo
  | HsFBType		FBTypeInfo
	-- ToDo: specialisations

data HsStrictnessInfo name
  = HsStrictnessInfo [Demand] 
		     (Maybe (name, [name]))	-- Worker, if any
						-- and needed constructors
  | HsBottom
\end{code}
