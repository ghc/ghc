%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsDecls]{Abstract syntax: global declarations}

Definitions for: @FixityDecl@, @TyDecl@ and @ConDecl@, @ClassDecl@,
@InstDecl@, @DefaultDecl@.

\begin{code}
module HsDecls where

#include "HsVersions.h"

-- friends:
import HsBinds		( HsBinds, MonoBinds, Sig, nullMonoBinds )
import HsPragmas	( DataPragmas, ClassPragmas )
import HsTypes
import HsCore		( UfExpr )
import BasicTypes	( Fixity, NewOrData(..) )
import IdInfo		( ArgUsageInfo, FBTypeInfo, ArityInfo, UpdateInfo )
import Demand		( Demand )

-- others:
import Name		( getOccName, OccName, NamedThing(..) )
import Outputable	
import SrcLoc		( SrcLoc )
import Util
\end{code}


%************************************************************************
%*									*
\subsection[HsDecl]{Declarations}
%*									*
%************************************************************************

\begin{code}
data HsDecl flexi name pat
  = TyD		(TyDecl name)
  | ClD		(ClassDecl flexi name pat)
  | InstD	(InstDecl  flexi name pat)
  | DefD	(DefaultDecl name)
  | ValD	(HsBinds flexi name pat)
  | SigD	(IfaceSig name)
\end{code}

\begin{code}
#ifdef DEBUG
hsDeclName :: (NamedThing name, Outputable name, Outputable pat)
	   => HsDecl flexi name pat -> name
#endif
hsDeclName (TyD (TyData _ _ name _ _ _ _ _))  	  = name
hsDeclName (TyD (TySynonym name _ _ _))       	  = name
hsDeclName (ClD (ClassDecl _ name _ _ _ _ _ _ _)) = name
hsDeclName (SigD (IfaceSig name _ _ _))	      	  = name
hsDeclName (InstD (InstDecl _ _ _ (Just name) _)) = name
-- Others don't make sense
#ifdef DEBUG
hsDeclName x				      = pprPanic "HsDecls.hsDeclName" (ppr x)
#endif
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat)
	=> Outputable (HsDecl flexi name pat) where

    ppr (TyD td)     = ppr td
    ppr (ClD cd)     = ppr cd
    ppr (SigD sig)   = ppr sig
    ppr (ValD binds) = ppr binds
    ppr (DefD def)   = ppr def
    ppr (InstD inst) = ppr inst

#ifdef DEBUG
-- hsDeclName needs more context when DEBUG is on
instance (NamedThing name, Outputable name, Outputable pat, Eq name)
      => Eq (HsDecl flex name pat) where
   d1 == d2 = hsDeclName d1 == hsDeclName d2
	
instance (NamedThing name, Outputable name, Outputable pat, Ord name)
      => Ord (HsDecl flex name pat) where
	d1 `compare` d2 = hsDeclName d1 `compare` hsDeclName d2
#else
instance (Eq name) => Eq (HsDecl flex name pat) where
	d1 == d2 = hsDeclName d1 == hsDeclName d2
	
instance (Ord name) => Ord (HsDecl flexi name pat) where
	d1 `compare` d2 = hsDeclName d1 `compare` hsDeclName d2
#endif
\end{code}


%************************************************************************
%*									*
\subsection[FixityDecl]{A fixity declaration}
%*									*
%************************************************************************

\begin{code}
data FixityDecl name  = FixityDecl name Fixity SrcLoc

instance Outputable name => Outputable (FixityDecl name) where
  ppr (FixityDecl name fixity loc) = sep [ppr fixity, ppr name]
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

    ppr (TySynonym tycon tyvars mono_ty src_loc)
      = hang (pp_decl_head SLIT("type") empty tycon tyvars)
	     4 (ppr mono_ty)

    ppr (TyData new_or_data context tycon tyvars condecls derivings pragmas src_loc)
      = pp_tydecl
		  (pp_decl_head keyword (pp_context_and_arrow context) tycon tyvars)
		  (pp_condecls condecls)
		  derivings
      where
	keyword = case new_or_data of
			NewType  -> SLIT("newtype")
			DataType -> SLIT("data")

pp_decl_head str pp_context tycon tyvars
  = hsep [ptext str, pp_context, ppr tycon,
	   interppSP tyvars, ptext SLIT("=")]

pp_condecls [] = empty		-- Curious!
pp_condecls (c:cs)
  = sep (ppr c : map (\ c -> (<>) (ptext SLIT("| ")) (ppr c)) cs)

pp_tydecl pp_head pp_decl_rhs derivings
  = hang pp_head 4 (sep [
	pp_decl_rhs,
	case derivings of
	  Nothing 	   -> empty
	  Just ds	   -> hsep [ptext SLIT("deriving"), parens (interpp'SP ds)]
    ])

pp_context_and_arrow :: Outputable name => Context name -> SDoc
pp_context_and_arrow [] = empty
pp_context_and_arrow theta = hsep [pprContext theta, ptext SLIT("=>")]
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

    ppr (SpecDataSig tycon ty _)
      = hsep [text "{-# SPECIALIZE data", ppr ty, text "#-}"]
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
    ppr (ConDecl con cxt con_details  loc)
      = pp_context_and_arrow cxt <+> ppr_con_details con con_details

ppr_con_details con (InfixCon ty1 ty2)
  = hsep [ppr_bang ty1, ppr con, ppr_bang ty2]

ppr_con_details con (VanillaCon tys)
  = ppr con <+> hsep (map (ppr_bang) tys)

ppr_con_details con (NewCon ty)
  = ppr con <+> pprParendHsType ty

ppr_con_details con (RecCon fields)
  = ppr con <+> braces (hsep (punctuate comma (map ppr_field fields)))
  where
    ppr_field (ns, ty) = hsep (map (ppr) ns) <+> 
			 ptext SLIT("::") <+>
			 ppr_bang ty

ppr_bang (Banged   ty) = ptext SLIT("!") <> pprParendHsType ty
ppr_bang (Unbanged ty) = pprParendHsType ty
\end{code}

%************************************************************************
%*									*
\subsection[ClassDecl]{A class declaration}
%*									*
%************************************************************************

\begin{code}
data ClassDecl flexi name pat
  = ClassDecl	(Context name)	    		-- context...
		name		    		-- name of the class
		[HsTyVar name]	    		-- the class type variables
		[Sig name]			-- methods' signatures
		(MonoBinds flexi name pat)	-- default methods
		(ClassPragmas name)
		name name			-- The names of the tycon and datacon for this class
						-- These are filled in by the renamer
		SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat)
		=> Outputable (ClassDecl flexi name pat) where

    ppr (ClassDecl context clas tyvars sigs methods pragmas _ _ src_loc)
      | null sigs	-- No "where" part
      = top_matter

      | otherwise	-- Laid out
      = sep [hsep [top_matter, ptext SLIT("where {")],
	       nest 4 (vcat [sep (map ppr_sig sigs),
				   ppr methods,
				   char '}'])]
      where
        top_matter = hsep [ptext SLIT("class"), pp_context_and_arrow context,
                            ppr clas, hsep (map (ppr) tyvars)]
	ppr_sig sig = ppr sig <> semi
\end{code}

%************************************************************************
%*									*
\subsection[InstDecl]{An instance declaration
%*									*
%************************************************************************

\begin{code}
data InstDecl flexi name pat
  = InstDecl	(HsType name)	-- Context => Class Instance-type
				-- Using a polytype means that the renamer conveniently
				-- figures out the quantified type variables for us.

		(MonoBinds flexi name pat)

		[Sig name]		-- User-supplied pragmatic info

		(Maybe name)		-- Name for the dictionary function

		SrcLoc
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat)
	      => Outputable (InstDecl flexi name pat) where

    ppr (InstDecl inst_ty binds uprags dfun_name src_loc)
      = getPprStyle $ \ sty ->
        if ifaceStyle sty || (nullMonoBinds binds && null uprags) then
           hsep [ptext SLIT("instance"), ppr inst_ty]
	else
	   vcat [hsep [ptext SLIT("instance"), ppr inst_ty, ptext SLIT("where")],
	         nest 4 (ppr uprags),
	         nest 4 (ppr binds) ]
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

    ppr (DefaultDecl tys src_loc)
      = ptext SLIT("default") <+> parens (interpp'SP tys)
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
    ppr (IfaceSig var ty _ _)
      = hang (hsep [ppr var, ptext SLIT("::")])
	     4 (ppr ty)

data HsIdInfo name
  = HsArity		ArityInfo
  | HsStrictness	(HsStrictnessInfo name)
  | HsUnfold		Bool (UfExpr name)	-- True <=> INLINE pragma
  | HsUpdate		UpdateInfo
  | HsArgUsage		ArgUsageInfo
  | HsFBType		FBTypeInfo
  | HsSpecialise	[HsTyVar name] [HsType name] (UfExpr name)


data HsStrictnessInfo name
  = HsStrictnessInfo [Demand] 
		     (Maybe (name, [name]))	-- Worker, if any
						-- and needed constructors
  | HsBottom
\end{code}
