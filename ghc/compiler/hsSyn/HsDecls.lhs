%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsDecls]{Abstract syntax: global declarations}

Definitions for: @TyDecl@ and @ConDecl@, @ClassDecl@,
@InstDecl@, @DefaultDecl@ and @ForeignDecl@.

\begin{code}
module HsDecls (
	HsDecl(..), TyClDecl(..), InstDecl(..),
	DefaultDecl(..), ForeignDecl(..), ForKind(..),
	ExtName(..), isDynamic,
	ConDecl(..), ConDetails(..), BangType(..),
	IfaceSig(..),  SpecDataSig(..), HsIdInfo(..), HsStrictnessInfo(..),
	hsDeclName, tyClDeclName, isClassDecl, isSynDecl, isDataDecl, countTyClDecls
    ) where

#include "HsVersions.h"

-- friends:
import HsBinds		( HsBinds, MonoBinds, Sig, FixitySig(..), nullMonoBinds )
import HsPragmas	( DataPragmas, ClassPragmas )
import HsTypes
import HsCore		( UfExpr )
import BasicTypes	( Fixity, NewOrData(..) )
import IdInfo		( ArityInfo, UpdateInfo, InlinePragInfo )
import Demand		( Demand )
import CallConv		( CallConv, pprCallConv )

-- others:
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
data HsDecl name pat
  = TyClD	(TyClDecl name pat)
  | InstD	(InstDecl  name pat)
  | DefD	(DefaultDecl name)
  | ValD	(HsBinds name pat)
  | ForD        (ForeignDecl name)
  | SigD	(IfaceSig name)
  | FixD	(FixitySig name)

-- NB: all top-level fixity decls are contained EITHER
-- EITHER FixDs
-- OR     in the ClassDecls in TyClDs
--
-- The former covers
-- 	a) data constructors
-- 	b) class methods (but they can be also done in the
-- 		signatures of class decls)
--	c) imported functions (that have an IfacSig)
--	d) top level decls
--
-- The latter is for class methods only

-- It's a bit wierd that the fixity decls in the ValD
-- cover all the classops and imported decls too, but it's convenient
-- For a start, it means we don't need a FixD
\end{code}

\begin{code}
#ifdef DEBUG
hsDeclName :: (Outputable name, Outputable pat)
	   => HsDecl name pat -> name
#endif
hsDeclName (TyClD decl)				    = tyClDeclName decl
hsDeclName (SigD  (IfaceSig name _ _ _))	    = name
hsDeclName (InstD (InstDecl _ _ _ (Just name) _))   = name
hsDeclName (ForD  (ForeignDecl name _ _ _ _ _))     = name
hsDeclName (FixD  (FixitySig name _ _))		    = name
-- Others don't make sense
#ifdef DEBUG
hsDeclName x				      = pprPanic "HsDecls.hsDeclName" (ppr x)
#endif

tyClDeclName :: TyClDecl name pat -> name
tyClDeclName (TyData _ _ name _ _ _ _ _)      = name
tyClDeclName (TySynonym name _ _ _)           = name
tyClDeclName (ClassDecl _ name _ _ _ _ _ _ _) = name
\end{code}

\begin{code}
instance (Outputable name, Outputable pat)
	=> Outputable (HsDecl name pat) where

    ppr (TyClD dcl)  = ppr dcl
    ppr (SigD sig)   = ppr sig
    ppr (ValD binds) = ppr binds
    ppr (DefD def)   = ppr def
    ppr (InstD inst) = ppr inst
    ppr (ForD fd)    = ppr fd
    ppr (FixD fd)    = ppr fd

{-	Why do we need ordering on decls?

#ifdef DEBUG
-- hsDeclName needs more context when DEBUG is on
instance (Outputable name, Outputable pat, Eq name)
      => Eq (HsDecl name pat) where
   d1 == d2 = hsDeclName d1 == hsDeclName d2
	
instance (Outputable name, Outputable pat, Ord name)
      => Ord (HsDecl name pat) where
	d1 `compare` d2 = hsDeclName d1 `compare` hsDeclName d2
#else
instance (Eq name) => Eq (HsDecl name pat) where
	d1 == d2 = hsDeclName d1 == hsDeclName d2
	
instance (Ord name) => Ord (HsDecl name pat) where
	d1 `compare` d2 = hsDeclName d1 `compare` hsDeclName d2
#endif
-}
\end{code}


%************************************************************************
%*									*
\subsection[TyDecl]{@data@, @newtype@ or @type@ (synonym) type declaration}
%*									*
%************************************************************************

\begin{code}
data TyClDecl name pat
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

  | ClassDecl	(Context name)	    		-- context...
		name		    		-- name of the class
		[HsTyVar name]	    		-- the class type variables
		[Sig name]			-- methods' signatures
		(MonoBinds name pat)	-- default methods
		(ClassPragmas name)
		name name			-- The names of the tycon and datacon for this class
						-- These are filled in by the renamer
		SrcLoc
\end{code}

\begin{code}
countTyClDecls :: [TyClDecl name pat] -> (Int, Int, Int, Int)
	-- class, data, newtype, synonym decls
countTyClDecls decls 
 = (length [() | ClassDecl _ _ _ _ _ _ _ _   _ <- decls],
    length [() | TyData DataType _ _ _ _ _ _ _ <- decls],
    length [() | TyData NewType  _ _ _ _ _ _ _ <- decls],
    length [() | TySynonym _ _ _ _	       <- decls])

isDataDecl, isSynDecl, isClassDecl :: TyClDecl name pat -> Bool

isSynDecl (TySynonym _ _ _ _) = True
isSynDecl other		      = False

isDataDecl (TyData _ _ _ _ _ _ _ _) = True
isDataDecl other		    = False

isClassDecl (ClassDecl _ _ _ _ _ _ _ _ _) = True
isClassDecl other			  = False
\end{code}

\begin{code}
instance (Outputable name, Outputable pat)
	      => Outputable (TyClDecl name pat) where

    ppr (TySynonym tycon tyvars mono_ty src_loc)
      = hang (pp_decl_head SLIT("type") empty tycon tyvars)
	     4 (ppr mono_ty)

    ppr (TyData new_or_data context tycon tyvars condecls derivings pragmas src_loc)
      = pp_tydecl
		  (pp_decl_head keyword (pprContext context) tycon tyvars)
		  (pp_condecls condecls)
		  derivings
      where
	keyword = case new_or_data of
			NewType  -> SLIT("newtype")
			DataType -> SLIT("data")

    ppr (ClassDecl context clas tyvars sigs methods pragmas _ _ src_loc)
      | null sigs	-- No "where" part
      = top_matter

      | otherwise	-- Laid out
      = sep [hsep [top_matter, ptext SLIT("where {")],
	       nest 4 (vcat [sep (map ppr_sig sigs),
				   ppr methods,
				   char '}'])]
      where
        top_matter = hsep [ptext SLIT("class"), pprContext context,
                            ppr clas, hsep (map (ppr) tyvars)]
	ppr_sig sig = ppr sig <> semi


pp_decl_head str pp_context tycon tyvars
  = hsep [ptext str, pp_context, ppr tycon,
	   interppSP tyvars, ptext SLIT("=")]

pp_condecls []     = empty		-- Curious!
pp_condecls (c:cs) = sep (ppr c : map (\ c -> ptext SLIT("|") <+> ppr c) cs)

pp_tydecl pp_head pp_decl_rhs derivings
  = hang pp_head 4 (sep [
	pp_decl_rhs,
	case derivings of
	  Nothing 	   -> empty
	  Just ds	   -> hsep [ptext SLIT("deriving"), parens (interpp'SP ds)]
    ])
\end{code}

A type for recording what types a datatype should be specialised to.
It's called a ``Sig'' because it's sort of like a ``type signature''
for an datatype declaration.

\begin{code}
data SpecDataSig name
  = SpecDataSig name		-- tycon to specialise
		(HsType name)
		SrcLoc

instance (Outputable name)
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

		[HsTyVar name]		-- Existentially quantified type variables
		(Context name)		-- ...and context
					-- If both are empty then there are no existentials

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

  | NewCon	 		-- newtype con decl, possibly with a labelled field.
		(HsType name)
		(Maybe name)	-- Just x => labelled field 'x'

data BangType name
  = Banged   (HsType name)	-- HsType: to allow Haskell extensions
  | Unbanged (HsType name)	-- (MonoType only needed for straight Haskell)
\end{code}

\begin{code}
instance (Outputable name) => Outputable (ConDecl name) where
    ppr (ConDecl con tvs cxt con_details  loc)
      = sep [pprForAll tvs, pprContext cxt, ppr_con_details con con_details]

ppr_con_details con (InfixCon ty1 ty2)
  = hsep [ppr_bang ty1, ppr con, ppr_bang ty2]

ppr_con_details con (VanillaCon tys)
  = ppr con <+> hsep (map (ppr_bang) tys)

ppr_con_details con (NewCon ty Nothing)
  = ppr con <+> pprParendHsType ty

ppr_con_details con (NewCon ty (Just x))
  = ppr con <+> braces pp_field 
   where
    pp_field = ppr x <+> dcolon <+> pprParendHsType ty
 
ppr_con_details con (RecCon fields)
  = ppr con <+> braces (hsep (punctuate comma (map ppr_field fields)))
  where
    ppr_field (ns, ty) = hsep (map (ppr) ns) <+> 
			 dcolon <+>
			 ppr_bang ty

ppr_bang (Banged   ty) = ptext SLIT("!") <> pprParendHsType ty
ppr_bang (Unbanged ty) = pprParendHsType ty
\end{code}


%************************************************************************
%*									*
\subsection[InstDecl]{An instance declaration
%*									*
%************************************************************************

\begin{code}
data InstDecl name pat
  = InstDecl	(HsType name)	-- Context => Class Instance-type
				-- Using a polytype means that the renamer conveniently
				-- figures out the quantified type variables for us.

		(MonoBinds name pat)

		[Sig name]		-- User-supplied pragmatic info

		(Maybe name)		-- Name for the dictionary function

		SrcLoc
\end{code}

\begin{code}
instance (Outputable name, Outputable pat)
	      => Outputable (InstDecl name pat) where

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

instance (Outputable name)
	      => Outputable (DefaultDecl name) where

    ppr (DefaultDecl tys src_loc)
      = ptext SLIT("default") <+> parens (interpp'SP tys)
\end{code}

%************************************************************************
%*									*
\subsection{Foreign function interface declaration}
%*									*
%************************************************************************

\begin{code}
data ForeignDecl name = 
   ForeignDecl 
        name 
	ForKind   
	(HsType name)
	ExtName
	CallConv
	SrcLoc

instance (Outputable name)
	      => Outputable (ForeignDecl name) where

    ppr (ForeignDecl nm imp_exp ty ext_name cconv src_loc)
      = ptext SLIT("foreign") <+> ppr_imp_exp <+> pprCallConv cconv <+> 
        ppr ext_name <+> ppr_unsafe <+> ppr nm <+> dcolon <+> ppr ty
        where
         (ppr_imp_exp, ppr_unsafe) =
	   case imp_exp of
	     FoLabel     -> (ptext SLIT("label"), empty)
	     FoExport    -> (ptext SLIT("export"), empty)
	     FoImport us 
		| us        -> (ptext SLIT("import"), ptext SLIT("unsafe"))
		| otherwise -> (ptext SLIT("import"), empty)

data ForKind
 = FoLabel
 | FoExport
 | FoImport Bool -- True  => unsafe call.

data ExtName
 = Dynamic 
 | ExtName FAST_STRING (Maybe FAST_STRING)

isDynamic :: ExtName -> Bool
isDynamic Dynamic = True
isDynamic _	  = False


instance Outputable ExtName where
  ppr Dynamic	   = ptext SLIT("dynamic")
  ppr (ExtName nm mb_mod) = 
     case mb_mod of { Nothing -> empty; Just m -> doubleQuotes (ptext m) } <+> 
     doubleQuotes (ptext nm)

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

instance (Outputable name) => Outputable (IfaceSig name) where
    ppr (IfaceSig var ty _ _)
      = hang (hsep [ppr var, dcolon])
	     4 (ppr ty)

data HsIdInfo name
  = HsArity		ArityInfo
  | HsStrictness	(HsStrictnessInfo name)
  | HsUnfold		InlinePragInfo (Maybe (UfExpr name))
  | HsUpdate		UpdateInfo
  | HsSpecialise	[HsTyVar name] [HsType name] (UfExpr name)
  | HsNoCafRefs


data HsStrictnessInfo name
  = HsStrictnessInfo ([Demand], Bool)
		     (Maybe (name, [name]))	-- Worker, if any
						-- and needed constructors
  | HsBottom
\end{code}
