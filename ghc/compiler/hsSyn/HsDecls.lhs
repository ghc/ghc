%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsDecls]{Abstract syntax: global declarations}

Definitions for: @TyDecl@ and @ConDecl@, @ClassDecl@,
@InstDecl@, @DefaultDecl@ and @ForeignDecl@.

\begin{code}
module HsDecls (
	HsDecl(..), TyClDecl(..), InstDecl(..), RuleDecl(..), RuleBndr(..),
	DefaultDecl(..), ForeignDecl(..), ForKind(..),
	ExtName(..), isDynamicExtName, extNameStatic,
	ConDecl(..), ConDetails(..), BangType(..),
	IfaceSig(..),  SpecDataSig(..), 
	DeprecDecl(..), DeprecTxt,
	hsDeclName, tyClDeclName, isClassDecl, isSynDecl, isDataDecl, countTyClDecls, toHsRule
    ) where

#include "HsVersions.h"

-- friends:
import HsBinds		( HsBinds, MonoBinds, Sig(..), FixitySig(..), nullMonoBinds )
import HsExpr		( HsExpr )
import HsPragmas	( DataPragmas, ClassPragmas )
import HsImpExp		( IE(..) )
import HsTypes
import PprCore		( pprCoreRule )
import HsCore		( UfExpr(UfVar), UfBinder, IfaceSig(..), eq_ufBinders, eq_ufExpr, pprUfExpr, toUfExpr, toUfBndr )
import CoreSyn		( CoreRule(..) )
import BasicTypes	( Fixity, NewOrData(..) )
import CallConv		( CallConv, pprCallConv )
import Var		( TyVar, Id )
import Name		( toRdrName )

-- others:
import PprType
import FunDeps		( pprFundeps )
import Class		( FunDep )
import CStrings		( CLabelString, pprCLabelString )
import Outputable	
import SrcLoc		( SrcLoc, noSrcLoc )
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
  | DeprecD	(DeprecDecl name)
  | RuleD	(RuleDecl name pat)

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
\end{code}

\begin{code}
#ifdef DEBUG
hsDeclName :: (Outputable name, Outputable pat)
	   => HsDecl name pat -> name
#endif
hsDeclName (TyClD decl)				  = tyClDeclName decl
hsDeclName (SigD    (IfaceSig name _ _ _))	  = name
hsDeclName (InstD   (InstDecl _ _ _ name _))      = name
hsDeclName (ForD    (ForeignDecl name _ _ _ _ _)) = name
hsDeclName (FixD    (FixitySig name _ _))	  = name
-- Others don't make sense
#ifdef DEBUG
hsDeclName x				      = pprPanic "HsDecls.hsDeclName" (ppr x)
#endif

tyClDeclName :: TyClDecl name pat -> name
tyClDeclName (TyData _ _ name _ _ _ _ _ _)          = name
tyClDeclName (TySynonym name _ _ _)                 = name
tyClDeclName (ClassDecl _ name _ _ _ _ _ _ _ _ _ _) = name
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
    ppr (RuleD rd)   = ppr rd
    ppr (DeprecD dd) = ppr dd
\end{code}

\begin{code}
instance Ord name => Eq (HsDecl name pat) where
	-- Used only when comparing interfaces, 
	-- at which time only signature and type/class decls
   (SigD s1)  == (SigD s2) = s1 == s2
   (TyClD d1) == (TyClD d2) = d1 == d2
\end{code}


%************************************************************************
%*									*
\subsection[TyDecl]{@data@, @newtype@ or @type@ (synonym) type declaration}
%*									*
%************************************************************************

\begin{code}
data TyClDecl name pat
  = TyData	NewOrData
		(HsContext name) -- context
		name		 -- type constructor
		[HsTyVarBndr name]	 -- type variables
		[ConDecl name]	 -- data constructors (empty if abstract)
		Int		 -- Number of data constructors (valid even if type is abstract)
		(Maybe [name])	 -- derivings; Nothing => not specified
				 -- (i.e., derive default); Just [] => derive
				 -- *nothing*; Just <list> => as you would
				 -- expect...
		(DataPragmas name)
		SrcLoc

  | TySynonym	name		-- type constructor
		[HsTyVarBndr name]	-- type variables
		(HsType name)	-- synonym expansion
		SrcLoc

  | ClassDecl	(HsContext name)    	-- context...
		name		    	-- name of the class
		[HsTyVarBndr name]	-- the class type variables
		[FunDep name]		-- functional dependencies
		[Sig name]		-- methods' signatures
		(MonoBinds name pat)	-- default methods
		(ClassPragmas name)
		name name name [name]	-- The names of the tycon, datacon wrapper, datacon worker,
					-- and superclass selectors for this class.
					-- These are filled in as the ClassDecl is made.
		SrcLoc

instance Ord name => Eq (TyClDecl name pat) where
	-- Used only when building interface files
  (==) (TyData nd1 cxt1 n1 tvs1 cons1 _ _ _ _)
       (TyData nd2 cxt2 n2 tvs2 cons2 _ _ _ _)
    = n1 == n2 &&
      nd1 == nd2 &&
      eqWithHsTyVars tvs1 tvs2 (\ env -> 
   	  eq_hsContext env cxt1 cxt2 &&
	  eqListBy (eq_ConDecl env) cons1 cons2
      )

  (==) (TySynonym n1 tvs1 ty1 _)
       (TySynonym n2 tvs2 ty2 _)
    =  n1 == n2 &&
       eqWithHsTyVars tvs1 tvs2 (\ env -> eq_hsType env ty1 ty2)

  (==) (ClassDecl cxt1 n1 tvs1 fds1 sigs1 _ _ _ _ _ _ _)
       (ClassDecl cxt2 n2 tvs2 fds2 sigs2 _ _ _ _ _ _ _)
    =  n1 == n2 &&
       eqWithHsTyVars tvs1 tvs2 (\ env -> 
	  eq_hsContext env cxt1 cxt2 &&
	  eqListBy (eq_hsFD env) fds1 fds2 &&
	  eqListBy (eq_cls_sig env) sigs1 sigs2
       )

eq_hsFD env (ns1,ms1) (ns2,ms2)
  = eqListBy (eq_hsVar env) ns1 ns2 && eqListBy (eq_hsVar env) ms1 ms2

eq_cls_sig env (ClassOpSig n1 _ b1 ty1 _) (ClassOpSig n2 _ b2 ty2 _)
  = n1==n2 && b1==b2 && eq_hsType env ty1 ty2
\end{code}

\begin{code}
countTyClDecls :: [TyClDecl name pat] -> (Int, Int, Int, Int)
	-- class, data, newtype, synonym decls
countTyClDecls decls 
 = (length [() | ClassDecl _ _ _ _ _ _ _ _ _ _ _ _ <- decls],
    length [() | TyData DataType _ _ _ _ _ _ _ _   <- decls],
    length [() | TyData NewType  _ _ _ _ _ _ _ _   <- decls],
    length [() | TySynonym _ _ _ _	           <- decls])

isDataDecl, isSynDecl, isClassDecl :: TyClDecl name pat -> Bool

isSynDecl (TySynonym _ _ _ _) = True
isSynDecl other		      = False

isDataDecl (TyData _ _ _ _ _ _ _ _ _) = True
isDataDecl other		      = False

isClassDecl (ClassDecl _ _ _ _ _ _ _ _ _ _ _ _) = True
isClassDecl other		 	        = False
\end{code}

\begin{code}
instance (Outputable name, Outputable pat)
	      => Outputable (TyClDecl name pat) where

    ppr (TySynonym tycon tyvars mono_ty src_loc)
      = hang (ptext SLIT("type") <+> pp_decl_head [] tycon tyvars <+> equals)
	     4 (ppr mono_ty)

    ppr (TyData new_or_data context tycon tyvars condecls ncons derivings pragmas src_loc)
      = pp_tydecl
		  (ptext keyword <+> pp_decl_head context tycon tyvars <+> equals)
		  (pp_condecls condecls ncons)
		  derivings
      where
	keyword = case new_or_data of
			NewType  -> SLIT("newtype")
			DataType -> SLIT("data")

    ppr (ClassDecl context clas tyvars fds sigs methods pragmas _ _ _ _ src_loc)
      | null sigs	-- No "where" part
      = top_matter

      | otherwise	-- Laid out
      = sep [hsep [top_matter, ptext SLIT("where {")],
	     nest 4 (sep [sep (map ppr_sig sigs), pp_methods, char '}'])]
      where
        top_matter  = ptext SLIT("class") <+> pp_decl_head context clas tyvars <+> pprFundeps fds
	ppr_sig sig = ppr sig <> semi
	pp_methods = getPprStyle $ \ sty ->
        	     if ifaceStyle sty then empty else ppr methods
        

pp_decl_head :: Outputable name => HsContext name -> name -> [HsTyVarBndr name] -> SDoc
pp_decl_head context thing tyvars = hsep [pprHsContext context, ppr thing, interppSP tyvars]

pp_condecls []     ncons = ptext SLIT("{- abstract with") <+> int ncons <+> ptext SLIT("constructors -}")
pp_condecls (c:cs) ncons = sep (ppr c : map (\ c -> ptext SLIT("|") <+> ppr c) cs)

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
  = ConDecl 	name			-- Constructor name; this is used for the
					-- DataCon itself, and for the user-callable wrapper Id

		name			-- Name of the constructor's 'worker Id'
					-- Filled in as the ConDecl is built

		[HsTyVarBndr name]		-- Existentially quantified type variables
		(HsContext name)	-- ...and context
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
  | Unpacked (HsType name)	-- Field is strict and to be unpacked if poss.


eq_ConDecl env (ConDecl n1 _ tvs1 cxt1 cds1 _)
	       (ConDecl n2 _ tvs2 cxt2 cds2 _)
  = n1 == n2 &&
    (eqWithHsTyVars tvs1 tvs2	$ \ env ->
     eq_hsContext env cxt1 cxt2	&&
     eq_ConDetails env cds1 cds2)

eq_ConDetails env (VanillaCon bts1) (VanillaCon bts2)
  = eqListBy (eq_btype env) bts1 bts2
eq_ConDetails env (InfixCon bta1 btb1) (InfixCon bta2 btb2)
  = eq_btype env bta1 bta2 && eq_btype env btb1 btb2
eq_ConDetails env (RecCon fs1) (RecCon fs2)
  = eqListBy (eq_fld env) fs1 fs2
eq_ConDetails env (NewCon t1 mn1) (NewCon t2 mn2)
  = eq_hsType env t1 t2 && mn1 == mn2
eq_ConDetails env _ _ = False

eq_fld env (ns1,bt1) (ns2, bt2) = ns1==ns2 && eq_btype env bt1 bt2

eq_btype env (Banged t1)   (Banged t2)   = eq_hsType env t1 t2
eq_btype env (Unbanged t1) (Unbanged t2) = eq_hsType env t1 t2
eq_btype env (Unpacked t1) (Unpacked t2) = eq_hsType env t1 t2
\end{code}

\begin{code}
instance (Outputable name) => Outputable (ConDecl name) where
    ppr (ConDecl con _ tvs cxt con_details  loc)
      = sep [pprHsForAll tvs cxt, ppr_con_details con con_details]

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

instance Outputable name => Outputable (BangType name) where
    ppr = ppr_bang

ppr_bang (Banged   ty) = ptext SLIT("!") <> pprParendHsType ty
ppr_bang (Unbanged ty) = pprParendHsType ty
ppr_bang (Unpacked ty) = ptext SLIT("! !") <> pprParendHsType ty
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

		name			-- Name for the dictionary function

		SrcLoc
\end{code}

\begin{code}
instance (Outputable name, Outputable pat)
	      => Outputable (InstDecl name pat) where

    ppr (InstDecl inst_ty binds uprags dfun_name src_loc)
      = getPprStyle $ \ sty ->
        if ifaceStyle sty then
           hsep [ptext SLIT("instance"), ppr inst_ty, equals, ppr dfun_name]
	else
	   vcat [hsep [ptext SLIT("instance"), ppr inst_ty, ptext SLIT("where")],
	         nest 4 (ppr uprags),
	         nest 4 (ppr binds) ]
\end{code}

\begin{code}
instance Ord name => Eq (InstDecl name pat) where
	-- Used for interface comparison only, so don't compare bindings
  (==) (InstDecl inst_ty1 _ _ dfun1 _) (InstDecl inst_ty2 _ _ dfun2 _)
       = inst_ty1 == inst_ty2 && dfun1 == dfun2
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
 | ExtName CLabelString 	-- The external name of the foreign thing,
	   (Maybe CLabelString)	-- and optionally its DLL or module name
				-- Both of these are completely unencoded; 
				-- we just print them as they are

isDynamicExtName :: ExtName -> Bool
isDynamicExtName Dynamic = True
isDynamicExtName _	 = False

extNameStatic :: ExtName -> CLabelString
extNameStatic (ExtName f _) = f
extNameStatic Dynamic	    = panic "staticExtName: Dynamic - shouldn't ever happen."

instance Outputable ExtName where
  ppr Dynamic	   = ptext SLIT("dynamic")
  ppr (ExtName nm mb_mod) = 
     case mb_mod of { Nothing -> empty; Just m -> doubleQuotes (ptext m) } <+> 
     doubleQuotes (pprCLabelString nm)
\end{code}

%************************************************************************
%*									*
\subsection{Transformation rules}
%*									*
%************************************************************************

\begin{code}
data RuleDecl name pat
  = HsRule			-- Source rule
	FAST_STRING		-- Rule name
	[name]			-- Forall'd tyvars, filled in by the renamer with
				-- tyvars mentioned in sigs; then filled out by typechecker
	[RuleBndr name]		-- Forall'd term vars
	(HsExpr name pat)	-- LHS
	(HsExpr name pat)	-- RHS
	SrcLoc		

  | IfaceRule	 		-- One that's come in from an interface file; pre-typecheck
	FAST_STRING
	[UfBinder name]		-- Tyvars and term vars
	name			-- Head of lhs
	[UfExpr name]		-- Args of LHS
	(UfExpr name)		-- Pre typecheck
	SrcLoc		

  | IfaceRuleOut		-- Post typecheck
	name			-- Head of LHS
	CoreRule


data RuleBndr name
  = RuleBndr name
  | RuleBndrSig name (HsType name)

instance Ord name => Eq (RuleDecl name pat) where
  -- Works for IfaceRules only; used when comparing interface file versions
  (IfaceRule n1 bs1 f1 es1 rhs1 _) == (IfaceRule n2 bs2 f2 es2 rhs2 _)
     = n1==n2 && f1 == f2 && 
       eq_ufBinders emptyEqHsEnv bs1 bs2 (\env -> 
       eqListBy (eq_ufExpr env) (rhs1:es1) (rhs2:es2))

instance (Outputable name, Outputable pat)
	      => Outputable (RuleDecl name pat) where
  ppr (HsRule name tvs ns lhs rhs loc)
	= sep [text "{-# RULES" <+> doubleQuotes (ptext name),
	       pp_forall, ppr lhs, equals <+> ppr rhs,
               text "#-}" ]
	where
	  pp_forall | null tvs && null ns = empty
		    | otherwise		  = text "forall" <+> 
					    fsep (map ppr tvs ++ map ppr ns)
					    <> dot

  ppr (IfaceRule name tpl_vars fn tpl_args rhs loc) 
    = hsep [ doubleQuotes (ptext name),
	   ptext SLIT("__forall") <+> braces (interppSP tpl_vars),
	   ppr fn <+> sep (map (pprUfExpr parens) tpl_args),
	   ptext SLIT("=") <+> ppr rhs
      ] <+> semi

  ppr (IfaceRuleOut fn rule) = pprCoreRule (ppr fn) rule

instance Outputable name => Outputable (RuleBndr name) where
   ppr (RuleBndr name) = ppr name
   ppr (RuleBndrSig name ty) = ppr name <> dcolon <> ppr ty

toHsRule id (BuiltinRule _)
  = pprTrace "toHsRule: builtin" (ppr id) (bogusIfaceRule id)

toHsRule id (Rule name bndrs args rhs)
  = IfaceRule name (map toUfBndr bndrs) (toRdrName id)
	      (map toUfExpr args) (toUfExpr rhs) noSrcLoc

bogusIfaceRule id
  = IfaceRule SLIT("bogus") [] (toRdrName id) [] (UfVar (toRdrName id)) noSrcLoc
\end{code}


%************************************************************************
%*									*
\subsection[DeprecDecl]{Deprecations}
%*									*
%************************************************************************

We use exported entities for things to deprecate. Cunning trick (hack?):
`IEModuleContents undefined' is used for module deprecation.

\begin{code}
data DeprecDecl name = Deprecation (IE name) DeprecTxt SrcLoc

type DeprecTxt = FAST_STRING	-- reason/explanation for deprecation

instance Outputable name => Outputable (DeprecDecl name) where
   ppr (Deprecation (IEModuleContents _) txt _)
      = hsep [text "{-# DEPRECATED",            doubleQuotes (ppr txt), text "#-}"]
   ppr (Deprecation thing txt _)
      = hsep [text "{-# DEPRECATED", ppr thing, doubleQuotes (ppr txt), text "#-}"]
\end{code}
