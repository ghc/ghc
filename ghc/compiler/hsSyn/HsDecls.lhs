%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsDecls]{Abstract syntax: global declarations}

Definitions for: @TyDecl@ and @oCnDecl@, @ClassDecl@,
@InstDecl@, @DefaultDecl@ and @ForeignDecl@.

\begin{code}
module HsDecls (
	HsDecl(..), TyClDecl(..), InstDecl(..), RuleDecl(..), RuleBndr(..),
	DefaultDecl(..), 
	ForeignDecl(..), ForeignImport(..), ForeignExport(..),
	CImportSpec(..), FoType(..),
	ConDecl(..), ConDetails(..), 
	BangType(..), getBangType, getBangStrictness, unbangedType,
	DeprecDecl(..), DeprecTxt,
	hsDeclName, instDeclName, 
	tyClDeclName, tyClDeclNames, tyClDeclSysNames, tyClDeclTyVars,
	isClassDecl, isSynDecl, isDataDecl, isIfaceSigDecl, countTyClDecls,
	mkClassDeclSysNames, isSourceInstDecl, ifaceRuleDeclName,
	getClassDeclSysNames, conDetailsTys,
	collectRuleBndrSigTys
    ) where

#include "HsVersions.h"

-- friends:
import HsBinds		( HsBinds, MonoBinds, Sig(..), FixitySig(..) )
import HsExpr		( HsExpr )
import HsImpExp		( ppr_var )
import HsTypes
import PprCore		( pprCoreRule )
import HsCore		( UfExpr, UfBinder, HsIdInfo, pprHsIdInfo,
			  eq_ufBinders, eq_ufExpr, pprUfExpr 
			)
import CoreSyn		( CoreRule(..), RuleName )
import BasicTypes	( NewOrData(..), StrictnessMark(..), Activation(..) )
import ForeignCall	( CCallTarget(..), DNCallSpec, CCallConv, Safety,
			  CExportSpec(..)) 

-- others:
import Name		( NamedThing )
import FunDeps		( pprFundeps )
import TyCon		( DataConDetails(..), visibleDataCons )
import Class		( FunDep, DefMeth(..) )
import CStrings		( CLabelString )
import Outputable	
import Util		( eqListBy, count )
import SrcLoc		( SrcLoc )
import FastString

import Maybe		( isNothing, fromJust )	
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
hsDeclName :: (NamedThing name, Outputable name, Outputable pat)
	   => HsDecl name pat -> name
#endif
hsDeclName (TyClD decl)			= tyClDeclName     decl
hsDeclName (InstD decl)		        = instDeclName     decl
hsDeclName (ForD  decl)		        = foreignDeclName decl
hsDeclName (FixD  (FixitySig name _ _)) = name
-- Others don't make sense
#ifdef DEBUG
hsDeclName x				= pprPanic "HsDecls.hsDeclName" (ppr x)
#endif


instDeclName :: InstDecl name pat -> name
instDeclName (InstDecl _ _ _ (Just name) _) = name

\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat)
	=> Outputable (HsDecl name pat) where

    ppr (TyClD dcl)  = ppr dcl
    ppr (ValD binds) = ppr binds
    ppr (DefD def)   = ppr def
    ppr (InstD inst) = ppr inst
    ppr (ForD fd)    = ppr fd
    ppr (FixD fd)    = ppr fd
    ppr (RuleD rd)   = ppr rd
    ppr (DeprecD dd) = ppr dd
\end{code}


%************************************************************************
%*									*
\subsection[TyDecl]{@data@, @newtype@ or @type@ (synonym) type declaration}
%*									*
%************************************************************************

		--------------------------------
			THE NAMING STORY
		--------------------------------

Here is the story about the implicit names that go with type, class, and instance
decls.  It's a bit tricky, so pay attention!

"Implicit" (or "system") binders
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Each data type decl defines 
	a worker name for each constructor
	to-T and from-T convertors
  Each class decl defines
	a tycon for the class
	a data constructor for that tycon
	the worker for that constructor
	a selector for each superclass

All have occurrence names that are derived uniquely from their parent declaration.

None of these get separate definitions in an interface file; they are
fully defined by the data or class decl.  But they may *occur* in
interface files, of course.  Any such occurrence must haul in the
relevant type or class decl.

Plan of attack:
 - Make up their occurrence names immediately
   This is done in RdrHsSyn.mkClassDecl, mkTyDecl, mkConDecl

 - Ensure they "point to" the parent data/class decl 
   when loading that decl from an interface file
   (See RnHiFiles.getTyClDeclSysNames)

 - When renaming the decl look them up in the name cache,
   ensure correct module and provenance is set

Default methods
~~~~~~~~~~~~~~~
 - Occurrence name is derived uniquely from the method name
   E.g. $dmmax

 - If there is a default method name at all, it's recorded in
   the ClassOpSig (in HsBinds), in the DefMeth field.
   (DefMeth is defined in Class.lhs)

Source-code class decls and interface-code class decls are treated subtly
differently, which has given me a great deal of confusion over the years.
Here's the deal.  (We distinguish the two cases because source-code decls
have (Just binds) in the tcdMeths field, whereas interface decls have Nothing.

In *source-code* class declarations:
 - When parsing, every ClassOpSig gets a DefMeth with a suitable RdrName
   This is done by RdrHsSyn.mkClassOpSigDM

 - The renamer renames it to a Name

 - During typechecking, we generate a binding for each $dm for 
   which there's a programmer-supplied default method:
	class Foo a where
	  op1 :: <type>
	  op2 :: <type>
	  op1 = ...
   We generate a binding for $dmop1 but not for $dmop2.
   The Class for Foo has a NoDefMeth for op2 and a DefMeth for op1.
   The Name for $dmop2 is simply discarded.

In *interface-file* class declarations:
  - When parsing, we see if there's an explicit programmer-supplied default method
    because there's an '=' sign to indicate it:
	class Foo a where
	  op1 = :: <type>	-- NB the '='
  	  op2   :: <type>
    We use this info to generate a DefMeth with a suitable RdrName for op1,
    and a NoDefMeth for op2
  - The interface file has a separate definition for $dmop1, with unfolding etc.
  - The renamer renames it to a Name.
  - The renamer treats $dmop1 as a free variable of the declaration, so that
    the binding for $dmop1 will be sucked in.  (See RnHsSyn.tyClDeclFVs)  
    This doesn't happen for source code class decls, because they *bind* the default method.

Dictionary functions
~~~~~~~~~~~~~~~~~~~~
Each instance declaration gives rise to one dictionary function binding.

The type checker makes up new source-code instance declarations
(e.g. from 'deriving' or generic default methods --- see
TcInstDcls.tcInstDecls1).  So we can't generate the names for
dictionary functions in advance (we don't know how many we need).

On the other hand for interface-file instance declarations, the decl
specifies the name of the dictionary function, and it has a binding elsewhere
in the interface file:
	instance {Eq Int} = dEqInt
	dEqInt :: {Eq Int} <pragma info>

So again we treat source code and interface file code slightly differently.

Source code:
  - Source code instance decls have a Nothing in the (Maybe name) field
    (see data InstDecl below)

  - The typechecker makes up a Local name for the dict fun for any source-code
    instance decl, whether it comes from a source-code instance decl, or whether
    the instance decl is derived from some other construct (e.g. 'deriving').

  - The occurrence name it chooses is derived from the instance decl (just for 
    documentation really) --- e.g. dNumInt.  Two dict funs may share a common
    occurrence name, but will have different uniques.  E.g.
	instance Foo [Int]  where ...
	instance Foo [Bool] where ...
    These might both be dFooList

  - The CoreTidy phase externalises the name, and ensures the occurrence name is
    unique (this isn't special to dict funs).  So we'd get dFooList and dFooList1.

  - We can take this relaxed approach (changing the occurrence name later) 
    because dict fun Ids are not captured in a TyCon or Class (unlike default
    methods, say).  Instead, they are kept separately in the InstEnv.  This
    makes it easy to adjust them after compiling a module.  (Once we've finished
    compiling that module, they don't change any more.)


Interface file code:
  - The instance decl gives the dict fun name, so the InstDecl has a (Just name)
    in the (Maybe name) field.

  - RnHsSyn.instDeclFVs treats the dict fun name as free in the decl, so that we
    suck in the dfun binding


\begin{code}
-- TyClDecls are precisely the kind of declarations that can 
-- appear in interface files; or (internally) in GHC's interface
-- for a module.  That's why (despite the misnomer) IfaceSig and ForeignType
-- are both in TyClDecl

data TyClDecl name pat
  = IfaceSig {	tcdName :: name,		-- It may seem odd to classify an interface-file signature
		tcdType :: HsType name,		-- as a 'TyClDecl', but it's very convenient.  
		tcdIdInfo :: [HsIdInfo name],
		tcdLoc :: SrcLoc
    }

  | ForeignType { tcdName    :: name,		-- See remarks about IfaceSig above
		  tcdExtName :: Maybe FastString,
		  tcdFoType  :: FoType,
		  tcdLoc     :: SrcLoc }

  | TyData {	tcdND     :: NewOrData,
		tcdCtxt   :: HsContext name,	 -- context
		tcdName   :: name,		 -- type constructor
		tcdTyVars :: [HsTyVarBndr name], -- type variables
		tcdCons	  :: DataConDetails (ConDecl name),	 -- data constructors (empty if abstract)
		tcdDerivs :: Maybe (HsContext name),	-- derivings; Nothing => not specified
							-- Just [] => derive exactly what is asked
		tcdSysNames :: DataSysNames name,	-- Generic converter functions
		tcdLoc	    :: SrcLoc
    }

  | TySynonym {	tcdName :: name,		        -- type constructor
		tcdTyVars :: [HsTyVarBndr name],	-- type variables
		tcdSynRhs :: HsType name,	        -- synonym expansion
		tcdLoc    :: SrcLoc
    }

  | ClassDecl {	tcdCtxt    :: HsContext name, 	 	-- Context...
		tcdName    :: name,		    	-- Name of the class
		tcdTyVars  :: [HsTyVarBndr name],	-- The class type variables
		tcdFDs     :: [FunDep name],		-- Functional dependencies
		tcdSigs    :: [Sig name],		-- Methods' signatures
		tcdMeths   :: Maybe (MonoBinds name pat),	-- Default methods
								-- Nothing for imported class decls
								-- Just bs for source   class decls
		tcdSysNames :: ClassSysNames name,
		tcdLoc      :: SrcLoc
    }
\end{code}

Simple classifiers

\begin{code}
isIfaceSigDecl, isDataDecl, isSynDecl, isClassDecl :: TyClDecl name pat -> Bool

isIfaceSigDecl (IfaceSig {}) = True
isIfaceSigDecl other	     = False

isSynDecl (TySynonym {}) = True
isSynDecl other		 = False

isDataDecl (TyData {}) = True
isDataDecl other       = False

isClassDecl (ClassDecl {}) = True
isClassDecl other	   = False
\end{code}

Dealing with names

\begin{code}
--------------------------------
tyClDeclName :: TyClDecl name pat -> name
tyClDeclName tycl_decl = tcdName tycl_decl

--------------------------------
tyClDeclNames :: Eq name => TyClDecl name pat -> [(name, SrcLoc)]
-- Returns all the *binding* names of the decl, along with their SrcLocs
-- The first one is guaranteed to be the name of the decl
-- For record fields, the first one counts as the SrcLoc
-- We use the equality to filter out duplicate field names

tyClDeclNames (TySynonym   {tcdName = name, tcdLoc = loc})  = [(name,loc)]
tyClDeclNames (IfaceSig    {tcdName = name, tcdLoc = loc})  = [(name,loc)]
tyClDeclNames (ForeignType {tcdName = name, tcdLoc = loc})  = [(name,loc)]

tyClDeclNames (ClassDecl {tcdName = cls_name, tcdSigs = sigs, tcdLoc = loc})
  = (cls_name,loc) : [(n,loc) | ClassOpSig n _ _ loc <- sigs]

tyClDeclNames (TyData {tcdName = tc_name, tcdCons = cons, tcdLoc = loc})
  = (tc_name,loc) : conDeclsNames cons


tyClDeclTyVars (TySynonym {tcdTyVars = tvs}) = tvs
tyClDeclTyVars (TyData    {tcdTyVars = tvs}) = tvs
tyClDeclTyVars (ClassDecl {tcdTyVars = tvs}) = tvs
tyClDeclTyVars (ForeignType {})		     = []
tyClDeclTyVars (IfaceSig {})		     = []


--------------------------------
-- The "system names" are extra implicit names *bound* by the decl.
-- They are kept in a list rather than a tuple 
-- to make the renamer easier.

type ClassSysNames name = [name]
-- For class decls they are:
-- 	[tycon, datacon wrapper, datacon worker, 
--	 superclass selector 1, ..., superclass selector n]

type DataSysNames name =  [name]
-- For data decls they are
--	[from, to]
-- where from :: T -> Tring
--	 to   :: Tring -> T

tyClDeclSysNames :: TyClDecl name pat -> [(name, SrcLoc)]
-- Similar to tyClDeclNames, but returns the "implicit" 
-- or "system" names of the declaration

tyClDeclSysNames (ClassDecl {tcdSysNames = names, tcdLoc = loc})
  = [(n,loc) | n <- names]
tyClDeclSysNames (TyData {tcdCons = DataCons cons, tcdSysNames = names, tcdLoc = loc})
  = [(n,loc) | n <- names] ++ 
    [(wkr_name,loc) | ConDecl _ wkr_name _ _ _ loc <- cons]
tyClDeclSysNames decl = []


mkClassDeclSysNames  :: (name, name, name, [name]) -> [name]
getClassDeclSysNames :: [name] -> (name, name, name, [name])
mkClassDeclSysNames  (a,b,c,ds) = a:b:c:ds
getClassDeclSysNames (a:b:c:ds) = (a,b,c,ds)
\end{code}

\begin{code}
instance (NamedThing name, Ord name) => Eq (TyClDecl name pat) where
	-- Used only when building interface files
  (==) d1@(IfaceSig {}) d2@(IfaceSig {})
      = tcdName d1 == tcdName d2 && 
	tcdType d1 == tcdType d2 && 
	tcdIdInfo d1 == tcdIdInfo d2

  (==) d1@(ForeignType {}) d2@(ForeignType {})
      = tcdName d1 == tcdName d2 && 
	tcdFoType d1 == tcdFoType d2

  (==) d1@(TyData {}) d2@(TyData {})
      = tcdName d1 == tcdName d2 && 
	tcdND d1   == tcdND   d2 && 
	eqWithHsTyVars (tcdTyVars d1) (tcdTyVars d2) (\ env -> 
   	  eq_hsContext env (tcdCtxt d1) (tcdCtxt d2)  &&
	  eq_hsCD      env (tcdCons d1) (tcdCons d2)
	)

  (==) d1@(TySynonym {}) d2@(TySynonym {})
      = tcdName d1 == tcdName d2 && 
	eqWithHsTyVars (tcdTyVars d1) (tcdTyVars d2) (\ env -> 
          eq_hsType env (tcdSynRhs d1) (tcdSynRhs d2)
        )

  (==) d1@(ClassDecl {}) d2@(ClassDecl {})
    = tcdName d1 == tcdName d2 && 
      eqWithHsTyVars (tcdTyVars d1) (tcdTyVars d2) (\ env -> 
   	  eq_hsContext env (tcdCtxt d1) (tcdCtxt d2)  &&
	  eqListBy (eq_hsFD env) (tcdFDs d1) (tcdFDs d2) &&
	  eqListBy (eq_cls_sig env) (tcdSigs d1) (tcdSigs d2)
       )

  (==) _ _ = False	-- default case

eq_hsCD env (DataCons c1) (DataCons c2) = eqListBy (eq_ConDecl env) c1 c2
eq_hsCD env Unknown	  Unknown	= True
eq_hsCD env (HasCons n1)  (HasCons n2)  = n1 == n2
eq_hsCD env d1		  d2		= False

eq_hsFD env (ns1,ms1) (ns2,ms2)
  = eqListBy (eq_hsVar env) ns1 ns2 && eqListBy (eq_hsVar env) ms1 ms2

eq_cls_sig env (ClassOpSig n1 dm1 ty1 _) (ClassOpSig n2 dm2 ty2 _)
  = n1==n2 && dm1 `eq_dm` dm2 && eq_hsType env ty1 ty2
  where
	-- Ignore the name of the default method for (DefMeth id)
	-- This is used for comparing declarations before putting
	-- them into interface files, and the name of the default 
	-- method isn't relevant
    NoDefMeth  `eq_dm` NoDefMeth  = True
    GenDefMeth `eq_dm` GenDefMeth = True
    DefMeth _  `eq_dm` DefMeth _  = True
    dm1	       `eq_dm` dm2	  = False

    
\end{code}

\begin{code}
countTyClDecls :: [TyClDecl name pat] -> (Int, Int, Int, Int, Int)
	-- class, data, newtype, synonym decls
countTyClDecls decls 
 = (count isClassDecl     decls,
    count isSynDecl       decls,
    count isIfaceSigDecl  decls,
    count isDataTy        decls,
    count isNewTy         decls) 
 where
   isDataTy TyData{tcdND=DataType} = True
   isDataTy _                      = False
   
   isNewTy TyData{tcdND=NewType} = True
   isNewTy _                     = False
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat)
	      => Outputable (TyClDecl name pat) where

    ppr (IfaceSig {tcdName = var, tcdType = ty, tcdIdInfo = info})
	= getPprStyle $ \ sty ->
	   hsep [ ppr_var var, dcolon, ppr ty, pprHsIdInfo info ]

    ppr (ForeignType {tcdName = tycon})
	= hsep [ptext SLIT("foreign import type dotnet"), ppr tycon]

    ppr (TySynonym {tcdName = tycon, tcdTyVars = tyvars, tcdSynRhs = mono_ty})
      = hang (ptext SLIT("type") <+> pp_decl_head [] tycon tyvars <+> equals)
	     4 (ppr mono_ty)

    ppr (TyData {tcdND = new_or_data, tcdCtxt = context, tcdName = tycon,
		 tcdTyVars = tyvars, tcdCons = condecls, 
		 tcdDerivs = derivings})
      = pp_tydecl (ptext keyword <+> pp_decl_head context tycon tyvars)
		  (pp_condecls condecls)
		  derivings
      where
	keyword = case new_or_data of
			NewType  -> SLIT("newtype")
			DataType -> SLIT("data")

    ppr (ClassDecl {tcdCtxt = context, tcdName = clas, tcdTyVars = tyvars, tcdFDs = fds,
		    tcdSigs = sigs, tcdMeths = methods})
      | null sigs	-- No "where" part
      = top_matter

      | otherwise	-- Laid out
      = sep [hsep [top_matter, ptext SLIT("where {")],
	     nest 4 (sep [sep (map ppr_sig sigs), pp_methods, char '}'])]
      where
        top_matter  = ptext SLIT("class") <+> pp_decl_head context clas tyvars <+> pprFundeps fds
	ppr_sig sig = ppr sig <> semi

	pp_methods = if isNothing methods
			then empty
			else ppr (fromJust methods)
        
pp_decl_head :: Outputable name => HsContext name -> name -> [HsTyVarBndr name] -> SDoc
pp_decl_head context thing tyvars = hsep [pprHsContext context, ppr thing, interppSP tyvars]

pp_condecls Unknown	  = ptext SLIT("{- abstract -}")
pp_condecls (HasCons n)   = ptext SLIT("{- abstract with") <+> int n <+> ptext SLIT("constructors -}")
pp_condecls (DataCons cs) = equals <+> sep (punctuate (ptext SLIT(" |")) (map ppr cs))

pp_tydecl pp_head pp_decl_rhs derivings
  = hang pp_head 4 (sep [
	pp_decl_rhs,
	case derivings of
	  Nothing 	   -> empty
	  Just ds	   -> hsep [ptext SLIT("deriving"), ppr_hs_context ds]
    ])
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

		[HsTyVarBndr name]	-- Existentially quantified type variables
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
\end{code}

\begin{code}
conDeclsNames :: Eq name => DataConDetails (ConDecl name) -> [(name,SrcLoc)]
  -- See tyClDeclNames for what this does
  -- The function is boringly complicated because of the records
  -- And since we only have equality, we have to be a little careful
conDeclsNames cons
  = snd (foldl do_one ([], []) (visibleDataCons cons))
  where
    do_one (flds_seen, acc) (ConDecl name _ _ _ details loc)
	= do_details ((name,loc):acc) details
	where
	  do_details acc (RecCon flds) = foldl do_fld (flds_seen, acc) flds
	  do_details acc other	       = (flds_seen, acc)

	  do_fld acc (flds, _) = foldl do_fld1 acc flds

	  do_fld1 (flds_seen, acc) fld
		| fld `elem` flds_seen = (flds_seen,acc)
		| otherwise	       = (fld:flds_seen, (fld,loc):acc)
\end{code}

\begin{code}
conDetailsTys :: ConDetails name -> [HsType name]
conDetailsTys (VanillaCon btys)    = map getBangType btys
conDetailsTys (InfixCon bty1 bty2) = [getBangType bty1, getBangType bty2]
conDetailsTys (RecCon fields)	   = [getBangType bty | (_, bty) <- fields]


eq_ConDecl env (ConDecl n1 _ tvs1 cxt1 cds1 _)
	       (ConDecl n2 _ tvs2 cxt2 cds2 _)
  = n1 == n2 &&
    (eq_hsTyVars env tvs1 tvs2	$ \ env ->
     eq_hsContext env cxt1 cxt2	&&
     eq_ConDetails env cds1 cds2)

eq_ConDetails env (VanillaCon bts1) (VanillaCon bts2)
  = eqListBy (eq_btype env) bts1 bts2
eq_ConDetails env (InfixCon bta1 btb1) (InfixCon bta2 btb2)
  = eq_btype env bta1 bta2 && eq_btype env btb1 btb2
eq_ConDetails env (RecCon fs1) (RecCon fs2)
  = eqListBy (eq_fld env) fs1 fs2
eq_ConDetails env _ _ = False

eq_fld env (ns1,bt1) (ns2, bt2) = ns1==ns2 && eq_btype env bt1 bt2
\end{code}
  
\begin{code}
data BangType name = BangType StrictnessMark (HsType name)

getBangType       (BangType _ ty) = ty
getBangStrictness (BangType s _)  = s

unbangedType ty = BangType NotMarkedStrict ty

eq_btype env (BangType s1 t1) (BangType s2 t2) = s1==s2 && eq_hsType env t1 t2
\end{code}

\begin{code}
instance (Outputable name) => Outputable (ConDecl name) where
    ppr (ConDecl con _ tvs cxt con_details  loc)
      = sep [pprHsForAll tvs cxt, ppr_con_details con con_details]

ppr_con_details con (InfixCon ty1 ty2)
  = hsep [ppr_bang ty1, ppr con, ppr_bang ty2]

-- ConDecls generated by MkIface.ifaceTyThing always have a VanillaCon, even
-- if the constructor is an infix one.  This is because in an interface file
-- we don't distinguish between the two.  Hence when printing these for the
-- user, we need to parenthesise infix constructor names.
ppr_con_details con (VanillaCon tys)
  = hsep (ppr_var con : map (ppr_bang) tys)

ppr_con_details con (RecCon fields)
  = ppr con <+> braces (sep (punctuate comma (map ppr_field fields)))
  where
    ppr_field (ns, ty) = hsep (map (ppr) ns) <+> 
			 dcolon <+>
			 ppr_bang ty

instance Outputable name => Outputable (BangType name) where
    ppr = ppr_bang

ppr_bang (BangType s ty) = ppr s <> pprParendHsType ty
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
					-- Nothing for source-file instance decls

		SrcLoc

isSourceInstDecl :: InstDecl name pat -> Bool
isSourceInstDecl (InstDecl _ _ _ maybe_dfun _) = isNothing maybe_dfun
\end{code}

\begin{code}
instance (Outputable name, Outputable pat)
	      => Outputable (InstDecl name pat) where

    ppr (InstDecl inst_ty binds uprags maybe_dfun_name src_loc)
      = vcat [hsep [ptext SLIT("instance"), ppr inst_ty, ptext SLIT("where")],
	      nest 4 (ppr uprags),
	      nest 4 (ppr binds) ]
      where
	pp_dfun = case maybe_dfun_name of
		    Just df -> ppr df
		    Nothing -> empty
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

-- foreign declarations are distinguished as to whether they define or use a
-- Haskell name
--
-- * the Boolean value indicates whether the pre-standard deprecated syntax
--   has been used
--
data ForeignDecl name
  = ForeignImport name (HsType name) ForeignImport Bool SrcLoc  -- defines name
  | ForeignExport name (HsType name) ForeignExport Bool SrcLoc  -- uses name

-- yield the Haskell name defined or used in a foreign declaration
--
foreignDeclName                           :: ForeignDecl name -> name
foreignDeclName (ForeignImport n _ _ _ _)  = n
foreignDeclName (ForeignExport n _ _ _ _)  = n

-- specification of an imported external entity in dependence on the calling
-- convention 
--
data ForeignImport = -- import of a C entity
		     --
                     -- * the two strings specifying a header file or library
                     --   may be empty, which indicates the absence of a
                     --   header or object specification (both are not used
                     --   in the case of `CWrapper' and when `CFunction'
                     --   has a dynamic target)
		     --
		     -- * the calling convention is irrelevant for code
		     --   generation in the case of `CLabel', but is needed
		     --   for pretty printing 
		     --
		     -- * `Safety' is irrelevant for `CLabel' and `CWrapper'
		     --
		     CImport  CCallConv	      -- ccall or stdcall
			      Safety	      -- safe or unsafe
			      FastString      -- name of C header
			      FastString      -- name of library object
			      CImportSpec     -- details of the C entity

                     -- import of a .NET function
		     --
		   | DNImport DNCallSpec

-- details of an external C entity
--
data CImportSpec = CLabel    CLabelString     -- import address of a C label
		 | CFunction CCallTarget      -- static or dynamic function
		 | CWrapper		      -- wrapper to expose closures
					      -- (former f.e.d.)

-- specification of an externally exported entity in dependence on the calling
-- convention
--
data ForeignExport = CExport  CExportSpec    -- contains the calling convention
		   | DNExport		     -- presently unused

-- abstract type imported from .NET
--
data FoType = DNType 		-- In due course we'll add subtype stuff
	    deriving (Eq)	-- Used for equality instance for TyClDecl


-- pretty printing of foreign declarations
--

instance Outputable name => Outputable (ForeignDecl name) where
  ppr (ForeignImport n ty fimport _ _) =
    ptext SLIT("foreign import") <+> ppr fimport <+> 
    ppr n <+> dcolon <+> ppr ty
  ppr (ForeignExport n ty fexport _ _) =
    ptext SLIT("foreign export") <+> ppr fexport <+> 
    ppr n <+> dcolon <+> ppr ty

instance Outputable ForeignImport where
  ppr (DNImport			        spec) = 
    ptext SLIT("dotnet") <+> ppr spec
  ppr (CImport  cconv safety header lib spec) =
    ppr cconv <+> ppr safety <+> 
    char '"' <> pprCEntity header lib spec <> char '"'
    where
      pprCEntity header lib (CLabel lbl) = 
        ptext SLIT("static") <+> ptext header <+> char '&' <>
	pprLib lib <> ppr lbl
      pprCEntity header lib (CFunction (StaticTarget lbl)) = 
        ptext SLIT("static") <+> ptext header <+> char '&' <>
	pprLib lib <> ppr lbl
      pprCEntity header lib (CFunction (DynamicTarget)) = 
        ptext SLIT("dynamic")
      pprCEntity header lib (CFunction (CasmTarget _)) = 
        panic "HsDecls.pprCEntity: malformed C function target"
      pprCEntity _      _   (CWrapper) = ptext SLIT("wrapper")
      --
      pprLib lib | nullFastString lib = empty
		 | otherwise	      = char '[' <> ppr lib <> char ']'

instance Outputable ForeignExport where
  ppr (CExport  (CExportStatic lbl cconv)) = 
    ppr cconv <+> char '"' <> ppr lbl <> char '"'
  ppr (DNExport                          ) = 
    ptext SLIT("dotnet") <+> ptext SLIT("\"<unused>\"")

instance Outputable FoType where
  ppr DNType = ptext SLIT("type dotnet")
\end{code}


%************************************************************************
%*									*
\subsection{Transformation rules}
%*									*
%************************************************************************

\begin{code}
data RuleDecl name pat
  = HsRule			-- Source rule
	RuleName		-- Rule name
	Activation
	[RuleBndr name]		-- Forall'd vars; after typechecking this includes tyvars
	(HsExpr name pat)	-- LHS
	(HsExpr name pat)	-- RHS
	SrcLoc		

  | IfaceRule	 		-- One that's come in from an interface file; pre-typecheck
	RuleName
	Activation
	[UfBinder name]		-- Tyvars and term vars
	name			-- Head of lhs
	[UfExpr name]		-- Args of LHS
	(UfExpr name)		-- Pre typecheck
	SrcLoc		

  | IfaceRuleOut		-- Post typecheck
	name			-- Head of LHS
	CoreRule

ifaceRuleDeclName :: RuleDecl name pat -> name
ifaceRuleDeclName (IfaceRule _ _ _ n _ _ _) = n
ifaceRuleDeclName (IfaceRuleOut n r)	    = n
ifaceRuleDeclName (HsRule fs _ _ _ _ _)     = pprPanic "ifaceRuleDeclName" (ppr fs)

data RuleBndr name
  = RuleBndr name
  | RuleBndrSig name (HsType name)

collectRuleBndrSigTys :: [RuleBndr name] -> [HsType name]
collectRuleBndrSigTys bndrs = [ty | RuleBndrSig _ ty <- bndrs]

instance (NamedThing name, Ord name) => Eq (RuleDecl name pat) where
  -- Works for IfaceRules only; used when comparing interface file versions
  (IfaceRule n1 a1 bs1 f1 es1 rhs1 _) == (IfaceRule n2 a2 bs2 f2 es2 rhs2 _)
     = n1==n2 && f1 == f2 && a1==a2 &&
       eq_ufBinders emptyEqHsEnv bs1 bs2 (\env -> 
       eqListBy (eq_ufExpr env) (rhs1:es1) (rhs2:es2))

instance (NamedThing name, Outputable name, Outputable pat)
	      => Outputable (RuleDecl name pat) where
  ppr (HsRule name act ns lhs rhs loc)
	= sep [text "{-# RULES" <+> doubleQuotes (ptext name) <+> ppr act,
	       pp_forall, ppr lhs, equals <+> ppr rhs,
               text "#-}" ]
	where
	  pp_forall | null ns   = empty
		    | otherwise	= text "forall" <+> fsep (map ppr ns) <> dot

  ppr (IfaceRule name act tpl_vars fn tpl_args rhs loc) 
    = hsep [ doubleQuotes (ptext name), ppr act,
	   ptext SLIT("__forall") <+> braces (interppSP tpl_vars),
	   ppr fn <+> sep (map (pprUfExpr parens) tpl_args),
	   ptext SLIT("=") <+> ppr rhs
      ] <+> semi

  ppr (IfaceRuleOut fn rule) = pprCoreRule (ppr fn) rule

instance Outputable name => Outputable (RuleBndr name) where
   ppr (RuleBndr name) = ppr name
   ppr (RuleBndrSig name ty) = ppr name <> dcolon <> ppr ty
\end{code}


%************************************************************************
%*									*
\subsection[DeprecDecl]{Deprecations}
%*									*
%************************************************************************

We use exported entities for things to deprecate.

\begin{code}
data DeprecDecl name = Deprecation name DeprecTxt SrcLoc

type DeprecTxt = FAST_STRING	-- reason/explanation for deprecation

instance Outputable name => Outputable (DeprecDecl name) where
    ppr (Deprecation thing txt _)
      = hsep [text "{-# DEPRECATED", ppr thing, doubleQuotes (ppr txt), text "#-}"]
\end{code}
