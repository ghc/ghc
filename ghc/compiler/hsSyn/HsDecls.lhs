%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsDecls]{Abstract syntax: global declarations}

Definitions for: @TyDecl@ and @oCnDecl@, @ClassDecl@,
@InstDecl@, @DefaultDecl@ and @ForeignDecl@.

\begin{code}
module HsDecls (
	HsDecl(..), TyClDecl(..), InstDecl(..), RuleDecl(..), RuleBndr(..),
	DefaultDecl(..), ForeignDecl(..), ForKind(..),
	ExtName(..), isDynamicExtName, extNameStatic,
	ConDecl(..), ConDetails(..), 
	BangType(..), getBangType, getBangStrictness, unbangedType,
	DeprecDecl(..), DeprecTxt,
	hsDeclName, instDeclName, tyClDeclName, tyClDeclNames, tyClDeclSysNames,
	isClassDecl, isSynDecl, isDataDecl, isIfaceSigDecl, countTyClDecls,
	mkClassDeclSysNames, isIfaceRuleDecl, ifaceRuleDeclName,
	getClassDeclSysNames, conDetailsTys
    ) where

#include "HsVersions.h"

-- friends:
import HsBinds		( HsBinds, MonoBinds, Sig(..), FixitySig(..) )
import HsExpr		( HsExpr )
import HsTypes
import PprCore		( pprCoreRule )
import HsCore		( UfExpr, UfBinder, HsIdInfo, pprHsIdInfo,
			  eq_ufBinders, eq_ufExpr, pprUfExpr 
			)
import CoreSyn		( CoreRule(..) )
import BasicTypes	( NewOrData(..) )
import Demand		( StrictnessMark(..) )
import ForeignCall	( CCallConv )

-- others:
import ForeignCall	( Safety )
import Name		( NamedThing )
import FunDeps		( pprFundeps )
import Class		( FunDep, DefMeth(..) )
import CStrings		( CLabelString, pprCLabelString )
import Outputable	
import SrcLoc		( SrcLoc )
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
hsDeclName (TyClD decl)				    = tyClDeclName decl
hsDeclName (InstD   decl)			    = instDeclName decl
hsDeclName (ForD    (ForeignDecl name _ _ _ _ _))   = name
hsDeclName (FixD    (FixitySig name _ _))	    = name
-- Others don't make sense
#ifdef DEBUG
hsDeclName x				      = pprPanic "HsDecls.hsDeclName" (ppr x)
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

  - The CoreTidy phase globalises the name, and ensures the occurrence name is
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
data TyClDecl name pat
  = IfaceSig {	tcdName :: name,		-- It may seem odd to classify an interface-file signature
		tcdType :: HsType name,		-- as a 'TyClDecl', but it's very convenient.  These three
		tcdIdInfo :: [HsIdInfo name],	-- are the kind that appear in interface files.
		tcdLoc :: SrcLoc
    }

  | TyData {	tcdND     :: NewOrData,
		tcdCtxt   :: HsContext name,	 -- context
		tcdName   :: name,		 -- type constructor
		tcdTyVars :: [HsTyVarBndr name], -- type variables
		tcdCons	  :: [ConDecl name],	 -- data constructors (empty if abstract)
		tcdNCons  :: Int,		 -- Number of data constructors (valid even if type is abstract)
		tcdDerivs :: Maybe [name],	 -- derivings; Nothing => not specified
				 -- (i.e., derive default); Just [] => derive
				 -- *nothing*; Just <list> => as you would
				 -- expect...
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

tyClDeclNames (TySynonym {tcdName = name, tcdLoc = loc})  = [(name,loc)]
tyClDeclNames (IfaceSig  {tcdName = name, tcdLoc = loc})  = [(name,loc)]

tyClDeclNames (ClassDecl {tcdName = cls_name, tcdSigs = sigs, tcdLoc = loc})
  = (cls_name,loc) : [(n,loc) | ClassOpSig n _ _ loc <- sigs]

tyClDeclNames (TyData {tcdName = tc_name, tcdCons = cons, tcdLoc = loc})
  = (tc_name,loc) : conDeclsNames cons


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
tyClDeclSysNames (TyData {tcdCons = cons, tcdSysNames = names, tcdLoc = loc})
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

  (==) d1@(TyData {}) d2@(TyData {})
      = tcdName d1 == tcdName d2 && 
	tcdND d1   == tcdND   d2 && 
	eqWithHsTyVars (tcdTyVars d1) (tcdTyVars d2) (\ env -> 
   	  eq_hsContext env (tcdCtxt d1) (tcdCtxt d2)  &&
	  eqListBy (eq_ConDecl env) (tcdCons d1) (tcdCons d2)
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
 = (length [() | ClassDecl {} <- decls],
    length [() | TySynonym {} <- decls],
    length [() | IfaceSig  {} <- decls],
    length [() | TyData {tcdND = DataType} <- decls],
    length [() | TyData {tcdND = NewType} <- decls])
\end{code}

\begin{code}
instance (NamedThing name, Outputable name, Outputable pat)
	      => Outputable (TyClDecl name pat) where

    ppr (IfaceSig {tcdName = var, tcdType = ty, tcdIdInfo = info})
	= hsep [ppr var, dcolon, ppr ty, pprHsIdInfo info]

    ppr (TySynonym {tcdName = tycon, tcdTyVars = tyvars, tcdSynRhs = mono_ty})
      = hang (ptext SLIT("type") <+> pp_decl_head [] tycon tyvars <+> equals)
	     4 (ppr mono_ty)

    ppr (TyData {tcdND = new_or_data, tcdCtxt = context, tcdName = tycon,
		 tcdTyVars = tyvars, tcdCons = condecls, tcdNCons = ncons,
		 tcdDerivs = derivings})
      = pp_tydecl (ptext keyword <+> pp_decl_head context tycon tyvars <+> equals)
		  (pp_condecls condecls ncons)
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
conDeclsNames :: Eq name => [ConDecl name] -> [(name,SrcLoc)]
  -- See tyClDeclNames for what this does
  -- The function is boringly complicated because of the records
  -- And since we only have equality, we have to be a little careful
conDeclsNames cons
  = snd (foldl do_one ([], []) cons)
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

ppr_con_details con (VanillaCon tys)
  = ppr con <+> hsep (map (ppr_bang) tys)

ppr_con_details con (RecCon fields)
  = ppr con <+> braces (hsep (punctuate comma (map ppr_field fields)))
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
\end{code}

\begin{code}
instance (Outputable name, Outputable pat)
	      => Outputable (InstDecl name pat) where

    ppr (InstDecl inst_ty binds uprags maybe_dfun_name src_loc)
      = getPprStyle $ \ sty ->
        if ifaceStyle sty then
           hsep [ptext SLIT("instance"), ppr inst_ty, equals, pp_dfun]
	else
	   vcat [hsep [ptext SLIT("instance"), ppr inst_ty, ptext SLIT("where")],
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
data ForeignDecl name = 
   ForeignDecl 
        name 
	ForKind   
	(HsType name)
	ExtName
	CCallConv
	SrcLoc

instance (Outputable name)
	      => Outputable (ForeignDecl name) where

    ppr (ForeignDecl nm imp_exp ty ext_name cconv src_loc)
      = ptext SLIT("foreign") <+> ppr_imp_exp <+> ppr cconv <+> 
        ppr ext_name <+> ppr_unsafe <+> ppr nm <+> dcolon <+> ppr ty
        where
         (ppr_imp_exp, ppr_unsafe) =
	   case imp_exp of
	     FoLabel     -> (ptext SLIT("label"), empty)
	     FoExport    -> (ptext SLIT("export"), empty)
	     FoImport us -> (ptext SLIT("import"), ppr us)

data ForKind
 = FoLabel
 | FoExport
 | FoImport Safety

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

isIfaceRuleDecl (HsRule _ _ _ _ _ _) = False
isIfaceRuleDecl other		     = True

ifaceRuleDeclName :: RuleDecl name pat -> name
ifaceRuleDeclName (IfaceRule _ _ n _ _ _) = n
ifaceRuleDeclName (IfaceRuleOut n r)	  = n
ifaceRuleDeclName (HsRule fs _ _ _ _ _)   = pprPanic "ifaceRuleDeclName" (ppr fs)

data RuleBndr name
  = RuleBndr name
  | RuleBndrSig name (HsType name)

instance (NamedThing name, Ord name) => Eq (RuleDecl name pat) where
  -- Works for IfaceRules only; used when comparing interface file versions
  (IfaceRule n1 bs1 f1 es1 rhs1 _) == (IfaceRule n2 bs2 f2 es2 rhs2 _)
     = n1==n2 && f1 == f2 && 
       eq_ufBinders emptyEqHsEnv bs1 bs2 (\env -> 
       eqListBy (eq_ufExpr env) (rhs1:es1) (rhs2:es2))

instance (NamedThing name, Outputable name, Outputable pat)
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
