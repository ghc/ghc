%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

HsDecls: Abstract syntax: global declarations

Definitions for: @TyDecl@ and @oCnDecl@, @ClassDecl@,
@InstDecl@, @DefaultDecl@ and @ForeignDecl@.

\begin{code}
module HsDecls (
	HsDecl(..), LHsDecl, TyClDecl(..), LTyClDecl, 
	InstDecl(..), LInstDecl, DerivDecl(..), LDerivDecl, NewOrData(..),
	FamilyFlavour(..),
	RuleDecl(..), LRuleDecl, RuleBndr(..),
	DefaultDecl(..), LDefaultDecl, SpliceDecl(..),
	ForeignDecl(..), LForeignDecl, ForeignImport(..), ForeignExport(..),
	CImportSpec(..), FoType(..),
	ConDecl(..), ResType(..), LConDecl,	
	DocDecl(..), LDocDecl, docDeclDoc,
	DeprecDecl(..),  LDeprecDecl,
	HsGroup(..),  emptyRdrGroup, emptyRnGroup, appendGroups,
	tcdName, tyClDeclNames, tyClDeclTyVars,
	isClassDecl, isSynDecl, isDataDecl, isTypeDecl, isFamilyDecl,
	isFamInstDecl, 
	countTyClDecls,
	conDetailsTys,
	instDeclATs,
	collectRuleBndrSigTys, 
    ) where

#include "HsVersions.h"

-- friends:
import {-# SOURCE #-}	HsExpr( HsExpr, pprExpr )
	-- Because Expr imports Decls via HsBracket

import HsBinds
import HsPat
import HsImpExp
import HsTypes
import HsDoc
import NameSet
import CoreSyn
import {- Kind parts of -} Type
import BasicTypes
import ForeignCall

-- others:
import Class
import Outputable	
import Util
import SrcLoc
import FastString

import Data.Maybe       ( isJust )
\end{code}

%************************************************************************
%*									*
\subsection[HsDecl]{Declarations}
%*									*
%************************************************************************

\begin{code}
type LHsDecl id = Located (HsDecl id)

data HsDecl id
  = TyClD	(TyClDecl id)
  | InstD	(InstDecl  id)
  | DerivD      (DerivDecl id)
  | ValD	(HsBind id)
  | SigD	(Sig id)
  | DefD	(DefaultDecl id)
  | ForD        (ForeignDecl id)
  | DeprecD	(DeprecDecl id)
  | RuleD	(RuleDecl id)
  | SpliceD	(SpliceDecl id)
  | DocD	(DocDecl id)


-- NB: all top-level fixity decls are contained EITHER
-- EITHER SigDs
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

-- A [HsDecl] is categorised into a HsGroup before being 
-- fed to the renamer.
data HsGroup id
  = HsGroup {
	hs_valds  :: HsValBinds id,
	hs_tyclds :: [LTyClDecl id],
	hs_instds :: [LInstDecl id],
        hs_derivds :: [LDerivDecl id],

	hs_fixds  :: [LFixitySig id],
		-- Snaffled out of both top-level fixity signatures,
		-- and those in class declarations

	hs_defds  :: [LDefaultDecl id],
	hs_fords  :: [LForeignDecl id],
	hs_depds  :: [LDeprecDecl id],
	hs_ruleds :: [LRuleDecl id],

	hs_docs   :: [LDocDecl id]
  }

emptyGroup, emptyRdrGroup, emptyRnGroup :: HsGroup a
emptyRdrGroup = emptyGroup { hs_valds = emptyValBindsIn }
emptyRnGroup  = emptyGroup { hs_valds = emptyValBindsOut }

emptyGroup = HsGroup { hs_tyclds = [], hs_instds = [], hs_derivds = [],
		       hs_fixds = [], hs_defds = [], hs_fords = [], 
		       hs_depds = [], hs_ruleds = [],
		       hs_valds = error "emptyGroup hs_valds: Can't happen",
                       hs_docs = [] }

appendGroups :: HsGroup a -> HsGroup a -> HsGroup a
appendGroups 
    HsGroup { 
	hs_valds  = val_groups1,
	hs_tyclds = tyclds1, 
	hs_instds = instds1,
        hs_derivds = derivds1,
	hs_fixds  = fixds1, 
	hs_defds  = defds1,
	hs_fords  = fords1, 
	hs_depds  = depds1,
	hs_ruleds = rulds1,
  hs_docs   = docs1 }
    HsGroup { 
	hs_valds  = val_groups2,
	hs_tyclds = tyclds2, 
	hs_instds = instds2,
        hs_derivds = derivds2,
	hs_fixds  = fixds2, 
	hs_defds  = defds2,
	hs_fords  = fords2, 
	hs_depds  = depds2,
	hs_ruleds = rulds2,
  hs_docs   = docs2 }
  = 
    HsGroup { 
	hs_valds  = val_groups1 `plusHsValBinds` val_groups2,
	hs_tyclds = tyclds1 ++ tyclds2, 
	hs_instds = instds1 ++ instds2,
        hs_derivds = derivds1 ++ derivds2,
	hs_fixds  = fixds1 ++ fixds2, 
	hs_defds  = defds1 ++ defds2,
	hs_fords  = fords1 ++ fords2, 
	hs_depds  = depds1 ++ depds2,
	hs_ruleds = rulds1 ++ rulds2,
  hs_docs   = docs1  ++ docs2 }
\end{code}

\begin{code}
instance OutputableBndr name => Outputable (HsDecl name) where
    ppr (TyClD dcl)             = ppr dcl
    ppr (ValD binds)            = ppr binds
    ppr (DefD def)              = ppr def
    ppr (InstD inst)            = ppr inst
    ppr (DerivD deriv)          = ppr deriv
    ppr (ForD fd)               = ppr fd
    ppr (SigD sd)               = ppr sd
    ppr (RuleD rd)              = ppr rd
    ppr (DeprecD dd)            = ppr dd
    ppr (SpliceD dd)            = ppr dd
    ppr (DocD doc)              = ppr doc

instance OutputableBndr name => Outputable (HsGroup name) where
    ppr (HsGroup { hs_valds  = val_decls,
		   hs_tyclds = tycl_decls,
		   hs_instds = inst_decls,
                   hs_derivds = deriv_decls,
		   hs_fixds  = fix_decls,
		   hs_depds  = deprec_decls,
		   hs_fords  = foreign_decls,
		   hs_defds  = default_decls,
		   hs_ruleds = rule_decls })
	= vcat [ppr_ds fix_decls, ppr_ds default_decls, 
		ppr_ds deprec_decls, ppr_ds rule_decls,
		ppr val_decls,
		ppr_ds tycl_decls, ppr_ds inst_decls,
                ppr_ds deriv_decls,
		ppr_ds foreign_decls]
	where
	  ppr_ds [] = empty
	  ppr_ds ds = text "" $$ vcat (map ppr ds)

data SpliceDecl id = SpliceDecl (Located (HsExpr id))	-- Top level splice

instance OutputableBndr name => Outputable (SpliceDecl name) where
   ppr (SpliceDecl e) = ptext SLIT("$") <> parens (pprExpr (unLoc e))
\end{code}


%************************************************************************
%*									*
\subsection[TyDecl]{@data@, @newtype@ or @type@ (synonym) type declaration}
%*									*
%************************************************************************

		--------------------------------
			THE NAMING STORY
		--------------------------------

Here is the story about the implicit names that go with type, class,
and instance decls.  It's a bit tricky, so pay attention!

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

All have occurrence names that are derived uniquely from their parent
declaration.

None of these get separate definitions in an interface file; they are
fully defined by the data or class decl.  But they may *occur* in
interface files, of course.  Any such occurrence must haul in the
relevant type or class decl.

Plan of attack:
 - Ensure they "point to" the parent data/class decl 
   when loading that decl from an interface file
   (See RnHiFiles.getSysBinders)

 - When typechecking the decl, we build the implicit TyCons and Ids.
   When doing so we look them up in the name cache (RnEnv.lookupSysName),
   to ensure correct module and provenance is set

These are the two places that we have to conjure up the magic derived
names.  (The actual magic is in OccName.mkWorkerOcc, etc.)

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
-- Representation of indexed types
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Family kind signatures are represented by the variant `TyFamily'.  It
-- covers "type family", "newtype family", and "data family" declarations,
-- distinguished by the value of the field `tcdFlavour'.
--
-- Indexed types are represented by 'TyData' and 'TySynonym' using the field
-- 'tcdTyPats::Maybe [LHsType name]', with the following meaning:
--
--   * If it is 'Nothing', we have a *vanilla* data type declaration or type
--     synonym declaration and 'tcdVars' contains the type parameters of the
--     type constructor.
--
--   * If it is 'Just pats', we have the definition of an indexed type.  Then,
--     'pats' are type patterns for the type-indexes of the type constructor
--     and 'tcdVars' are the variables in those patterns.  Hence, the arity of
--     the indexed type (ie, the number of indexes) is 'length tcdTyPats' and
--     *not* 'length tcdVars'.
--
-- In both cases, 'tcdVars' collects all variables we need to quantify over.

type LTyClDecl name = Located (TyClDecl name)

data TyClDecl name
  = ForeignType { 
		tcdLName    :: Located name,
		tcdExtName  :: Maybe FastString,
		tcdFoType   :: FoType
    }

  | TyFamily {  tcdFlavour:: FamilyFlavour,	        -- type, new, or data
		tcdLName  :: Located name,	        -- type constructor
		tcdTyVars :: [LHsTyVarBndr name],	-- type variables
		tcdKind   :: Maybe Kind			-- result kind
    }

  | TyData {	tcdND     :: NewOrData,
		tcdCtxt   :: LHsContext name,	 	-- Context
		tcdLName  :: Located name,	 	-- Type constructor

		tcdTyVars :: [LHsTyVarBndr name], 	-- Type variables
			
		tcdTyPats :: Maybe [LHsType name],	-- Type patterns
			-- Just [t1..tn] for data instance T t1..tn = ...
			--	in this case tcdTyVars = fv( tcdTyPats )
			-- Nothing for everything else

		tcdKindSig:: Maybe Kind,		-- Optional kind sig 
			-- (Just k) for a GADT-style 'data', or 'data
			-- instance' decl with explicit kind sig

		tcdCons	  :: [LConDecl name],	 	-- Data constructors
			-- For data T a = T1 | T2 a          the LConDecls all have ResTyH98
			-- For data T a where { T1 :: T a }  the LConDecls all have ResTyGADT

		tcdDerivs :: Maybe [LHsType name]
			-- Derivings; Nothing => not specified
			-- 	      Just [] => derive exactly what is asked
			-- These "types" must be of form
			--	forall ab. C ty1 ty2
			-- Typically the foralls and ty args are empty, but they
			-- are non-empty for the newtype-deriving case
    }
	-- data instance: tcdPats = Just tys
	--
	-- data:	  tcdPats = Nothing, 

  | TySynonym {	tcdLName  :: Located name,	        -- type constructor
		tcdTyVars :: [LHsTyVarBndr name],	-- type variables
		tcdTyPats :: Maybe [LHsType name],	-- Type patterns
			-- See comments for tcdTyPats in TyData
			-- 'Nothing' => vanilla type synonym

		tcdSynRhs :: LHsType name	        -- synonym expansion
    }

  | ClassDecl {	tcdCtxt    :: LHsContext name, 	 	-- Context...
		tcdLName   :: Located name,	    	-- Name of the class
		tcdTyVars  :: [LHsTyVarBndr name],	-- Class type variables
		tcdFDs     :: [Located (FunDep name)],	-- Functional deps
		tcdSigs    :: [LSig name],		-- Methods' signatures
		tcdMeths   :: LHsBinds name,		-- Default methods
		tcdATs	   :: [LTyClDecl name],		-- Associated types; ie
							--   only 'TyData',
							--   'TyFunction',
							--   and 'TySynonym'
		tcdDocs    :: [LDocDecl name]		-- Haddock docs
    }

data NewOrData
  = NewType			-- "newtype Blah ..."
  | DataType			-- "data Blah ..."
  deriving( Eq )		-- Needed because Demand derives Eq

data FamilyFlavour
  = TypeFamily			-- "type family ..."
  | DataFamily NewOrData	-- "newtype family ..." or "data family ..."
\end{code}

Simple classifiers

\begin{code}
isDataDecl, isTypeDecl, isSynDecl, isClassDecl, isFamilyDecl, isFamInstDecl :: 
  TyClDecl name -> Bool

-- data/newtype or data/newtype instance declaration
isDataDecl (TyData {}) = True
isDataDecl _other      = False

-- type or type instance declaration
isTypeDecl (TySynonym {}) = True
isTypeDecl _other	  = False

-- vanilla Haskell type synonym (ie, not a type instance)
isSynDecl (TySynonym {tcdTyPats = Nothing}) = True
isSynDecl _other	                    = False

-- type class
isClassDecl (ClassDecl {}) = True
isClassDecl other	   = False

-- type family declaration
isFamilyDecl (TyFamily {}) = True
isFamilyDecl _other        = False

-- family instance (types, newtypes, and data types)
isFamInstDecl tydecl
   | isTypeDecl tydecl
     || isDataDecl tydecl = isJust (tcdTyPats tydecl)
   | otherwise	          = False
\end{code}

Dealing with names

\begin{code}
tcdName :: TyClDecl name -> name
tcdName decl = unLoc (tcdLName decl)

tyClDeclNames :: Eq name => TyClDecl name -> [Located name]
-- Returns all the *binding* names of the decl, along with their SrcLocs
-- The first one is guaranteed to be the name of the decl
-- For record fields, the first one counts as the SrcLoc
-- We use the equality to filter out duplicate field names

tyClDeclNames (TyFamily    {tcdLName = name})    = [name]
tyClDeclNames (TySynonym   {tcdLName = name})    = [name]
tyClDeclNames (ForeignType {tcdLName = name})    = [name]

tyClDeclNames (ClassDecl {tcdLName = cls_name, tcdSigs = sigs, tcdATs = ats})
  = cls_name : 
    concatMap (tyClDeclNames . unLoc) ats ++ [n | L _ (TypeSig n _) <- sigs]

tyClDeclNames (TyData {tcdLName = tc_name, tcdCons = cons})
  = tc_name : conDeclsNames (map unLoc cons)

tyClDeclTyVars (TyFamily    {tcdTyVars = tvs}) = tvs
tyClDeclTyVars (TySynonym   {tcdTyVars = tvs}) = tvs
tyClDeclTyVars (TyData      {tcdTyVars = tvs}) = tvs
tyClDeclTyVars (ClassDecl   {tcdTyVars = tvs}) = tvs
tyClDeclTyVars (ForeignType {})		       = []
\end{code}

\begin{code}
countTyClDecls :: [TyClDecl name] -> (Int, Int, Int, Int, Int, Int)
	-- class, synonym decls, data, newtype, family decls, family instances
countTyClDecls decls 
 = (count isClassDecl    decls,
    count isSynDecl      decls,  -- excluding...
    count isDataTy       decls,  -- ...family...
    count isNewTy        decls,  -- ...instances
    count isFamilyDecl   decls,
    count isFamInstDecl  decls)
 where
   isDataTy TyData{tcdND = DataType, tcdTyPats = Nothing} = True
   isDataTy _                                             = False
   
   isNewTy TyData{tcdND = NewType, tcdTyPats = Nothing} = True
   isNewTy _                                            = False
\end{code}

\begin{code}
instance OutputableBndr name
	      => Outputable (TyClDecl name) where

    ppr (ForeignType {tcdLName = ltycon})
	= hsep [ptext SLIT("foreign import type dotnet"), ppr ltycon]

    ppr (TyFamily {tcdFlavour = flavour, tcdLName = ltycon, 
		   tcdTyVars = tyvars, tcdKind = mb_kind})
      = pp_flavour <+> pp_decl_head [] ltycon tyvars Nothing <+> pp_kind
        where
	  pp_flavour = case flavour of
		         TypeFamily          -> ptext SLIT("type family")
			 DataFamily NewType  -> ptext SLIT("newtype family")
			 DataFamily DataType -> ptext SLIT("data family")

          pp_kind = case mb_kind of
		      Nothing   -> empty
		      Just kind -> dcolon <+> pprKind kind

    ppr (TySynonym {tcdLName = ltycon, tcdTyVars = tyvars, tcdTyPats = typats,
		    tcdSynRhs = mono_ty})
      = hang (ptext SLIT("type") <+> 
	      (if isJust typats then ptext SLIT("instance") else empty) <+>
	      pp_decl_head [] ltycon tyvars typats <+> 
	      equals)
	     4 (ppr mono_ty)

    ppr (TyData {tcdND = new_or_data, tcdCtxt = context, tcdLName = ltycon,
		 tcdTyVars = tyvars, tcdTyPats = typats, tcdKindSig = mb_sig, 
		 tcdCons = condecls, tcdDerivs = derivings})
      = pp_tydecl (null condecls && isJust mb_sig) 
                  (ppr new_or_data <+> 
		   (if isJust typats then ptext SLIT("instance") else empty) <+>
		   pp_decl_head (unLoc context) ltycon tyvars typats <+> 
		   ppr_sig mb_sig)
		  (pp_condecls condecls)
		  derivings
      where
	ppr_sig Nothing = empty
	ppr_sig (Just kind) = dcolon <+> pprKind kind

    ppr (ClassDecl {tcdCtxt = context, tcdLName = lclas, tcdTyVars = tyvars, 
		    tcdFDs = fds, 
		    tcdSigs = sigs, tcdMeths = methods, tcdATs = ats})
      | null sigs && null ats  -- No "where" part
      = top_matter

      | otherwise	-- Laid out
      = sep [hsep [top_matter, ptext SLIT("where {")],
	     nest 4 (sep [ sep (map ppr_semi ats)
			 , sep (map ppr_semi sigs)
			 , pprLHsBinds methods
			 , char '}'])]
      where
        top_matter    =     ptext SLIT("class") 
		        <+> pp_decl_head (unLoc context) lclas tyvars Nothing
		        <+> pprFundeps (map unLoc fds)
	ppr_semi decl = ppr decl <> semi

pp_decl_head :: OutputableBndr name
   => HsContext name
   -> Located name
   -> [LHsTyVarBndr name]
   -> Maybe [LHsType name]
   -> SDoc
pp_decl_head context thing tyvars Nothing	-- no explicit type patterns
  = hsep [pprHsContext context, ppr thing, interppSP tyvars]
pp_decl_head context thing _      (Just typats) -- explicit type patterns
  = hsep [ pprHsContext context, ppr thing
	 , hsep (map (pprParendHsType.unLoc) typats)]

pp_condecls cs@(L _ ConDecl{ con_res = ResTyGADT _ } : _) -- In GADT syntax
  = hang (ptext SLIT("where")) 2 (vcat (map ppr cs))
pp_condecls cs 			  -- In H98 syntax
  = equals <+> sep (punctuate (ptext SLIT(" |")) (map ppr cs))

pp_tydecl True pp_head pp_decl_rhs derivings
  = pp_head
pp_tydecl False pp_head pp_decl_rhs derivings
  = hang pp_head 4 (sep [
      pp_decl_rhs,
      case derivings of
        Nothing -> empty
	Just ds -> hsep [ptext SLIT("deriving"), parens (interpp'SP ds)]
    ])

instance Outputable NewOrData where
  ppr NewType  = ptext SLIT("newtype")
  ppr DataType = ptext SLIT("data")
\end{code}


%************************************************************************
%*									*
\subsection[ConDecl]{A data-constructor declaration}
%*									*
%************************************************************************

\begin{code}
type LConDecl name = Located (ConDecl name)

-- data T b = forall a. Eq a => MkT a b
--   MkT :: forall b a. Eq a => MkT a b

-- data T b where
--	MkT1 :: Int -> T Int

-- data T = Int `MkT` Int
--	  | MkT2

-- data T a where
--	Int `MkT` Int :: T Int

data ConDecl name
  = ConDecl
    { con_name      :: Located name	    -- Constructor name; this is used for the
                                            -- DataCon itself, and for the user-callable wrapper Id

    , con_explicit  :: HsExplicitForAll     -- Is there an user-written forall? (cf. HStypes.HsForAllTy)

    , con_qvars     :: [LHsTyVarBndr name]  -- ResTyH98: the constructor's existential type variables
					    -- ResTyGADT:    all the constructor's quantified type variables

    , con_cxt       :: LHsContext name      -- The context.  This *does not* include the
					    -- "stupid theta" which lives only in the TyData decl

    , con_details   :: HsConDetails name (LBangType name)	-- The main payload

    , con_res       :: ResType name         -- Result type of the constructor

    , con_doc       :: Maybe (LHsDoc name)  -- A possible Haddock comment
    }

data ResType name
   = ResTyH98		-- Constructor was declared using Haskell 98 syntax
   | ResTyGADT (LHsType name)	-- Constructor was declared using GADT-style syntax,
				--	and here is its result type
\end{code}

\begin{code}
conDeclsNames :: Eq name => [ConDecl name] -> [Located name]
  -- See tyClDeclNames for what this does
  -- The function is boringly complicated because of the records
  -- And since we only have equality, we have to be a little careful
conDeclsNames cons
  = snd (foldl do_one ([], []) cons)
  where
    do_one (flds_seen, acc) (ConDecl { con_name = lname, con_details = RecCon flds })
	= (map unLoc new_flds ++ flds_seen, lname : [f | f <- new_flds] ++ acc)
	where
	  new_flds = [ f | (HsRecField f _ _) <- flds, not (unLoc f `elem` flds_seen) ]

    do_one (flds_seen, acc) c
	= (flds_seen, (con_name c):acc)

conDetailsTys details = map getBangType (hsConArgs details)
\end{code}
  

\begin{code}
instance (OutputableBndr name) => Outputable (ConDecl name) where
    ppr = pprConDecl

pprConDecl (ConDecl con expl tvs cxt details ResTyH98 doc)
  = sep [ppr_mbDoc doc, pprHsForAll expl tvs cxt, ppr_details con details]
  where
    ppr_details con (InfixCon t1 t2) = hsep [ppr t1, pprHsVar con, ppr t2]
    ppr_details con (PrefixCon tys)  = hsep (pprHsVar con : map ppr tys)
    ppr_details con (RecCon fields)  = ppr con <+> ppr_fields fields

pprConDecl (ConDecl con expl tvs cxt (PrefixCon arg_tys) (ResTyGADT res_ty) _)
  = ppr con <+> dcolon <+> 
    sep [pprHsForAll expl tvs cxt, ppr (foldr mk_fun_ty res_ty arg_tys)]
  where
    mk_fun_ty a b = noLoc (HsFunTy a b)

pprConDecl (ConDecl con expl tvs cxt (RecCon fields) (ResTyGADT res_ty) _)
  = sep [pprHsForAll expl tvs cxt, ppr con <+> ppr_fields fields <+> dcolon <+> ppr res_ty]

ppr_fields fields = braces (sep (punctuate comma (map ppr fields)))
\end{code}

%************************************************************************
%*									*
\subsection[InstDecl]{An instance declaration
%*									*
%************************************************************************

\begin{code}
type LInstDecl name = Located (InstDecl name)

data InstDecl name
  = InstDecl	(LHsType name)	-- Context => Class Instance-type
				-- Using a polytype means that the renamer conveniently
				-- figures out the quantified type variables for us.
		(LHsBinds name)
		[LSig name]	-- User-supplied pragmatic info
		[LTyClDecl name]-- Associated types (ie, 'TyData' and
				-- 'TySynonym' only)

instance (OutputableBndr name) => Outputable (InstDecl name) where

    ppr (InstDecl inst_ty binds uprags ats)
      = vcat [hsep [ptext SLIT("instance"), ppr inst_ty, ptext SLIT("where")],
	      nest 4 (ppr ats),
	      nest 4 (ppr uprags),
	      nest 4 (pprLHsBinds binds) ]

-- Extract the declarations of associated types from an instance
--
instDeclATs :: InstDecl name -> [LTyClDecl name]
instDeclATs (InstDecl _ _ _ ats) = ats
\end{code}

%************************************************************************
%*									*
\subsection[DerivDecl]{A stand-alone instance deriving declaration
%*									*
%************************************************************************

\begin{code}
type LDerivDecl name = Located (DerivDecl name)

data DerivDecl name = DerivDecl (LHsType name)

instance (OutputableBndr name) => Outputable (DerivDecl name) where
    ppr (DerivDecl ty) 
        = hsep [ptext SLIT("derived instance"), ppr ty]
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
type LDefaultDecl name = Located (DefaultDecl name)

data DefaultDecl name
  = DefaultDecl	[LHsType name]

instance (OutputableBndr name)
	      => Outputable (DefaultDecl name) where

    ppr (DefaultDecl tys)
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
--  * the Boolean value indicates whether the pre-standard deprecated syntax
--   has been used
--
type LForeignDecl name = Located (ForeignDecl name)

data ForeignDecl name
  = ForeignImport (Located name) (LHsType name) ForeignImport  -- defines name
  | ForeignExport (Located name) (LHsType name) ForeignExport  -- uses name

-- Specification Of an imported external entity in dependence on the calling
-- convention 
--
data ForeignImport = -- import of a C entity
		     --
                     --  * the two strings specifying a header file or library
                     --   may be empty, which indicates the absence of a
                     --   header or object specification (both are not used
                     --   in the case of `CWrapper' and when `CFunction'
                     --   has a dynamic target)
		     --
		     --  * the calling convention is irrelevant for code
		     --   generation in the case of `CLabel', but is needed
		     --   for pretty printing 
		     --
		     --  * `Safety' is irrelevant for `CLabel' and `CWrapper'
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

instance OutputableBndr name => Outputable (ForeignDecl name) where
  ppr (ForeignImport n ty fimport) =
    ptext SLIT("foreign import") <+> ppr fimport <+> 
    ppr n <+> dcolon <+> ppr ty
  ppr (ForeignExport n ty fexport) =
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
        ptext SLIT("static") <+> ftext header <+> char '&' <>
	pprLib lib <> ppr lbl
      pprCEntity header lib (CFunction (StaticTarget lbl)) = 
        ptext SLIT("static") <+> ftext header <+> char '&' <>
	pprLib lib <> ppr lbl
      pprCEntity header lib (CFunction (DynamicTarget)) = 
        ptext SLIT("dynamic")
      pprCEntity _      _   (CWrapper) = ptext SLIT("wrapper")
      --
      pprLib lib | nullFS lib = empty
		 | otherwise  = char '[' <> ppr lib <> char ']'

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
type LRuleDecl name = Located (RuleDecl name)

data RuleDecl name
  = HsRule			-- Source rule
	RuleName		-- Rule name
	Activation
	[RuleBndr name]		-- Forall'd vars; after typechecking this includes tyvars
	(Located (HsExpr name))	-- LHS
        NameSet                 -- Free-vars from the LHS
	(Located (HsExpr name))	-- RHS
        NameSet                 -- Free-vars from the RHS

data RuleBndr name
  = RuleBndr (Located name)
  | RuleBndrSig (Located name) (LHsType name)

collectRuleBndrSigTys :: [RuleBndr name] -> [LHsType name]
collectRuleBndrSigTys bndrs = [ty | RuleBndrSig _ ty <- bndrs]

instance OutputableBndr name => Outputable (RuleDecl name) where
  ppr (HsRule name act ns lhs fv_lhs rhs fv_rhs)
	= sep [text "{-# RULES" <+> doubleQuotes (ftext name) <+> ppr act,
	       nest 4 (pp_forall <+> pprExpr (unLoc lhs)), 
	       nest 4 (equals <+> pprExpr (unLoc rhs) <+> text "#-}") ]
	where
	  pp_forall | null ns   = empty
		    | otherwise	= text "forall" <+> fsep (map ppr ns) <> dot

instance OutputableBndr name => Outputable (RuleBndr name) where
   ppr (RuleBndr name) = ppr name
   ppr (RuleBndrSig name ty) = ppr name <> dcolon <> ppr ty
\end{code}

%************************************************************************
%*									*
\subsection[DocDecl]{Document comments}
%*									*
%************************************************************************

\begin{code}

type LDocDecl name = Located (DocDecl name)

data DocDecl name
  = DocCommentNext (HsDoc name)
  | DocCommentPrev (HsDoc name)
  | DocCommentNamed String (HsDoc name)
  | DocGroup Int (HsDoc name)
 
-- Okay, I need to reconstruct the document comments, but for now:
instance Outputable (DocDecl name) where
  ppr _ = text "<document comment>"

docDeclDoc (DocCommentNext d) = d
docDeclDoc (DocCommentPrev d) = d
docDeclDoc (DocCommentNamed _ d) = d
docDeclDoc (DocGroup _ d) = d

\end{code}

%************************************************************************
%*									*
\subsection[DeprecDecl]{Deprecations}
%*									*
%************************************************************************

We use exported entities for things to deprecate.

\begin{code}
type LDeprecDecl name = Located (DeprecDecl name)

data DeprecDecl name = Deprecation name DeprecTxt

instance OutputableBndr name => Outputable (DeprecDecl name) where
    ppr (Deprecation thing txt)
      = hsep [text "{-# DEPRECATED", ppr thing, doubleQuotes (ppr txt), text "#-}"]
\end{code}
