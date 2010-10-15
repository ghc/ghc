%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module IfaceSyn (
	module IfaceType,		-- Re-export all this

	IfaceDecl(..), IfaceClassOp(..), IfaceConDecl(..), IfaceConDecls(..),
	IfaceExpr(..), IfaceAlt, IfaceNote(..), IfaceLetBndr(..),
	IfaceBinding(..), IfaceConAlt(..), 
	IfaceIdInfo(..), IfaceIdDetails(..), IfaceUnfolding(..),
	IfaceInfoItem(..), IfaceRule(..), IfaceAnnotation(..), IfaceAnnTarget,
	IfaceInst(..), IfaceFamInst(..),

	-- Misc
        ifaceDeclSubBndrs, visibleIfConDecls,

        -- Free Names
        freeNamesIfDecl, freeNamesIfRule,

	-- Pretty printing
	pprIfaceExpr, pprIfaceDeclHead 
    ) where

#include "HsVersions.h"

import IfaceType

import Demand
import Annotations
import Class
import NameSet 
import Name
import CostCentre
import Literal
import ForeignCall
import Serialized
import BasicTypes
import Outputable
import FastString
import Module

infixl 3 &&&
\end{code}


%************************************************************************
%*									*
		Data type declarations
%*									*
%************************************************************************

\begin{code}
data IfaceDecl 
  = IfaceId { ifName   	  :: OccName,
	      ifType   	  :: IfaceType, 
	      ifIdDetails :: IfaceIdDetails,
	      ifIdInfo    :: IfaceIdInfo }

  | IfaceData { ifName       :: OccName,	-- Type constructor
		ifTyVars     :: [IfaceTvBndr],	-- Type variables
		ifCtxt	     :: IfaceContext,	-- The "stupid theta"
		ifCons	     :: IfaceConDecls,	-- Includes new/data info
	        ifRec	     :: RecFlag,	-- Recursive or not?
		ifGadtSyntax :: Bool,		-- True <=> declared using
						-- GADT syntax 
		ifGeneric    :: Bool,		-- True <=> generic converter
						--          functions available
    						-- We need this for imported
    						-- data decls, since the
    						-- imported modules may have
    						-- been compiled with
    						-- different flags to the
    						-- current compilation unit 
                ifFamInst    :: Maybe (IfaceTyCon, [IfaceType])
                                                -- Just <=> instance of family
                                                -- Invariant: 
                                                --   ifCons /= IfOpenDataTyCon
                                                --   for family instances
    }

  | IfaceSyn  {	ifName    :: OccName,		-- Type constructor
		ifTyVars  :: [IfaceTvBndr],	-- Type variables
		ifSynKind :: IfaceKind,		-- Kind of the *rhs* (not of the tycon)
		ifSynRhs  :: Maybe IfaceType,	-- Just rhs for an ordinary synonyn
						-- Nothing for an open family
                ifFamInst :: Maybe (IfaceTyCon, [IfaceType])
                                                -- Just <=> instance of family
                                                -- Invariant: ifOpenSyn == False
                                                --   for family instances
    }

  | IfaceClass { ifCtxt    :: IfaceContext, 	-- Context...
		 ifName    :: OccName,		-- Name of the class
		 ifTyVars  :: [IfaceTvBndr],	-- Type variables
		 ifFDs     :: [FunDep FastString], -- Functional dependencies
		 ifATs	   :: [IfaceDecl],	-- Associated type families
		 ifSigs    :: [IfaceClassOp],	-- Method signatures
	         ifRec	   :: RecFlag		-- Is newtype/datatype associated with the class recursive?
    }

  | IfaceForeign { ifName :: OccName,           -- Needs expanding when we move
                                                -- beyond .NET
		   ifExtName :: Maybe FastString }

data IfaceClassOp = IfaceClassOp OccName DefMethSpec IfaceType
	-- Nothing    => no default method
	-- Just False => ordinary polymorphic default method
	-- Just True  => generic default method

data IfaceConDecls
  = IfAbstractTyCon		-- No info
  | IfOpenDataTyCon		-- Open data family
  | IfDataTyCon [IfaceConDecl]	-- data type decls
  | IfNewTyCon  IfaceConDecl	-- newtype decls

visibleIfConDecls :: IfaceConDecls -> [IfaceConDecl]
visibleIfConDecls IfAbstractTyCon  = []
visibleIfConDecls IfOpenDataTyCon  = []
visibleIfConDecls (IfDataTyCon cs) = cs
visibleIfConDecls (IfNewTyCon c)   = [c]

data IfaceConDecl 
  = IfCon {
	ifConOcc     :: OccName,   		-- Constructor name
	ifConWrapper :: Bool,			-- True <=> has a wrapper
	ifConInfix   :: Bool,			-- True <=> declared infix
	ifConUnivTvs :: [IfaceTvBndr],		-- Universal tyvars
	ifConExTvs   :: [IfaceTvBndr],		-- Existential tyvars
	ifConEqSpec  :: [(OccName,IfaceType)],	-- Equality contraints
	ifConCtxt    :: IfaceContext,		-- Non-stupid context
	ifConArgTys  :: [IfaceType],		-- Arg types
	ifConFields  :: [OccName],		-- ...ditto... (field labels)
	ifConStricts :: [HsBang]}		-- Empty (meaning all lazy),
						-- or 1-1 corresp with arg tys

data IfaceInst 
  = IfaceInst { ifInstCls  :: Name,     		-- See comments with
		ifInstTys  :: [Maybe IfaceTyCon],	-- the defn of Instance
		ifDFun     :: Name,     		-- The dfun
		ifOFlag    :: OverlapFlag,		-- Overlap flag
		ifInstOrph :: Maybe OccName }		-- See Note [Orphans]
	-- There's always a separate IfaceDecl for the DFun, which gives 
	-- its IdInfo with its full type and version number.
	-- The instance declarations taken together have a version number,
	-- and we don't want that to wobble gratuitously
	-- If this instance decl is *used*, we'll record a usage on the dfun;
	-- and if the head does not change it won't be used if it wasn't before

data IfaceFamInst
  = IfaceFamInst { ifFamInstFam   :: Name                -- Family tycon
		 , ifFamInstTys   :: [Maybe IfaceTyCon]  -- Rough match types
		 , ifFamInstTyCon :: IfaceTyCon		 -- Instance decl
		 }

data IfaceRule
  = IfaceRule { 
	ifRuleName   :: RuleName,
	ifActivation :: Activation,
	ifRuleBndrs  :: [IfaceBndr],	-- Tyvars and term vars
	ifRuleHead   :: Name,   	-- Head of lhs
	ifRuleArgs   :: [IfaceExpr],	-- Args of LHS
	ifRuleRhs    :: IfaceExpr,
	ifRuleAuto   :: Bool,
	ifRuleOrph   :: Maybe OccName	-- Just like IfaceInst
    }

data IfaceAnnotation
  = IfaceAnnotation {
        ifAnnotatedTarget :: IfaceAnnTarget,
        ifAnnotatedValue :: Serialized
  }

type IfaceAnnTarget = AnnTarget OccName

-- We only serialise the IdDetails of top-level Ids, and even then
-- we only need a very limited selection.  Notably, none of the
-- implicit ones are needed here, becuase they are not put it
-- interface files

data IfaceIdDetails
  = IfVanillaId
  | IfRecSelId IfaceTyCon Bool
  | IfDFunId

data IfaceIdInfo
  = NoInfo			-- When writing interface file without -O
  | HasInfo [IfaceInfoItem]	-- Has info, and here it is

-- Here's a tricky case:
--   * Compile with -O module A, and B which imports A.f
--   * Change function f in A, and recompile without -O
--   * When we read in old A.hi we read in its IdInfo (as a thunk)
--	(In earlier GHCs we used to drop IdInfo immediately on reading,
--	 but we do not do that now.  Instead it's discarded when the
--	 ModIface is read into the various decl pools.)
--   * The version comparsion sees that new (=NoInfo) differs from old (=HasInfo *)
--	and so gives a new version.

data IfaceInfoItem
  = HsArity	 Arity
  | HsStrictness StrictSig
  | HsInline     InlinePragma
  | HsUnfold	 Bool		  -- True <=> isNonRuleLoopBreaker is true
		 IfaceUnfolding   -- See Note [Expose recursive functions] 
  | HsNoCafRefs

-- NB: Specialisations and rules come in separately and are
-- only later attached to the Id.  Partial reason: some are orphans.

data IfaceUnfolding 
  = IfCoreUnfold Bool IfaceExpr -- True <=> INLINABLE, False <=> regular unfolding
                                -- Possibly could eliminate the Bool here, the information
                                -- is also in the InlinePragma.

  | IfCompulsory IfaceExpr	-- Only used for default methods, in fact

  | IfInlineRule Arity          -- INLINE pragmas
                 Bool		-- OK to inline even if *un*-saturated
		 Bool		-- OK to inline even if context is boring
                 IfaceExpr 

  | IfWrapper    Arity Name	  -- NB: we need a Name (not just OccName) because the worker
				  --     can simplify to a function in another module.

  | IfDFunUnfold [IfaceExpr]

--------------------------------
data IfaceExpr
  = IfaceLcl 	FastString
  | IfaceExt    Name
  | IfaceType   IfaceType
  | IfaceTuple 	Boxity [IfaceExpr]		-- Saturated; type arguments omitted
  | IfaceLam 	IfaceBndr IfaceExpr
  | IfaceApp 	IfaceExpr IfaceExpr
  | IfaceCase	IfaceExpr FastString IfaceType [IfaceAlt]
  | IfaceLet	IfaceBinding  IfaceExpr
  | IfaceNote	IfaceNote IfaceExpr
  | IfaceCast   IfaceExpr IfaceCoercion
  | IfaceLit	Literal
  | IfaceFCall	ForeignCall IfaceType
  | IfaceTick   Module Int

data IfaceNote = IfaceSCC CostCentre
               | IfaceCoreNote String

type IfaceAlt = (IfaceConAlt, [FastString], IfaceExpr)
	-- Note: FastString, not IfaceBndr (and same with the case binder)
	-- We reconstruct the kind/type of the thing from the context
	-- thus saving bulk in interface files

data IfaceConAlt = IfaceDefault
 		 | IfaceDataAlt Name
		 | IfaceTupleAlt Boxity
		 | IfaceLitAlt Literal

data IfaceBinding
  = IfaceNonRec	IfaceLetBndr IfaceExpr
  | IfaceRec 	[(IfaceLetBndr, IfaceExpr)]

-- IfaceLetBndr is like IfaceIdBndr, but has IdInfo too
-- It's used for *non-top-level* let/rec binders
-- See Note [IdInfo on nested let-bindings]
data IfaceLetBndr = IfLetBndr FastString IfaceType IfaceIdInfo
\end{code}

Note [Expose recursive functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For supercompilation we want to put *all* unfoldings in the interface
file, even for functions that are recursive (or big).  So we need to
know when an unfolding belongs to a loop-breaker so that we can refrain
from inlining it (except during supercompilation).

Note [IdInfo on nested let-bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Occasionally we want to preserve IdInfo on nested let bindings. The one
that came up was a NOINLINE pragma on a let-binding inside an INLINE
function.  The user (Duncan Coutts) really wanted the NOINLINE control
to cross the separate compilation boundary.

So a IfaceLetBndr keeps a trimmed-down list of IfaceIdInfo stuff.
Currently we only actually retain InlinePragInfo, but in principle we could
add strictness etc.


Note [Orphans]: the ifInstOrph and ifRuleOrph fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a module contains any "orphans", then its interface file is read
regardless, so that its instances are not missed.

Roughly speaking, an instance is an orphan if its head (after the =>)
mentions nothing defined in this module.  Functional dependencies
complicate the situation though. Consider

  module M where { class C a b | a -> b }

and suppose we are compiling module X:

  module X where
	import M
	data T = ...
	instance C Int T where ...

This instance is an orphan, because when compiling a third module Y we
might get a constraint (C Int v), and we'd want to improve v to T.  So
we must make sure X's instances are loaded, even if we do not directly
use anything from X.

More precisely, an instance is an orphan iff

  If there are no fundeps, then at least of the names in
  the instance head is locally defined.

  If there are fundeps, then for every fundep, at least one of the
  names free in a *non-determined* part of the instance head is
  defined in this module.  

(Note that these conditions hold trivially if the class is locally
defined.)

Note [Versioning of instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Now consider versioning.  If we *use* an instance decl in one compilation,
we'll depend on the dfun id for that instance, so we'll recompile if it changes.
But suppose we *don't* (currently) use an instance!  We must recompile if
the instance is changed in such a way that it becomes important.  (This would
only matter with overlapping instances, else the importing module wouldn't have
compiled before and the recompilation check is irrelevant.)

The is_orph field is set to (Just n) if the instance is not an orphan.
The 'n' is *any* of the locally-defined names mentioned anywhere in the
instance head.  This name is used for versioning; the instance decl is
considered part of the defn of this 'n'.

I'm worried about whether this works right if we pick a name from
a functionally-dependent part of the instance decl.  E.g.

  module M where { class C a b | a -> b }

and suppose we are compiling module X:

  module X where
	import M
	data S  = ...
	data T = ...
	instance C S T where ...

If we base the instance verion on T, I'm worried that changing S to S'
would change T's version, but not S or S'.  But an importing module might
not depend on T, and so might not be recompiled even though the new instance
(C S' T) might be relevant.  I have not been able to make a concrete example,
and it seems deeply obscure, so I'm going to leave it for now.


Note [Versioning of rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~
A rule that is not an orphan has an ifRuleOrph field of (Just n), where
n appears on the LHS of the rule; any change in the rule changes the version of n.


\begin{code}
-- -----------------------------------------------------------------------------
-- Utils on IfaceSyn

ifaceDeclSubBndrs :: IfaceDecl -> [OccName]
--  *Excludes* the 'main' name, but *includes* the implicitly-bound names
-- Deeply revolting, because it has to predict what gets bound,
-- especially the question of whether there's a wrapper for a datacon

-- N.B. the set of names returned here *must* match the set of
-- TyThings returned by HscTypes.implicitTyThings, in the sense that
-- TyThing.getOccName should define a bijection between the two lists.
-- This invariant is used in LoadIface.loadDecl (see note [Tricky iface loop])
-- The order of the list does not matter.
ifaceDeclSubBndrs IfaceData {ifCons = IfAbstractTyCon}  = []

-- Newtype
ifaceDeclSubBndrs (IfaceData {ifName = tc_occ,
                              ifCons = IfNewTyCon (
                                        IfCon { ifConOcc = con_occ }),
                              ifFamInst = famInst}) 
  =   -- implicit coerion and (possibly) family instance coercion
    (mkNewTyCoOcc tc_occ) : (famInstCo famInst tc_occ) ++
      -- data constructor and worker (newtypes don't have a wrapper)
    [con_occ, mkDataConWorkerOcc con_occ]


ifaceDeclSubBndrs (IfaceData {ifName = tc_occ,
			      ifCons = IfDataTyCon cons, 
			      ifFamInst = famInst})
  =   -- (possibly) family instance coercion;
      -- there is no implicit coercion for non-newtypes
    famInstCo famInst tc_occ
      -- for each data constructor in order,
      --    data constructor, worker, and (possibly) wrapper
    ++ concatMap dc_occs cons
  where
    dc_occs con_decl
	| has_wrapper = [con_occ, work_occ, wrap_occ]
	| otherwise   = [con_occ, work_occ]
	where
	  con_occ  = ifConOcc con_decl			-- DataCon namespace
	  wrap_occ = mkDataConWrapperOcc con_occ	-- Id namespace
	  work_occ = mkDataConWorkerOcc con_occ		-- Id namespace
	  has_wrapper = ifConWrapper con_decl		-- This is the reason for
	  	      		     			-- having the ifConWrapper field!

ifaceDeclSubBndrs (IfaceClass {ifCtxt = sc_ctxt, ifName = cls_occ, 
			       ifSigs = sigs, ifATs = ats })
  = -- dictionary datatype:
    --   type constructor
    tc_occ : 
    --   (possibly) newtype coercion
    co_occs ++
    --    data constructor (DataCon namespace)
    --    data worker (Id namespace)
    --    no wrapper (class dictionaries never have a wrapper)
    [dc_occ, dcww_occ] ++
    -- associated types
    [ifName at | at <- ats ] ++
    -- superclass selectors
    [mkSuperDictSelOcc n cls_occ | n <- [1..n_ctxt]] ++
    -- operation selectors
    [op | IfaceClassOp op  _ _ <- sigs]
  where
    n_ctxt = length sc_ctxt
    n_sigs = length sigs
    tc_occ  = mkClassTyConOcc cls_occ
    dc_occ  = mkClassDataConOcc cls_occ	
    co_occs | is_newtype = [mkNewTyCoOcc tc_occ]
	    | otherwise  = []
    dcww_occ = mkDataConWorkerOcc dc_occ
    is_newtype = n_sigs + n_ctxt == 1			-- Sigh 

ifaceDeclSubBndrs (IfaceSyn {ifName = tc_occ,
			     ifFamInst = famInst})
  = famInstCo famInst tc_occ

ifaceDeclSubBndrs _ = []

-- coercion for data/newtype family instances
famInstCo :: Maybe (IfaceTyCon, [IfaceType]) -> OccName -> [OccName]
famInstCo Nothing  _       = []
famInstCo (Just _) baseOcc = [mkInstTyCoOcc baseOcc]

----------------------------- Printing IfaceDecl ------------------------------

instance Outputable IfaceDecl where
  ppr = pprIfaceDecl

pprIfaceDecl :: IfaceDecl -> SDoc
pprIfaceDecl (IfaceId {ifName = var, ifType = ty, 
                       ifIdDetails = details, ifIdInfo = info})
  = sep [ ppr var <+> dcolon <+> ppr ty, 
    	  nest 2 (ppr details),
	  nest 2 (ppr info) ]

pprIfaceDecl (IfaceForeign {ifName = tycon})
  = hsep [ptext (sLit "foreign import type dotnet"), ppr tycon]

pprIfaceDecl (IfaceSyn {ifName = tycon, ifTyVars = tyvars, 
		        ifSynRhs = Just mono_ty, 
                        ifFamInst = mbFamInst})
  = hang (ptext (sLit "type") <+> pprIfaceDeclHead [] tycon tyvars)
       4 (vcat [equals <+> ppr mono_ty, pprFamily mbFamInst])

pprIfaceDecl (IfaceSyn {ifName = tycon, ifTyVars = tyvars, 
		        ifSynRhs = Nothing, ifSynKind = kind })
  = hang (ptext (sLit "type family") <+> pprIfaceDeclHead [] tycon tyvars)
       4 (dcolon <+> ppr kind)

pprIfaceDecl (IfaceData {ifName = tycon, ifGeneric = gen, ifCtxt = context,
			 ifTyVars = tyvars, ifCons = condecls, 
			 ifRec = isrec, ifFamInst = mbFamInst})
  = hang (pp_nd <+> pprIfaceDeclHead context tycon tyvars)
       4 (vcat [pprRec isrec, pprGen gen, pp_condecls tycon condecls,
	        pprFamily mbFamInst])
  where
    pp_nd = case condecls of
		IfAbstractTyCon -> ptext (sLit "data")
		IfOpenDataTyCon -> ptext (sLit "data family")
		IfDataTyCon _   -> ptext (sLit "data")
		IfNewTyCon _  	-> ptext (sLit "newtype")

pprIfaceDecl (IfaceClass {ifCtxt = context, ifName = clas, ifTyVars = tyvars, 
			  ifFDs = fds, ifATs = ats, ifSigs = sigs, 
			  ifRec = isrec})
  = hang (ptext (sLit "class") <+> pprIfaceDeclHead context clas tyvars <+> pprFundeps fds)
       4 (vcat [pprRec isrec,
	        sep (map ppr ats),
		sep (map ppr sigs)])

pprRec :: RecFlag -> SDoc
pprRec isrec = ptext (sLit "RecFlag") <+> ppr isrec

pprGen :: Bool -> SDoc
pprGen True  = ptext (sLit "Generics: yes")
pprGen False = ptext (sLit "Generics: no")

pprFamily :: Maybe (IfaceTyCon, [IfaceType]) -> SDoc
pprFamily Nothing        = ptext (sLit "FamilyInstance: none")
pprFamily (Just famInst) = ptext (sLit "FamilyInstance:") <+> ppr famInst

instance Outputable IfaceClassOp where
   ppr (IfaceClassOp n dm ty) = ppr n <+> ppr dm <+> dcolon <+> ppr ty

pprIfaceDeclHead :: IfaceContext -> OccName -> [IfaceTvBndr] -> SDoc
pprIfaceDeclHead context thing tyvars
  = hsep [pprIfaceContext context, parenSymOcc thing (ppr thing), 
	  pprIfaceTvBndrs tyvars]

pp_condecls :: OccName -> IfaceConDecls -> SDoc
pp_condecls _  IfAbstractTyCon  = ptext (sLit "{- abstract -}")
pp_condecls tc (IfNewTyCon c)   = equals <+> pprIfaceConDecl tc c
pp_condecls _  IfOpenDataTyCon  = empty
pp_condecls tc (IfDataTyCon cs) = equals <+> sep (punctuate (ptext (sLit " |"))
							     (map (pprIfaceConDecl tc) cs))

pprIfaceConDecl :: OccName -> IfaceConDecl -> SDoc
pprIfaceConDecl tc
	(IfCon { ifConOcc = name, ifConInfix = is_infix, ifConWrapper = has_wrap,
		 ifConUnivTvs = univ_tvs, ifConExTvs = ex_tvs, 
		 ifConEqSpec = eq_spec, ifConCtxt = ctxt, ifConArgTys = arg_tys, 
		 ifConStricts = strs, ifConFields = fields })
  = sep [main_payload,
	 if is_infix then ptext (sLit "Infix") else empty,
	 if has_wrap then ptext (sLit "HasWrapper") else empty,
	 ppUnless (null strs) $
	    nest 4 (ptext (sLit "Stricts:") <+> hsep (map ppr_bang strs)),
	 ppUnless (null fields) $
	    nest 4 (ptext (sLit "Fields:") <+> hsep (map ppr fields))]
  where
    ppr_bang HsNoBang = char '_'	-- Want to see these
    ppr_bang bang     = ppr bang
        
    main_payload = ppr name <+> dcolon <+> 
		   pprIfaceForAllPart (univ_tvs ++ ex_tvs) (eq_ctxt ++ ctxt) pp_tau

    eq_ctxt = [(IfaceEqPred (IfaceTyVar (occNameFS tv)) ty) 
	      | (tv,ty) <- eq_spec] 

	-- A bit gruesome this, but we can't form the full con_tau, and ppr it,
	-- because we don't have a Name for the tycon, only an OccName
    pp_tau = case map pprParendIfaceType arg_tys ++ [pp_res_ty] of
		(t:ts) -> fsep (t : map (arrow <+>) ts)
		[]     -> panic "pp_con_taus"

    pp_res_ty = ppr tc <+> fsep [ppr tv | (tv,_) <- univ_tvs]

instance Outputable IfaceRule where
  ppr (IfaceRule { ifRuleName = name, ifActivation = act, ifRuleBndrs = bndrs,
		   ifRuleHead = fn, ifRuleArgs = args, ifRuleRhs = rhs }) 
    = sep [hsep [doubleQuotes (ftext name), ppr act,
		 ptext (sLit "forall") <+> pprIfaceBndrs bndrs],
	   nest 2 (sep [ppr fn <+> sep (map pprParendIfaceExpr args),
		        ptext (sLit "=") <+> ppr rhs])
      ]

instance Outputable IfaceInst where
  ppr (IfaceInst {ifDFun = dfun_id, ifOFlag = flag, 
		  ifInstCls = cls, ifInstTys = mb_tcs})
    = hang (ptext (sLit "instance") <+> ppr flag 
		<+> ppr cls <+> brackets (pprWithCommas ppr_rough mb_tcs))
         2 (equals <+> ppr dfun_id)

instance Outputable IfaceFamInst where
  ppr (IfaceFamInst {ifFamInstFam = fam, ifFamInstTys = mb_tcs,
		     ifFamInstTyCon = tycon_id})
    = hang (ptext (sLit "family instance") <+> 
	    ppr fam <+> brackets (pprWithCommas ppr_rough mb_tcs))
         2 (equals <+> ppr tycon_id)

ppr_rough :: Maybe IfaceTyCon -> SDoc
ppr_rough Nothing   = dot
ppr_rough (Just tc) = ppr tc
\end{code}


----------------------------- Printing IfaceExpr ------------------------------------

\begin{code}
instance Outputable IfaceExpr where
    ppr e = pprIfaceExpr noParens e

pprParendIfaceExpr :: IfaceExpr -> SDoc
pprParendIfaceExpr = pprIfaceExpr parens

pprIfaceExpr :: (SDoc -> SDoc) -> IfaceExpr -> SDoc
	-- The function adds parens in context that need
	-- an atomic value (e.g. function args)

pprIfaceExpr _       (IfaceLcl v)       = ppr v
pprIfaceExpr _       (IfaceExt v)       = ppr v
pprIfaceExpr _       (IfaceLit l)       = ppr l
pprIfaceExpr _       (IfaceFCall cc ty) = braces (ppr cc <+> ppr ty)
pprIfaceExpr _       (IfaceTick m ix)   = braces (text "tick" <+> ppr m <+> ppr ix)
pprIfaceExpr _       (IfaceType ty)     = char '@' <+> pprParendIfaceType ty

pprIfaceExpr add_par app@(IfaceApp _ _) = add_par (pprIfaceApp app [])
pprIfaceExpr _       (IfaceTuple c as)  = tupleParens c (interpp'SP as)

pprIfaceExpr add_par e@(IfaceLam _ _)   
  = add_par (sep [char '\\' <+> sep (map ppr bndrs) <+> arrow,
		  pprIfaceExpr noParens body])
  where 
    (bndrs,body) = collect [] e
    collect bs (IfaceLam b e) = collect (b:bs) e
    collect bs e              = (reverse bs, e)

pprIfaceExpr add_par (IfaceCase scrut bndr ty [(con, bs, rhs)])
  = add_par (sep [ptext (sLit "case") <+> char '@' <+> pprParendIfaceType ty
			<+> pprIfaceExpr noParens scrut <+> ptext (sLit "of") 
			<+> ppr bndr <+> char '{' <+> ppr_con_bs con bs <+> arrow,
  		  pprIfaceExpr noParens rhs <+> char '}'])

pprIfaceExpr add_par (IfaceCase scrut bndr ty alts)
  = add_par (sep [ptext (sLit "case") <+> char '@' <+> pprParendIfaceType ty
		 	<+> pprIfaceExpr noParens scrut <+> ptext (sLit "of") 
			<+> ppr bndr <+> char '{',
  		  nest 2 (sep (map ppr_alt alts)) <+> char '}'])

pprIfaceExpr _       (IfaceCast expr co)
  = sep [pprParendIfaceExpr expr,
	 nest 2 (ptext (sLit "`cast`")),
	 pprParendIfaceType co]

pprIfaceExpr add_par (IfaceLet (IfaceNonRec b rhs) body)
  = add_par (sep [ptext (sLit "let {"), 
		  nest 2 (ppr_bind (b, rhs)),
		  ptext (sLit "} in"), 
		  pprIfaceExpr noParens body])

pprIfaceExpr add_par (IfaceLet (IfaceRec pairs) body)
  = add_par (sep [ptext (sLit "letrec {"),
		  nest 2 (sep (map ppr_bind pairs)), 
		  ptext (sLit "} in"),
		  pprIfaceExpr noParens body])

pprIfaceExpr add_par (IfaceNote note body) = add_par (ppr note <+> pprParendIfaceExpr body)

ppr_alt :: (IfaceConAlt, [FastString], IfaceExpr) -> SDoc
ppr_alt (con, bs, rhs) = sep [ppr_con_bs con bs, 
			      arrow <+> pprIfaceExpr noParens rhs]

ppr_con_bs :: IfaceConAlt -> [FastString] -> SDoc
ppr_con_bs (IfaceTupleAlt tup_con) bs = tupleParens tup_con (interpp'SP bs)
ppr_con_bs con bs		      = ppr con <+> hsep (map ppr bs)
  
ppr_bind :: (IfaceLetBndr, IfaceExpr) -> SDoc
ppr_bind (IfLetBndr b ty info, rhs) 
  = sep [hang (ppr b <+> dcolon <+> ppr ty) 2 (ppr info),
	 equals <+> pprIfaceExpr noParens rhs]

------------------
pprIfaceApp :: IfaceExpr -> [SDoc] -> SDoc
pprIfaceApp (IfaceApp fun arg) args = pprIfaceApp fun (nest 2 (pprParendIfaceExpr arg) : args)
pprIfaceApp fun	 	       args = sep (pprParendIfaceExpr fun : args)

------------------
instance Outputable IfaceNote where
    ppr (IfaceSCC cc)     = pprCostCentreCore cc
    ppr (IfaceCoreNote s) = ptext (sLit "__core_note") <+> pprHsString (mkFastString s)


instance Outputable IfaceConAlt where
    ppr IfaceDefault      = text "DEFAULT"
    ppr (IfaceLitAlt l)   = ppr l
    ppr (IfaceDataAlt d)  = ppr d
    ppr (IfaceTupleAlt _) = panic "ppr IfaceConAlt" 
    -- IfaceTupleAlt is handled by the case-alternative printer

------------------
instance Outputable IfaceIdDetails where
  ppr IfVanillaId    = empty
  ppr (IfRecSelId tc b) = ptext (sLit "RecSel") <+> ppr tc
      		          <+> if b then ptext (sLit "<naughty>") else empty
  ppr IfDFunId       = ptext (sLit "DFunId")

instance Outputable IfaceIdInfo where
  ppr NoInfo       = empty
  ppr (HasInfo is) = ptext (sLit "{-") <+> pprWithCommas ppr is <+> ptext (sLit "-}")

instance Outputable IfaceInfoItem where
  ppr (HsUnfold lb unf)  = ptext (sLit "Unfolding") <> ppWhen lb (ptext (sLit "(loop-breaker)")) 
                           <> colon <+> ppr unf
  ppr (HsInline prag)    = ptext (sLit "Inline:") <+> ppr prag
  ppr (HsArity arity)    = ptext (sLit "Arity:") <+> int arity
  ppr (HsStrictness str) = ptext (sLit "Strictness:") <+> pprIfaceStrictSig str
  ppr HsNoCafRefs	 = ptext (sLit "HasNoCafRefs")

instance Outputable IfaceUnfolding where
  ppr (IfCompulsory e)     = ptext (sLit "<compulsory>") <+> parens (ppr e)
  ppr (IfCoreUnfold s e)   = (if s then ptext (sLit "<stable>") else empty) <+> parens (ppr e)
  ppr (IfInlineRule a uok bok e) = sep [ptext (sLit "InlineRule") <+> ppr (a,uok,bok),
      		           	        pprParendIfaceExpr e]
  ppr (IfWrapper a wkr)    = ptext (sLit "Worker:") <+> ppr wkr
                             <+> parens (ptext (sLit "arity") <+> int a)
  ppr (IfDFunUnfold ns)    = ptext (sLit "DFun:")
                             <+> brackets (pprWithCommas pprParendIfaceExpr ns)


-- -----------------------------------------------------------------------------
-- Finding the Names in IfaceSyn

-- This is used for dependency analysis in MkIface, so that we
-- fingerprint a declaration before the things that depend on it.  It
-- is specific to interface-file fingerprinting in the sense that we
-- don't collect *all* Names: for example, the DFun of an instance is
-- recorded textually rather than by its fingerprint when
-- fingerprinting the instance, so DFuns are not dependencies.

freeNamesIfDecl :: IfaceDecl -> NameSet
freeNamesIfDecl (IfaceId _s t d i) = 
  freeNamesIfType t &&&
  freeNamesIfIdInfo i &&&
  freeNamesIfIdDetails d
freeNamesIfDecl IfaceForeign{} = 
  emptyNameSet
freeNamesIfDecl d@IfaceData{} =
  freeNamesIfTvBndrs (ifTyVars d) &&&
  freeNamesIfTcFam (ifFamInst d) &&&
  freeNamesIfContext (ifCtxt d) &&&
  freeNamesIfConDecls (ifCons d)
freeNamesIfDecl d@IfaceSyn{} =
  freeNamesIfTvBndrs (ifTyVars d) &&&
  freeNamesIfSynRhs (ifSynRhs d) &&&
  freeNamesIfTcFam (ifFamInst d)
freeNamesIfDecl d@IfaceClass{} =
  freeNamesIfTvBndrs (ifTyVars d) &&&
  freeNamesIfContext (ifCtxt d) &&&
  freeNamesIfDecls   (ifATs d) &&&
  fnList freeNamesIfClsSig (ifSigs d)

freeNamesIfIdDetails :: IfaceIdDetails -> NameSet
freeNamesIfIdDetails (IfRecSelId tc _) = freeNamesIfTc tc
freeNamesIfIdDetails _                 = emptyNameSet

-- All other changes are handled via the version info on the tycon
freeNamesIfSynRhs :: Maybe IfaceType -> NameSet
freeNamesIfSynRhs (Just ty) = freeNamesIfType ty
freeNamesIfSynRhs Nothing   = emptyNameSet

freeNamesIfTcFam :: Maybe (IfaceTyCon, [IfaceType]) -> NameSet
freeNamesIfTcFam (Just (tc,tys)) = 
  freeNamesIfTc tc &&& fnList freeNamesIfType tys
freeNamesIfTcFam Nothing =
  emptyNameSet

freeNamesIfContext :: IfaceContext -> NameSet
freeNamesIfContext = fnList freeNamesIfPredType

freeNamesIfDecls :: [IfaceDecl] -> NameSet
freeNamesIfDecls = fnList freeNamesIfDecl

freeNamesIfClsSig :: IfaceClassOp -> NameSet
freeNamesIfClsSig (IfaceClassOp _n _dm ty) = freeNamesIfType ty

freeNamesIfConDecls :: IfaceConDecls -> NameSet
freeNamesIfConDecls (IfDataTyCon c) = fnList freeNamesIfConDecl c
freeNamesIfConDecls (IfNewTyCon c)  = freeNamesIfConDecl c
freeNamesIfConDecls _               = emptyNameSet

freeNamesIfConDecl :: IfaceConDecl -> NameSet
freeNamesIfConDecl c = 
  freeNamesIfTvBndrs (ifConUnivTvs c) &&&
  freeNamesIfTvBndrs (ifConExTvs c) &&&
  freeNamesIfContext (ifConCtxt c) &&& 
  fnList freeNamesIfType (ifConArgTys c) &&&
  fnList freeNamesIfType (map snd (ifConEqSpec c)) -- equality constraints

freeNamesIfPredType :: IfacePredType -> NameSet
freeNamesIfPredType (IfaceClassP cl tys) = 
   unitNameSet cl &&& fnList freeNamesIfType tys
freeNamesIfPredType (IfaceIParam _n ty) =
   freeNamesIfType ty
freeNamesIfPredType (IfaceEqPred ty1 ty2) =
   freeNamesIfType ty1 &&& freeNamesIfType ty2

freeNamesIfType :: IfaceType -> NameSet
freeNamesIfType (IfaceTyVar _)        = emptyNameSet
freeNamesIfType (IfaceAppTy s t)      = freeNamesIfType s &&& freeNamesIfType t
freeNamesIfType (IfacePredTy st)      = freeNamesIfPredType st
freeNamesIfType (IfaceTyConApp tc ts) = 
   freeNamesIfTc tc &&& fnList freeNamesIfType ts
freeNamesIfType (IfaceForAllTy tv t)  =
   freeNamesIfTvBndr tv &&& freeNamesIfType t
freeNamesIfType (IfaceFunTy s t)      = freeNamesIfType s &&& freeNamesIfType t

freeNamesIfTvBndrs :: [IfaceTvBndr] -> NameSet
freeNamesIfTvBndrs = fnList freeNamesIfTvBndr

freeNamesIfBndr :: IfaceBndr -> NameSet
freeNamesIfBndr (IfaceIdBndr b) = freeNamesIfIdBndr b
freeNamesIfBndr (IfaceTvBndr b) = freeNamesIfTvBndr b

freeNamesIfLetBndr :: IfaceLetBndr -> NameSet
-- Remember IfaceLetBndr is used only for *nested* bindings
-- The cut-down IdInfo never contains any Names, but the type may!
freeNamesIfLetBndr (IfLetBndr _name ty _info) = freeNamesIfType ty

freeNamesIfTvBndr :: IfaceTvBndr -> NameSet
freeNamesIfTvBndr (_fs,k) = freeNamesIfType k
    -- kinds can have Names inside, when the Kind is an equality predicate

freeNamesIfIdBndr :: IfaceIdBndr -> NameSet
freeNamesIfIdBndr = freeNamesIfTvBndr

freeNamesIfIdInfo :: IfaceIdInfo -> NameSet
freeNamesIfIdInfo NoInfo = emptyNameSet
freeNamesIfIdInfo (HasInfo i) = fnList freeNamesItem i

freeNamesItem :: IfaceInfoItem -> NameSet
freeNamesItem (HsUnfold _ u) = freeNamesIfUnfold u
freeNamesItem _              = emptyNameSet

freeNamesIfUnfold :: IfaceUnfolding -> NameSet
freeNamesIfUnfold (IfCoreUnfold _ e)     = freeNamesIfExpr e
freeNamesIfUnfold (IfCompulsory e)       = freeNamesIfExpr e
freeNamesIfUnfold (IfInlineRule _ _ _ e) = freeNamesIfExpr e
freeNamesIfUnfold (IfWrapper _ v)        = unitNameSet v
freeNamesIfUnfold (IfDFunUnfold vs)      = fnList freeNamesIfExpr vs

freeNamesIfExpr :: IfaceExpr -> NameSet
freeNamesIfExpr (IfaceExt v)	  = unitNameSet v
freeNamesIfExpr (IfaceFCall _ ty) = freeNamesIfType ty
freeNamesIfExpr (IfaceType ty)    = freeNamesIfType ty
freeNamesIfExpr (IfaceTuple _ as) = fnList freeNamesIfExpr as
freeNamesIfExpr (IfaceLam b body) = freeNamesIfBndr b &&& freeNamesIfExpr body
freeNamesIfExpr (IfaceApp f a)    = freeNamesIfExpr f &&& freeNamesIfExpr a
freeNamesIfExpr (IfaceCast e co)  = freeNamesIfExpr e &&& freeNamesIfType co
freeNamesIfExpr (IfaceNote _n r)   = freeNamesIfExpr r

freeNamesIfExpr (IfaceCase s _ ty alts)
  = freeNamesIfExpr s 
    &&& fnList fn_alt alts &&& fn_cons alts
    &&& freeNamesIfType ty
  where
    fn_alt (_con,_bs,r) = freeNamesIfExpr r

    -- Depend on the data constructors.  Just one will do!
    -- Note [Tracking data constructors]
    fn_cons []                              = emptyNameSet
    fn_cons ((IfaceDefault    ,_,_) : alts) = fn_cons alts
    fn_cons ((IfaceDataAlt con,_,_) : _   ) = unitNameSet con    
    fn_cons (_                      : _   ) = emptyNameSet

freeNamesIfExpr (IfaceLet (IfaceNonRec bndr rhs) body)
  = freeNamesIfLetBndr bndr &&& freeNamesIfExpr rhs &&& freeNamesIfExpr body

freeNamesIfExpr (IfaceLet (IfaceRec as) x)
  = fnList fn_pair as &&& freeNamesIfExpr x
  where
    fn_pair (bndr, rhs) = freeNamesIfLetBndr bndr &&& freeNamesIfExpr rhs

freeNamesIfExpr _ = emptyNameSet


freeNamesIfTc :: IfaceTyCon -> NameSet
freeNamesIfTc (IfaceTc tc) = unitNameSet tc
-- ToDo: shouldn't we include IfaceIntTc & co.?
freeNamesIfTc _ = emptyNameSet

freeNamesIfRule :: IfaceRule -> NameSet
freeNamesIfRule (IfaceRule { ifRuleBndrs = bs, ifRuleHead = f
                           , ifRuleArgs = es, ifRuleRhs = rhs })
  = unitNameSet f &&&
    fnList freeNamesIfBndr bs &&&
    fnList freeNamesIfExpr es &&&
    freeNamesIfExpr rhs

-- helpers
(&&&) :: NameSet -> NameSet -> NameSet
(&&&) = unionNameSets

fnList :: (a -> NameSet) -> [a] -> NameSet
fnList f = foldr (&&&) emptyNameSet . map f
\end{code}

Note [Tracking data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a case expression 
   case e of { C a -> ...; ... }
You might think that we don't need to include the datacon C
in the free names, because its type will probably show up in 
the free names of 'e'.  But in rare circumstances this may
not happen.   Here's the one that bit me:

   module DynFlags where 
     import {-# SOURCE #-} Packages( PackageState )
     data DynFlags = DF ... PackageState ...

   module Packages where 
     import DynFlags
     data PackageState = PS ...
     lookupModule (df :: DynFlags)
        = case df of
              DF ...p... -> case p of
                               PS ... -> ...

Now, lookupModule depends on DynFlags, but the transitive dependency
on the *locally-defined* type PackageState is not visible. We need
to take account of the use of the data constructor PS in the pattern match.
