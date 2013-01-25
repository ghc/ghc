%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module IfaceSyn (
        module IfaceType,

        IfaceDecl(..), IfaceClassOp(..), IfaceAT(..), IfaceATDefault(..),
        IfaceConDecl(..), IfaceConDecls(..),
        IfaceExpr(..), IfaceAlt, IfaceLetBndr(..),
        IfaceBinding(..), IfaceConAlt(..),
        IfaceIdInfo(..), IfaceIdDetails(..), IfaceUnfolding(..),
        IfaceInfoItem(..), IfaceRule(..), IfaceAnnotation(..), IfaceAnnTarget,
        IfaceClsInst(..), IfaceFamInst(..), IfaceTickish(..), 
        IfaceBang(..), IfaceAxBranch(..),

        -- Misc
        ifaceDeclImplicitBndrs, visibleIfConDecls,
        ifaceDeclFingerprints,

        -- Free Names
        freeNamesIfDecl, freeNamesIfRule, freeNamesIfFamInst,

        -- Pretty printing
        pprIfaceExpr, pprIfaceDeclHead
    ) where

#include "HsVersions.h"

import TyCon( SynTyConRhs(..) )
import IfaceType
import CoreSyn( DFunArg, dfunArgExprs )
import PprCore()            -- Printing DFunArgs
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
import TysWiredIn ( eqTyConName )
import Fingerprint
import Binary

import System.IO.Unsafe

infixl 3 &&&
\end{code}


%************************************************************************
%*                                                                      *
    Data type declarations
%*                                                                      *
%************************************************************************

\begin{code}
data IfaceDecl
  = IfaceId { ifName      :: OccName,
              ifType      :: IfaceType,
              ifIdDetails :: IfaceIdDetails,
              ifIdInfo    :: IfaceIdInfo }

  | IfaceData { ifName       :: OccName,        -- Type constructor
                ifCType      :: Maybe CType,    -- C type for CAPI FFI
                ifTyVars     :: [IfaceTvBndr],  -- Type variables
                ifCtxt       :: IfaceContext,   -- The "stupid theta"
                ifCons       :: IfaceConDecls,  -- Includes new/data/data family info
                ifRec        :: RecFlag,        -- Recursive or not?
                ifPromotable :: Bool,           -- Promotable to kind level?
                ifGadtSyntax :: Bool,           -- True <=> declared using
                                                -- GADT syntax
                ifAxiom      :: Maybe IfExtName -- The axiom, for a newtype, 
                                                -- or data/newtype family instance
    }

  | IfaceSyn  { ifName    :: OccName,           -- Type constructor
                ifTyVars  :: [IfaceTvBndr],     -- Type variables
                ifSynKind :: IfaceKind,         -- Kind of the *rhs* (not of the tycon)
                ifSynRhs  :: SynTyConRhs IfaceType }

  | IfaceClass { ifCtxt    :: IfaceContext,     -- Context...
                 ifName    :: OccName,          -- Name of the class TyCon
                 ifTyVars  :: [IfaceTvBndr],    -- Type variables
                 ifFDs     :: [FunDep FastString], -- Functional dependencies
                 ifATs     :: [IfaceAT],      -- Associated type families
                 ifSigs    :: [IfaceClassOp],   -- Method signatures
                 ifRec     :: RecFlag           -- Is newtype/datatype associated
                                                --   with the class recursive?
    }

  | IfaceAxiom { ifName       :: OccName,        -- Axiom name
                 ifTyCon      :: IfaceTyCon,     -- LHS TyCon
                 ifAxBranches :: [IfaceAxBranch] -- Branches
    }

  | IfaceForeign { ifName :: OccName,           -- Needs expanding when we move
                                                -- beyond .NET
                   ifExtName :: Maybe FastString }

data IfaceClassOp = IfaceClassOp OccName DefMethSpec IfaceType
        -- Nothing    => no default method
        -- Just False => ordinary polymorphic default method
        -- Just True  => generic default method

data IfaceAT = IfaceAT IfaceDecl [IfaceATDefault]
        -- Nothing => no default associated type instance
        -- Just ds => default associated type instance from these templates

data IfaceATDefault = IfaceATD [IfaceTvBndr] [IfaceType] IfaceType
        -- Each associated type default template is a triple of:
        --   1. TyVars of the RHS and family arguments (including the class TVs)
        --   3. The instantiated family arguments
        --   2. The RHS of the synonym

-- this is just like CoAxBranch
data IfaceAxBranch = IfaceAxBranch { ifaxbTyVars :: [IfaceTvBndr]
                                   , ifaxbLHS    :: [IfaceType]
                                   , ifaxbRHS    :: IfaceType }

data IfaceConDecls
  = IfAbstractTyCon Bool        -- c.f TyCon.AbstractTyCon
  | IfDataFamTyCon              -- Data family
  | IfDataTyCon [IfaceConDecl]  -- Data type decls
  | IfNewTyCon  IfaceConDecl    -- Newtype decls

visibleIfConDecls :: IfaceConDecls -> [IfaceConDecl]
visibleIfConDecls (IfAbstractTyCon {}) = []
visibleIfConDecls IfDataFamTyCon      = []
visibleIfConDecls (IfDataTyCon cs)     = cs
visibleIfConDecls (IfNewTyCon c)       = [c]

data IfaceConDecl
  = IfCon {
        ifConOcc     :: OccName,                -- Constructor name
        ifConWrapper :: Bool,                   -- True <=> has a wrapper
        ifConInfix   :: Bool,                   -- True <=> declared infix
        ifConUnivTvs :: [IfaceTvBndr],          -- Universal tyvars
        ifConExTvs   :: [IfaceTvBndr],          -- Existential tyvars
        ifConEqSpec  :: [(OccName,IfaceType)],  -- Equality contraints
        ifConCtxt    :: IfaceContext,           -- Non-stupid context
        ifConArgTys  :: [IfaceType],            -- Arg types
        ifConFields  :: [OccName],              -- ...ditto... (field labels)
        ifConStricts :: [IfaceBang]}            -- Empty (meaning all lazy),
                                                -- or 1-1 corresp with arg tys

data IfaceBang
  = IfNoBang | IfStrict | IfUnpack | IfUnpackCo IfaceCoercion

data IfaceClsInst
  = IfaceClsInst { ifInstCls  :: IfExtName,                -- See comments with
                   ifInstTys  :: [Maybe IfaceTyCon],       -- the defn of ClsInst
                   ifDFun     :: IfExtName,                -- The dfun
                   ifOFlag    :: OverlapFlag,              -- Overlap flag
                   ifInstOrph :: Maybe OccName }           -- See Note [Orphans]
        -- There's always a separate IfaceDecl for the DFun, which gives
        -- its IdInfo with its full type and version number.
        -- The instance declarations taken together have a version number,
        -- and we don't want that to wobble gratuitously
        -- If this instance decl is *used*, we'll record a usage on the dfun;
        -- and if the head does not change it won't be used if it wasn't before

-- The ifFamInstTys field of IfaceFamInst contains a list of the rough
-- match types, one per branch... but each "rough match types" is itself
-- a list of Maybe IfaceTyCon. So, we get [[Maybe IfaceTyCon]].
data IfaceFamInst
  = IfaceFamInst { ifFamInstFam   :: IfExtName            -- Family name
                 , ifFamInstGroup :: Bool                 -- Is this a group?
                 , ifFamInstTys   :: [[Maybe IfaceTyCon]] -- See above
                 , ifFamInstAxiom :: IfExtName            -- The axiom
                 , ifFamInstOrph  :: Maybe OccName        -- Just like IfaceClsInst
                 }

data IfaceRule
  = IfaceRule {
        ifRuleName   :: RuleName,
        ifActivation :: Activation,
        ifRuleBndrs  :: [IfaceBndr],    -- Tyvars and term vars
        ifRuleHead   :: IfExtName,      -- Head of lhs
        ifRuleArgs   :: [IfaceExpr],    -- Args of LHS
        ifRuleRhs    :: IfaceExpr,
        ifRuleAuto   :: Bool,
        ifRuleOrph   :: Maybe OccName   -- Just like IfaceClsInst
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
  | IfDFunId Int          -- Number of silent args

data IfaceIdInfo
  = NoInfo                      -- When writing interface file without -O
  | HasInfo [IfaceInfoItem]     -- Has info, and here it is

-- Here's a tricky case:
--   * Compile with -O module A, and B which imports A.f
--   * Change function f in A, and recompile without -O
--   * When we read in old A.hi we read in its IdInfo (as a thunk)
--      (In earlier GHCs we used to drop IdInfo immediately on reading,
--       but we do not do that now.  Instead it's discarded when the
--       ModIface is read into the various decl pools.)
--   * The version comparison sees that new (=NoInfo) differs from old (=HasInfo *)
--      and so gives a new version.

data IfaceInfoItem
  = HsArity         Arity
  | HsStrictness    StrictSig
  | HsInline        InlinePragma
  | HsUnfold        Bool             -- True <=> isStrongLoopBreaker is true
                    IfaceUnfolding   -- See Note [Expose recursive functions]
  | HsNoCafRefs

-- NB: Specialisations and rules come in separately and are
-- only later attached to the Id.  Partial reason: some are orphans.

data IfaceUnfolding
  = IfCoreUnfold Bool IfaceExpr -- True <=> INLINABLE, False <=> regular unfolding
                                -- Possibly could eliminate the Bool here, the information
                                -- is also in the InlinePragma.

  | IfCompulsory IfaceExpr      -- Only used for default methods, in fact

  | IfInlineRule Arity          -- INLINE pragmas
                 Bool           -- OK to inline even if *un*-saturated
                 Bool           -- OK to inline even if context is boring
                 IfaceExpr

  | IfExtWrapper Arity IfExtName  -- NB: sometimes we need a IfExtName (not just IfLclName)
  | IfLclWrapper Arity IfLclName  --     because the worker can simplify to a function in
                                  --     another module.

  | IfDFunUnfold [DFunArg IfaceExpr]

--------------------------------
data IfaceExpr
  = IfaceLcl    IfLclName
  | IfaceExt    IfExtName
  | IfaceType   IfaceType
  | IfaceCo     IfaceType		-- We re-use IfaceType for coercions
  | IfaceTuple 	TupleSort [IfaceExpr]	-- Saturated; type arguments omitted
  | IfaceLam 	IfaceBndr IfaceExpr
  | IfaceApp 	IfaceExpr IfaceExpr
  | IfaceCase	IfaceExpr IfLclName [IfaceAlt]
  | IfaceECase  IfaceExpr IfaceType     -- See Note [Empty case alternatives]
  | IfaceLet	IfaceBinding  IfaceExpr
  | IfaceCast   IfaceExpr IfaceCoercion
  | IfaceLit    Literal
  | IfaceFCall  ForeignCall IfaceType
  | IfaceTick   IfaceTickish IfaceExpr    -- from Tick tickish E

data IfaceTickish
  = IfaceHpcTick Module Int                -- from HpcTick x
  | IfaceSCC     CostCentre Bool Bool      -- from ProfNote
  -- no breakpoints: we never export these into interface files

type IfaceAlt = (IfaceConAlt, [IfLclName], IfaceExpr)
        -- Note: IfLclName, not IfaceBndr (and same with the case binder)
        -- We reconstruct the kind/type of the thing from the context
        -- thus saving bulk in interface files

data IfaceConAlt = IfaceDefault
                 | IfaceDataAlt IfExtName
                 | IfaceLitAlt Literal

data IfaceBinding
  = IfaceNonRec IfaceLetBndr IfaceExpr
  | IfaceRec    [(IfaceLetBndr, IfaceExpr)]

-- IfaceLetBndr is like IfaceIdBndr, but has IdInfo too
-- It's used for *non-top-level* let/rec binders
-- See Note [IdInfo on nested let-bindings]
data IfaceLetBndr = IfLetBndr IfLclName IfaceType IfaceIdInfo
\end{code}

Note [Empty case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In IfaceSyn an IfaceCase does not record the types of the alternatives,
unlike CorSyn Case.  But we need this type if the alternatives are empty.
Hence IfaceECase.  See Note [Empty case alternatives] in CoreSyn.

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

In general we retain all info that is left by CoreTidy.tidyLetBndr, since
that is what is seen by importing module with --make

Note [Orphans]: the ifInstOrph and ifRuleOrph fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Class instances, rules, and family instances are divided into orphans
and non-orphans.  Roughly speaking, an instance/rule is an orphan if
its left hand side mentions nothing defined in this module.  Orphan-hood
has two major consequences

 * A non-orphan is not finger-printed separately.  Instead, for
   fingerprinting purposes it is treated as part of the entity it
   mentions on the LHS.  For example
      data T = T1 | T2
      instance Eq T where ....
   The instance (Eq T) is incorprated as part of T's fingerprint.

   In constrast, orphans are all fingerprinted together in the 
   mi_orph_hash field of the ModIface. 
  
   See MkIface.addFingerprints.

 * A module that contains orphans is called an "orphan module".  If
   the module being compiled depends (transitively) on an oprhan
   module M, then M.hi is read in regardless of whether M is oherwise
   needed. This is to ensure that we don't miss any instance decls in
   M.  But it's painful, because it means we need to keep track of all
   the orphan modules below us.

Orphan-hood is computed when we generate an IfaceInst, IfaceRule, or
IfaceFamInst respectively: 

 - If an instance is an orphan its ifInstOprh field is Nothing
   Otherwise ifInstOrph is (Just n) where n is the Name of a
   local class or tycon that witnesses its non-orphan-hood.
   This computation is done by MkIface.instanceToIfaceInst

 - Similarly for ifRuleOrph
   The computation is done by MkIface.coreRuleToIfaceRule

Note [When exactly is an instance decl an orphan?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (see MkIface.instanceToIfaceInst, which implements this)
Roughly speaking, an instance is an orphan if its head (after the =>)
mentions nothing defined in this module.  

Functional dependencies complicate the situation though. Consider

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
See [http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance#Instances]

\begin{code}
-- -----------------------------------------------------------------------------
-- Utils on IfaceSyn

ifaceDeclImplicitBndrs :: IfaceDecl -> [OccName]
--  *Excludes* the 'main' name, but *includes* the implicitly-bound names
-- Deeply revolting, because it has to predict what gets bound,
-- especially the question of whether there's a wrapper for a datacon
-- See Note [Implicit TyThings] in HscTypes

-- N.B. the set of names returned here *must* match the set of
-- TyThings returned by HscTypes.implicitTyThings, in the sense that
-- TyThing.getOccName should define a bijection between the two lists.
-- This invariant is used in LoadIface.loadDecl (see note [Tricky iface loop])
-- The order of the list does not matter.
ifaceDeclImplicitBndrs IfaceData {ifCons = IfAbstractTyCon {}}  = []

-- Newtype
ifaceDeclImplicitBndrs (IfaceData {ifName = tc_occ,
                              ifCons = IfNewTyCon (
                                        IfCon { ifConOcc = con_occ })})
  =   -- implicit newtype coercion
    (mkNewTyCoOcc tc_occ) : -- JPM: newtype coercions shouldn't be implicit
      -- data constructor and worker (newtypes don't have a wrapper)
    [con_occ, mkDataConWorkerOcc con_occ]


ifaceDeclImplicitBndrs (IfaceData {ifName = _tc_occ,
                              ifCons = IfDataTyCon cons })
  = -- for each data constructor in order,
    --    data constructor, worker, and (possibly) wrapper
    concatMap dc_occs cons
  where
    dc_occs con_decl
        | has_wrapper = [con_occ, work_occ, wrap_occ]
        | otherwise   = [con_occ, work_occ]
        where
          con_occ  = ifConOcc con_decl            -- DataCon namespace
          wrap_occ = mkDataConWrapperOcc con_occ  -- Id namespace
          work_occ = mkDataConWorkerOcc con_occ   -- Id namespace
          has_wrapper = ifConWrapper con_decl     -- This is the reason for
                                                  -- having the ifConWrapper field!

ifaceDeclImplicitBndrs (IfaceClass {ifCtxt = sc_ctxt, ifName = cls_tc_occ,
                               ifSigs = sigs, ifATs = ats })
  = --   (possibly) newtype coercion
    co_occs ++
    --    data constructor (DataCon namespace)
    --    data worker (Id namespace)
    --    no wrapper (class dictionaries never have a wrapper)
    [dc_occ, dcww_occ] ++
    -- associated types
    [ifName at | IfaceAT at _ <- ats ] ++
    -- superclass selectors
    [mkSuperDictSelOcc n cls_tc_occ | n <- [1..n_ctxt]] ++
    -- operation selectors
    [op | IfaceClassOp op  _ _ <- sigs]
  where
    n_ctxt = length sc_ctxt
    n_sigs = length sigs
    co_occs | is_newtype = [mkNewTyCoOcc cls_tc_occ]
            | otherwise  = []
    dcww_occ = mkDataConWorkerOcc dc_occ
    dc_occ = mkClassDataConOcc cls_tc_occ
    is_newtype = n_sigs + n_ctxt == 1 -- Sigh

ifaceDeclImplicitBndrs _ = []

-- -----------------------------------------------------------------------------
-- The fingerprints of an IfaceDecl

       -- We better give each name bound by the declaration a
       -- different fingerprint!  So we calculate the fingerprint of
       -- each binder by combining the fingerprint of the whole
       -- declaration with the name of the binder. (#5614, #7215)
ifaceDeclFingerprints :: Fingerprint -> IfaceDecl -> [(OccName,Fingerprint)]
ifaceDeclFingerprints hash decl
  = (ifName decl, hash) :
    [ (occ, computeFingerprint' (hash,occ))
    | occ <- ifaceDeclImplicitBndrs decl ]
  where
     computeFingerprint' =
       unsafeDupablePerformIO
        . computeFingerprint (panic "ifaceDeclFingerprints")

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

pprIfaceDecl (IfaceSyn {ifName = tycon,
                        ifTyVars = tyvars,
                        ifSynRhs = SynonymTyCon mono_ty})
  = hang (ptext (sLit "type") <+> pprIfaceDeclHead [] tycon tyvars)
       4 (vcat [equals <+> ppr mono_ty])

pprIfaceDecl (IfaceSyn {ifName = tycon, ifTyVars = tyvars,
                        ifSynRhs = SynFamilyTyCon {}, ifSynKind = kind })
  = hang (ptext (sLit "type family") <+> pprIfaceDeclHead [] tycon tyvars)
       4 (dcolon <+> ppr kind)

pprIfaceDecl (IfaceData {ifName = tycon, ifCType = cType,
                         ifCtxt = context,
                         ifTyVars = tyvars, ifCons = condecls,
                         ifRec = isrec, ifPromotable = is_prom,
                         ifAxiom = mbAxiom})
  = hang (pp_nd <+> pprIfaceDeclHead context tycon tyvars)
       4 (vcat [ pprCType cType
               , pprRec isrec <> comma <+> pp_prom 
               , pp_condecls tycon condecls
               , pprAxiom mbAxiom])
  where
    pp_prom | is_prom   = ptext (sLit "Promotable")
            | otherwise = ptext (sLit "Not promotable")
    pp_nd = case condecls of
                IfAbstractTyCon dis -> ptext (sLit "abstract") <> parens (ppr dis)
                IfDataFamTyCon     -> ptext (sLit "data family")
                IfDataTyCon _       -> ptext (sLit "data")
                IfNewTyCon _        -> ptext (sLit "newtype")

pprIfaceDecl (IfaceClass {ifCtxt = context, ifName = clas, ifTyVars = tyvars,
                          ifFDs = fds, ifATs = ats, ifSigs = sigs,
                          ifRec = isrec})
  = hang (ptext (sLit "class") <+> pprIfaceDeclHead context clas tyvars <+> pprFundeps fds)
       4 (vcat [pprRec isrec,
                sep (map ppr ats),
                sep (map ppr sigs)])

pprIfaceDecl (IfaceAxiom {ifName = name, ifTyCon = tycon, ifAxBranches = branches })
  = hang (ptext (sLit "axiom") <+> ppr name <> colon)
       2 (vcat $ map (pprIfaceAxBranch tycon) branches)

pprIfaceAxBranch :: IfaceTyCon -> IfaceAxBranch -> SDoc
pprIfaceAxBranch tc (IfaceAxBranch { ifaxbTyVars = tyvars, ifaxbLHS = lhs, ifaxbRHS = rhs })
  = pprIfaceTvBndrs tyvars <> dot <+> ppr (IfaceTyConApp tc lhs) <+> text "~#" <+> ppr rhs

pprCType :: Maybe CType -> SDoc
pprCType Nothing = ptext (sLit "No C type associated")
pprCType (Just cType) = ptext (sLit "C type:") <+> ppr cType

pprRec :: RecFlag -> SDoc
pprRec isrec = ptext (sLit "RecFlag") <+> ppr isrec

pprAxiom :: Maybe Name -> SDoc
pprAxiom Nothing   = ptext (sLit "FamilyInstance: none")
pprAxiom (Just ax) = ptext (sLit "FamilyInstance:") <+> ppr ax

instance Outputable IfaceClassOp where
   ppr (IfaceClassOp n dm ty) = ppr n <+> ppr dm <+> dcolon <+> ppr ty

instance Outputable IfaceAT where
   ppr (IfaceAT d defs) = hang (ppr d) 2 (vcat (map ppr defs))

instance Outputable IfaceATDefault where
   ppr (IfaceATD tvs pat_tys ty) = ppr tvs <+> hsep (map ppr pat_tys) <+> char '=' <+> ppr ty

pprIfaceDeclHead :: IfaceContext -> OccName -> [IfaceTvBndr] -> SDoc
pprIfaceDeclHead context thing tyvars
  = hsep [pprIfaceContext context, parenSymOcc thing (ppr thing),
          pprIfaceTvBndrs tyvars]

pp_condecls :: OccName -> IfaceConDecls -> SDoc
pp_condecls _  (IfAbstractTyCon {}) = empty
pp_condecls _  IfDataFamTyCon      = empty
pp_condecls tc (IfNewTyCon c)   = equals <+> pprIfaceConDecl tc c
pp_condecls tc (IfDataTyCon cs) = equals <+> sep (punctuate (ptext (sLit " |"))
                                                            (map (pprIfaceConDecl tc) cs))

mkIfaceEqPred :: IfaceType -> IfaceType -> IfacePredType
-- IA0_NOTE: This is wrong, but only used for pretty-printing.
mkIfaceEqPred ty1 ty2 = IfaceTyConApp (IfaceTc eqTyConName) [ty1, ty2]

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
    ppr_bang IfNoBang = char '_'        -- Want to see these
    ppr_bang IfStrict = char '!'
    ppr_bang IfUnpack = ptext (sLit "!!")
    ppr_bang (IfUnpackCo co) = ptext (sLit "!!") <> pprParendIfaceType co

    main_payload = ppr name <+> dcolon <+>
                   pprIfaceForAllPart (univ_tvs ++ ex_tvs) (eq_ctxt ++ ctxt) pp_tau

    eq_ctxt = [(mkIfaceEqPred (IfaceTyVar (occNameFS tv)) ty)
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

instance Outputable IfaceClsInst where
  ppr (IfaceClsInst {ifDFun = dfun_id, ifOFlag = flag,
                  ifInstCls = cls, ifInstTys = mb_tcs})
    = hang (ptext (sLit "instance") <+> ppr flag
                <+> ppr cls <+> brackets (pprWithCommas ppr_rough mb_tcs))
         2 (equals <+> ppr dfun_id)

instance Outputable IfaceFamInst where
  ppr (IfaceFamInst {ifFamInstFam = fam, ifFamInstTys = mb_tcss,
                     ifFamInstAxiom = tycon_ax})
    = hang (ptext (sLit "family instance") <+>
            ppr fam <+> pprWithCommas (brackets . pprWithCommas ppr_rough) mb_tcss)
         2 (equals <+> ppr tycon_ax)

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

-- | Pretty Print an IfaceExpre
--
-- The first argument should be a function that adds parens in context that need
-- an atomic value (e.g. function args)
pprIfaceExpr :: (SDoc -> SDoc) -> IfaceExpr -> SDoc

pprIfaceExpr _       (IfaceLcl v)       = ppr v
pprIfaceExpr _       (IfaceExt v)       = ppr v
pprIfaceExpr _       (IfaceLit l)       = ppr l
pprIfaceExpr _       (IfaceFCall cc ty) = braces (ppr cc <+> ppr ty)
pprIfaceExpr _       (IfaceType ty)     = char '@' <+> pprParendIfaceType ty
pprIfaceExpr _       (IfaceCo co)       = text "@~" <+> pprParendIfaceType co

pprIfaceExpr add_par app@(IfaceApp _ _) = add_par (pprIfaceApp app [])
pprIfaceExpr _       (IfaceTuple c as)  = tupleParens c (interpp'SP as)

pprIfaceExpr add_par i@(IfaceLam _ _)
  = add_par (sep [char '\\' <+> sep (map ppr bndrs) <+> arrow,
                  pprIfaceExpr noParens body])
  where
    (bndrs,body) = collect [] i
    collect bs (IfaceLam b e) = collect (b:bs) e
    collect bs e              = (reverse bs, e)

pprIfaceExpr add_par (IfaceECase scrut ty)
  = add_par (sep [ ptext (sLit "case") <+> pprIfaceExpr noParens scrut 
                 , ptext (sLit "ret_ty") <+> pprParendIfaceType ty
                 , ptext (sLit "of {}") ])

pprIfaceExpr add_par (IfaceCase scrut bndr [(con, bs, rhs)])
  = add_par (sep [ptext (sLit "case") 
			<+> pprIfaceExpr noParens scrut <+> ptext (sLit "of") 
			<+> ppr bndr <+> char '{' <+> ppr_con_bs con bs <+> arrow,
  		  pprIfaceExpr noParens rhs <+> char '}'])

pprIfaceExpr add_par (IfaceCase scrut bndr alts)
  = add_par (sep [ptext (sLit "case") 
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

pprIfaceExpr add_par (IfaceTick tickish e)
  = add_par (pprIfaceTickish tickish <+> pprIfaceExpr noParens e)

ppr_alt :: (IfaceConAlt, [IfLclName], IfaceExpr) -> SDoc
ppr_alt (con, bs, rhs) = sep [ppr_con_bs con bs,
                         arrow <+> pprIfaceExpr noParens rhs]

ppr_con_bs :: IfaceConAlt -> [IfLclName] -> SDoc
ppr_con_bs con bs = ppr con <+> hsep (map ppr bs)

ppr_bind :: (IfaceLetBndr, IfaceExpr) -> SDoc
ppr_bind (IfLetBndr b ty info, rhs)
  = sep [hang (ppr b <+> dcolon <+> ppr ty) 2 (ppr info),
         equals <+> pprIfaceExpr noParens rhs]

------------------
pprIfaceTickish :: IfaceTickish -> SDoc
pprIfaceTickish (IfaceHpcTick m ix)
  = braces (text "tick" <+> ppr m <+> ppr ix)
pprIfaceTickish (IfaceSCC cc tick scope)
  = braces (pprCostCentreCore cc <+> ppr tick <+> ppr scope)

------------------
pprIfaceApp :: IfaceExpr -> [SDoc] -> SDoc
pprIfaceApp (IfaceApp fun arg) args = pprIfaceApp fun $
                                          nest 2 (pprParendIfaceExpr arg) : args
pprIfaceApp fun                args = sep (pprParendIfaceExpr fun : args)

------------------
instance Outputable IfaceConAlt where
    ppr IfaceDefault      = text "DEFAULT"
    ppr (IfaceLitAlt l)   = ppr l
    ppr (IfaceDataAlt d)  = ppr d

------------------
instance Outputable IfaceIdDetails where
  ppr IfVanillaId       = empty
  ppr (IfRecSelId tc b) = ptext (sLit "RecSel") <+> ppr tc
                          <+> if b then ptext (sLit "<naughty>") else empty
  ppr (IfDFunId ns)     = ptext (sLit "DFunId") <> brackets (int ns)

instance Outputable IfaceIdInfo where
  ppr NoInfo       = empty
  ppr (HasInfo is) = ptext (sLit "{-") <+> pprWithCommas ppr is
                     <+> ptext (sLit "-}")

instance Outputable IfaceInfoItem where
  ppr (HsUnfold lb unf)     = ptext (sLit "Unfolding")
                              <> ppWhen lb (ptext (sLit "(loop-breaker)"))
                              <> colon <+> ppr unf
  ppr (HsInline prag)       = ptext (sLit "Inline:") <+> ppr prag
  ppr (HsArity arity)       = ptext (sLit "Arity:") <+> int arity
  ppr (HsStrictness str) = ptext (sLit "Strictness:") <+> pprIfaceStrictSig str
  ppr HsNoCafRefs           = ptext (sLit "HasNoCafRefs")

instance Outputable IfaceUnfolding where
  ppr (IfCompulsory e)     = ptext (sLit "<compulsory>") <+> parens (ppr e)
  ppr (IfCoreUnfold s e)   = (if s then ptext (sLit "<stable>") else empty)
                              <+> parens (ppr e)
  ppr (IfInlineRule a uok bok e) = sep [ptext (sLit "InlineRule")
                                            <+> ppr (a,uok,bok),
                                        pprParendIfaceExpr e]
  ppr (IfLclWrapper a wkr) = ptext (sLit "Worker(lcl):") <+> ppr wkr
                             <+> parens (ptext (sLit "arity") <+> int a)
  ppr (IfExtWrapper a wkr) = ptext (sLit "Worker(ext0:") <+> ppr wkr
                             <+> parens (ptext (sLit "arity") <+> int a)
  ppr (IfDFunUnfold ns)    = ptext (sLit "DFun:")
                             <+> brackets (pprWithCommas ppr ns)

-- -----------------------------------------------------------------------------
-- | Finding the Names in IfaceSyn

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
  maybe emptyNameSet unitNameSet (ifAxiom d) &&&
  freeNamesIfContext (ifCtxt d) &&&
  freeNamesIfConDecls (ifCons d)
freeNamesIfDecl d@IfaceSyn{} =
  freeNamesIfTvBndrs (ifTyVars d) &&&
  freeNamesIfSynRhs (ifSynRhs d) &&&
  freeNamesIfKind (ifSynKind d) -- IA0_NOTE: because of promotion, we
                                -- return names in the kind signature
freeNamesIfDecl d@IfaceClass{} =
  freeNamesIfTvBndrs (ifTyVars d) &&&
  freeNamesIfContext (ifCtxt d) &&&
  fnList freeNamesIfAT     (ifATs d) &&&
  fnList freeNamesIfClsSig (ifSigs d)
freeNamesIfDecl d@IfaceAxiom{} =
  freeNamesIfTc (ifTyCon d) &&&
  fnList freeNamesIfAxBranch (ifAxBranches d)

freeNamesIfAxBranch :: IfaceAxBranch -> NameSet
freeNamesIfAxBranch (IfaceAxBranch { ifaxbTyVars = tyvars
                                   , ifaxbLHS    = lhs
                                   , ifaxbRHS    = rhs }) =
  freeNamesIfTvBndrs tyvars &&&
  fnList freeNamesIfType lhs &&&
  freeNamesIfType rhs

freeNamesIfIdDetails :: IfaceIdDetails -> NameSet
freeNamesIfIdDetails (IfRecSelId tc _) = freeNamesIfTc tc
freeNamesIfIdDetails _                 = emptyNameSet

-- All other changes are handled via the version info on the tycon
freeNamesIfSynRhs :: SynTyConRhs IfaceType -> NameSet
freeNamesIfSynRhs (SynonymTyCon ty) = freeNamesIfType ty
freeNamesIfSynRhs _                 = emptyNameSet

freeNamesIfContext :: IfaceContext -> NameSet
freeNamesIfContext = fnList freeNamesIfType

freeNamesIfAT :: IfaceAT -> NameSet
freeNamesIfAT (IfaceAT decl defs)
  = freeNamesIfDecl decl &&&
    fnList fn_at_def defs
  where
    fn_at_def (IfaceATD tvs pat_tys ty)
      = freeNamesIfTvBndrs tvs &&&
        fnList freeNamesIfType pat_tys &&&
        freeNamesIfType ty

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

freeNamesIfKind :: IfaceType -> NameSet
freeNamesIfKind = freeNamesIfType

freeNamesIfType :: IfaceType -> NameSet
freeNamesIfType (IfaceTyVar _)        = emptyNameSet
freeNamesIfType (IfaceAppTy s t)      = freeNamesIfType s &&& freeNamesIfType t
freeNamesIfType (IfaceTyConApp tc ts) =
   freeNamesIfTc tc &&& fnList freeNamesIfType ts
freeNamesIfType (IfaceLitTy _)        = emptyNameSet
freeNamesIfType (IfaceForAllTy tv t)  =
   freeNamesIfTvBndr tv &&& freeNamesIfType t
freeNamesIfType (IfaceFunTy s t)      = freeNamesIfType s &&& freeNamesIfType t
freeNamesIfType (IfaceCoConApp tc ts) = 
   freeNamesIfCo tc &&& fnList freeNamesIfType ts

freeNamesIfTvBndrs :: [IfaceTvBndr] -> NameSet
freeNamesIfTvBndrs = fnList freeNamesIfTvBndr

freeNamesIfBndr :: IfaceBndr -> NameSet
freeNamesIfBndr (IfaceIdBndr b) = freeNamesIfIdBndr b
freeNamesIfBndr (IfaceTvBndr b) = freeNamesIfTvBndr b

freeNamesIfLetBndr :: IfaceLetBndr -> NameSet
-- Remember IfaceLetBndr is used only for *nested* bindings
-- The IdInfo can contain an unfolding (in the case of
-- local INLINE pragmas), so look there too
freeNamesIfLetBndr (IfLetBndr _name ty info) = freeNamesIfType ty
                                             &&& freeNamesIfIdInfo info

freeNamesIfTvBndr :: IfaceTvBndr -> NameSet
freeNamesIfTvBndr (_fs,k) = freeNamesIfKind k
    -- kinds can have Names inside, because of promotion

freeNamesIfIdBndr :: IfaceIdBndr -> NameSet
freeNamesIfIdBndr = freeNamesIfTvBndr

freeNamesIfIdInfo :: IfaceIdInfo -> NameSet
freeNamesIfIdInfo NoInfo      = emptyNameSet
freeNamesIfIdInfo (HasInfo i) = fnList freeNamesItem i

freeNamesItem :: IfaceInfoItem -> NameSet
freeNamesItem (HsUnfold _ u) = freeNamesIfUnfold u
freeNamesItem _              = emptyNameSet

freeNamesIfUnfold :: IfaceUnfolding -> NameSet
freeNamesIfUnfold (IfCoreUnfold _ e)     = freeNamesIfExpr e
freeNamesIfUnfold (IfCompulsory e)       = freeNamesIfExpr e
freeNamesIfUnfold (IfInlineRule _ _ _ e) = freeNamesIfExpr e
freeNamesIfUnfold (IfExtWrapper _ v)     = unitNameSet v
freeNamesIfUnfold (IfLclWrapper {})      = emptyNameSet
freeNamesIfUnfold (IfDFunUnfold vs)      = fnList freeNamesIfExpr (dfunArgExprs vs)

freeNamesIfExpr :: IfaceExpr -> NameSet
freeNamesIfExpr (IfaceExt v)      = unitNameSet v
freeNamesIfExpr (IfaceFCall _ ty) = freeNamesIfType ty
freeNamesIfExpr (IfaceType ty)    = freeNamesIfType ty
freeNamesIfExpr (IfaceCo co)      = freeNamesIfType co
freeNamesIfExpr (IfaceTuple _ as) = fnList freeNamesIfExpr as
freeNamesIfExpr (IfaceLam b body) = freeNamesIfBndr b &&& freeNamesIfExpr body
freeNamesIfExpr (IfaceApp f a)    = freeNamesIfExpr f &&& freeNamesIfExpr a
freeNamesIfExpr (IfaceCast e co)  = freeNamesIfExpr e &&& freeNamesIfType co
freeNamesIfExpr (IfaceTick _ e)   = freeNamesIfExpr e
freeNamesIfExpr (IfaceECase e ty) = freeNamesIfExpr e &&& freeNamesIfType ty
freeNamesIfExpr (IfaceCase s _ alts)
  = freeNamesIfExpr s 
    &&& fnList fn_alt alts &&& fn_cons alts
  where
    fn_alt (_con,_bs,r) = freeNamesIfExpr r

    -- Depend on the data constructors.  Just one will do!
    -- Note [Tracking data constructors]
    fn_cons []                            = emptyNameSet
    fn_cons ((IfaceDefault    ,_,_) : xs) = fn_cons xs
    fn_cons ((IfaceDataAlt con,_,_) : _ ) = unitNameSet con
    fn_cons (_                      : _ ) = emptyNameSet

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

freeNamesIfCo :: IfaceCoCon -> NameSet
freeNamesIfCo (IfaceCoAx tc _) = unitNameSet tc
-- ToDo: include IfaceIPCoAx? Probably not necessary.
freeNamesIfCo _ = emptyNameSet

freeNamesIfRule :: IfaceRule -> NameSet
freeNamesIfRule (IfaceRule { ifRuleBndrs = bs, ifRuleHead = f
                           , ifRuleArgs = es, ifRuleRhs = rhs })
  = unitNameSet f &&&
    fnList freeNamesIfBndr bs &&&
    fnList freeNamesIfExpr es &&&
    freeNamesIfExpr rhs
    
freeNamesIfFamInst :: IfaceFamInst -> NameSet
freeNamesIfFamInst (IfaceFamInst { ifFamInstFam = famName
                                 , ifFamInstAxiom = axName })
  = unitNameSet famName &&&
    unitNameSet axName

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

