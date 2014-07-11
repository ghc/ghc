{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998
-}

{-# LANGUAGE CPP #-}

module IfaceSyn (
        module IfaceType,

        IfaceDecl(..), IfaceFamTyConFlav(..), IfaceClassOp(..), IfaceAT(..),
        IfaceConDecl(..), IfaceConDecls(..), IfaceEqSpec,
        IfaceExpr(..), IfaceAlt, IfaceLetBndr(..),
        IfaceBinding(..), IfaceConAlt(..),
        IfaceIdInfo(..), IfaceIdDetails(..), IfaceUnfolding(..),
        IfaceInfoItem(..), IfaceRule(..), IfaceAnnotation(..), IfaceAnnTarget,
        IfaceClsInst(..), IfaceFamInst(..), IfaceTickish(..),
        IfaceBang(..),
        IfaceSrcBang(..), SrcUnpackedness(..), SrcStrictness(..),
        IfaceAxBranch(..),
        IfaceTyConParent(..),

        -- Misc
        ifaceDeclImplicitBndrs, visibleIfConDecls,
        ifaceDeclFingerprints,

        -- Free Names
        freeNamesIfDecl, freeNamesIfRule, freeNamesIfFamInst,

        -- Pretty printing
        pprIfaceExpr,
        pprIfaceDecl,
        ShowSub(..), ShowHowMuch(..)
    ) where

#include "HsVersions.h"

import IfaceType
import PprCore()            -- Printing DFunArgs
import Demand
import Class
import NameSet
import CoAxiom ( BranchIndex, Role )
import Name
import CostCentre
import Literal
import ForeignCall
import Annotations( AnnPayload, AnnTarget )
import BasicTypes
import Outputable
import FastString
import Module
import SrcLoc
import Fingerprint
import Binary
import BooleanFormula ( BooleanFormula )
import HsBinds
import TyCon ( Role (..), Injectivity(..) )
import StaticFlags (opt_PprStyle_Debug)
import Util( filterOut, filterByList )
import InstEnv
import DataCon (SrcStrictness(..), SrcUnpackedness(..))

import Control.Monad
import System.IO.Unsafe
import Data.Maybe (isJust)

infixl 3 &&&

{-
************************************************************************
*                                                                      *
                    Declarations
*                                                                      *
************************************************************************
-}

type IfaceTopBndr = OccName
  -- It's convenient to have an OccName in the IfaceSyn, altough in each
  -- case the namespace is implied by the context. However, having an
  -- OccNames makes things like ifaceDeclImplicitBndrs and ifaceDeclFingerprints
  -- very convenient.
  --
  -- We don't serialise the namespace onto the disk though; rather we
  -- drop it when serialising and add it back in when deserialising.

data IfaceDecl
  = IfaceId { ifName      :: IfaceTopBndr,
              ifType      :: IfaceType,
              ifIdDetails :: IfaceIdDetails,
              ifIdInfo    :: IfaceIdInfo }

  | IfaceData { ifName       :: IfaceTopBndr,        -- Type constructor
                ifCType      :: Maybe CType,    -- C type for CAPI FFI
                ifTyVars     :: [IfaceTvBndr],  -- Type variables
                ifRoles      :: [Role],         -- Roles
                ifCtxt       :: IfaceContext,   -- The "stupid theta"
                ifCons       :: IfaceConDecls,  -- Includes new/data/data family info
                ifRec        :: RecFlag,        -- Recursive or not?
                ifPromotable :: Bool,           -- Promotable to kind level?
                ifGadtSyntax :: Bool,           -- True <=> declared using
                                                -- GADT syntax
                ifParent     :: IfaceTyConParent -- The axiom, for a newtype,
                                                 -- or data/newtype family instance
    }

  | IfaceSynonym { ifName    :: IfaceTopBndr,      -- Type constructor
                   ifTyVars  :: [IfaceTvBndr],     -- Type variables
                   ifRoles   :: [Role],            -- Roles
                   ifSynKind :: IfaceKind,         -- Kind of the *rhs* (not of
                                                   -- the tycon)
                   ifSynRhs  :: IfaceType }

  | IfaceFamily  { ifName    :: IfaceTopBndr,      -- Type constructor
                   ifTyVars  :: [IfaceTvBndr],     -- Type variables
                   ifResVar  :: Maybe IfLclName,   -- Result variable name, used
                                                   -- only for pretty-printing
                                                   -- with --show-iface
                   ifFamKind :: IfaceKind,         -- Kind of the *rhs* (not of
                                                   -- the tycon)
                   ifFamFlav :: IfaceFamTyConFlav,
                   ifFamInj  :: Injectivity }      -- injectivity information

  | IfaceClass { ifCtxt    :: IfaceContext,             -- Superclasses
                 ifName    :: IfaceTopBndr,             -- Name of the class TyCon
                 ifTyVars  :: [IfaceTvBndr],            -- Type variables
                 ifRoles   :: [Role],                   -- Roles
                 ifFDs     :: [FunDep FastString],      -- Functional dependencies
                 ifATs     :: [IfaceAT],                -- Associated type families
                 ifSigs    :: [IfaceClassOp],           -- Method signatures
                 ifMinDef  :: BooleanFormula IfLclName, -- Minimal complete definition
                 ifRec     :: RecFlag                   -- Is newtype/datatype associated
                                                        --   with the class recursive?
    }

  | IfaceAxiom { ifName       :: IfaceTopBndr,        -- Axiom name
                 ifTyCon      :: IfaceTyCon,     -- LHS TyCon
                 ifRole       :: Role,           -- Role of axiom
                 ifAxBranches :: [IfaceAxBranch] -- Branches
    }

  | IfacePatSyn { ifName          :: IfaceTopBndr,           -- Name of the pattern synonym
                  ifPatIsInfix    :: Bool,
                  ifPatMatcher    :: (IfExtName, Bool),
                  ifPatBuilder    :: Maybe (IfExtName, Bool),
                  -- Everything below is redundant,
                  -- but needed to implement pprIfaceDecl
                  ifPatUnivTvs    :: [IfaceTvBndr],
                  ifPatExTvs      :: [IfaceTvBndr],
                  ifPatProvCtxt   :: IfaceContext,
                  ifPatReqCtxt    :: IfaceContext,
                  ifPatArgs       :: [IfaceType],
                  ifPatTy         :: IfaceType }


data IfaceTyConParent
  = IfNoParent
  | IfDataInstance IfExtName
                   IfaceTyCon
                   IfaceTcArgs

data IfaceFamTyConFlav
  = IfaceOpenSynFamilyTyCon
  | IfaceClosedSynFamilyTyCon (Maybe (IfExtName, [IfaceAxBranch]))
    -- ^ Name of associated axiom and branches for pretty printing purposes,
    -- or 'Nothing' for an empty closed family without an axiom
  | IfaceAbstractClosedSynFamilyTyCon
  | IfaceBuiltInSynFamTyCon -- for pretty printing purposes only

data IfaceClassOp = IfaceClassOp IfaceTopBndr DefMethSpec IfaceType
        -- Nothing    => no default method
        -- Just False => ordinary polymorphic default method
        -- Just True  => generic default method

data IfaceAT = IfaceAT  -- See Class.ClassATItem
                  IfaceDecl          -- The associated type declaration
                  (Maybe IfaceType)  -- Default associated type instance, if any


-- This is just like CoAxBranch
data IfaceAxBranch = IfaceAxBranch { ifaxbTyVars  :: [IfaceTvBndr]
                                   , ifaxbLHS     :: IfaceTcArgs
                                   , ifaxbRoles   :: [Role]
                                   , ifaxbRHS     :: IfaceType
                                   , ifaxbIncomps :: [BranchIndex] }
                                     -- See Note [Storing compatibility] in CoAxiom

data IfaceConDecls
  = IfAbstractTyCon Bool        -- c.f TyCon.AbstractTyCon
  | IfDataFamTyCon              -- Data family
  | IfDataTyCon [IfaceConDecl]  -- Data type decls
  | IfNewTyCon  IfaceConDecl    -- Newtype decls

data IfaceConDecl
  = IfCon {
        ifConOcc     :: IfaceTopBndr,                -- Constructor name
        ifConWrapper :: Bool,                   -- True <=> has a wrapper
        ifConInfix   :: Bool,                   -- True <=> declared infix

        -- The universal type variables are precisely those
        -- of the type constructor of this data constructor
        -- This is *easy* to guarantee when creating the IfCon
        -- but it's not so easy for the original TyCon/DataCon
        -- So this guarantee holds for IfaceConDecl, but *not* for DataCon

        ifConExTvs   :: [IfaceTvBndr],      -- Existential tyvars
        ifConEqSpec  :: IfaceEqSpec,        -- Equality constraints
        ifConCtxt    :: IfaceContext,       -- Non-stupid context
        ifConArgTys  :: [IfaceType],        -- Arg types
        ifConFields  :: [IfaceTopBndr],     -- ...ditto... (field labels)
        ifConStricts :: [IfaceBang],
          -- Empty (meaning all lazy),
          -- or 1-1 corresp with arg tys
          -- See Note [Bangs on imported data constructors] in MkId
        ifConSrcStricts :: [IfaceSrcBang] } -- empty meaning no src stricts

type IfaceEqSpec = [(IfLclName,IfaceType)]

-- | This corresponds to an HsImplBang; that is, the final
-- implementation decision about the data constructor arg
data IfaceBang
  = IfNoBang | IfStrict | IfUnpack | IfUnpackCo IfaceCoercion

-- | This corresponds to HsSrcBang
data IfaceSrcBang
  = IfSrcBang SrcUnpackedness SrcStrictness

data IfaceClsInst
  = IfaceClsInst { ifInstCls  :: IfExtName,                -- See comments with
                   ifInstTys  :: [Maybe IfaceTyCon],       -- the defn of ClsInst
                   ifDFun     :: IfExtName,                -- The dfun
                   ifOFlag    :: OverlapFlag,              -- Overlap flag
                   ifInstOrph :: IsOrphan }                -- See Note [Orphans] in InstEnv
        -- There's always a separate IfaceDecl for the DFun, which gives
        -- its IdInfo with its full type and version number.
        -- The instance declarations taken together have a version number,
        -- and we don't want that to wobble gratuitously
        -- If this instance decl is *used*, we'll record a usage on the dfun;
        -- and if the head does not change it won't be used if it wasn't before

-- The ifFamInstTys field of IfaceFamInst contains a list of the rough
-- match types
data IfaceFamInst
  = IfaceFamInst { ifFamInstFam      :: IfExtName            -- Family name
                 , ifFamInstTys      :: [Maybe IfaceTyCon]   -- See above
                 , ifFamInstAxiom    :: IfExtName            -- The axiom
                 , ifFamInstOrph     :: IsOrphan             -- Just like IfaceClsInst
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
        ifRuleOrph   :: IsOrphan   -- Just like IfaceClsInst
    }

data IfaceAnnotation
  = IfaceAnnotation {
        ifAnnotatedTarget :: IfaceAnnTarget,
        ifAnnotatedValue  :: AnnPayload
  }

type IfaceAnnTarget = AnnTarget OccName

-- Here's a tricky case:
--   * Compile with -O module A, and B which imports A.f
--   * Change function f in A, and recompile without -O
--   * When we read in old A.hi we read in its IdInfo (as a thunk)
--      (In earlier GHCs we used to drop IdInfo immediately on reading,
--       but we do not do that now.  Instead it's discarded when the
--       ModIface is read into the various decl pools.)
--   * The version comparison sees that new (=NoInfo) differs from old (=HasInfo *)
--      and so gives a new version.

data IfaceIdInfo
  = NoInfo                      -- When writing interface file without -O
  | HasInfo [IfaceInfoItem]     -- Has info, and here it is

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

  | IfDFunUnfold [IfaceBndr] [IfaceExpr]


-- We only serialise the IdDetails of top-level Ids, and even then
-- we only need a very limited selection.  Notably, none of the
-- implicit ones are needed here, because they are not put it
-- interface files

data IfaceIdDetails
  = IfVanillaId
  | IfRecSelId IfaceTyCon Bool
  | IfDFunId

{-
Note [Versioning of instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See [http://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance#Instances]


************************************************************************
*                                                                      *
                Functions over declarations
*                                                                      *
************************************************************************
-}

visibleIfConDecls :: IfaceConDecls -> [IfaceConDecl]
visibleIfConDecls (IfAbstractTyCon {}) = []
visibleIfConDecls IfDataFamTyCon       = []
visibleIfConDecls (IfDataTyCon cs)     = cs
visibleIfConDecls (IfNewTyCon c)       = [c]

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

{-
************************************************************************
*                                                                      *
                Expressions
*                                                                      *
************************************************************************
-}

data IfaceExpr
  = IfaceLcl    IfLclName
  | IfaceExt    IfExtName
  | IfaceType   IfaceType
  | IfaceCo     IfaceCoercion
  | IfaceTuple  TupleSort [IfaceExpr]   -- Saturated; type arguments omitted
  | IfaceLam    IfaceLamBndr IfaceExpr
  | IfaceApp    IfaceExpr IfaceExpr
  | IfaceCase   IfaceExpr IfLclName [IfaceAlt]
  | IfaceECase  IfaceExpr IfaceType     -- See Note [Empty case alternatives]
  | IfaceLet    IfaceBinding  IfaceExpr
  | IfaceCast   IfaceExpr IfaceCoercion
  | IfaceLit    Literal
  | IfaceFCall  ForeignCall IfaceType
  | IfaceTick   IfaceTickish IfaceExpr    -- from Tick tickish E

data IfaceTickish
  = IfaceHpcTick Module Int                -- from HpcTick x
  | IfaceSCC     CostCentre Bool Bool      -- from ProfNote
  | IfaceSource  RealSrcSpan String        -- from SourceNote
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

{-
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


************************************************************************
*                                                                      *
              Printing IfaceDecl
*                                                                      *
************************************************************************
-}

pprAxBranch :: SDoc -> IfaceAxBranch -> SDoc
-- The TyCon might be local (just an OccName), or this might
-- be a branch for an imported TyCon, so it would be an ExtName
-- So it's easier to take an SDoc here
pprAxBranch pp_tc (IfaceAxBranch { ifaxbTyVars = tvs
                                  , ifaxbLHS = pat_tys
                                  , ifaxbRHS = rhs
                                  , ifaxbIncomps = incomps })
  = hang (pprUserIfaceForAll tvs)
       2 (hang pp_lhs 2 (equals <+> ppr rhs))
    $+$
    nest 2 maybe_incomps
  where
    pp_lhs = hang pp_tc 2 (pprParendIfaceTcArgs pat_tys)
    maybe_incomps = ppUnless (null incomps) $ parens $
                    ptext (sLit "incompatible indices:") <+> ppr incomps

instance Outputable IfaceAnnotation where
  ppr (IfaceAnnotation target value) = ppr target <+> colon <+> ppr value

instance HasOccName IfaceClassOp where
  occName (IfaceClassOp n _ _) = n

instance HasOccName IfaceConDecl where
  occName = ifConOcc

instance HasOccName IfaceDecl where
  occName = ifName

instance Outputable IfaceDecl where
  ppr = pprIfaceDecl showAll

data ShowSub
  = ShowSub
      { ss_ppr_bndr :: OccName -> SDoc  -- Pretty-printer for binders in IfaceDecl
                                        -- See Note [Printing IfaceDecl binders]
      , ss_how_much :: ShowHowMuch }

data ShowHowMuch
  = ShowHeader   -- Header information only, not rhs
  | ShowSome [OccName]    -- []     <=> Print all sub-components
                          -- (n:ns) <=> print sub-component 'n' with ShowSub=ns
                          --            elide other sub-components to "..."
                          -- May 14: the list is max 1 element long at the moment
  | ShowIface    -- Everything including GHC-internal information (used in --show-iface)

showAll :: ShowSub
showAll = ShowSub { ss_how_much = ShowIface, ss_ppr_bndr = ppr }

ppShowIface :: ShowSub -> SDoc -> SDoc
ppShowIface (ShowSub { ss_how_much = ShowIface }) doc = doc
ppShowIface _                                     _   = Outputable.empty

ppShowRhs :: ShowSub -> SDoc -> SDoc
ppShowRhs (ShowSub { ss_how_much = ShowHeader }) _   = Outputable.empty
ppShowRhs _                                      doc = doc

showSub :: HasOccName n => ShowSub -> n -> Bool
showSub (ShowSub { ss_how_much = ShowHeader })     _     = False
showSub (ShowSub { ss_how_much = ShowSome (n:_) }) thing = n == occName thing
showSub (ShowSub { ss_how_much = _ })              _     = True

{-
Note [Printing IfaceDecl binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The binders in an IfaceDecl are just OccNames, so we don't know what module they
come from.  But when we pretty-print a TyThing by converting to an IfaceDecl
(see PprTyThing), the TyThing may come from some other module so we really need
the module qualifier.  We solve this by passing in a pretty-printer for the
binders.

When printing an interface file (--show-iface), we want to print
everything unqualified, so we can just print the OccName directly.
-}

ppr_trim :: [Maybe SDoc] -> [SDoc]
-- Collapse a group of Nothings to a single "..."
ppr_trim xs
  = snd (foldr go (False, []) xs)
  where
    go (Just doc) (_,     so_far) = (False, doc : so_far)
    go Nothing    (True,  so_far) = (True, so_far)
    go Nothing    (False, so_far) = (True, ptext (sLit "...") : so_far)

isIfaceDataInstance :: IfaceTyConParent -> Bool
isIfaceDataInstance IfNoParent = False
isIfaceDataInstance _          = True

pprIfaceDecl :: ShowSub -> IfaceDecl -> SDoc
-- NB: pprIfaceDecl is also used for pretty-printing TyThings in GHCi
--     See Note [Pretty-printing TyThings] in PprTyThing
pprIfaceDecl ss (IfaceData { ifName = tycon, ifCType = ctype,
                             ifCtxt = context, ifTyVars = tc_tyvars,
                             ifRoles = roles, ifCons = condecls,
                             ifParent = parent, ifRec = isrec,
                             ifGadtSyntax = gadt,
                             ifPromotable = is_prom })

  | gadt_style = vcat [ pp_roles
                      , pp_nd <+> pp_lhs <+> pp_where
                      , nest 2 (vcat pp_cons)
                      , nest 2 $ ppShowIface ss pp_extra ]
  | otherwise  = vcat [ pp_roles
                      , hang (pp_nd <+> pp_lhs) 2 (add_bars pp_cons)
                      , nest 2 $ ppShowIface ss pp_extra ]
  where
    is_data_instance = isIfaceDataInstance parent

    gadt_style = gadt || any (not . isVanillaIfaceConDecl) cons
    cons       = visibleIfConDecls condecls
    pp_where   = ppWhen (gadt_style && not (null cons)) $ ptext (sLit "where")
    pp_cons    = ppr_trim (map show_con cons) :: [SDoc]

    pp_lhs = case parent of
               IfNoParent -> pprIfaceDeclHead context ss tycon tc_tyvars
               _          -> ptext (sLit "instance") <+> pprIfaceTyConParent parent

    pp_roles
      | is_data_instance = Outputable.empty
      | otherwise        = pprRoles (== Representational) (pprPrefixIfDeclBndr ss tycon)
                                    tc_tyvars roles
            -- Don't display roles for data family instances (yet)
            -- See discussion on Trac #8672.

    add_bars []     = Outputable.empty
    add_bars (c:cs) = sep ((equals <+> c) : map (char '|' <+>) cs)

    ok_con dc = showSub ss dc || any (showSub ss) (ifConFields dc)

    show_con dc
      | ok_con dc = Just $ pprIfaceConDecl ss gadt_style mk_user_con_res_ty dc
      | otherwise = Nothing

    mk_user_con_res_ty :: IfaceEqSpec -> ([IfaceTvBndr], SDoc)
    -- See Note [Result type of a data family GADT]
    mk_user_con_res_ty eq_spec
      | IfDataInstance _ tc tys <- parent
      = (con_univ_tvs, pprIfaceType (IfaceTyConApp tc (substIfaceTcArgs gadt_subst tys)))
      | otherwise
      = (con_univ_tvs, sdocWithDynFlags (ppr_tc_app gadt_subst))
      where
        gadt_subst = mkFsEnv eq_spec
        done_univ_tv (tv,_) = isJust (lookupFsEnv gadt_subst tv)
        con_univ_tvs = filterOut done_univ_tv tc_tyvars

    ppr_tc_app gadt_subst dflags
       = pprPrefixIfDeclBndr ss tycon
         <+> sep [ pprParendIfaceType (substIfaceTyVar gadt_subst tv)
                 | (tv,_kind) <- stripIfaceKindVars dflags tc_tyvars ]

    pp_nd = case condecls of
              IfAbstractTyCon d -> ptext (sLit "abstract") <> ppShowIface ss (parens (ppr d))
              IfDataFamTyCon    -> ptext (sLit "data family")
              IfDataTyCon _     -> ptext (sLit "data")
              IfNewTyCon _      -> ptext (sLit "newtype")

    pp_extra = vcat [pprCType ctype, pprRec isrec, pp_prom]

    pp_prom | is_prom   = ptext (sLit "Promotable")
            | otherwise = Outputable.empty


pprIfaceDecl ss (IfaceClass { ifATs = ats, ifSigs = sigs, ifRec = isrec
                            , ifCtxt   = context, ifName  = clas
                            , ifTyVars = tyvars,  ifRoles = roles
                            , ifFDs    = fds })
  = vcat [ pprRoles (== Nominal) (pprPrefixIfDeclBndr ss clas) tyvars roles
         , ptext (sLit "class") <+> pprIfaceDeclHead context ss clas tyvars
                                <+> pprFundeps fds <+> pp_where
         , nest 2 (vcat [vcat asocs, vcat dsigs, pprec])]
    where
      pp_where = ppShowRhs ss $ ppUnless (null sigs && null ats) (ptext (sLit "where"))

      asocs = ppr_trim $ map maybeShowAssoc ats
      dsigs = ppr_trim $ map maybeShowSig sigs
      pprec = ppShowIface ss (pprRec isrec)

      maybeShowAssoc :: IfaceAT -> Maybe SDoc
      maybeShowAssoc asc@(IfaceAT d _)
        | showSub ss d = Just $ pprIfaceAT ss asc
        | otherwise    = Nothing

      maybeShowSig :: IfaceClassOp -> Maybe SDoc
      maybeShowSig sg
        | showSub ss sg = Just $  pprIfaceClassOp ss sg
        | otherwise     = Nothing

pprIfaceDecl ss (IfaceSynonym { ifName   = tc
                              , ifTyVars = tv
                              , ifSynRhs = mono_ty })
  = hang (ptext (sLit "type") <+> pprIfaceDeclHead [] ss tc tv <+> equals)
       2 (sep [pprIfaceForAll tvs, pprIfaceContextArr theta, ppr tau])
  where
    (tvs, theta, tau) = splitIfaceSigmaTy mono_ty

pprIfaceDecl ss (IfaceFamily { ifName = tycon, ifTyVars = tyvars
                             , ifFamFlav = rhs, ifFamKind = kind
                             , ifResVar = res_var, ifFamInj = inj })
  = vcat [ hang (text "type family" <+> pprIfaceDeclHead [] ss tycon tyvars)
              2 (pp_inj res_var inj <+> ppShowRhs ss (pp_rhs rhs))
         , ppShowRhs ss (nest 2 (pp_branches rhs)) ]
  where
    pp_inj Nothing    _   = dcolon <+> ppr kind
    pp_inj (Just res) inj
       | Injective injectivity <- inj = hsep [ equals, ppr res, dcolon, ppr kind
                                             , pp_inj_cond res injectivity]
       | otherwise = hsep [ equals, ppr res, dcolon, ppr kind ]

    pp_inj_cond res inj = case filterByList inj tyvars of
       []  -> empty
       tvs -> hsep [text "|", ppr res, text "->", interppSP (map fst tvs)]

    pp_rhs IfaceOpenSynFamilyTyCon
      = ppShowIface ss (ptext (sLit "open"))
    pp_rhs IfaceAbstractClosedSynFamilyTyCon
      = ppShowIface ss (ptext (sLit "closed, abstract"))
    pp_rhs (IfaceClosedSynFamilyTyCon _)
      = ptext (sLit "where")
    pp_rhs IfaceBuiltInSynFamTyCon
      = ppShowIface ss (ptext (sLit "built-in"))

    pp_branches (IfaceClosedSynFamilyTyCon (Just (ax, brs)))
      = vcat (map (pprAxBranch (pprPrefixIfDeclBndr ss tycon)) brs)
        $$ ppShowIface ss (ptext (sLit "axiom") <+> ppr ax)
    pp_branches _ = Outputable.empty

pprIfaceDecl _ (IfacePatSyn { ifName = name, ifPatBuilder = builder,
                              ifPatUnivTvs = univ_tvs, ifPatExTvs = ex_tvs,
                              ifPatProvCtxt = prov_ctxt, ifPatReqCtxt = req_ctxt,
                              ifPatArgs = arg_tys,
                              ifPatTy = pat_ty} )
  = pprPatSynSig name is_bidirectional
                 (pprUserIfaceForAll tvs)
                 (pprIfaceContextMaybe prov_ctxt)
                 (pprIfaceContextMaybe req_ctxt)
                 (pprIfaceType ty)
  where
    is_bidirectional = isJust builder
    tvs = univ_tvs ++ ex_tvs
    ty = foldr IfaceFunTy pat_ty arg_tys

pprIfaceDecl ss (IfaceId { ifName = var, ifType = ty,
                              ifIdDetails = details, ifIdInfo = info })
  = vcat [ hang (pprPrefixIfDeclBndr ss var <+> dcolon)
              2 (pprIfaceSigmaType ty)
         , ppShowIface ss (ppr details)
         , ppShowIface ss (ppr info) ]

pprIfaceDecl _ (IfaceAxiom { ifName = name, ifTyCon = tycon
                           , ifAxBranches = branches })
  = hang (ptext (sLit "axiom") <+> ppr name <> dcolon)
       2 (vcat $ map (pprAxBranch (ppr tycon)) branches)


pprCType :: Maybe CType -> SDoc
pprCType Nothing      = Outputable.empty
pprCType (Just cType) = ptext (sLit "C type:") <+> ppr cType

-- if, for each role, suppress_if role is True, then suppress the role
-- output
pprRoles :: (Role -> Bool) -> SDoc -> [IfaceTvBndr] -> [Role] -> SDoc
pprRoles suppress_if tyCon tyvars roles
  = sdocWithDynFlags $ \dflags ->
      let froles = suppressIfaceKinds dflags tyvars roles
      in ppUnless (all suppress_if roles || null froles) $
         ptext (sLit "type role") <+> tyCon <+> hsep (map ppr froles)

pprRec :: RecFlag -> SDoc
pprRec NonRecursive = Outputable.empty
pprRec Recursive    = ptext (sLit "RecFlag: Recursive")

pprInfixIfDeclBndr, pprPrefixIfDeclBndr :: ShowSub -> OccName -> SDoc
pprInfixIfDeclBndr (ShowSub { ss_ppr_bndr = ppr_bndr }) occ
  = pprInfixVar (isSymOcc occ) (ppr_bndr occ)
pprPrefixIfDeclBndr (ShowSub { ss_ppr_bndr = ppr_bndr }) occ
  = parenSymOcc occ (ppr_bndr occ)

instance Outputable IfaceClassOp where
   ppr = pprIfaceClassOp showAll

pprIfaceClassOp :: ShowSub -> IfaceClassOp -> SDoc
pprIfaceClassOp ss (IfaceClassOp n dm ty) = hang opHdr 2 (pprIfaceSigmaType ty)
  where opHdr = pprPrefixIfDeclBndr ss n
                <+> ppShowIface ss (ppr dm) <+> dcolon

instance Outputable IfaceAT where
   ppr = pprIfaceAT showAll

pprIfaceAT :: ShowSub -> IfaceAT -> SDoc
pprIfaceAT ss (IfaceAT d mb_def)
  = vcat [ pprIfaceDecl ss d
         , case mb_def of
              Nothing  -> Outputable.empty
              Just rhs -> nest 2 $
                          ptext (sLit "Default:") <+> ppr rhs ]

instance Outputable IfaceTyConParent where
  ppr p = pprIfaceTyConParent p

pprIfaceTyConParent :: IfaceTyConParent -> SDoc
pprIfaceTyConParent IfNoParent
  = Outputable.empty
pprIfaceTyConParent (IfDataInstance _ tc tys)
  = sdocWithDynFlags $ \dflags ->
    let ftys = stripKindArgs dflags tys
    in pprIfaceTypeApp tc ftys

pprIfaceDeclHead :: IfaceContext -> ShowSub -> OccName -> [IfaceTvBndr] -> SDoc
pprIfaceDeclHead context ss tc_occ tv_bndrs
  = sdocWithDynFlags $ \ dflags ->
    sep [ pprIfaceContextArr context
        , pprPrefixIfDeclBndr ss tc_occ
          <+> pprIfaceTvBndrs (stripIfaceKindVars dflags tv_bndrs) ]

isVanillaIfaceConDecl :: IfaceConDecl -> Bool
isVanillaIfaceConDecl (IfCon { ifConExTvs  = ex_tvs
                             , ifConEqSpec = eq_spec
                             , ifConCtxt   = ctxt })
  = (null ex_tvs) && (null eq_spec) && (null ctxt)

pprIfaceConDecl :: ShowSub -> Bool
                -> (IfaceEqSpec -> ([IfaceTvBndr], SDoc))
                -> IfaceConDecl -> SDoc
pprIfaceConDecl ss gadt_style mk_user_con_res_ty
        (IfCon { ifConOcc = name, ifConInfix = is_infix,
                 ifConExTvs = ex_tvs,
                 ifConEqSpec = eq_spec, ifConCtxt = ctxt, ifConArgTys = arg_tys,
                 ifConStricts = stricts, ifConFields = labels })
  | gadt_style = pp_prefix_con <+> dcolon <+> ppr_ty
  | otherwise  = ppr_fields tys_w_strs
  where
    tys_w_strs :: [(IfaceBang, IfaceType)]
    tys_w_strs = zip stricts arg_tys
    pp_prefix_con = pprPrefixIfDeclBndr ss name

    (univ_tvs, pp_res_ty) = mk_user_con_res_ty eq_spec
    ppr_ty = pprIfaceForAllPart (univ_tvs ++ ex_tvs) ctxt pp_tau

        -- A bit gruesome this, but we can't form the full con_tau, and ppr it,
        -- because we don't have a Name for the tycon, only an OccName
    pp_tau = case map pprParendIfaceType arg_tys ++ [pp_res_ty] of
                (t:ts) -> fsep (t : map (arrow <+>) ts)
                []     -> panic "pp_con_taus"

    ppr_bang IfNoBang = ppWhen opt_PprStyle_Debug $ char '_'
    ppr_bang IfStrict = char '!'
    ppr_bang IfUnpack = ptext (sLit "{-# UNPACK #-}")
    ppr_bang (IfUnpackCo co) = ptext (sLit "! {-# UNPACK #-}") <>
                               pprParendIfaceCoercion co

    pprParendBangTy (bang, ty) = ppr_bang bang <> pprParendIfaceType ty
    pprBangTy       (bang, ty) = ppr_bang bang <> ppr ty

    maybe_show_label (lbl,bty)
      | showSub ss lbl = Just (pprPrefixIfDeclBndr ss lbl <+> dcolon <+> pprBangTy bty)
      | otherwise      = Nothing

    ppr_fields [ty1, ty2]
      | is_infix && null labels
      = sep [pprParendBangTy ty1, pprInfixIfDeclBndr ss name, pprParendBangTy ty2]
    ppr_fields fields
      | null labels = pp_prefix_con <+> sep (map pprParendBangTy fields)
      | otherwise   = pp_prefix_con <+> (braces $ sep $ punctuate comma $ ppr_trim $
                                    map maybe_show_label (zip labels fields))

instance Outputable IfaceRule where
  ppr (IfaceRule { ifRuleName = name, ifActivation = act, ifRuleBndrs = bndrs,
                   ifRuleHead = fn, ifRuleArgs = args, ifRuleRhs = rhs })
    = sep [hsep [pprRuleName name, ppr act,
                 ptext (sLit "forall") <+> pprIfaceBndrs bndrs],
           nest 2 (sep [ppr fn <+> sep (map pprParendIfaceExpr args),
                        ptext (sLit "=") <+> ppr rhs])
      ]

instance Outputable IfaceClsInst where
  ppr (IfaceClsInst { ifDFun = dfun_id, ifOFlag = flag
                    , ifInstCls = cls, ifInstTys = mb_tcs})
    = hang (ptext (sLit "instance") <+> ppr flag
                <+> ppr cls <+> brackets (pprWithCommas ppr_rough mb_tcs))
         2 (equals <+> ppr dfun_id)

instance Outputable IfaceFamInst where
  ppr (IfaceFamInst { ifFamInstFam = fam, ifFamInstTys = mb_tcs
                    , ifFamInstAxiom = tycon_ax})
    = hang (ptext (sLit "family instance") <+>
            ppr fam <+> pprWithCommas (brackets . ppr_rough) mb_tcs)
         2 (equals <+> ppr tycon_ax)

ppr_rough :: Maybe IfaceTyCon -> SDoc
ppr_rough Nothing   = dot
ppr_rough (Just tc) = ppr tc

{-
Note [Result type of a data family GADT]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data family T a
   data instance T (p,q) where
      T1 :: T (Int, Maybe c)
      T2 :: T (Bool, q)

The IfaceDecl actually looks like

   data TPr p q where
      T1 :: forall p q. forall c. (p~Int,q~Maybe c) => TPr p q
      T2 :: forall p q. (p~Bool) => TPr p q

To reconstruct the result types for T1 and T2 that we
want to pretty print, we substitute the eq-spec
[p->Int, q->Maybe c] in the arg pattern (p,q) to give
   T (Int, Maybe c)
Remember that in IfaceSyn, the TyCon and DataCon share the same
universal type variables.

----------------------------- Printing IfaceExpr ------------------------------------
-}

instance Outputable IfaceExpr where
    ppr e = pprIfaceExpr noParens e

noParens :: SDoc -> SDoc
noParens pp = pp

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
pprIfaceExpr _       (IfaceCo co)       = text "@~" <+> pprParendIfaceCoercion co

pprIfaceExpr add_par app@(IfaceApp _ _) = add_par (pprIfaceApp app [])
pprIfaceExpr _       (IfaceTuple c as)  = tupleParens c (pprWithCommas ppr as)

pprIfaceExpr add_par i@(IfaceLam _ _)
  = add_par (sep [char '\\' <+> sep (map pprIfaceLamBndr bndrs) <+> arrow,
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
         pprParendIfaceCoercion co]

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
pprIfaceTickish (IfaceSource src _names)
  = braces (pprUserRealSpan True src)

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
  ppr IfVanillaId       = Outputable.empty
  ppr (IfRecSelId tc b) = ptext (sLit "RecSel") <+> ppr tc
                          <+> if b
                                then ptext (sLit "<naughty>")
                                else Outputable.empty
  ppr IfDFunId          = ptext (sLit "DFunId")

instance Outputable IfaceIdInfo where
  ppr NoInfo       = Outputable.empty
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
  ppr (IfCoreUnfold s e)   = (if s
                                then ptext (sLit "<stable>")
                                else Outputable.empty)
                              <+> parens (ppr e)
  ppr (IfInlineRule a uok bok e) = sep [ptext (sLit "InlineRule")
                                            <+> ppr (a,uok,bok),
                                        pprParendIfaceExpr e]
  ppr (IfDFunUnfold bs es) = hang (ptext (sLit "DFun:") <+> sep (map ppr bs) <> dot)
                                2 (sep (map pprParendIfaceExpr es))

{-
************************************************************************
*                                                                      *
              Finding the Names in IfaceSyn
*                                                                      *
************************************************************************

This is used for dependency analysis in MkIface, so that we
fingerprint a declaration before the things that depend on it.  It
is specific to interface-file fingerprinting in the sense that we
don't collect *all* Names: for example, the DFun of an instance is
recorded textually rather than by its fingerprint when
fingerprinting the instance, so DFuns are not dependencies.
-}

freeNamesIfDecl :: IfaceDecl -> NameSet
freeNamesIfDecl (IfaceId _s t d i) =
  freeNamesIfType t &&&
  freeNamesIfIdInfo i &&&
  freeNamesIfIdDetails d
freeNamesIfDecl d@IfaceData{} =
  freeNamesIfTvBndrs (ifTyVars d) &&&
  freeNamesIfaceTyConParent (ifParent d) &&&
  freeNamesIfContext (ifCtxt d) &&&
  freeNamesIfConDecls (ifCons d)
freeNamesIfDecl d@IfaceSynonym{} =
  freeNamesIfTvBndrs (ifTyVars d) &&&
  freeNamesIfType (ifSynRhs d) &&&
  freeNamesIfKind (ifSynKind d) -- IA0_NOTE: because of promotion, we
                                -- return names in the kind signature
freeNamesIfDecl d@IfaceFamily{} =
  freeNamesIfTvBndrs (ifTyVars d) &&&
  freeNamesIfFamFlav (ifFamFlav d) &&&
  freeNamesIfKind (ifFamKind d) -- IA0_NOTE: because of promotion, we
                                -- return names in the kind signature
freeNamesIfDecl d@IfaceClass{} =
  freeNamesIfTvBndrs (ifTyVars d) &&&
  freeNamesIfContext (ifCtxt d) &&&
  fnList freeNamesIfAT     (ifATs d) &&&
  fnList freeNamesIfClsSig (ifSigs d)
freeNamesIfDecl d@IfaceAxiom{} =
  freeNamesIfTc (ifTyCon d) &&&
  fnList freeNamesIfAxBranch (ifAxBranches d)
freeNamesIfDecl d@IfacePatSyn{} =
  unitNameSet (fst (ifPatMatcher d)) &&&
  maybe emptyNameSet (unitNameSet . fst) (ifPatBuilder d) &&&
  freeNamesIfTvBndrs (ifPatUnivTvs d) &&&
  freeNamesIfTvBndrs (ifPatExTvs d) &&&
  freeNamesIfContext (ifPatProvCtxt d) &&&
  freeNamesIfContext (ifPatReqCtxt d) &&&
  fnList freeNamesIfType (ifPatArgs d) &&&
  freeNamesIfType (ifPatTy d)

freeNamesIfAxBranch :: IfaceAxBranch -> NameSet
freeNamesIfAxBranch (IfaceAxBranch { ifaxbTyVars = tyvars
                                   , ifaxbLHS    = lhs
                                   , ifaxbRHS    = rhs }) =
  freeNamesIfTvBndrs tyvars &&&
  freeNamesIfTcArgs lhs &&&
  freeNamesIfType rhs

freeNamesIfIdDetails :: IfaceIdDetails -> NameSet
freeNamesIfIdDetails (IfRecSelId tc _) = freeNamesIfTc tc
freeNamesIfIdDetails _                 = emptyNameSet

-- All other changes are handled via the version info on the tycon
freeNamesIfFamFlav :: IfaceFamTyConFlav -> NameSet
freeNamesIfFamFlav IfaceOpenSynFamilyTyCon           = emptyNameSet
freeNamesIfFamFlav (IfaceClosedSynFamilyTyCon (Just (ax, br)))
  = unitNameSet ax &&& fnList freeNamesIfAxBranch br
freeNamesIfFamFlav (IfaceClosedSynFamilyTyCon Nothing) = emptyNameSet
freeNamesIfFamFlav IfaceAbstractClosedSynFamilyTyCon = emptyNameSet
freeNamesIfFamFlav IfaceBuiltInSynFamTyCon = emptyNameSet

freeNamesIfContext :: IfaceContext -> NameSet
freeNamesIfContext = fnList freeNamesIfType

freeNamesIfAT :: IfaceAT -> NameSet
freeNamesIfAT (IfaceAT decl mb_def)
  = freeNamesIfDecl decl &&&
    case mb_def of
      Nothing  -> emptyNameSet
      Just rhs -> freeNamesIfType rhs

freeNamesIfClsSig :: IfaceClassOp -> NameSet
freeNamesIfClsSig (IfaceClassOp _n _dm ty) = freeNamesIfType ty

freeNamesIfConDecls :: IfaceConDecls -> NameSet
freeNamesIfConDecls (IfDataTyCon c) = fnList freeNamesIfConDecl c
freeNamesIfConDecls (IfNewTyCon c)  = freeNamesIfConDecl c
freeNamesIfConDecls _               = emptyNameSet

freeNamesIfConDecl :: IfaceConDecl -> NameSet
freeNamesIfConDecl c
  = freeNamesIfTvBndrs (ifConExTvs c) &&&
    freeNamesIfContext (ifConCtxt c) &&&
    fnList freeNamesIfType (ifConArgTys c) &&&
    fnList freeNamesIfType (map snd (ifConEqSpec c)) -- equality constraints

freeNamesIfKind :: IfaceType -> NameSet
freeNamesIfKind = freeNamesIfType

freeNamesIfTcArgs :: IfaceTcArgs -> NameSet
freeNamesIfTcArgs (ITC_Type t ts) = freeNamesIfType t &&& freeNamesIfTcArgs ts
freeNamesIfTcArgs (ITC_Kind k ks) = freeNamesIfKind k &&& freeNamesIfTcArgs ks
freeNamesIfTcArgs ITC_Nil         = emptyNameSet

freeNamesIfType :: IfaceType -> NameSet
freeNamesIfType (IfaceTyVar _)        = emptyNameSet
freeNamesIfType (IfaceAppTy s t)      = freeNamesIfType s &&& freeNamesIfType t
freeNamesIfType (IfaceTyConApp tc ts) = freeNamesIfTc tc &&& freeNamesIfTcArgs ts
freeNamesIfType (IfaceTupleTy _ _ ts) = freeNamesIfTcArgs ts
freeNamesIfType (IfaceLitTy _)        = emptyNameSet
freeNamesIfType (IfaceForAllTy tv t)  = freeNamesIfTvBndr tv &&& freeNamesIfType t
freeNamesIfType (IfaceFunTy s t)      = freeNamesIfType s &&& freeNamesIfType t
freeNamesIfType (IfaceDFunTy s t)     = freeNamesIfType s &&& freeNamesIfType t

freeNamesIfCoercion :: IfaceCoercion -> NameSet
freeNamesIfCoercion (IfaceReflCo _ t) = freeNamesIfType t
freeNamesIfCoercion (IfaceFunCo _ c1 c2)
  = freeNamesIfCoercion c1 &&& freeNamesIfCoercion c2
freeNamesIfCoercion (IfaceTyConAppCo _ tc cos)
  = freeNamesIfTc tc &&& fnList freeNamesIfCoercion cos
freeNamesIfCoercion (IfaceAppCo c1 c2)
  = freeNamesIfCoercion c1 &&& freeNamesIfCoercion c2
freeNamesIfCoercion (IfaceForAllCo tv co)
  = freeNamesIfTvBndr tv &&& freeNamesIfCoercion co
freeNamesIfCoercion (IfaceCoVarCo _)
  = emptyNameSet
freeNamesIfCoercion (IfaceAxiomInstCo ax _ cos)
  = unitNameSet ax &&& fnList freeNamesIfCoercion cos
freeNamesIfCoercion (IfaceUnivCo _ _ t1 t2)
  = freeNamesIfType t1 &&& freeNamesIfType t2
freeNamesIfCoercion (IfaceSymCo c)
  = freeNamesIfCoercion c
freeNamesIfCoercion (IfaceTransCo c1 c2)
  = freeNamesIfCoercion c1 &&& freeNamesIfCoercion c2
freeNamesIfCoercion (IfaceNthCo _ co)
  = freeNamesIfCoercion co
freeNamesIfCoercion (IfaceLRCo _ co)
  = freeNamesIfCoercion co
freeNamesIfCoercion (IfaceInstCo co ty)
  = freeNamesIfCoercion co &&& freeNamesIfType ty
freeNamesIfCoercion (IfaceSubCo co)
  = freeNamesIfCoercion co
freeNamesIfCoercion (IfaceAxiomRuleCo _ax tys cos)
  -- the axiom is just a string, so we don't count it as a name.
  = fnList freeNamesIfType tys &&&
    fnList freeNamesIfCoercion cos

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
freeNamesIfUnfold (IfDFunUnfold bs es)   = fnList freeNamesIfBndr bs &&& fnList freeNamesIfExpr es

freeNamesIfExpr :: IfaceExpr -> NameSet
freeNamesIfExpr (IfaceExt v)          = unitNameSet v
freeNamesIfExpr (IfaceFCall _ ty)     = freeNamesIfType ty
freeNamesIfExpr (IfaceType ty)        = freeNamesIfType ty
freeNamesIfExpr (IfaceCo co)          = freeNamesIfCoercion co
freeNamesIfExpr (IfaceTuple _ as)     = fnList freeNamesIfExpr as
freeNamesIfExpr (IfaceLam (b,_) body) = freeNamesIfBndr b &&& freeNamesIfExpr body
freeNamesIfExpr (IfaceApp f a)        = freeNamesIfExpr f &&& freeNamesIfExpr a
freeNamesIfExpr (IfaceCast e co)      = freeNamesIfExpr e &&& freeNamesIfCoercion co
freeNamesIfExpr (IfaceTick _ e)       = freeNamesIfExpr e
freeNamesIfExpr (IfaceECase e ty)     = freeNamesIfExpr e &&& freeNamesIfType ty
freeNamesIfExpr (IfaceCase s _ alts)
  = freeNamesIfExpr s &&& fnList fn_alt alts &&& fn_cons alts
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
freeNamesIfTc tc = unitNameSet (ifaceTyConName tc)
-- ToDo: shouldn't we include IfaceIntTc & co.?

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

freeNamesIfaceTyConParent :: IfaceTyConParent -> NameSet
freeNamesIfaceTyConParent IfNoParent = emptyNameSet
freeNamesIfaceTyConParent (IfDataInstance ax tc tys)
  = unitNameSet ax &&& freeNamesIfTc tc &&& freeNamesIfTcArgs tys

-- helpers
(&&&) :: NameSet -> NameSet -> NameSet
(&&&) = unionNameSet

fnList :: (a -> NameSet) -> [a] -> NameSet
fnList f = foldr (&&&) emptyNameSet . map f

{-
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


************************************************************************
*                                                                      *
                Binary instances
*                                                                      *
************************************************************************
-}

instance Binary IfaceDecl where
    put_ bh (IfaceId name ty details idinfo) = do
        putByte bh 0
        put_ bh (occNameFS name)
        put_ bh ty
        put_ bh details
        put_ bh idinfo

    put_ bh (IfaceData a1 a2 a3 a4 a5 a6 a7 a8 a9 a10) = do
        putByte bh 2
        put_ bh (occNameFS a1)
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        put_ bh a8
        put_ bh a9
        put_ bh a10

    put_ bh (IfaceSynonym a1 a2 a3 a4 a5) = do
        putByte bh 3
        put_ bh (occNameFS a1)
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5

    put_ bh (IfaceFamily a1 a2 a3 a4 a5 a6) = do
        putByte bh 4
        put_ bh (occNameFS a1)
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6

    put_ bh (IfaceClass a1 a2 a3 a4 a5 a6 a7 a8 a9) = do
        putByte bh 5
        put_ bh a1
        put_ bh (occNameFS a2)
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        put_ bh a8
        put_ bh a9

    put_ bh (IfaceAxiom a1 a2 a3 a4) = do
        putByte bh 6
        put_ bh (occNameFS a1)
        put_ bh a2
        put_ bh a3
        put_ bh a4

    put_ bh (IfacePatSyn name a2 a3 a4 a5 a6 a7 a8 a9 a10) = do
        putByte bh 7
        put_ bh (occNameFS name)
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        put_ bh a8
        put_ bh a9
        put_ bh a10

    get bh = do
        h <- getByte bh
        case h of
            0 -> do name    <- get bh
                    ty      <- get bh
                    details <- get bh
                    idinfo  <- get bh
                    occ <- return $! mkVarOccFS name
                    return (IfaceId occ ty details idinfo)
            1 -> error "Binary.get(TyClDecl): ForeignType"
            2 -> do a1  <- get bh
                    a2  <- get bh
                    a3  <- get bh
                    a4  <- get bh
                    a5  <- get bh
                    a6  <- get bh
                    a7  <- get bh
                    a8  <- get bh
                    a9  <- get bh
                    a10 <- get bh
                    occ <- return $! mkTcOccFS a1
                    return (IfaceData occ a2 a3 a4 a5 a6 a7 a8 a9 a10)
            3 -> do a1 <- get bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    a5 <- get bh
                    occ <- return $! mkTcOccFS a1
                    return (IfaceSynonym occ a2 a3 a4 a5)
            4 -> do a1 <- get bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    a5 <- get bh
                    a6 <- get bh
                    occ <- return $! mkTcOccFS a1
                    return (IfaceFamily occ a2 a3 a4 a5 a6)
            5 -> do a1 <- get bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    a5 <- get bh
                    a6 <- get bh
                    a7 <- get bh
                    a8 <- get bh
                    a9 <- get bh
                    occ <- return $! mkClsOccFS a2
                    return (IfaceClass a1 occ a3 a4 a5 a6 a7 a8 a9)
            6 -> do a1 <- get bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    occ <- return $! mkTcOccFS a1
                    return (IfaceAxiom occ a2 a3 a4)
            7 -> do a1 <- get bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    a5 <- get bh
                    a6 <- get bh
                    a7 <- get bh
                    a8 <- get bh
                    a9 <- get bh
                    a10 <- get bh
                    occ <- return $! mkDataOccFS a1
                    return (IfacePatSyn occ a2 a3 a4 a5 a6 a7 a8 a9 a10)
            _ -> panic (unwords ["Unknown IfaceDecl tag:", show h])

instance Binary IfaceFamTyConFlav where
    put_ bh IfaceOpenSynFamilyTyCon           = putByte bh 0
    put_ bh (IfaceClosedSynFamilyTyCon mb)    = putByte bh 1 >> put_ bh mb
    put_ bh IfaceAbstractClosedSynFamilyTyCon = putByte bh 2
    put_ _ IfaceBuiltInSynFamTyCon
        = pprPanic "Cannot serialize IfaceBuiltInSynFamTyCon, used for pretty-printing only" Outputable.empty

    get bh = do { h <- getByte bh
                ; case h of
                    0 -> return IfaceOpenSynFamilyTyCon
                    1 -> do { mb <- get bh
                            ; return (IfaceClosedSynFamilyTyCon mb) }
                    _ -> return IfaceAbstractClosedSynFamilyTyCon }

instance Binary IfaceClassOp where
    put_ bh (IfaceClassOp n def ty) = do
        put_ bh (occNameFS n)
        put_ bh def
        put_ bh ty
    get bh = do
        n   <- get bh
        def <- get bh
        ty  <- get bh
        occ <- return $! mkVarOccFS n
        return (IfaceClassOp occ def ty)

instance Binary IfaceAT where
    put_ bh (IfaceAT dec defs) = do
        put_ bh dec
        put_ bh defs
    get bh = do
        dec  <- get bh
        defs <- get bh
        return (IfaceAT dec defs)

instance Binary IfaceAxBranch where
    put_ bh (IfaceAxBranch a1 a2 a3 a4 a5) = do
        put_ bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
    get bh = do
        a1 <- get bh
        a2 <- get bh
        a3 <- get bh
        a4 <- get bh
        a5 <- get bh
        return (IfaceAxBranch a1 a2 a3 a4 a5)

instance Binary IfaceConDecls where
    put_ bh (IfAbstractTyCon d) = putByte bh 0 >> put_ bh d
    put_ bh IfDataFamTyCon     = putByte bh 1
    put_ bh (IfDataTyCon cs)    = putByte bh 2 >> put_ bh cs
    put_ bh (IfNewTyCon c)      = putByte bh 3 >> put_ bh c
    get bh = do
        h <- getByte bh
        case h of
            0 -> liftM IfAbstractTyCon $ get bh
            1 -> return IfDataFamTyCon
            2 -> liftM IfDataTyCon $ get bh
            _ -> liftM IfNewTyCon $ get bh

instance Binary IfaceConDecl where
    put_ bh (IfCon a1 a2 a3 a4 a5 a6 a7 a8 a9 a10) = do
        put_ bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        put_ bh a8
        put_ bh a9
        put_ bh a10
    get bh = do
        a1 <- get bh
        a2 <- get bh
        a3 <- get bh
        a4 <- get bh
        a5 <- get bh
        a6 <- get bh
        a7 <- get bh
        a8 <- get bh
        a9 <- get bh
        a10 <- get bh
        return (IfCon a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)

instance Binary IfaceBang where
    put_ bh IfNoBang        = putByte bh 0
    put_ bh IfStrict        = putByte bh 1
    put_ bh IfUnpack        = putByte bh 2
    put_ bh (IfUnpackCo co) = putByte bh 3 >> put_ bh co

    get bh = do
            h <- getByte bh
            case h of
              0 -> do return IfNoBang
              1 -> do return IfStrict
              2 -> do return IfUnpack
              _ -> do { a <- get bh; return (IfUnpackCo a) }

instance Binary IfaceSrcBang where
    put_ bh (IfSrcBang a1 a2) =
      do put_ bh a1
         put_ bh a2

    get bh =
      do a1 <- get bh
         a2 <- get bh
         return (IfSrcBang a1 a2)

instance Binary IfaceClsInst where
    put_ bh (IfaceClsInst cls tys dfun flag orph) = do
        put_ bh cls
        put_ bh tys
        put_ bh dfun
        put_ bh flag
        put_ bh orph
    get bh = do
        cls  <- get bh
        tys  <- get bh
        dfun <- get bh
        flag <- get bh
        orph <- get bh
        return (IfaceClsInst cls tys dfun flag orph)

instance Binary IfaceFamInst where
    put_ bh (IfaceFamInst fam tys name orph) = do
        put_ bh fam
        put_ bh tys
        put_ bh name
        put_ bh orph
    get bh = do
        fam      <- get bh
        tys      <- get bh
        name     <- get bh
        orph     <- get bh
        return (IfaceFamInst fam tys name orph)

instance Binary IfaceRule where
    put_ bh (IfaceRule a1 a2 a3 a4 a5 a6 a7 a8) = do
        put_ bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        put_ bh a8
    get bh = do
        a1 <- get bh
        a2 <- get bh
        a3 <- get bh
        a4 <- get bh
        a5 <- get bh
        a6 <- get bh
        a7 <- get bh
        a8 <- get bh
        return (IfaceRule a1 a2 a3 a4 a5 a6 a7 a8)

instance Binary IfaceAnnotation where
    put_ bh (IfaceAnnotation a1 a2) = do
        put_ bh a1
        put_ bh a2
    get bh = do
        a1 <- get bh
        a2 <- get bh
        return (IfaceAnnotation a1 a2)

instance Binary IfaceIdDetails where
    put_ bh IfVanillaId      = putByte bh 0
    put_ bh (IfRecSelId a b) = putByte bh 1 >> put_ bh a >> put_ bh b
    put_ bh IfDFunId         = putByte bh 2
    get bh = do
        h <- getByte bh
        case h of
            0 -> return IfVanillaId
            1 -> do { a <- get bh; b <- get bh; return (IfRecSelId a b) }
            _ -> return IfDFunId

instance Binary IfaceIdInfo where
    put_ bh NoInfo      = putByte bh 0
    put_ bh (HasInfo i) = putByte bh 1 >> lazyPut bh i -- NB lazyPut

    get bh = do
        h <- getByte bh
        case h of
            0 -> return NoInfo
            _ -> liftM HasInfo $ lazyGet bh    -- NB lazyGet

instance Binary IfaceInfoItem where
    put_ bh (HsArity aa)          = putByte bh 0 >> put_ bh aa
    put_ bh (HsStrictness ab)     = putByte bh 1 >> put_ bh ab
    put_ bh (HsUnfold lb ad)      = putByte bh 2 >> put_ bh lb >> put_ bh ad
    put_ bh (HsInline ad)         = putByte bh 3 >> put_ bh ad
    put_ bh HsNoCafRefs           = putByte bh 4
    get bh = do
        h <- getByte bh
        case h of
            0 -> liftM HsArity $ get bh
            1 -> liftM HsStrictness $ get bh
            2 -> do lb <- get bh
                    ad <- get bh
                    return (HsUnfold lb ad)
            3 -> liftM HsInline $ get bh
            _ -> return HsNoCafRefs

instance Binary IfaceUnfolding where
    put_ bh (IfCoreUnfold s e) = do
        putByte bh 0
        put_ bh s
        put_ bh e
    put_ bh (IfInlineRule a b c d) = do
        putByte bh 1
        put_ bh a
        put_ bh b
        put_ bh c
        put_ bh d
    put_ bh (IfDFunUnfold as bs) = do
        putByte bh 2
        put_ bh as
        put_ bh bs
    put_ bh (IfCompulsory e) = do
        putByte bh 3
        put_ bh e
    get bh = do
        h <- getByte bh
        case h of
            0 -> do s <- get bh
                    e <- get bh
                    return (IfCoreUnfold s e)
            1 -> do a <- get bh
                    b <- get bh
                    c <- get bh
                    d <- get bh
                    return (IfInlineRule a b c d)
            2 -> do as <- get bh
                    bs <- get bh
                    return (IfDFunUnfold as bs)
            _ -> do e <- get bh
                    return (IfCompulsory e)


instance Binary IfaceExpr where
    put_ bh (IfaceLcl aa) = do
        putByte bh 0
        put_ bh aa
    put_ bh (IfaceType ab) = do
        putByte bh 1
        put_ bh ab
    put_ bh (IfaceCo ab) = do
        putByte bh 2
        put_ bh ab
    put_ bh (IfaceTuple ac ad) = do
        putByte bh 3
        put_ bh ac
        put_ bh ad
    put_ bh (IfaceLam (ae, os) af) = do
        putByte bh 4
        put_ bh ae
        put_ bh os
        put_ bh af
    put_ bh (IfaceApp ag ah) = do
        putByte bh 5
        put_ bh ag
        put_ bh ah
    put_ bh (IfaceCase ai aj ak) = do
        putByte bh 6
        put_ bh ai
        put_ bh aj
        put_ bh ak
    put_ bh (IfaceLet al am) = do
        putByte bh 7
        put_ bh al
        put_ bh am
    put_ bh (IfaceTick an ao) = do
        putByte bh 8
        put_ bh an
        put_ bh ao
    put_ bh (IfaceLit ap) = do
        putByte bh 9
        put_ bh ap
    put_ bh (IfaceFCall as at) = do
        putByte bh 10
        put_ bh as
        put_ bh at
    put_ bh (IfaceExt aa) = do
        putByte bh 11
        put_ bh aa
    put_ bh (IfaceCast ie ico) = do
        putByte bh 12
        put_ bh ie
        put_ bh ico
    put_ bh (IfaceECase a b) = do
        putByte bh 13
        put_ bh a
        put_ bh b
    get bh = do
        h <- getByte bh
        case h of
            0 -> do aa <- get bh
                    return (IfaceLcl aa)
            1 -> do ab <- get bh
                    return (IfaceType ab)
            2 -> do ab <- get bh
                    return (IfaceCo ab)
            3 -> do ac <- get bh
                    ad <- get bh
                    return (IfaceTuple ac ad)
            4 -> do ae <- get bh
                    os <- get bh
                    af <- get bh
                    return (IfaceLam (ae, os) af)
            5 -> do ag <- get bh
                    ah <- get bh
                    return (IfaceApp ag ah)
            6 -> do ai <- get bh
                    aj <- get bh
                    ak <- get bh
                    return (IfaceCase ai aj ak)
            7 -> do al <- get bh
                    am <- get bh
                    return (IfaceLet al am)
            8 -> do an <- get bh
                    ao <- get bh
                    return (IfaceTick an ao)
            9 -> do ap <- get bh
                    return (IfaceLit ap)
            10 -> do as <- get bh
                     at <- get bh
                     return (IfaceFCall as at)
            11 -> do aa <- get bh
                     return (IfaceExt aa)
            12 -> do ie <- get bh
                     ico <- get bh
                     return (IfaceCast ie ico)
            13 -> do a <- get bh
                     b <- get bh
                     return (IfaceECase a b)
            _ -> panic ("get IfaceExpr " ++ show h)

instance Binary IfaceTickish where
    put_ bh (IfaceHpcTick m ix) = do
        putByte bh 0
        put_ bh m
        put_ bh ix
    put_ bh (IfaceSCC cc tick push) = do
        putByte bh 1
        put_ bh cc
        put_ bh tick
        put_ bh push
    put_ bh (IfaceSource src name) = do
        putByte bh 2
        put_ bh (srcSpanFile src)
        put_ bh (srcSpanStartLine src)
        put_ bh (srcSpanStartCol src)
        put_ bh (srcSpanEndLine src)
        put_ bh (srcSpanEndCol src)
        put_ bh name

    get bh = do
        h <- getByte bh
        case h of
            0 -> do m <- get bh
                    ix <- get bh
                    return (IfaceHpcTick m ix)
            1 -> do cc <- get bh
                    tick <- get bh
                    push <- get bh
                    return (IfaceSCC cc tick push)
            2 -> do file <- get bh
                    sl <- get bh
                    sc <- get bh
                    el <- get bh
                    ec <- get bh
                    let start = mkRealSrcLoc file sl sc
                        end = mkRealSrcLoc file el ec
                    name <- get bh
                    return (IfaceSource (mkRealSrcSpan start end) name)
            _ -> panic ("get IfaceTickish " ++ show h)

instance Binary IfaceConAlt where
    put_ bh IfaceDefault      = putByte bh 0
    put_ bh (IfaceDataAlt aa) = putByte bh 1 >> put_ bh aa
    put_ bh (IfaceLitAlt ac)  = putByte bh 2 >> put_ bh ac
    get bh = do
        h <- getByte bh
        case h of
            0 -> return IfaceDefault
            1 -> liftM IfaceDataAlt $ get bh
            _ -> liftM IfaceLitAlt  $ get bh

instance Binary IfaceBinding where
    put_ bh (IfaceNonRec aa ab) = putByte bh 0 >> put_ bh aa >> put_ bh ab
    put_ bh (IfaceRec ac)       = putByte bh 1 >> put_ bh ac
    get bh = do
        h <- getByte bh
        case h of
            0 -> do { aa <- get bh; ab <- get bh; return (IfaceNonRec aa ab) }
            _ -> do { ac <- get bh; return (IfaceRec ac) }

instance Binary IfaceLetBndr where
    put_ bh (IfLetBndr a b c) = do
            put_ bh a
            put_ bh b
            put_ bh c
    get bh = do a <- get bh
                b <- get bh
                c <- get bh
                return (IfLetBndr a b c)

instance Binary IfaceTyConParent where
    put_ bh IfNoParent = putByte bh 0
    put_ bh (IfDataInstance ax pr ty) = do
        putByte bh 1
        put_ bh ax
        put_ bh pr
        put_ bh ty
    get bh = do
        h <- getByte bh
        case h of
            0 -> return IfNoParent
            _ -> do
                ax <- get bh
                pr <- get bh
                ty <- get bh
                return $ IfDataInstance ax pr ty
