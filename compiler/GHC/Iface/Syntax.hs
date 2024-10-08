{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998
-}


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}

module GHC.Iface.Syntax (
        module GHC.Iface.Type,

        IfaceDecl(..), IfaceFamTyConFlav(..), IfaceClassOp(..), IfaceAT(..),
        IfaceConDecl(..), IfaceConDecls(..), IfaceEqSpec,
        IfaceExpr(..), IfaceAlt(..), IfaceLetBndr(..), IfaceBinding,
        IfaceBindingX(..), IfaceMaybeRhs(..), IfaceConAlt(..),
        IfaceIdInfo, IfaceIdDetails(..), IfaceUnfolding(..), IfGuidance(..),
        IfaceInfoItem(..), IfaceRule(..), IfaceAnnotation(..), IfaceAnnTarget,
        IfaceWarnings(..), IfaceWarningTxt(..), IfaceStringLiteral(..),
        IfaceDefault(..), IfaceClsInst(..), IfaceFamInst(..), IfaceTickish(..),
        IfaceClassBody(..), IfaceBooleanFormula(..),
        IfaceBang(..),
        IfaceSrcBang(..), SrcUnpackedness(..), SrcStrictness(..),
        IfaceAxBranch(..),
        IfaceTyConParent(..),
        IfaceCompleteMatch(..),
        IfaceLFInfo(..), IfaceTopBndrInfo(..),
        IfaceImport(..),
        ImpIfaceList(..),

        -- * Binding names
        IfaceTopBndr,
        putIfaceTopBndr, getIfaceTopBndr,

        -- Misc
        ifaceDeclImplicitBndrs, visibleIfConDecls,
        ifaceDeclFingerprints,
        fromIfaceWarnings,
        fromIfaceWarningTxt,

        -- Free Names
        freeNamesIfDecl, freeNamesIfRule, freeNamesIfFamInst,
        freeNamesIfConDecls,

        -- Pretty printing
        pprIfaceExpr,
        pprIfaceDecl,
        AltPpr(..), ShowSub(..), ShowHowMuch(..), showToIface, showToHeader
    ) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Builtin.Names ( unrestrictedFunTyConKey, liftedTypeKindTyConKey,
                           constraintKindTyConKey )
import GHC.Types.Unique ( hasKey )
import GHC.Iface.Type
import GHC.Iface.Recomp.Binary
import GHC.Core( IsOrphan, isOrphan, UnfoldingCache(..) )
import GHC.Types.Demand
import GHC.Types.Cpr
import GHC.Core.Class
import GHC.Types.FieldLabel
import GHC.Types.Name.Set
import GHC.Core.Coercion.Axiom ( BranchIndex )
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.CostCentre
import GHC.Types.Literal
import GHC.Types.ForeignCall
import GHC.Types.Annotations( AnnPayload, AnnTarget )
import GHC.Types.Basic
import GHC.Unit.Module
import GHC.Unit.Module.Warnings
import GHC.Types.SrcLoc
import GHC.Types.SourceText
import GHC.Types.Var( VarBndr(..), binderVar, tyVarSpecToBinders, visArgTypeLike )
import GHC.Core.TyCon ( Role (..), Injectivity(..), tyConBndrVisForAllTyFlag )
import GHC.Core.DataCon (SrcStrictness(..), SrcUnpackedness(..))
import GHC.Builtin.Types ( constraintKindTyConName )
import GHC.Stg.InferTags.TagSig
import GHC.Parser.Annotation (noLocA)
import GHC.Hs.Extension ( GhcRn )
import GHC.Hs.Doc ( WithHsDocIdentifiers(..) )

import GHC.Utils.Lexeme (isLexSym)
import GHC.Utils.Fingerprint
import GHC.Utils.Binary
import GHC.Utils.Binary.Typeable () -- instance Binary AnnPayload
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc( dropList, filterByList, notNull, unzipWith,
                       seqList, zipWithEqual )

import Control.Monad
import System.IO.Unsafe
import Control.DeepSeq
import Data.Proxy
import Data.List ( intersperse )

infixl 3 &&&

{-
************************************************************************
*                                                                      *
                    Declarations
*                                                                      *
************************************************************************
-}

data IfaceImport = IfaceImport ImpDeclSpec ImpIfaceList

data ImpIfaceList
  = ImpIfaceAll -- ^ no user import list
  | ImpIfaceExplicit !IfGlobalRdrEnv
  | ImpIfaceEverythingBut !NameSet

-- | A binding top-level 'Name' in an interface file (e.g. the name of an
-- 'IfaceDecl').
type IfaceTopBndr = Name
  -- It's convenient to have a Name in the Iface syntax, although in each
  -- case the namespace is implied by the context. However, having a
  -- Name makes things like ifaceDeclImplicitBndrs and ifaceDeclFingerprints
  -- very convenient. Moreover, having the key of the binder means that
  -- we can encode known-key things cleverly in the symbol table. See Note
  -- [Symbol table representation of Names]
  --
  -- We don't serialise the namespace onto the disk though; rather we
  -- drop it when serialising and add it back in when deserialising.

getIfaceTopBndr :: ReadBinHandle -> IO IfaceTopBndr
getIfaceTopBndr bh = get bh

putIfaceTopBndr :: WriteBinHandle -> IfaceTopBndr -> IO ()
putIfaceTopBndr bh name =
    case findUserDataWriter (Proxy @BindingName) bh of
      tbl ->
          --pprTrace "putIfaceTopBndr" (ppr name) $
          putEntry tbl bh (BindingName name)

data IfaceDecl
  = IfaceId { ifName      :: IfaceTopBndr,
              ifType      :: IfaceType,
              ifIdDetails :: IfaceIdDetails,
              ifIdInfo    :: IfaceIdInfo
              }

  | IfaceData { ifName       :: IfaceTopBndr,   -- Type constructor
                ifBinders    :: [IfaceTyConBinder],
                ifResKind    :: IfaceType,      -- Result kind of type constructor
                ifCType      :: Maybe CType,    -- C type for CAPI FFI
                ifRoles      :: [Role],         -- Roles
                ifCtxt       :: IfaceContext,   -- The "stupid theta"
                ifCons       :: IfaceConDecls,  -- Includes new/data/data family info
                ifGadtSyntax :: Bool,           -- True <=> declared using
                                                -- GADT syntax
                ifParent     :: IfaceTyConParent -- The axiom, for a newtype,
                                                 -- or data/newtype family instance
    }

  | IfaceSynonym { ifName    :: IfaceTopBndr,      -- Type constructor
                   ifRoles   :: [Role],            -- Roles
                   ifBinders :: [IfaceTyConBinder],
                   ifResKind :: IfaceKind,         -- Kind of the *result*
                   ifSynRhs  :: IfaceType }

  | IfaceFamily  { ifName    :: IfaceTopBndr,      -- Type constructor
                   ifResVar  :: Maybe IfLclName,   -- Result variable name, used
                                                   -- only for pretty-printing
                                                   -- with --show-iface
                   ifBinders :: [IfaceTyConBinder],
                   ifResKind :: IfaceKind,         -- Kind of the *tycon*
                   ifFamFlav :: IfaceFamTyConFlav,
                   ifFamInj  :: Injectivity }      -- injectivity information

  | IfaceClass { ifName    :: IfaceTopBndr,             -- Name of the class TyCon
                 ifRoles   :: [Role],                   -- Roles
                 ifBinders :: [IfaceTyConBinder],
                 ifFDs     :: [FunDep IfLclName],       -- Functional dependencies
                 ifBody    :: IfaceClassBody            -- Methods, superclasses, ATs
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
                  ifPatUnivBndrs  :: [IfaceForAllSpecBndr],
                  ifPatExBndrs    :: [IfaceForAllSpecBndr],
                  ifPatProvCtxt   :: IfaceContext,
                  ifPatReqCtxt    :: IfaceContext,
                  ifPatArgs       :: [IfaceType],
                  ifPatTy         :: IfaceType,
                  ifFieldLabels   :: [FieldLabel] }

-- See also 'ClassBody'
data IfaceClassBody
  -- Abstract classes don't specify their body; they only occur in @hs-boot@ and
  -- @hsig@ files.
  = IfAbstractClass
  | IfConcreteClass {
     ifClassCtxt :: IfaceContext,             -- Super classes
     ifATs       :: [IfaceAT],                -- Associated type families
     ifSigs      :: [IfaceClassOp],           -- Method signatures
     ifMinDef    :: IfaceBooleanFormula       -- Minimal complete definition
    }

data IfaceBooleanFormula
  = IfVar IfLclName
  | IfAnd [IfaceBooleanFormula]
  | IfOr [IfaceBooleanFormula]
  | IfParens IfaceBooleanFormula
  deriving Eq

data IfaceTyConParent
  = IfNoParent
  | IfDataInstance
       IfExtName     -- Axiom name
       IfaceTyCon    -- Family TyCon (pretty-printing only, not used in GHC.IfaceToCore)
                     -- see Note [Pretty printing via Iface syntax] in GHC.Types.TyThing.Ppr
       IfaceAppArgs  -- Arguments of the family TyCon

data IfaceFamTyConFlav
  = IfaceDataFamilyTyCon                      -- Data family
  | IfaceOpenSynFamilyTyCon
  | IfaceClosedSynFamilyTyCon (Maybe (IfExtName, [IfaceAxBranch]))
    -- ^ Name of associated axiom and branches for pretty printing purposes,
    -- or 'Nothing' for an empty closed family without an axiom
    -- See Note [Pretty printing via Iface syntax] in "GHC.Types.TyThing.Ppr"
  | IfaceAbstractClosedSynFamilyTyCon
  | IfaceBuiltInSynFamTyCon -- for pretty printing purposes only

data IfaceClassOp
  = IfaceClassOp IfaceTopBndr
                 IfaceType                         -- Class op type
                 (Maybe (DefMethSpec IfaceType))   -- Default method
                 -- The types of both the class op itself,
                 -- and the default method, are *not* quantified
                 -- over the class variables

data IfaceAT = IfaceAT  -- See GHC.Core.Class.ClassATItem
                  IfaceDecl          -- The associated type declaration
                  (Maybe IfaceType)  -- Default associated type instance, if any


-- This is just like CoAxBranch
data IfaceAxBranch = IfaceAxBranch { ifaxbTyVars    :: [IfaceTvBndr]
                                   , ifaxbEtaTyVars :: [IfaceTvBndr]
                                   , ifaxbCoVars    :: [IfaceIdBndr]
                                   , ifaxbLHS       :: IfaceAppArgs
                                   , ifaxbRoles     :: [Role]
                                   , ifaxbRHS       :: IfaceType
                                   , ifaxbIncomps   :: [BranchIndex] }
                                     -- See Note [Storing compatibility] in GHC.Core.Coercion.Axiom

data IfaceConDecls
  = IfAbstractTyCon -- c.f TyCon.AbstractTyCon
  | IfDataTyCon !Bool [IfaceConDecl] -- Data type decls
        -- The Bool is True for "type data" declarations.
        -- see Note [Type data declarations] in GHC.Rename.Module
  | IfNewTyCon  IfaceConDecl   -- Newtype decls

-- For IfDataTyCon and IfNewTyCon we store:
--  * the data constructor(s);
-- The field labels are stored individually in the IfaceConDecl
-- (there is some redundancy here, because a field label may occur
-- in multiple IfaceConDecls and represent the same field label)

data IfaceConDecl
  = IfCon {
        ifConName    :: IfaceTopBndr,                -- Constructor name
        ifConWrapper :: Bool,                   -- True <=> has a wrapper
        ifConInfix   :: Bool,                   -- True <=> declared infix

        -- The universal type variables are precisely those
        -- of the type constructor of this data constructor
        -- This is *easy* to guarantee when creating the IfCon
        -- but it's not so easy for the original TyCon/DataCon
        -- So this guarantee holds for IfaceConDecl, but *not* for DataCon

        ifConExTCvs   :: [IfaceBndr],  -- Existential ty/covars
        ifConUserTvBinders :: [IfaceForAllSpecBndr],
          -- The tyvars, in the order the user wrote them
          -- INVARIANT: the set of tyvars in ifConUserTvBinders is exactly the
          --            set of tyvars (*not* covars) of ifConExTCvs, unioned
          --            with the set of ifBinders (from the parent IfaceDecl)
          --            whose tyvars do not appear in ifConEqSpec
          -- See Note [DataCon user type variable binders] in GHC.Core.DataCon
        ifConEqSpec  :: IfaceEqSpec,        -- Equality constraints
        ifConCtxt    :: IfaceContext,       -- Non-stupid context
        ifConArgTys  :: [(IfaceMult, IfaceType)],-- Arg types
        ifConFields  :: [FieldLabel],  -- ...ditto... (field labels)
        ifConStricts :: [IfaceBang],
          -- Empty (meaning all lazy),
          -- or 1-1 corresp with arg tys
          -- See Note [Bangs on imported data constructors] in GHC.Types.Id.Make
        ifConSrcStricts :: [IfaceSrcBang] } -- empty meaning no src stricts

type IfaceEqSpec = [(IfLclName,IfaceType)]

-- | This corresponds to an HsImplBang; that is, the final
-- implementation decision about the data constructor arg
data IfaceBang
  = IfNoBang | IfStrict | IfUnpack | IfUnpackCo IfaceCoercion

-- | This corresponds to HsSrcBang
data IfaceSrcBang
  = IfSrcBang SrcUnpackedness SrcStrictness

-- See Note [Named default declarations] in GHC.Tc.Gen.Default
-- | Exported named defaults
data IfaceDefault
  = IfaceDefault { ifDefaultCls  :: IfaceTyCon,            -- Defaulted class
                   ifDefaultTys  :: [IfaceType],          -- List of defaults
                   ifDefaultWarn :: Maybe IfaceWarningTxt }

data IfaceClsInst
  = IfaceClsInst { ifInstCls  :: IfExtName,                -- See comments with
                   ifInstTys  :: [Maybe IfaceTyCon],       -- the defn of ClsInst
                   ifDFun     :: IfExtName,                -- The dfun
                   ifOFlag    :: OverlapFlag,              -- Overlap flag
                   ifInstOrph :: IsOrphan,                 -- See Note [Orphans] in GHC.Core.InstEnv
                   ifInstWarn :: Maybe IfaceWarningTxt }
                     -- Warning emitted when the instance is used
                     -- See Note [Implementation of deprecated instances]
                     -- in GHC.Tc.Solver.Dict
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

data IfaceWarnings
  = IfWarnAll IfaceWarningTxt
  | IfWarnSome [(OccName, IfaceWarningTxt)]
               [(IfExtName, IfaceWarningTxt)]

data IfaceWarningTxt
  = IfWarningTxt (Maybe WarningCategory) SourceText [(IfaceStringLiteral, [IfExtName])]
  | IfDeprecatedTxt                      SourceText [(IfaceStringLiteral, [IfExtName])]

data IfaceStringLiteral
  = IfStringLiteral SourceText FastString

data IfaceAnnotation
  = IfaceAnnotation {
        ifAnnotatedTarget :: IfaceAnnTarget,
        ifAnnotatedValue  :: AnnPayload
  }

type IfaceAnnTarget = AnnTarget OccName

data IfaceCompleteMatch = IfaceCompleteMatch [IfExtName] (Maybe IfExtName)

instance Outputable IfaceCompleteMatch where
  ppr (IfaceCompleteMatch cls mtc) = text "COMPLETE" <> colon <+> ppr cls <+> case mtc of
    Nothing -> empty
    Just tc -> dcolon <+> ppr tc

-- Here's a tricky case:
--   * Compile with -O module A, and B which imports A.f
--   * Change function f in A, and recompile without -O
--   * When we read in old A.hi we read in its IdInfo (as a thunk)
--      (In earlier GHCs we used to drop IdInfo immediately on reading,
--       but we do not do that now.  Instead it's discarded when the
--       ModIface is read into the various decl pools.)
--   * The version comparison sees that new (=NoInfo) differs from old (=HasInfo *)
--      and so gives a new version.

type IfaceIdInfo = [IfaceInfoItem]

data IfaceInfoItem
  = HsArity         Arity
  | HsDmdSig        DmdSig
  | HsCprSig        CprSig
  | HsInline        InlinePragma
  | HsUnfold        Bool             -- True <=> isStrongLoopBreaker is true
                    IfaceUnfolding   -- See Note [Expose recursive functions]
  | HsNoCafRefs
  | HsLFInfo        IfaceLFInfo
  | HsTagSig        TagSig

-- NB: Specialisations and rules come in separately and are
-- only later attached to the Id.  Partial reason: some are orphans.

data IfaceUnfolding
  = IfCoreUnfold UnfoldingSource
                 IfUnfoldingCache -- See Note [Tying the 'CoreUnfolding' knot]
                 IfGuidance
                 IfaceExpr
  | IfDFunUnfold [IfaceBndr] [IfaceExpr]

type IfUnfoldingCache = UnfoldingCache

data IfGuidance
  = IfNoGuidance            -- Compute it from the IfaceExpr
  | IfWhen Arity Bool Bool  -- Just like UnfWhen in Core.UnfoldingGuidance

-- We only serialise the IdDetails of top-level Ids, and even then
-- we only need a very limited selection.  Notably, none of the
-- implicit ones are needed here, because they are not put in
-- interface files

data IfaceIdDetails
  = IfVanillaId
  | IfWorkerLikeId [CbvMark]
  | IfRecSelId
    { ifRecSelIdParent     :: Either IfaceTyCon IfaceDecl
    , ifRecSelFirstCon     :: IfaceTopBndr
    , ifRecSelIdIsNaughty  :: Bool
    , ifRecSelIdFieldLabel :: FieldLabel }
  | IfDFunId

-- | Iface type for LambdaFormInfo. Fields not relevant for imported Ids are
-- omitted in this type.
data IfaceLFInfo
  = IfLFReEntrant !RepArity
  | IfLFThunk
      !Bool -- True <=> updatable
      !Bool -- True <=> might be a function type
  | IfLFCon !Name
  | IfLFUnknown !Bool
  | IfLFUnlifted

instance Outputable IfaceLFInfo where
    ppr (IfLFReEntrant arity) =
      text "LFReEntrant" <+> ppr arity

    ppr (IfLFThunk updatable mb_fun) =
      text "LFThunk" <+> parens
        (text "updatable=" <> ppr updatable <+>
         text "might_be_function=" <+> ppr mb_fun)

    ppr (IfLFCon con) =
      text "LFCon" <> brackets (ppr con)

    ppr IfLFUnlifted =
      text "LFUnlifted"

    ppr (IfLFUnknown fun_flag) =
      text "LFUnknown" <+> ppr fun_flag

instance Binary IfaceLFInfo where
    put_ bh (IfLFReEntrant arity) = do
        putByte bh 0
        put_ bh arity
    put_ bh (IfLFThunk updatable mb_fun) = do
        putByte bh 1
        put_ bh updatable
        put_ bh mb_fun
    put_ bh (IfLFCon con_name) = do
        putByte bh 2
        put_ bh con_name
    put_ bh (IfLFUnknown fun_flag) = do
        putByte bh 3
        put_ bh fun_flag
    put_ bh IfLFUnlifted =
        putByte bh 4
    get bh = do
        tag <- getByte bh
        case tag of
            0 -> IfLFReEntrant <$> get bh
            1 -> IfLFThunk <$> get bh <*> get bh
            2 -> IfLFCon <$> get bh
            3 -> IfLFUnknown <$> get bh
            4 -> pure IfLFUnlifted
            _ -> panic "Invalid byte"

{-
Note [Versioning of instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See [https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/recompilation-avoidance#instances]


************************************************************************
*                                                                      *
                Functions over declarations
*                                                                      *
************************************************************************
-}

visibleIfConDecls :: IfaceConDecls -> [IfaceConDecl]
visibleIfConDecls (IfAbstractTyCon {}) = []
visibleIfConDecls (IfDataTyCon _ cs)   = cs
visibleIfConDecls (IfNewTyCon c)       = [c]

ifaceDeclImplicitBndrs :: IfaceDecl -> [OccName]
--  *Excludes* the 'main' name, but *includes* the implicitly-bound names
-- Deeply revolting, because it has to predict what gets bound,
-- especially the question of whether there's a wrapper for a datacon
-- See Note [Implicit TyThings] in GHC.Driver.Env

-- N.B. the set of names returned here *must* match the set of TyThings
-- returned by GHC.Types.TyThing.implicitTyThings, in the sense that
-- TyThing.getOccName should define a bijection between the two lists.
-- This invariant is used in GHC.IfaceToCore.tc_iface_decl_fingerprint
-- (see Note [Tricky iface loop] in GHC.Types.TyThing.)
-- The order of the list does not matter.

ifaceDeclImplicitBndrs (IfaceData {ifName = tc_name, ifCons = cons })
  = case cons of
      IfAbstractTyCon {} -> []
      IfNewTyCon  cd     -> mkNewTyCoOcc (occName tc_name) : ifaceConDeclImplicitBndrs cd
      IfDataTyCon type_data cds
        | type_data ->
          -- Constructors in "type data" declarations have no implicits.
          -- see Note [Type data declarations] in GHC.Rename.Module
          [occName con_name | IfCon { ifConName = con_name } <- cds]
        | otherwise -> concatMap ifaceConDeclImplicitBndrs cds

ifaceDeclImplicitBndrs (IfaceClass { ifBody = IfAbstractClass })
  = []

ifaceDeclImplicitBndrs (IfaceClass { ifName = cls_tc_name
                                   , ifBody = IfConcreteClass {
                                        ifClassCtxt = sc_ctxt,
                                        ifSigs      = sigs,
                                        ifATs       = ats
                                     }})
  = --   (possibly) newtype coercion
    co_occs ++
    --    data constructor (DataCon namespace)
    --    data worker (Id namespace)
    --    no wrapper (class dictionaries never have a wrapper)
    [dc_occ, dcww_occ] ++
    -- associated types
    [occName (ifName at) | IfaceAT at _ <- ats ] ++
    -- superclass selectors
    [mkSuperDictSelOcc n cls_tc_occ | n <- [1..n_ctxt]] ++
    -- operation selectors
    [occName op | IfaceClassOp op  _ _ <- sigs]
  where
    cls_tc_occ = occName cls_tc_name
    n_ctxt = length sc_ctxt
    n_sigs = length sigs
    co_occs | is_newtype = [mkNewTyCoOcc cls_tc_occ]
            | otherwise  = []
    dcww_occ = mkDataConWorkerOcc dc_occ
    dc_occ = mkClassDataConOcc cls_tc_occ
    is_newtype = n_sigs + n_ctxt == 1 -- Sigh (keep this synced with buildClass)

ifaceDeclImplicitBndrs _ = []

ifaceConDeclImplicitBndrs :: IfaceConDecl -> [OccName]
ifaceConDeclImplicitBndrs (IfCon {
        ifConWrapper = has_wrapper, ifConName = con_name })
  = [occName con_name, work_occ] ++ wrap_occs
  where
    con_occ = occName con_name
    work_occ  = mkDataConWorkerOcc con_occ                   -- Id namespace
    wrap_occs | has_wrapper = [mkDataConWrapperOcc con_occ]  -- Id namespace
              | otherwise   = []

-- -----------------------------------------------------------------------------
-- The fingerprints of an IfaceDecl

       -- We better give each name bound by the declaration a
       -- different fingerprint!  So we calculate the fingerprint of
       -- each binder by combining the fingerprint of the whole
       -- declaration with the name of the binder. (#5614, #7215)
ifaceDeclFingerprints :: Fingerprint -> IfaceDecl -> [(OccName,Fingerprint)]
ifaceDeclFingerprints hash decl
  = (getOccName decl, hash) :
    [ (occ, computeFingerprint' (hash,occ))
    | occ <- ifaceDeclImplicitBndrs decl ]
  where
     computeFingerprint' =
       unsafeDupablePerformIO
        . computeFingerprint (panic "ifaceDeclFingerprints")

fromIfaceWarnings :: IfaceWarnings -> Warnings GhcRn
fromIfaceWarnings = \case
    IfWarnAll txt -> WarnAll (fromIfaceWarningTxt txt)
    IfWarnSome vs ds -> WarnSome [(occ, fromIfaceWarningTxt txt) | (occ, txt) <- vs]
                                 [(occ, fromIfaceWarningTxt txt) | (occ, txt) <- ds]

fromIfaceWarningTxt :: IfaceWarningTxt -> WarningTxt GhcRn
fromIfaceWarningTxt = \case
    IfWarningTxt mb_cat src strs -> WarningTxt (noLocA . fromWarningCategory <$> mb_cat) src (noLocA <$> map fromIfaceStringLiteralWithNames strs)
    IfDeprecatedTxt src strs -> DeprecatedTxt src (noLocA <$> map fromIfaceStringLiteralWithNames strs)

fromIfaceStringLiteralWithNames :: (IfaceStringLiteral, [IfExtName]) -> WithHsDocIdentifiers StringLiteral GhcRn
fromIfaceStringLiteralWithNames (str, names) = WithHsDocIdentifiers (fromIfaceStringLiteral str) (map noLoc names)

fromIfaceStringLiteral :: IfaceStringLiteral -> StringLiteral
fromIfaceStringLiteral (IfStringLiteral st fs) = StringLiteral st fs Nothing


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
  | IfaceLet    (IfaceBinding IfaceLetBndr) IfaceExpr
  | IfaceCast   IfaceExpr IfaceCoercion
  | IfaceLit    Literal
  | IfaceLitRubbish TypeOrConstraint IfaceType
       -- See GHC.Types.Literal Note [Rubbish literals] item (6)
  | IfaceFCall  ForeignCall IfaceType
  | IfaceTick   IfaceTickish IfaceExpr    -- from Tick tickish E


data IfaceTickish
  = IfaceHpcTick    Module Int               -- from HpcTick x
  | IfaceSCC        CostCentre Bool Bool     -- from ProfNote
  | IfaceSource  RealSrcSpan FastString      -- from SourceNote
  | IfaceBreakpoint Int [IfaceExpr] Module   -- from Breakpoint

data IfaceAlt = IfaceAlt IfaceConAlt [IfLclName] IfaceExpr
        -- Note: IfLclName, not IfaceBndr (and same with the case binder)
        -- We reconstruct the kind/type of the thing from the context
        -- thus saving bulk in interface files

data IfaceConAlt = IfaceDefaultAlt
                 | IfaceDataAlt IfExtName
                 | IfaceLitAlt Literal

type IfaceBinding b = IfaceBindingX IfaceExpr b

data IfaceBindingX r b
  = IfaceNonRec b r
  | IfaceRec    [(b, r)]
  deriving (Functor, Foldable, Traversable, Ord, Eq)

-- IfaceLetBndr is like IfaceIdBndr, but has IdInfo too
-- It's used for *non-top-level* let/rec binders
-- See Note [IdInfo on nested let-bindings]
data IfaceLetBndr = IfLetBndr IfLclName IfaceType IfaceIdInfo JoinPointHood

data IfaceTopBndrInfo = IfLclTopBndr IfLclName IfaceType IfaceIdInfo IfaceIdDetails
                      | IfGblTopBndr IfaceTopBndr

-- See Note [Interface File with Core: Sharing RHSs]
data IfaceMaybeRhs = IfUseUnfoldingRhs | IfRhs IfaceExpr

{-
Note [Empty case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Iface syntax an IfaceCase does not record the types of the alternatives,
unlike Core syntax Case. But we need this type if the alternatives are empty.
Hence IfaceECase. See Note [Empty case alternatives] in GHC.Core.

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

In general we retain all info that is left by GHC.Core.Tidy.tidyLetBndr, since
that is what is seen by importing module with --make

Note [Displaying axiom incompatibilities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With -fprint-axiom-incomps we display which closed type family equations
are incompatible with which. This information is sometimes necessary
because GHC doesn't try equations in order: any equation can be used when
all preceding equations that are incompatible with it do not apply.

For example, the last "a && a = a" equation in Data.Type.Bool.&& is
actually compatible with all previous equations, and can reduce at any
time.

This is displayed as:
Prelude> :i Data.Type.Equality.==
type family (==) (a :: k) (b :: k) :: Bool
  where
    {- #0 -} (==) (f a) (g b) = (f == g) && (a == b)
    {- #1 -} (==) a a = 'True
          -- incompatible with: #0
    {- #2 -} (==) _1 _2 = 'False
          -- incompatible with: #1, #0
The comment after an equation refers to all previous equations (0-indexed)
that are incompatible with it.

************************************************************************
*                                                                      *
              Printing IfaceDecl
*                                                                      *
************************************************************************
-}

pprAxBranch :: SDoc -> BranchIndex -> IfaceAxBranch -> SDoc
-- The TyCon might be local (just an OccName), or this might
-- be a branch for an imported TyCon, so it would be an ExtName
-- So it's easier to take an SDoc here
--
-- This function is used
--    to print interface files,
--    in debug messages
--    in :info F for GHCi, which goes via toConToIfaceDecl on the family tycon
-- For user error messages we use Coercion.pprCoAxiom and friends
pprAxBranch pp_tc idx (IfaceAxBranch { ifaxbTyVars = tvs
                                     , ifaxbCoVars = _cvs
                                     , ifaxbLHS = pat_tys
                                     , ifaxbRHS = rhs
                                     , ifaxbIncomps = incomps })
  = assertPpr (null _cvs) (pp_tc $$ ppr _cvs) $
    hang ppr_binders 2 (hang pp_lhs 2 (equals <+> ppr rhs))
    $+$
    nest 4 maybe_incomps
  where
    -- See Note [Printing foralls in type family instances] in GHC.Iface.Type
    ppr_binders = maybe_index <+>
      pprUserIfaceForAll (map (mkIfaceForAllTvBndr Specified) tvs)
    pp_lhs = hang pp_tc 2 (pprParendIfaceAppArgs pat_tys)

    -- See Note [Displaying axiom incompatibilities]
    maybe_index
      = ppWhenOption sdocPrintAxiomIncomps $
          text "{-" <+> (text "#" <> ppr idx) <+> text "-}"
    maybe_incomps
      = ppWhenOption sdocPrintAxiomIncomps $
          ppWhen (notNull incomps) $
            text "--" <+> text "incompatible with:"
            <+> pprWithCommas (\incomp -> text "#" <> ppr incomp) incomps

instance Outputable IfaceWarnings where
    ppr = \case
        IfWarnAll txt -> text "Warn all" <+> ppr txt
        IfWarnSome vs ds ->
          hang (text "Warnings:") 2 $
            text "Deprecated names:" <+> vcat [ppr name <+> ppr txt | (name, txt) <- vs] $$
            text "Deprecated exports:" <+> vcat [ppr name <+> ppr txt | (name, txt) <- ds]

instance Outputable IfaceWarningTxt where
    ppr = \case
        IfWarningTxt _ _ ws  -> pp_ws ws
        IfDeprecatedTxt _ ds -> pp_ws ds
      where
        pp_ws [msg] = pp_with_name msg
        pp_ws msgs = brackets $ vcat . punctuate comma . map pp_with_name $ msgs

        pp_with_name = ppr . fst

instance Outputable IfaceStringLiteral where
    ppr (IfStringLiteral st fs) = pprWithSourceText st (ftext fs)

instance Outputable IfaceAnnotation where
  ppr (IfaceAnnotation target value) = ppr target <+> colon <+> ppr value

instance NamedThing IfaceClassOp where
  getName (IfaceClassOp n _ _) = n

instance HasOccName IfaceClassOp where
  occName = getOccName

instance NamedThing IfaceConDecl where
  getName = ifConName

instance HasOccName IfaceConDecl where
  occName = getOccName

instance NamedThing IfaceDecl where
  getName = ifName

instance HasOccName IfaceDecl where
  occName = getOccName

instance Outputable IfaceDecl where
  ppr = pprIfaceDecl showToIface

instance (Outputable r, Outputable b) => Outputable (IfaceBindingX r b) where
  ppr b = case b of
            (IfaceNonRec b r) -> ppr_bind (b, r)
            (IfaceRec pairs) -> sep [text "rec {", nest 2 (sep (map ppr_bind pairs)),text "}"]
    where
      ppr_bind (b, r) = ppr b <+> equals <+> ppr r

instance Outputable IfaceTopBndrInfo where
    ppr (IfLclTopBndr lcl_name _ _ _) = ppr lcl_name
    ppr (IfGblTopBndr gbl) = ppr gbl

instance Outputable IfaceMaybeRhs where
  ppr IfUseUnfoldingRhs = text "<unfolding>"
  ppr (IfRhs ie) = ppr ie

{-
Note [Minimal complete definition]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The minimal complete definition should only be included if a complete
class definition is shown. Since the minimal complete definition is
anonymous we can't reuse the same mechanism that is used for the
filtering of method signatures. Instead we just check if anything at all is
filtered and hide it in that case.
-}

{-
Note [Printing IfaceDecl binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The binders in an IfaceDecl are just OccNames, so we don't know what module they
come from.  But when we pretty-print a TyThing by converting to an IfaceDecl
(see GHC.Types.TyThing.Ppr), the TyThing may come from some other module so we really need
the module qualifier.  We solve this by passing in a pretty-printer for the
binders.

When printing an interface file (--show-iface), we want to print
everything unqualified, so we can just print the OccName directly.
-}

-- | Show a declaration but not its RHS.
showToHeader :: ShowSub
showToHeader = ShowSub { ss_how_much = ShowHeader $ AltPpr Nothing
                       , ss_forall = ShowForAllWhen }

-- | Show declaration and its RHS, including GHc-internal information (e.g.
-- for @--show-iface@).
showToIface :: ShowSub
showToIface = ShowSub { ss_how_much = ShowIface
                      , ss_forall = ShowForAllWhen }

ppShowIface :: ShowSub -> SDoc -> SDoc
ppShowIface (ShowSub { ss_how_much = ShowIface }) doc = doc
ppShowIface _                                     _   = Outputable.empty

-- show if all sub-components or the complete interface is shown
ppShowAllSubs :: ShowSub -> SDoc -> SDoc -- See Note [Minimal complete definition]
ppShowAllSubs (ShowSub { ss_how_much = ShowSome Nothing _ }) doc
                                                        = doc
ppShowAllSubs (ShowSub { ss_how_much = ShowIface }) doc = doc
ppShowAllSubs _                                     _   = Outputable.empty

ppShowRhs :: ShowSub -> SDoc -> SDoc
ppShowRhs (ShowSub { ss_how_much = ShowHeader _ }) _   = Outputable.empty
ppShowRhs _                                        doc = doc

showSub :: HasOccName n => ShowSub -> n -> Bool
showSub (ShowSub { ss_how_much = ShowHeader _ }) _     = False
showSub (ShowSub { ss_how_much = ShowSome (Just f) _ }) thing
                                                       = f (occName thing)
showSub (ShowSub { ss_how_much = _ })            _     = True

ppr_trim :: [Maybe SDoc] -> [SDoc]
-- Collapse a group of Nothings to a single "..."
ppr_trim xs
  = snd (foldr go (False, []) xs)
  where
    go (Just doc) (_,     so_far) = (False, doc : so_far)
    go Nothing    (True,  so_far) = (True, so_far)
    go Nothing    (False, so_far) = (True, text "..." : so_far)

isIfaceDataInstance :: IfaceTyConParent -> Bool
isIfaceDataInstance IfNoParent = False
isIfaceDataInstance _          = True

pprClassRoles :: ShowSub -> IfaceTopBndr -> [IfaceTyConBinder] -> [Role] -> SDoc
pprClassRoles ss clas binders roles =
    pprRoles (== Nominal)
             (pprPrefixIfDeclBndr (ss_how_much ss) (occName clas))
             binders
             roles

pprClassStandaloneKindSig :: ShowSub -> IfaceTopBndr -> IfaceKind -> SDoc
pprClassStandaloneKindSig ss clas =
  pprStandaloneKindSig (pprPrefixIfDeclBndr (ss_how_much ss) (occName clas))

constraintIfaceKind :: IfaceKind
constraintIfaceKind =
  IfaceTyConApp (IfaceTyCon constraintKindTyConName (mkIfaceTyConInfo NotPromoted IfaceNormalTyCon)) IA_Nil

pprIfaceDecl :: ShowSub -> IfaceDecl -> SDoc
-- NB: pprIfaceDecl is also used for pretty-printing TyThings in GHCi
--     See Note [Pretty printing via Iface syntax] in GHC.Types.TyThing.Ppr
pprIfaceDecl ss (IfaceData { ifName = tycon, ifCType = ctype,
                             ifCtxt = context, ifResKind = kind,
                             ifRoles = roles, ifCons = condecls,
                             ifParent = parent,
                             ifGadtSyntax = gadt,
                             ifBinders = binders })

  | gadt      = vcat [ pp_roles
                     , pp_ki_sig
                     , pp_nd <+> pp_lhs <+> pp_kind <+> pp_where
                     , nest 2 (vcat pp_cons)
                     , nest 2 $ ppShowIface ss pp_extra ]
  | otherwise = vcat [ pp_roles
                     , pp_ki_sig
                     , hang (pp_nd <+> pp_lhs) 2 (add_bars pp_cons)
                     , nest 2 $ ppShowIface ss pp_extra ]
  where
    is_data_instance = isIfaceDataInstance parent
    -- See Note [Printing foralls in type family instances] in GHC.Iface.Type
    pp_data_inst_forall :: SDoc
    pp_data_inst_forall = pprUserIfaceForAll forall_bndrs

    forall_bndrs :: [IfaceForAllBndr]
    forall_bndrs = [Bndr (binderVar tc_bndr) Specified | tc_bndr <- binders]

    cons       = visibleIfConDecls condecls
    pp_where   = ppWhen (gadt && not (null cons)) $ text "where"
    pp_cons    = ppr_trim (map show_con cons) :: [SDoc]
    pp_kind    = ppUnless (ki_sig_printable || isIfaceLiftedTypeKind kind)
                          (dcolon <+> ppr kind)

    pp_lhs = case parent of
               IfNoParent -> pprIfaceDeclHead suppress_bndr_sig context ss tycon binders
               IfDataInstance{}
                          -> text "instance" <+> pp_data_inst_forall
                                             <+> pprIfaceTyConParent parent

    pp_roles
      | is_data_instance = empty
      | otherwise        = pprRoles (== Representational) name_doc binders roles
            -- Don't display roles for data family instances (yet)
            -- See discussion on #8672.

    ki_sig_printable =
      -- If we print a standalone kind signature for a data instance, we leak
      -- the internal constructor name:
      --
      --    type T15827.R:Dka :: forall k. k -> *
      --    data instance forall k (a :: k). D a = MkD (Proxy a)
      --
      -- This T15827.R:Dka is a compiler-generated type constructor for the
      -- data instance.
      not is_data_instance

    pp_ki_sig = ppWhen ki_sig_printable $
                pprStandaloneKindSig name_doc (mkIfaceTyConKind binders kind)

    -- See Note [Suppressing binder signatures] in GHC.Iface.Type
    suppress_bndr_sig = SuppressBndrSig ki_sig_printable

    name_doc = pprPrefixIfDeclBndr (ss_how_much ss) (occName tycon)

    add_bars []     = Outputable.empty
    add_bars (c:cs) = sep ((equals <+> c) : map (vbar <+>) cs)

    ok_con dc = showSub ss dc || any (showSub ss . flSelector) (ifConFields dc)

    show_con dc
      | ok_con dc = Just $ pprIfaceConDecl ss gadt tycon binders parent dc
      | otherwise = Nothing

    pp_nd = case condecls of
              IfAbstractTyCon{} -> text "data"
              IfDataTyCon True _ -> text "type data"
              IfDataTyCon{}     -> text "data"
              IfNewTyCon{}      -> text "newtype"

    pp_extra = vcat [pprCType ctype]

pprIfaceDecl ss (IfaceClass { ifName  = clas
                            , ifRoles = roles
                            , ifFDs    = fds
                            , ifBinders = binders
                            , ifBody = IfAbstractClass })
  = vcat [ pprClassRoles ss clas binders roles
         , pprClassStandaloneKindSig ss clas (mkIfaceTyConKind binders constraintIfaceKind)
         , text "class" <+> pprIfaceDeclHead suppress_bndr_sig [] ss clas binders <+> pprFundeps fds ]
  where
    -- See Note [Suppressing binder signatures] in GHC.Iface.Type
    suppress_bndr_sig = SuppressBndrSig True

pprIfaceDecl ss (IfaceClass { ifName  = clas
                            , ifRoles = roles
                            , ifFDs    = fds
                            , ifBinders = binders
                            , ifBody = IfConcreteClass {
                                ifATs = ats,
                                ifSigs = sigs,
                                ifClassCtxt = context,
                                ifMinDef = minDef
                              }})
  = vcat [ pprClassRoles ss clas binders roles
         , pprClassStandaloneKindSig ss clas (mkIfaceTyConKind binders constraintIfaceKind)
         , text "class" <+> pprIfaceDeclHead suppress_bndr_sig context ss clas binders <+> pprFundeps fds <+> pp_where
         , nest 2 (vcat [ vcat asocs, vcat dsigs
                        , ppShowAllSubs ss (pprMinDef minDef)])]
    where
      pp_where = ppShowRhs ss $ ppUnless (null sigs && null ats) (text "where")

      asocs = ppr_trim $ map maybeShowAssoc ats
      dsigs = ppr_trim $ map maybeShowSig sigs

      maybeShowAssoc :: IfaceAT -> Maybe SDoc
      maybeShowAssoc asc@(IfaceAT d _)
        | showSub ss d = Just $ pprIfaceAT ss asc
        | otherwise    = Nothing

      maybeShowSig :: IfaceClassOp -> Maybe SDoc
      maybeShowSig sg
        | showSub ss sg = Just $  pprIfaceClassOp ss sg
        | otherwise     = Nothing

      pprMinDef :: IfaceBooleanFormula -> SDoc
      pprMinDef minDef = ppUnless (ifLclIsTrue minDef) $ -- hide empty definitions
        text "{-# MINIMAL" <+>
        pprifLclBooleanFormula
          (\_ def -> let fs = ifLclNameFS def in cparen (isLexSym fs) (ppr fs)) 0 minDef <+>
        text "#-}"

      ifLclIsTrue :: IfaceBooleanFormula -> Bool
      ifLclIsTrue (IfAnd []) = True
      ifLclIsTrue _          = False

      pprifLclBooleanFormula  :: (Rational -> IfLclName -> SDoc)
                              -> Rational -> IfaceBooleanFormula -> SDoc
      pprifLclBooleanFormula pprVar = go
        where
        go p (IfVar x)  = pprVar p x
        go p (IfAnd []) = cparen (p > 0) empty
        go p (IfAnd xs) = pprAnd p (map (go 3) xs)
        go _ (IfOr  []) = keyword $ text "FALSE"
        go p (IfOr  xs) = pprOr p (map (go 2) xs)
        go p (IfParens x) = go p x
        pprAnd p = cparen (p > 3) . fsep . punctuate comma
        pprOr  p = cparen (p > 2) . fsep . intersperse vbar

      -- See Note [Suppressing binder signatures] in GHC.Iface.Type
      suppress_bndr_sig = SuppressBndrSig True

pprIfaceDecl ss (IfaceSynonym { ifName    = tc
                              , ifBinders = binders
                              , ifSynRhs  = mono_ty
                              , ifResKind = res_kind})
  = vcat [ pprStandaloneKindSig name_doc (mkIfaceTyConKind binders res_kind)
         , hang (text "type" <+> pprIfaceDeclHead suppress_bndr_sig [] ss tc binders <+> equals)
           2 (sep [ pprIfaceForAll tvs, pprIfaceContextArr theta, ppr_tau
                  , ppUnless (isIfaceLiftedTypeKind res_kind) (dcolon <+> ppr res_kind) ])
         ]
  where
    (tvs, theta, tau) = splitIfaceSigmaTy mono_ty
    name_doc = pprPrefixIfDeclBndr (ss_how_much ss) (occName tc)

    -- See Note [Printing type abbreviations] in GHC.Iface.Type
    ppr_tau | tc `hasKey` liftedTypeKindTyConKey ||
              tc `hasKey` unrestrictedFunTyConKey ||
              tc `hasKey` constraintKindTyConKey
            = updSDocContext (\ctx -> ctx { sdocPrintTypeAbbreviations = False }) $ ppr tau
            | otherwise = ppr tau

    -- See Note [Suppressing binder signatures] in GHC.Iface.Type
    suppress_bndr_sig = SuppressBndrSig True

pprIfaceDecl ss (IfaceFamily { ifName = tycon
                             , ifFamFlav = rhs, ifBinders = binders
                             , ifResKind = res_kind
                             , ifResVar = res_var, ifFamInj = inj })
  | IfaceDataFamilyTyCon <- rhs
  = vcat [ pprStandaloneKindSig name_doc (mkIfaceTyConKind binders res_kind)
         , text "data family" <+> pprIfaceDeclHead suppress_bndr_sig [] ss tycon binders
         ]

  | otherwise
  = vcat [ pprStandaloneKindSig name_doc (mkIfaceTyConKind binders res_kind)
         , hang (text "type family"
                   <+> pprIfaceDeclHead suppress_bndr_sig [] ss tycon binders
                   <+> pp_inj res_var inj
                   <+> ppShowRhs ss (pp_where rhs))
              2 (ppShowRhs ss (pp_rhs rhs))
           $$
           nest 2 (ppShowRhs ss (pp_branches rhs))
         ]
  where
    name_doc = pprPrefixIfDeclBndr (ss_how_much ss) (occName tycon)

    pp_where (IfaceClosedSynFamilyTyCon {}) = text "where"
    pp_where _                              = empty

    pp_inj Nothing    _   = empty
    pp_inj (Just res) inj
       | Injective injectivity <- inj = hsep [ equals, ppr res
                                             , pp_inj_cond res injectivity]
       | otherwise = hsep [ equals, ppr res ]

    pp_inj_cond res inj = case filterByList inj binders of
       []  -> empty
       tvs -> hsep [vbar, ppr res, text "->", interppSP (map ifTyConBinderName tvs)]

    pp_rhs IfaceDataFamilyTyCon
      = ppShowIface ss (text "data")
    pp_rhs IfaceOpenSynFamilyTyCon
      = ppShowIface ss (text "open")
    pp_rhs IfaceAbstractClosedSynFamilyTyCon
      = ppShowIface ss (text "closed, abstract")
    pp_rhs (IfaceClosedSynFamilyTyCon {})
      = empty  -- see pp_branches
    pp_rhs IfaceBuiltInSynFamTyCon
      = ppShowIface ss (text "built-in")

    pp_branches (IfaceClosedSynFamilyTyCon (Just (ax, brs)))
      = vcat (unzipWith (pprAxBranch
                     (pprPrefixIfDeclBndr
                       (ss_how_much ss)
                       (occName tycon))
                  ) $ zip [0..] brs)
        $$ ppShowIface ss (text "axiom" <+> ppr ax)
    pp_branches _ = Outputable.empty

    -- See Note [Suppressing binder signatures] in GHC.Iface.Type
    suppress_bndr_sig = SuppressBndrSig True

pprIfaceDecl _ (IfacePatSyn { ifName = name,
                              ifPatUnivBndrs = univ_bndrs, ifPatExBndrs = ex_bndrs,
                              ifPatProvCtxt = prov_ctxt, ifPatReqCtxt = req_ctxt,
                              ifPatArgs = arg_tys, ifFieldLabels = pat_fldlbls,
                              ifPatTy = pat_ty} )
  = sdocWithContext mk_msg
  where
    pat_keywrd = text "pattern"
    mk_msg sdocCtx
      = vcat [ ppr_pat_ty
             -- only print this for record pattern synonyms
             , if null pat_fldlbls then Outputable.empty
               else pat_keywrd <+> pprPrefixOcc name <+> pat_body]
      where
        ppr_pat_ty =
          hang (pat_keywrd <+> pprPrefixOcc name)
            2 (dcolon <+> sep [univ_msg
                              , pprIfaceContextArr req_ctxt
                              , ppWhen insert_empty_ctxt $ parens empty <+> darrow
                              , ex_msg
                              , pprIfaceContextArr prov_ctxt
                              , pprIfaceType $ foldr (IfaceFunTy visArgTypeLike many_ty)
                                                     pat_ty arg_tys ])
        pat_body = braces $ sep $ punctuate comma $ map ppr pat_fldlbls
        univ_msg = pprUserIfaceForAll $ tyVarSpecToBinders univ_bndrs
        ex_msg   = pprUserIfaceForAll $ tyVarSpecToBinders ex_bndrs

        insert_empty_ctxt = null req_ctxt
            && not (null prov_ctxt && isEmpty sdocCtx ex_msg)

pprIfaceDecl ss (IfaceId { ifName = var, ifType = ty,
                              ifIdDetails = details, ifIdInfo = info })
  = vcat [ hang (pprPrefixIfDeclBndr (ss_how_much ss) (occName var) <+> dcolon)
              2 (pprIfaceSigmaType (ss_forall ss) ty)
         , ppShowIface ss (ppr details)
         , ppShowIface ss (ppr info) ]

pprIfaceDecl _ (IfaceAxiom { ifName = name, ifTyCon = tycon
                           , ifAxBranches = branches })
  = hang (text "axiom" <+> ppr name <+> dcolon)
       2 (vcat $ unzipWith (pprAxBranch (ppr tycon)) $ zip [0..] branches)

pprCType :: Maybe CType -> SDoc
pprCType Nothing      = Outputable.empty
pprCType (Just cType) = text "C type:" <+> ppr cType

-- if, for each role, suppress_if role is True, then suppress the role
-- output
pprRoles :: (Role -> Bool) -> SDoc -> [IfaceTyConBinder]
         -> [Role] -> SDoc
pprRoles suppress_if tyCon bndrs roles
  = sdocOption sdocPrintExplicitKinds $ \print_kinds ->
      let froles = suppressIfaceInvisibles (PrintExplicitKinds print_kinds) bndrs roles
      in ppUnless (all suppress_if froles || null froles) $
         text "type role" <+> tyCon <+> hsep (map ppr froles)

pprStandaloneKindSig :: SDoc -> IfaceType -> SDoc
pprStandaloneKindSig tyCon ty = text "type" <+> tyCon <+> text "::" <+> ppr ty

pprInfixIfDeclBndr :: ShowHowMuch -> OccName -> SDoc
pprInfixIfDeclBndr (ShowSome _ (AltPpr (Just ppr_bndr))) name
  = pprInfixVar (isSymOcc name) (ppr_bndr name)
pprInfixIfDeclBndr _ name
  = pprInfixVar (isSymOcc name) (ppr name)

pprPrefixIfDeclBndr :: ShowHowMuch -> OccName -> SDoc
pprPrefixIfDeclBndr (ShowHeader (AltPpr (Just ppr_bndr))) name
  = parenSymOcc name (ppr_bndr name)
pprPrefixIfDeclBndr (ShowSome _ (AltPpr (Just ppr_bndr))) name
  = parenSymOcc name (ppr_bndr name)
pprPrefixIfDeclBndr _ name
  = parenSymOcc name (ppr name)

instance Outputable IfaceClassOp where
   ppr = pprIfaceClassOp showToIface

pprIfaceClassOp :: ShowSub -> IfaceClassOp -> SDoc
pprIfaceClassOp ss (IfaceClassOp n ty dm)
  = pp_sig n ty $$ generic_dm
  where
   generic_dm | Just (GenericDM dm_ty) <- dm
              =  text "default" <+> pp_sig n dm_ty
              | otherwise
              = empty
   pp_sig n ty
     = pprPrefixIfDeclBndr (ss_how_much ss) (occName n)
     <+> dcolon
     <+> pprIfaceSigmaType ShowForAllWhen ty

instance Outputable IfaceAT where
   ppr = pprIfaceAT showToIface

pprIfaceAT :: ShowSub -> IfaceAT -> SDoc
pprIfaceAT ss (IfaceAT d mb_def)
  = vcat [ pprIfaceDecl ss d
         , case mb_def of
              Nothing  -> Outputable.empty
              Just rhs -> nest 2 $
                          text "Default:" <+> ppr rhs ]

instance Outputable IfaceTyConParent where
  ppr p = pprIfaceTyConParent p

pprIfaceTyConParent :: IfaceTyConParent -> SDoc
pprIfaceTyConParent IfNoParent
  = Outputable.empty
pprIfaceTyConParent (IfDataInstance _ tc tys)
  = pprIfaceTypeApp topPrec tc tys

pprIfaceDeclHead :: SuppressBndrSig
                 -> IfaceContext -> ShowSub -> Name
                 -> [IfaceTyConBinder]   -- of the tycon, for invisible-suppression
                 -> SDoc
pprIfaceDeclHead suppress_sig context ss tc_occ bndrs
  = sdocOption sdocPrintExplicitKinds $ \print_kinds ->
    sep [ pprIfaceContextArr context
        , pprPrefixIfDeclBndr (ss_how_much ss) (occName tc_occ)
          <+> pprIfaceTyConBinders suppress_sig
                (suppressIfaceInvisibles (PrintExplicitKinds print_kinds) bndrs bndrs) ]

pprIfaceConDecl :: ShowSub -> Bool
                -> IfaceTopBndr
                -> [IfaceTyConBinder]
                -> IfaceTyConParent
                -> IfaceConDecl -> SDoc
pprIfaceConDecl ss gadt_style tycon tc_binders parent
        (IfCon { ifConName = name, ifConInfix = is_infix,
                 ifConUserTvBinders = user_tvbs,
                 ifConEqSpec = eq_spec, ifConCtxt = ctxt, ifConArgTys = arg_tys,
                 ifConStricts = stricts, ifConFields = fields })
  | gadt_style = pp_prefix_con <+> dcolon <+> ppr_gadt_ty
  | otherwise  = ppr_ex_quant pp_h98_con
  where
    pp_h98_con
      | not (null fields) = pp_prefix_con <+> pp_field_args
      | is_infix
      , [ty1, ty2] <- pp_args
      = sep [ ty1
            , pprInfixIfDeclBndr how_much (occName name)
            , ty2]
      | otherwise = pp_prefix_con <+> sep pp_args

    how_much = ss_how_much ss
    tys_w_strs :: [(IfaceBang, IfaceType)]
    tys_w_strs = zip stricts (map snd arg_tys)
    pp_prefix_con = pprPrefixIfDeclBndr how_much (occName name)

    -- If we're pretty-printing a H98-style declaration with existential
    -- quantification, then user_tvbs will always consist of the universal
    -- tyvar binders followed by the existential tyvar binders. So to recover
    -- the visibilities of the existential tyvar binders, we can simply drop
    -- the universal tyvar binders from user_tvbs.
    ex_tvbs = dropList tc_binders user_tvbs
    ppr_ex_quant = pprIfaceForAllPartMust (ifaceForAllSpecToBndrs ex_tvbs) ctxt
    pp_gadt_res_ty = mk_user_con_res_ty eq_spec
    ppr_gadt_ty = pprIfaceForAllPart (ifaceForAllSpecToBndrs user_tvbs) ctxt pp_tau

        -- A bit gruesome this, but we can't form the full con_tau, and ppr it,
        -- because we don't have a Name for the tycon, only an OccName
    pp_tau | null fields
           = case pp_args ++ [pp_gadt_res_ty] of
                (t:ts) -> fsep (t : zipWithEqual "pprIfaceConDecl" (\(w,_) d -> ppr_arr w <+> d)
                                                 arg_tys ts)
                []     -> panic "pp_con_taus"
           | otherwise
           = sep [pp_field_args, arrow <+> pp_gadt_res_ty]

    -- Constructors are linear by default, but we don't want to show
    -- linear arrows when -XLinearTypes is disabled
    ppr_arr w = sdocOption sdocLinearTypes $ \linearTypes ->
                if linearTypes
                then pprTypeArrow visArgTypeLike w
                else arrow

    ppr_bang IfNoBang = whenPprDebug $ char '_'
    ppr_bang IfStrict = char '!'
    ppr_bang IfUnpack = text "{-# UNPACK #-}"
    ppr_bang (IfUnpackCo co) = text "! {-# UNPACK #-}" <>
                               pprParendIfaceCoercion co

    pprFieldArgTy, pprArgTy :: (IfaceBang, IfaceType) -> SDoc
    -- If using record syntax, the only reason one would need to parenthesize
    -- a compound field type is if it's preceded by a bang pattern.
    pprFieldArgTy (bang, ty) = ppr_arg_ty (bang_prec bang) bang ty
    -- If not using record syntax, a compound field type might need to be
    -- parenthesized if one of the following holds:
    --
    -- 1. We're using Haskell98 syntax.
    -- 2. The field type is preceded with a bang pattern.
    pprArgTy (bang, ty) = ppr_arg_ty (max gadt_prec (bang_prec bang)) bang ty

    ppr_arg_ty :: PprPrec -> IfaceBang -> IfaceType -> SDoc
    ppr_arg_ty prec bang ty = ppr_bang bang <> pprPrecIfaceType prec ty

    -- If we're displaying the fields GADT-style, e.g.,
    --
    --   data Foo a where
    --     MkFoo :: (Int -> Int) -> Maybe a -> Foo
    --
    -- Then we use `funPrec`, since that will ensure `Int -> Int` gets the
    -- parentheses that it requires, but simple compound types like `Maybe a`
    -- (which don't require parentheses in a function argument position) won't
    -- get them, assuming that there are no bang patterns (see bang_prec).
    --
    -- If we're displaying the fields Haskell98-style, e.g.,
    --
    --   data Foo a = MkFoo (Int -> Int) (Maybe a)
    --
    -- Then not only must we parenthesize `Int -> Int`, we must also
    -- parenthesize compound fields like (Maybe a). Therefore, we pick
    -- `appPrec`, which has higher precedence than `funPrec`.
    gadt_prec :: PprPrec
    gadt_prec
      | gadt_style = funPrec
      | otherwise  = appPrec

    -- The presence of bang patterns or UNPACK annotations requires
    -- surrounding the type with parentheses, if needed (#13699)
    bang_prec :: IfaceBang -> PprPrec
    bang_prec IfNoBang     = topPrec
    bang_prec IfStrict     = appPrec
    bang_prec IfUnpack     = appPrec
    bang_prec IfUnpackCo{} = appPrec

    pp_args :: [SDoc] -- No records, e.g., `  Maybe a  ->  Int -> ...` or
                      --                   `!(Maybe a) -> !Int -> ...`
    pp_args = map pprArgTy tys_w_strs

    pp_field_args :: SDoc -- Records, e.g., { x ::   Maybe a,  y ::  Int } or
                          --                { x :: !(Maybe a), y :: !Int }
    pp_field_args = braces $ sep $ punctuate comma $ ppr_trim $
                    zipWith maybe_show_label fields tys_w_strs

    maybe_show_label :: FieldLabel -> (IfaceBang, IfaceType) -> Maybe SDoc
    maybe_show_label lbl bty
      | showSub ss sel = Just (pprPrefixIfDeclBndr how_much occ
                                <+> dcolon <+> pprFieldArgTy bty)
      | otherwise      = Nothing
      where
        sel = flSelector lbl
        occ = nameOccName sel

    mk_user_con_res_ty :: IfaceEqSpec -> SDoc
    -- See Note [Result type of a data family GADT]
    mk_user_con_res_ty eq_spec
      | IfDataInstance _ tc tys <- parent
      = pprIfaceType (IfaceTyConApp tc (substIfaceAppArgs gadt_subst tys))
      | otherwise
      = ppr_tc_app gadt_subst
      where
        gadt_subst = mkIfaceTySubst eq_spec

    -- When pretty-printing a GADT return type, we:
    --
    -- 1. Take the data tycon binders, extract their variable names and
    --    visibilities, and construct suitable arguments from them. (This is
    --    the role of mk_tc_app_args.)
    -- 2. Apply the GADT substitution constructed from the eq_spec.
    --    (See Note [Result type of a data family GADT].)
    -- 3. Pretty-print the data type constructor applied to its arguments.
    --    This process will omit any invisible arguments, such as coercion
    --    variables, if necessary. (See Note
    --    [VarBndrs, ForAllTyBinders, TyConBinders, and visibility] in GHC.Core.TyCo.Rep.)
    ppr_tc_app gadt_subst =
      pprPrefixIfDeclBndr how_much (occName tycon)
      <+> pprParendIfaceAppArgs
            (substIfaceAppArgs gadt_subst (mk_tc_app_args tc_binders))

    mk_tc_app_args :: [IfaceTyConBinder] -> IfaceAppArgs
    mk_tc_app_args [] = IA_Nil
    mk_tc_app_args (Bndr bndr vis:tc_bndrs) =
      IA_Arg (IfaceTyVar (ifaceBndrName bndr)) (tyConBndrVisForAllTyFlag vis)
             (mk_tc_app_args tc_bndrs)

instance Outputable IfaceRule where
  ppr (IfaceRule { ifRuleName = name, ifActivation = act, ifRuleBndrs = bndrs,
                   ifRuleHead = fn, ifRuleArgs = args, ifRuleRhs = rhs,
                   ifRuleOrph = orph })
    = sep [ hsep [ pprRuleName name
                 , if isOrphan orph then text "[orphan]" else Outputable.empty
                 , ppr act
                 , pp_foralls ]
          , nest 2 (sep [ppr fn <+> sep (map pprParendIfaceExpr args),
                        text "=" <+> ppr rhs]) ]
    where
      pp_foralls = ppUnless (null bndrs) $ forAllLit <+> pprIfaceBndrs bndrs <> dot

instance Outputable IfaceDefault where
  ppr (IfaceDefault { ifDefaultCls = cls, ifDefaultTys = tcs })
    = text "default" <+> ppr cls <+> parens (pprWithCommas ppr tcs)

instance Outputable IfaceClsInst where
  ppr (IfaceClsInst { ifDFun = dfun_id, ifOFlag = flag
                    , ifInstCls = cls, ifInstTys = mb_tcs
                    , ifInstOrph = orph })
    = hang (text "instance" <+> ppr flag
              <+> (if isOrphan orph then text "[orphan]" else Outputable.empty)
              <+> ppr cls <+> brackets (pprWithCommas ppr_rough mb_tcs))
         2 (equals <+> ppr dfun_id)

instance Outputable IfaceFamInst where
  ppr (IfaceFamInst { ifFamInstFam = fam, ifFamInstTys = mb_tcs
                    , ifFamInstAxiom = tycon_ax, ifFamInstOrph = orph })
    = hang (text "family instance"
              <+> (if isOrphan orph then text "[orphan]" else Outputable.empty)
              <+> ppr fam <+> pprWithCommas (brackets . ppr_rough) mb_tcs)
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

-- | Pretty Print an IfaceExpr
--
-- The first argument should be a function that adds parens in context that need
-- an atomic value (e.g. function args)
pprIfaceExpr :: (SDoc -> SDoc) -> IfaceExpr -> SDoc

pprIfaceExpr _ (IfaceLcl v)       = ppr v
pprIfaceExpr _ (IfaceExt v)       = ppr v
pprIfaceExpr _ (IfaceLit l)       = ppr l
pprIfaceExpr _ (IfaceFCall cc ty) = braces (ppr cc <+> ppr ty)
pprIfaceExpr _ (IfaceType ty)     = char '@' <> pprParendIfaceType ty
pprIfaceExpr _ (IfaceCo co)       = text "@~" <> pprParendIfaceCoercion co
pprIfaceExpr _ (IfaceTuple c as)  = tupleParens c (pprWithCommas ppr as)

pprIfaceExpr _ (IfaceLitRubbish tc r)
  = text "RUBBISH"
    <> (case tc of { TypeLike -> empty; ConstraintLike -> text "[c]" })
    <> parens (ppr r)

pprIfaceExpr add_par app@(IfaceApp _ _) = add_par (pprIfaceApp app [])

pprIfaceExpr add_par i@(IfaceLam _ _)
  = add_par (sep [char '\\' <+> sep (map pprIfaceLamBndr bndrs) <+> arrow,
                  pprIfaceExpr noParens body])
  where
    (bndrs,body) = collect [] i
    collect bs (IfaceLam b e) = collect (b:bs) e
    collect bs e              = (reverse bs, e)

pprIfaceExpr add_par (IfaceECase scrut ty)
  = add_par (sep [ text "case" <+> pprIfaceExpr noParens scrut
                 , text "ret_ty" <+> pprParendIfaceType ty
                 , text "of {}" ])

pprIfaceExpr add_par (IfaceCase scrut bndr [IfaceAlt con bs rhs])
  = add_par (sep [text "case"
                        <+> pprIfaceExpr noParens scrut <+> text "of"
                        <+> ppr bndr <+> char '{' <+> ppr_con_bs con bs <+> arrow,
                  pprIfaceExpr noParens rhs <+> char '}'])

pprIfaceExpr add_par (IfaceCase scrut bndr alts)
  = add_par (sep [text "case"
                        <+> pprIfaceExpr noParens scrut <+> text "of"
                        <+> ppr bndr <+> char '{',
                  nest 2 (sep (map pprIfaceAlt alts)) <+> char '}'])

pprIfaceExpr _       (IfaceCast expr co)
  = sep [pprParendIfaceExpr expr,
         nest 2 (text "`cast`"),
         pprParendIfaceCoercion co]

pprIfaceExpr add_par (IfaceLet (IfaceNonRec b rhs) body)
  = add_par (sep [text "let {",
                  nest 2 (ppr_bind (b, rhs)),
                  text "} in",
                  pprIfaceExpr noParens body])

pprIfaceExpr add_par (IfaceLet (IfaceRec pairs) body)
  = add_par (sep [text "letrec {",
                  nest 2 (sep (map ppr_bind pairs)),
                  text "} in",
                  pprIfaceExpr noParens body])

pprIfaceExpr add_par (IfaceTick tickish e)
  = add_par (pprIfaceTickish tickish <+> pprIfaceExpr noParens e)

pprIfaceAlt :: IfaceAlt -> SDoc
pprIfaceAlt (IfaceAlt con bs rhs)
  = sep [ppr_con_bs con bs, arrow <+> pprIfaceExpr noParens rhs]

ppr_con_bs :: IfaceConAlt -> [IfLclName] -> SDoc
ppr_con_bs con bs = ppr con <+> hsep (map ppr bs)

ppr_bind :: (IfaceLetBndr, IfaceExpr) -> SDoc
ppr_bind (IfLetBndr b ty info ji, rhs)
  = sep [hang (ppr b <+> dcolon <+> ppr ty) 2 (ppr ji <+> ppr info),
         equals <+> pprIfaceExpr noParens rhs]

------------------
pprIfaceTickish :: IfaceTickish -> SDoc
pprIfaceTickish (IfaceHpcTick m ix)
  = braces (text "tick" <+> ppr m <+> ppr ix)
pprIfaceTickish (IfaceSCC cc tick scope)
  = braces (pprCostCentreCore cc <+> ppr tick <+> ppr scope)
pprIfaceTickish (IfaceSource src _names)
  = braces (pprUserRealSpan True src)
pprIfaceTickish (IfaceBreakpoint m ix fvs)
  = braces (text "break" <+> ppr m <+> ppr ix <+> ppr fvs)

------------------
pprIfaceApp :: IfaceExpr -> [SDoc] -> SDoc
pprIfaceApp (IfaceApp fun arg) args = pprIfaceApp fun $
                                          nest 2 (pprParendIfaceExpr arg) : args
pprIfaceApp fun                args = sep (pprParendIfaceExpr fun : args)

------------------
instance Outputable IfaceConAlt where
    ppr IfaceDefaultAlt   = text "DEFAULT"
    ppr (IfaceLitAlt l)   = ppr l
    ppr (IfaceDataAlt d)  = ppr d

------------------
instance Outputable IfaceIdDetails where
  ppr IfVanillaId       = Outputable.empty
  ppr (IfWorkerLikeId dmd) = text "StrWork" <> parens (ppr dmd)
  ppr (IfRecSelId tc _c b _fl) = text "RecSel" <+> ppr tc
                            <+> if b
                                  then text "<naughty>"
                                  else Outputable.empty
  ppr IfDFunId          = text "DFunId"

instance Outputable IfaceInfoItem where
  ppr (HsUnfold lb unf)     = text "Unfolding"
                              <> ppWhen lb (text "(loop-breaker)")
                              <> colon <+> ppr unf
  ppr (HsInline prag)       = text "Inline:" <+> ppr prag
  ppr (HsArity arity)       = text "Arity:" <+> int arity
  ppr (HsDmdSig str)        = text "Strictness:" <+> ppr str
  ppr (HsCprSig cpr)        = text "CPR:" <+> ppr cpr
  ppr HsNoCafRefs           = text "HasNoCafRefs"
  ppr (HsLFInfo lf_info)    = text "LambdaFormInfo:" <+> ppr lf_info
  ppr (HsTagSig tag_sig)    = text "TagSig:" <+> ppr tag_sig

instance Outputable IfaceUnfolding where
  ppr (IfCoreUnfold src _ guide e)
    = sep [ text "Core:" <+> ppr src <+> ppr guide, ppr e ]
  ppr (IfDFunUnfold bs es) = hang (text "DFun:" <+> sep (map ppr bs) <> dot)
                                2 (sep (map pprParendIfaceExpr es))

instance Outputable IfGuidance where
  ppr IfNoGuidance   = empty
  ppr (IfWhen a u b) = angleBrackets (ppr a <> comma <> ppr u <> ppr b)

{-
************************************************************************
*                                                                      *
              Finding the Names in Iface syntax
*                                                                      *
************************************************************************

This is used for dependency analysis in GHC.Iface.Make, so that we
fingerprint a declaration before the things that depend on it.  It
is specific to interface-file fingerprinting in the sense that we
don't collect *all* Names: for example, the DFun of an instance is
recorded textually rather than by its fingerprint when
fingerprinting the instance, so DFuns are not dependencies.
-}

freeNamesIfDecl :: IfaceDecl -> NameSet
freeNamesIfDecl (IfaceId { ifType = t, ifIdDetails = d, ifIdInfo = i})
  = freeNamesIfType t &&&
    freeNamesIfIdInfo i &&&
    freeNamesIfIdDetails d

freeNamesIfDecl (IfaceData { ifBinders = bndrs, ifResKind = res_k
                           , ifParent = p, ifCtxt = ctxt, ifCons = cons })
  = freeNamesIfVarBndrs bndrs &&&
    freeNamesIfType res_k &&&
    freeNamesIfaceTyConParent p &&&
    freeNamesIfContext ctxt &&&
    freeNamesIfConDecls cons

freeNamesIfDecl (IfaceSynonym { ifBinders = bndrs, ifResKind = res_k
                              , ifSynRhs = rhs })
  = freeNamesIfVarBndrs bndrs &&&
    freeNamesIfKind res_k &&&
    freeNamesIfType rhs

freeNamesIfDecl (IfaceFamily { ifBinders = bndrs, ifResKind = res_k
                             , ifFamFlav = flav })
  = freeNamesIfVarBndrs bndrs &&&
    freeNamesIfKind res_k &&&
    freeNamesIfFamFlav flav

freeNamesIfDecl (IfaceClass{ ifBinders = bndrs, ifBody = cls_body })
  = freeNamesIfVarBndrs bndrs &&&
    freeNamesIfClassBody cls_body

freeNamesIfDecl (IfaceAxiom { ifTyCon = tc, ifAxBranches = branches })
  = freeNamesIfTc tc &&&
    fnList freeNamesIfAxBranch branches

freeNamesIfDecl (IfacePatSyn { ifPatMatcher = (matcher, _)
                             , ifPatBuilder = mb_builder
                             , ifPatUnivBndrs = univ_bndrs
                             , ifPatExBndrs = ex_bndrs
                             , ifPatProvCtxt = prov_ctxt
                             , ifPatReqCtxt = req_ctxt
                             , ifPatArgs = args
                             , ifPatTy = pat_ty
                             , ifFieldLabels = lbls })
  = unitNameSet matcher &&&
    maybe emptyNameSet (unitNameSet . fst) mb_builder &&&
    freeNamesIfVarBndrs univ_bndrs &&&
    freeNamesIfVarBndrs ex_bndrs &&&
    freeNamesIfContext prov_ctxt &&&
    freeNamesIfContext req_ctxt &&&
    fnList freeNamesIfType args &&&
    freeNamesIfType pat_ty &&&
    mkNameSet (map flSelector lbls)

freeNamesIfClassBody :: IfaceClassBody -> NameSet
freeNamesIfClassBody IfAbstractClass
  = emptyNameSet
freeNamesIfClassBody (IfConcreteClass{ ifClassCtxt = ctxt, ifATs = ats, ifSigs = sigs })
  = freeNamesIfContext ctxt  &&&
    fnList freeNamesIfAT ats &&&
    fnList freeNamesIfClsSig sigs

freeNamesIfAxBranch :: IfaceAxBranch -> NameSet
freeNamesIfAxBranch (IfaceAxBranch { ifaxbTyVars   = tyvars
                                   , ifaxbCoVars   = covars
                                   , ifaxbLHS      = lhs
                                   , ifaxbRHS      = rhs })
  = fnList freeNamesIfTvBndr tyvars &&&
    fnList freeNamesIfIdBndr covars &&&
    freeNamesIfAppArgs lhs &&&
    freeNamesIfType rhs

freeNamesIfIdDetails :: IfaceIdDetails -> NameSet
freeNamesIfIdDetails (IfRecSelId tc first_con _ fl) =
  either freeNamesIfTc freeNamesIfDecl tc &&&
  unitFV first_con &&&
  unitFV (flSelector fl)
freeNamesIfIdDetails IfVanillaId         = emptyNameSet
freeNamesIfIdDetails (IfWorkerLikeId {}) = emptyNameSet
freeNamesIfIdDetails IfDFunId            = emptyNameSet

-- All other changes are handled via the version info on the tycon
freeNamesIfFamFlav :: IfaceFamTyConFlav -> NameSet
freeNamesIfFamFlav IfaceOpenSynFamilyTyCon             = emptyNameSet
freeNamesIfFamFlav IfaceDataFamilyTyCon                = emptyNameSet
freeNamesIfFamFlav (IfaceClosedSynFamilyTyCon (Just (ax, br)))
  = unitNameSet ax &&& fnList freeNamesIfAxBranch br
freeNamesIfFamFlav (IfaceClosedSynFamilyTyCon Nothing) = emptyNameSet
freeNamesIfFamFlav IfaceAbstractClosedSynFamilyTyCon   = emptyNameSet
freeNamesIfFamFlav IfaceBuiltInSynFamTyCon             = emptyNameSet

freeNamesIfContext :: IfaceContext -> NameSet
freeNamesIfContext = fnList freeNamesIfType

freeNamesIfAT :: IfaceAT -> NameSet
freeNamesIfAT (IfaceAT decl mb_def)
  = freeNamesIfDecl decl &&&
    case mb_def of
      Nothing  -> emptyNameSet
      Just rhs -> freeNamesIfType rhs

freeNamesIfClsSig :: IfaceClassOp -> NameSet
freeNamesIfClsSig (IfaceClassOp _n ty dm) = freeNamesIfType ty &&& freeNamesDM dm

freeNamesDM :: Maybe (DefMethSpec IfaceType) -> NameSet
freeNamesDM (Just (GenericDM ty)) = freeNamesIfType ty
freeNamesDM _                     = emptyNameSet

freeNamesIfConDecls :: IfaceConDecls -> NameSet
freeNamesIfConDecls (IfDataTyCon _ cs) = fnList freeNamesIfConDecl cs
freeNamesIfConDecls (IfNewTyCon    c)  = freeNamesIfConDecl c
freeNamesIfConDecls _                  = emptyNameSet

freeNamesIfConDecl :: IfaceConDecl -> NameSet
freeNamesIfConDecl (IfCon { ifConExTCvs  = ex_tvs, ifConCtxt = ctxt
                          , ifConArgTys  = arg_tys
                          , ifConFields  = flds
                          , ifConEqSpec  = eq_spec
                          , ifConStricts = bangs })
  = fnList freeNamesIfBndr ex_tvs &&&
    freeNamesIfContext ctxt &&&
    fnList freeNamesIfType (map fst arg_tys) &&& -- these are multiplicities, represented as types
    fnList freeNamesIfType (map snd arg_tys) &&&
    mkNameSet (map flSelector flds) &&&
    fnList freeNamesIfType (map snd eq_spec) &&& -- equality constraints
    fnList freeNamesIfBang bangs

freeNamesIfBang :: IfaceBang -> NameSet
freeNamesIfBang (IfUnpackCo co) = freeNamesIfCoercion co
freeNamesIfBang _               = emptyNameSet

freeNamesIfKind :: IfaceType -> NameSet
freeNamesIfKind = freeNamesIfType

freeNamesIfAppArgs :: IfaceAppArgs -> NameSet
freeNamesIfAppArgs (IA_Arg t _ ts) = freeNamesIfType t &&& freeNamesIfAppArgs ts
freeNamesIfAppArgs IA_Nil          = emptyNameSet

freeNamesIfType :: IfaceType -> NameSet
freeNamesIfType (IfaceFreeTyVar _)    = emptyNameSet
freeNamesIfType (IfaceTyVar _)        = emptyNameSet
freeNamesIfType (IfaceAppTy s t)      = freeNamesIfType s &&& freeNamesIfAppArgs t
freeNamesIfType (IfaceTyConApp tc ts) = freeNamesIfTc tc &&& freeNamesIfAppArgs ts
freeNamesIfType (IfaceTupleTy _ _ ts) = freeNamesIfAppArgs ts
freeNamesIfType (IfaceLitTy _)        = emptyNameSet
freeNamesIfType (IfaceForAllTy tv t)  = freeNamesIfVarBndr tv &&& freeNamesIfType t
freeNamesIfType (IfaceFunTy _ w s t)  = freeNamesIfType s &&& freeNamesIfType t &&& freeNamesIfType w
freeNamesIfType (IfaceCastTy t c)     = freeNamesIfType t &&& freeNamesIfCoercion c
freeNamesIfType (IfaceCoercionTy c)   = freeNamesIfCoercion c

freeNamesIfMCoercion :: IfaceMCoercion -> NameSet
freeNamesIfMCoercion IfaceMRefl    = emptyNameSet
freeNamesIfMCoercion (IfaceMCo co) = freeNamesIfCoercion co

freeNamesIfCoercion :: IfaceCoercion -> NameSet
freeNamesIfCoercion (IfaceReflCo t) = freeNamesIfType t
freeNamesIfCoercion (IfaceGReflCo _ t mco)
  = freeNamesIfType t &&& freeNamesIfMCoercion mco
freeNamesIfCoercion (IfaceFunCo _ c_mult c1 c2)
  = freeNamesIfCoercion c_mult &&& freeNamesIfCoercion c1 &&& freeNamesIfCoercion c2
freeNamesIfCoercion (IfaceTyConAppCo _ tc cos)
  = freeNamesIfTc tc &&& fnList freeNamesIfCoercion cos
freeNamesIfCoercion (IfaceAppCo c1 c2)
  = freeNamesIfCoercion c1 &&& freeNamesIfCoercion c2
freeNamesIfCoercion (IfaceForAllCo _tcv _visL _visR kind_co co)
  = freeNamesIfCoercion kind_co &&& freeNamesIfCoercion co
freeNamesIfCoercion (IfaceFreeCoVar _) = emptyNameSet
freeNamesIfCoercion (IfaceCoVarCo _)   = emptyNameSet
freeNamesIfCoercion (IfaceHoleCo _)    = emptyNameSet
freeNamesIfCoercion (IfaceUnivCo _ _ t1 t2 cos)
  = freeNamesIfType t1 &&& freeNamesIfType t2 &&& fnList freeNamesIfCoercion cos
freeNamesIfCoercion (IfaceSymCo c)
  = freeNamesIfCoercion c
freeNamesIfCoercion (IfaceTransCo c1 c2)
  = freeNamesIfCoercion c1 &&& freeNamesIfCoercion c2
freeNamesIfCoercion (IfaceSelCo _ co)
  = freeNamesIfCoercion co
freeNamesIfCoercion (IfaceLRCo _ co)
  = freeNamesIfCoercion co
freeNamesIfCoercion (IfaceInstCo co co2)
  = freeNamesIfCoercion co &&& freeNamesIfCoercion co2
freeNamesIfCoercion (IfaceKindCo c)
  = freeNamesIfCoercion c
freeNamesIfCoercion (IfaceSubCo co)
  = freeNamesIfCoercion co
freeNamesIfCoercion (IfaceAxiomCo ax cos)
  = fnAxRule ax &&& fnList freeNamesIfCoercion cos

fnAxRule :: IfaceAxiomRule -> NameSet
fnAxRule (IfaceAR_X _)   = emptyNameSet -- the axiom is just a string, so we don't count it as a name.
fnAxRule (IfaceAR_U n)   = unitNameSet n
fnAxRule (IfaceAR_B n _) = unitNameSet n

freeNamesIfVarBndr :: VarBndr IfaceBndr vis -> NameSet
freeNamesIfVarBndr (Bndr bndr _) = freeNamesIfBndr bndr

freeNamesIfVarBndrs :: [VarBndr IfaceBndr vis] -> NameSet
freeNamesIfVarBndrs = fnList freeNamesIfVarBndr

freeNamesIfBndr :: IfaceBndr -> NameSet
freeNamesIfBndr (IfaceIdBndr b) = freeNamesIfIdBndr b
freeNamesIfBndr (IfaceTvBndr b) = freeNamesIfTvBndr b

freeNamesIfBndrs :: [IfaceBndr] -> NameSet
freeNamesIfBndrs = fnList freeNamesIfBndr

freeNamesIfLetBndr :: IfaceLetBndr -> NameSet
-- Remember IfaceLetBndr is used only for *nested* bindings
-- The IdInfo can contain an unfolding (in the case of
-- local INLINE pragmas), so look there too
freeNamesIfLetBndr (IfLetBndr _name ty info _ji) = freeNamesIfType ty
                                                 &&& freeNamesIfIdInfo info

freeNamesIfTvBndr :: IfaceTvBndr -> NameSet
freeNamesIfTvBndr (_fs,k) = freeNamesIfKind k
    -- kinds can have Names inside, because of promotion

freeNamesIfIdBndr :: IfaceIdBndr -> NameSet
freeNamesIfIdBndr (_, _fs,k) = freeNamesIfKind k

freeNamesIfIdInfo :: IfaceIdInfo -> NameSet
freeNamesIfIdInfo = fnList freeNamesItem

freeNamesItem :: IfaceInfoItem -> NameSet
freeNamesItem (HsUnfold _ u)         = freeNamesIfUnfold u
freeNamesItem (HsLFInfo (IfLFCon n)) = unitNameSet n
freeNamesItem _                      = emptyNameSet

freeNamesIfUnfold :: IfaceUnfolding -> NameSet
freeNamesIfUnfold (IfCoreUnfold _ _ _ e) = freeNamesIfExpr e
freeNamesIfUnfold (IfDFunUnfold bs es)   = freeNamesIfBndrs bs &&& fnList freeNamesIfExpr es

freeNamesIfExpr :: IfaceExpr -> NameSet
freeNamesIfExpr (IfaceExt v)          = unitNameSet v
freeNamesIfExpr (IfaceFCall _ ty)     = freeNamesIfType ty
freeNamesIfExpr (IfaceType ty)        = freeNamesIfType ty
freeNamesIfExpr (IfaceCo co)          = freeNamesIfCoercion co
freeNamesIfExpr (IfaceTuple _ as)     = fnList freeNamesIfExpr as
freeNamesIfExpr (IfaceLam (b,_) body) = freeNamesIfBndr b &&& freeNamesIfExpr body
freeNamesIfExpr (IfaceApp f a)        = freeNamesIfExpr f &&& freeNamesIfExpr a
freeNamesIfExpr (IfaceCast e co)      = freeNamesIfExpr e &&& freeNamesIfCoercion co
freeNamesIfExpr (IfaceTick t e)       = freeNamesIfTickish t &&& freeNamesIfExpr e
freeNamesIfExpr (IfaceECase e ty)     = freeNamesIfExpr e &&& freeNamesIfType ty
freeNamesIfExpr (IfaceCase s _ alts)
  = freeNamesIfExpr s &&& fnList fn_alt alts &&& fn_cons alts
  where
    fn_alt (IfaceAlt _con _bs r) = freeNamesIfExpr r

    -- Depend on the data constructors.  Just one will do!
    -- Note [Tracking data constructors]
    fn_cons []                                     = emptyNameSet
    fn_cons (IfaceAlt IfaceDefaultAlt    _ _ : xs) = fn_cons xs
    fn_cons (IfaceAlt (IfaceDataAlt con) _ _ : _ ) = unitNameSet con
    fn_cons (_                               : _ ) = emptyNameSet

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
  = unitNameSet ax &&& freeNamesIfTc tc &&& freeNamesIfAppArgs tys

freeNamesIfTickish :: IfaceTickish -> NameSet
freeNamesIfTickish (IfaceBreakpoint _ fvs _) =
  fnList freeNamesIfExpr fvs
freeNamesIfTickish _ = emptyNameSet

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
     import GHC.Driver.DynFlags
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

Note that there is a bit of subtlety here when we encode names. While
IfaceTopBndrs is really just a synonym for Name, we need to take care to
encode them with {get,put}IfaceTopBndr. The difference becomes important when
we go to fingerprint an IfaceDecl. See Note [Fingerprinting IfaceDecls] for
details.

-}

instance Binary IfaceDecl where
    put_ bh (IfaceId name ty details idinfo) = do
        putByte bh 0
        putIfaceTopBndr bh name
        lazyPut bh (ty, details, idinfo)
        -- See Note [Lazy deserialization of IfaceId]

    put_ bh (IfaceData a1 a2 a3 a4 a5 a6 a7 a8 a9) = do
        putByte bh 2
        putIfaceTopBndr bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        put_ bh a8
        put_ bh a9

    put_ bh (IfaceSynonym a1 a2 a3 a4 a5) = do
        putByte bh 3
        putIfaceTopBndr bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5

    put_ bh (IfaceFamily a1 a2 a3 a4 a5 a6) = do
        putByte bh 4
        putIfaceTopBndr bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6

    -- NB: Written in a funny way to avoid an interface change
    put_ bh (IfaceClass {
                ifName    = a2,
                ifRoles   = a3,
                ifBinders = a4,
                ifFDs     = a5,
                ifBody = IfConcreteClass {
                    ifClassCtxt = a1,
                    ifATs  = a6,
                    ifSigs = a7,
                    ifMinDef  = a8
                }}) = do
        putByte bh 5
        put_ bh a1
        putIfaceTopBndr bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        put_ bh a8

    put_ bh (IfaceAxiom a1 a2 a3 a4) = do
        putByte bh 6
        putIfaceTopBndr bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4

    put_ bh (IfacePatSyn a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11) = do
        putByte bh 7
        putIfaceTopBndr bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        put_ bh a8
        put_ bh a9
        put_ bh a10
        put_ bh a11

    put_ bh (IfaceClass {
                ifName    = a1,
                ifRoles   = a2,
                ifBinders = a3,
                ifFDs     = a4,
                ifBody = IfAbstractClass }) = do
        putByte bh 8
        putIfaceTopBndr bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4

    get bh = do
        h <- getByte bh
        case h of
            0 -> do name <- get bh
                    ~(ty, details, idinfo) <- lazyGet bh
                    -- See Note [Lazy deserialization of IfaceId]
                    return (IfaceId name ty details idinfo)
            1 -> error "Binary.get(TyClDecl): ForeignType"
            2 -> do a1  <- getIfaceTopBndr bh
                    a2  <- get bh
                    a3  <- get bh
                    a4  <- get bh
                    a5  <- get bh
                    a6  <- get bh
                    a7  <- get bh
                    a8  <- get bh
                    a9  <- get bh
                    return (IfaceData a1 a2 a3 a4 a5 a6 a7 a8 a9)
            3 -> do a1 <- getIfaceTopBndr bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    a5 <- get bh
                    return (IfaceSynonym a1 a2 a3 a4 a5)
            4 -> do a1 <- getIfaceTopBndr bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    a5 <- get bh
                    a6 <- get bh
                    return (IfaceFamily a1 a2 a3 a4 a5 a6)
            5 -> do a1 <- get bh
                    a2 <- getIfaceTopBndr bh
                    a3 <- get bh
                    a4 <- get bh
                    a5 <- get bh
                    a6 <- get bh
                    a7 <- get bh
                    a8 <- get bh
                    return (IfaceClass {
                        ifName    = a2,
                        ifRoles   = a3,
                        ifBinders = a4,
                        ifFDs     = a5,
                        ifBody = IfConcreteClass {
                            ifClassCtxt = a1,
                            ifATs  = a6,
                            ifSigs = a7,
                            ifMinDef  = a8
                        }})
            6 -> do a1 <- getIfaceTopBndr bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    return (IfaceAxiom a1 a2 a3 a4)
            7 -> do a1 <- getIfaceTopBndr bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    a5 <- get bh
                    a6 <- get bh
                    a7 <- get bh
                    a8 <- get bh
                    a9 <- get bh
                    a10 <- get bh
                    a11 <- get bh
                    return (IfacePatSyn a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
            8 -> do a1 <- getIfaceTopBndr bh
                    a2 <- get bh
                    a3 <- get bh
                    a4 <- get bh
                    return (IfaceClass {
                        ifName    = a1,
                        ifRoles   = a2,
                        ifBinders = a3,
                        ifFDs     = a4,
                        ifBody = IfAbstractClass })
            _ -> panic (unwords ["Unknown IfaceDecl tag:", show h])

instance Binary IfaceBooleanFormula where
    put_ bh = \case
        IfVar a1    -> putByte bh 0 >> put_ bh a1
        IfAnd a1    -> putByte bh 1 >> put_ bh a1
        IfOr a1     -> putByte bh 2 >> put_ bh a1
        IfParens a1 -> putByte bh 3 >> put_ bh a1

    get bh = do
        getByte bh >>= \case
            0 -> IfVar    <$> get bh
            1 -> IfAnd    <$> get bh
            2 -> IfOr     <$> get bh
            _ -> IfParens <$> get bh

{- Note [Lazy deserialization of IfaceId]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The use of lazyPut and lazyGet in the IfaceId Binary instance is
purely for performance reasons, to avoid deserializing details about
identifiers that will never be used. It's not involved in tying the
knot in the type checker. It saved ~1% of the total build time of GHC.

When we read an interface file, we extend the PTE, a mapping of Names
to TyThings, with the declarations we have read. The extension of the
PTE is strict in the Names, but not in the TyThings themselves.
GHC.IfaceToCore.tcIfaceDecls calculates the list of (Name, TyThing) bindings
to add to the PTE.  For an IfaceId, there's just one binding to add; and
the ty, details, and idinfo fields of an IfaceId are used only in the
TyThing. So by reading those fields lazily we may be able to save the
work of ever having to deserialize them (into IfaceType, etc.).

For IfaceData and IfaceClass, tcIfaceDecls creates extra implicit bindings
(the constructors and field selectors of the data declaration, or the
methods of the class), whose Names depend on more than just the Name
of the type constructor or class itself. So deserializing them lazily
would be more involved. Similar comments apply to the other
constructors of IfaceDecl with the additional point that they probably
represent a small proportion of all declarations.
-}

instance Binary IfaceFamTyConFlav where
    put_ bh IfaceDataFamilyTyCon              = putByte bh 0
    put_ bh IfaceOpenSynFamilyTyCon           = putByte bh 1
    put_ bh (IfaceClosedSynFamilyTyCon mb)    = putByte bh 2 >> put_ bh mb
    put_ bh IfaceAbstractClosedSynFamilyTyCon = putByte bh 3
    put_ _ IfaceBuiltInSynFamTyCon
        = pprPanic "Cannot serialize IfaceBuiltInSynFamTyCon, used for pretty-printing only" Outputable.empty

    get bh = do { h <- getByte bh
                ; case h of
                    0 -> return IfaceDataFamilyTyCon
                    1 -> return IfaceOpenSynFamilyTyCon
                    2 -> do { mb <- get bh
                            ; return (IfaceClosedSynFamilyTyCon mb) }
                    3 -> return IfaceAbstractClosedSynFamilyTyCon
                    _ -> pprPanic "Binary.get(IfaceFamTyConFlav): Invalid tag"
                                  (ppr (fromIntegral h :: Int)) }

instance Binary IfaceClassOp where
    put_ bh (IfaceClassOp n ty def) = do
        putIfaceTopBndr bh n
        put_ bh ty
        put_ bh def
    get bh = do
        n   <- getIfaceTopBndr bh
        ty  <- get bh
        def <- get bh
        return (IfaceClassOp n ty def)

instance Binary IfaceAT where
    put_ bh (IfaceAT dec defs) = do
        put_ bh dec
        put_ bh defs
    get bh = do
        dec  <- get bh
        defs <- get bh
        return (IfaceAT dec defs)

instance Binary IfaceAxBranch where
    put_ bh (IfaceAxBranch a1 a2 a3 a4 a5 a6 a7) = do
        put_ bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
    get bh = do
        a1 <- get bh
        a2 <- get bh
        a3 <- get bh
        a4 <- get bh
        a5 <- get bh
        a6 <- get bh
        a7 <- get bh
        return (IfaceAxBranch a1 a2 a3 a4 a5 a6 a7)

instance Binary IfaceConDecls where
    put_ bh IfAbstractTyCon  = putByte bh 0
    put_ bh (IfDataTyCon False cs) = putByte bh 1 >> put_ bh cs
    put_ bh (IfDataTyCon True cs) = putByte bh 2 >> put_ bh cs
    put_ bh (IfNewTyCon c)   = putByte bh 3 >> put_ bh c
    get bh = do
        h <- getByte bh
        case h of
            0 -> return IfAbstractTyCon
            1 -> liftM (IfDataTyCon False) (get bh)
            2 -> liftM (IfDataTyCon True) (get bh)
            3 -> liftM IfNewTyCon (get bh)
            _ -> error "Binary(IfaceConDecls).get: Invalid IfaceConDecls"

instance Binary IfaceConDecl where
    put_ bh (IfCon a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11) = do
        putIfaceTopBndr bh a1
        put_ bh a2
        put_ bh a3
        put_ bh a4
        put_ bh a5
        put_ bh a6
        put_ bh a7
        put_ bh a8
        put_ bh (length a9)
        mapM_ (put_ bh) a9
        put_ bh a10
        put_ bh a11
    get bh = do
        a1 <- getIfaceTopBndr bh
        a2 <- get bh
        a3 <- get bh
        a4 <- get bh
        a5 <- get bh
        a6 <- get bh
        a7 <- get bh
        a8 <- get bh
        n_fields <- get bh
        a9 <- replicateM n_fields (get bh)
        a10 <- get bh
        a11 <- get bh
        return (IfCon a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)

instance Binary IfaceBang where
    put_ bh IfNoBang        = putByte bh 0
    put_ bh IfStrict        = putByte bh 1
    put_ bh IfUnpack        = putByte bh 2
    put_ bh (IfUnpackCo co) = putByte bh 3 >> put_ bh co

    get bh = do
            h <- getByte bh
            case h of
              0 -> return IfNoBang
              1 -> return IfStrict
              2 -> return IfUnpack
              _ -> IfUnpackCo <$> get bh

instance Binary IfaceSrcBang where
    put_ bh (IfSrcBang a1 a2) =
      do put_ bh a1
         put_ bh a2

    get bh =
      do a1 <- get bh
         a2 <- get bh
         return (IfSrcBang a1 a2)

instance Binary IfaceDefault where
    put_ bh (IfaceDefault cls tys warn) = do
        put_ bh cls
        put_ bh tys
        put_ bh warn
    get bh = do
        cls  <- get bh
        tys  <- get bh
        warn <- get bh
        return (IfaceDefault cls tys warn)

instance Binary IfaceClsInst where
    put_ bh (IfaceClsInst cls tys dfun flag orph warn) = do
        put_ bh cls
        put_ bh tys
        put_ bh dfun
        put_ bh flag
        put_ bh orph
        put_ bh warn
    get bh = do
        cls  <- get bh
        tys  <- get bh
        dfun <- get bh
        flag <- get bh
        orph <- get bh
        warn <- get bh
        return (IfaceClsInst cls tys dfun flag orph warn)

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

instance Binary IfaceWarnings where
    put_ bh = \case
        IfWarnAll txt  -> putByte bh 0 *> put_ bh txt
        IfWarnSome vs ds -> putByte bh 1 *> put_ bh vs *> put_ bh ds
    get bh = getByte bh >>= \case
        0 -> pure IfWarnAll    <*> get bh
        1 -> pure IfWarnSome   <*> get bh <*> get bh
        _ -> fail "invalid tag(IfaceWarnings)"

instance Binary IfaceWarningTxt where
    put_ bh = \case
        IfWarningTxt a1 a2 a3 -> putByte bh 0 *> put_ bh a1 *> put_ bh a2 *> put_ bh a3
        IfDeprecatedTxt a1 a2 -> putByte bh 1 *> put_ bh a1 *> put_ bh a2
    get bh = getByte bh >>= \case
        0 -> pure IfWarningTxt    <*> get bh <*> get bh <*> get bh
        _ -> pure IfDeprecatedTxt <*> get bh <*> get bh

instance Binary IfaceStringLiteral where
    put_ bh (IfStringLiteral a1 a2) = put_ bh a1 *> put_ bh a2
    get bh = IfStringLiteral <$> get bh <*> get bh

instance Binary IfaceAnnotation where
    put_ bh (IfaceAnnotation a1 a2) = do
        put_ bh a1
        put_ bh a2
    get bh = do
        a1 <- get bh
        a2 <- get bh
        return (IfaceAnnotation a1 a2)

instance Binary IfaceIdDetails where
    put_ bh IfVanillaId           = putByte bh 0
    put_ bh (IfRecSelId a b c d)  = do { putByte bh 1
                                       ; put_ bh a
                                       ; put_ bh b
                                       ; put_ bh c
                                       ; put_ bh d }
    put_ bh (IfWorkerLikeId dmds) = putByte bh 2 >> put_ bh dmds
    put_ bh IfDFunId              = putByte bh 3
    get bh = do
        h <- getByte bh
        case h of
            0 -> return IfVanillaId
            1 -> do { a <- get bh
                    ; b <- get bh
                    ; c <- get bh
                    ; d <- get bh
                    ; return (IfRecSelId a b c d) }
            2 -> do { dmds <- get bh
                    ; return (IfWorkerLikeId dmds) }
            _ -> return IfDFunId

instance Binary IfaceInfoItem where
    put_ bh (HsArity aa)          = putByte bh 0 >> put_ bh aa
    put_ bh (HsDmdSig ab)         = putByte bh 1 >> put_ bh ab
    put_ bh (HsUnfold lb ad)      = putByte bh 2 >> put_ bh lb >> put_ bh ad
    put_ bh (HsInline ad)         = putByte bh 3 >> put_ bh ad
    put_ bh HsNoCafRefs           = putByte bh 4
    put_ bh (HsCprSig cpr)        = putByte bh 6 >> put_ bh cpr
    put_ bh (HsLFInfo lf_info)    = putByte bh 7 >> put_ bh lf_info
    put_ bh (HsTagSig sig)        = putByte bh 8 >> put_ bh sig

    get bh = do
        h <- getByte bh
        case h of
            0 -> liftM HsArity $ get bh
            1 -> liftM HsDmdSig $ get bh
            2 -> do lb <- get bh
                    ad <- get bh
                    return (HsUnfold lb ad)
            3 -> liftM HsInline $ get bh
            4 -> return HsNoCafRefs
            6 -> HsCprSig <$> get bh
            7 -> HsLFInfo <$> get bh
            _ -> HsTagSig <$> get bh

instance Binary IfaceUnfolding where
    put_ bh (IfCoreUnfold s c g e) = do
        putByte bh 0
        put_ bh s
        putUnfoldingCache bh c
        put_ bh g
        put_ bh e
    put_ bh (IfDFunUnfold as bs) = do
        putByte bh 1
        put_ bh as
        put_ bh bs
    get bh = do
        h <- getByte bh
        case h of
            0 -> do s <- get bh
                    c <- getUnfoldingCache bh
                    g <- get bh
                    e <- get bh
                    return (IfCoreUnfold s c g e)
            _ -> do as <- get bh
                    bs <- get bh
                    return (IfDFunUnfold as bs)

instance Binary IfGuidance where
    put_ bh IfNoGuidance = putByte bh 0
    put_ bh (IfWhen a b c ) = do
        putByte bh 1
        put_ bh a
        put_ bh b
        put_ bh c
    get bh = do
        h <- getByte bh
        case h of
            0 -> return IfNoGuidance
            _ -> do a <- get bh
                    b <- get bh
                    c <- get bh
                    return (IfWhen a b c)

putUnfoldingCache :: WriteBinHandle -> IfUnfoldingCache -> IO ()
putUnfoldingCache bh (UnfoldingCache { uf_is_value = hnf, uf_is_conlike = conlike
                                     , uf_is_work_free = wf, uf_expandable = exp }) = do
    let b = zeroBits .<<|. hnf .<<|. conlike .<<|. wf .<<|. exp
    putByte bh b

getUnfoldingCache :: ReadBinHandle -> IO IfUnfoldingCache
getUnfoldingCache bh = do
    b <- getByte bh
    let hnf     = testBit b 3
        conlike = testBit b 2
        wf      = testBit b 1
        exp     = testBit b 0
    return (UnfoldingCache { uf_is_value = hnf, uf_is_conlike = conlike
                           , uf_is_work_free = wf, uf_expandable = exp })

infixl 9 .<<|.
(.<<|.) :: (Num a, Bits a) => a -> Bool -> a
x .<<|. b = (if b then (`setBit` 0) else id) (x `shiftL` 1)
{-# INLINE (.<<|.) #-}

instance Binary IfaceAlt where
    put_ bh (IfaceAlt a b c) = do
        put_ bh a
        put_ bh b
        put_ bh c
    get bh = do
        a <- get bh
        b <- get bh
        c <- get bh
        return (IfaceAlt a b c)

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
    put_ bh (IfaceLitRubbish TypeLike r) = do
        putByte bh 14
        put_ bh r
    put_ bh (IfaceLitRubbish ConstraintLike r) = do
        putByte bh 15
        put_ bh r
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
            14 -> do r <- get bh
                     return (IfaceLitRubbish TypeLike r)
            15 -> do r <- get bh
                     return (IfaceLitRubbish ConstraintLike r)
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
    put_ bh (IfaceBreakpoint m ix fvs) = do
        putByte bh 3
        put_ bh m
        put_ bh ix
        put_ bh fvs

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
            3 -> do m <- get bh
                    ix <- get bh
                    fvs <- get bh
                    return (IfaceBreakpoint m ix fvs)
            _ -> panic ("get IfaceTickish " ++ show h)

instance Binary IfaceConAlt where
    put_ bh IfaceDefaultAlt   = putByte bh 0
    put_ bh (IfaceDataAlt aa) = putByte bh 1 >> put_ bh aa
    put_ bh (IfaceLitAlt ac)  = putByte bh 2 >> put_ bh ac
    get bh = do
        h <- getByte bh
        case h of
            0 -> return IfaceDefaultAlt
            1 -> liftM IfaceDataAlt $ get bh
            _ -> liftM IfaceLitAlt  $ get bh

instance (Binary r, Binary b) => Binary (IfaceBindingX b r) where
    put_ bh (IfaceNonRec aa ab) = putByte bh 0 >> put_ bh aa >> put_ bh ab
    put_ bh (IfaceRec ac)       = putByte bh 1 >> put_ bh ac
    get bh = do
        h <- getByte bh
        case h of
            0 -> do { aa <- get bh; ab <- get bh; return (IfaceNonRec aa ab) }
            _ -> do { ac <- get bh; return (IfaceRec ac) }

instance Binary IfaceLetBndr where
    put_ bh (IfLetBndr a b c d) = do
            put_ bh a
            put_ bh b
            put_ bh c
            put_ bh d
    get bh = do a <- get bh
                b <- get bh
                c <- get bh
                d <- get bh
                return (IfLetBndr a b c d)

instance Binary IfaceTopBndrInfo where
    put_ bh (IfLclTopBndr lcl ty info dets) = do
            putByte bh 0
            put_ bh lcl
            put_ bh ty
            put_ bh info
            put_ bh dets
    put_ bh (IfGblTopBndr gbl) = do
            putByte bh 1
            put_ bh gbl
    get bh = do
      tag <- getByte bh
      case tag of
        0 -> IfLclTopBndr <$> get bh <*> get bh <*> get bh <*> get bh
        1 -> IfGblTopBndr <$> get bh
        _ -> pprPanic "IfaceTopBndrInfo" (intWithCommas tag)

instance Binary IfaceMaybeRhs where
  put_ bh IfUseUnfoldingRhs = putByte bh 0
  put_ bh (IfRhs e) = do
    putByte bh 1
    put_ bh e

  get bh = do
    b <- getByte bh
    case b of
      0 -> return IfUseUnfoldingRhs
      1 -> IfRhs <$> get bh
      _ -> pprPanic "IfaceMaybeRhs" (intWithCommas b)

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

instance Binary IfaceCompleteMatch where
  put_ bh (IfaceCompleteMatch cs mtc) = put_ bh cs >> put_ bh mtc
  get bh = IfaceCompleteMatch <$> get bh <*> get bh


{-
************************************************************************
*                                                                      *
                NFData instances
   See Note [Avoiding space leaks in toIface*] in GHC.CoreToIface
*                                                                      *
************************************************************************
-}

instance NFData IfaceImport where
  rnf (IfaceImport a b) = rnf a `seq` rnf b

instance NFData ImpIfaceList where
  rnf ImpIfaceAll = ()
  rnf (ImpIfaceEverythingBut ns) = rnf ns
  rnf (ImpIfaceExplicit gre) = rnf gre

instance NFData IfaceDecl where
  rnf = \case
    IfaceId f1 f2 f3 f4 ->
      rnf f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4

    IfaceData f1 f2 f3 f4 f5 f6 f7 f8 f9 ->
      f1 `seq` seqList f2 `seq` f3 `seq` f4 `seq` f5 `seq`
      rnf f6 `seq` rnf f7 `seq` rnf f8 `seq` rnf f9

    IfaceSynonym f1 f2 f3 f4 f5 ->
      rnf f1 `seq` f2 `seq` seqList f3 `seq` rnf f4 `seq` rnf f5

    IfaceFamily f1 f2 f3 f4 f5 f6 ->
      rnf f1 `seq` rnf f2 `seq` seqList f3 `seq` rnf f4 `seq` rnf f5 `seq` f6 `seq` ()

    IfaceClass f1 f2 f3 f4 f5 ->
      rnf f1 `seq` f2 `seq` seqList f3 `seq` rnf f4 `seq` rnf f5

    IfaceAxiom nm tycon role ax ->
      rnf nm `seq`
      rnf tycon `seq`
      role `seq`
      rnf ax

    IfacePatSyn f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 ->
      rnf f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4 `seq` f5 `seq` f6 `seq`
      rnf f7 `seq` rnf f8 `seq` rnf f9 `seq` rnf f10 `seq` f11 `seq` ()

instance NFData IfaceAxBranch where
  rnf (IfaceAxBranch f1 f2 f3 f4 f5 f6 f7) =
    rnf f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4 `seq` f5 `seq` rnf f6 `seq` rnf f7

instance NFData IfaceClassBody where
  rnf = \case
    IfAbstractClass -> ()
    IfConcreteClass f1 f2 f3 f4 -> rnf f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4 `seq` ()

instance NFData IfaceBooleanFormula where
  rnf = \case
      IfVar f1    -> rnf f1
      IfAnd f1    -> rnf f1
      IfOr f1     -> rnf f1
      IfParens f1 -> rnf f1

instance NFData IfaceAT where
  rnf (IfaceAT f1 f2) = rnf f1 `seq` rnf f2

instance NFData IfaceClassOp where
  rnf (IfaceClassOp f1 f2 f3) = rnf f1 `seq` rnf f2 `seq` f3 `seq` ()

instance NFData IfaceTyConParent where
  rnf = \case
    IfNoParent -> ()
    IfDataInstance f1 f2 f3 -> rnf f1 `seq` rnf f2 `seq` rnf f3

instance NFData IfaceConDecls where
  rnf = \case
    IfAbstractTyCon -> ()
    IfDataTyCon _ f1 -> rnf f1
    IfNewTyCon f1 -> rnf f1

instance NFData IfaceConDecl where
  rnf (IfCon f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11) =
    rnf f1 `seq` rnf f2 `seq` rnf f3 `seq` rnf f4 `seq` f5 `seq` rnf f6 `seq`
    rnf f7 `seq` rnf f8 `seq` f9 `seq` rnf f10 `seq` rnf f11

instance NFData IfaceSrcBang where
  rnf (IfSrcBang f1 f2) = f1 `seq` f2 `seq` ()

instance NFData IfaceBang where
  rnf x = x `seq` ()

instance NFData IfaceIdDetails where
  rnf = \case
    IfVanillaId -> ()
    IfWorkerLikeId dmds -> dmds `seqList` ()
    IfRecSelId (Left tycon) b c d -> rnf tycon `seq` rnf b `seq` rnf c `seq` rnf d
    IfRecSelId (Right decl) b c d -> rnf decl `seq` rnf b `seq` rnf c `seq` rnf d
    IfDFunId -> ()

instance NFData IfaceInfoItem where
  rnf = \case
    HsArity a -> rnf a
    HsDmdSig str -> seqDmdSig str
    HsInline p -> p `seq` () -- TODO: seq further?
    HsUnfold b unf -> rnf b `seq` rnf unf
    HsNoCafRefs -> ()
    HsCprSig cpr -> cpr `seq` ()
    HsLFInfo lf_info -> lf_info `seq` () -- TODO: seq further?
    HsTagSig sig -> sig `seq` ()

instance NFData IfGuidance where
  rnf = \case
    IfNoGuidance -> ()
    IfWhen a b c -> a `seq` b `seq` c `seq` ()

instance NFData IfaceUnfolding where
  rnf = \case
    IfCoreUnfold src cache guidance expr -> src `seq` cache `seq` rnf guidance `seq` rnf expr
    IfDFunUnfold bndrs exprs             -> rnf bndrs `seq` rnf exprs
    -- See Note [UnfoldingCache] in GHC.Core for why it suffices to merely `seq` on cache

instance NFData IfaceExpr where
  rnf = \case
    IfaceLcl nm -> rnf nm
    IfaceExt nm -> rnf nm
    IfaceType ty -> rnf ty
    IfaceCo co -> rnf co
    IfaceTuple sort exprs -> sort `seq` rnf exprs
    IfaceLam bndr expr -> rnf bndr `seq` rnf expr
    IfaceApp e1 e2 -> rnf e1 `seq` rnf e2
    IfaceCase e nm alts -> rnf e `seq` nm `seq` rnf alts
    IfaceECase e ty -> rnf e `seq` rnf ty
    IfaceLet bind e -> rnf bind `seq` rnf e
    IfaceCast e co -> rnf e `seq` rnf co
    IfaceLit l -> l `seq` () -- FIXME
    IfaceLitRubbish tc r -> tc `seq` rnf r `seq` ()
    IfaceFCall fc ty -> fc `seq` rnf ty
    IfaceTick tick e -> rnf tick `seq` rnf e

instance NFData IfaceAlt where
  rnf (IfaceAlt con bndrs rhs) = rnf con `seq` rnf bndrs `seq` rnf rhs

instance (NFData b, NFData a) => NFData (IfaceBindingX a b) where
  rnf = \case
    IfaceNonRec bndr e -> rnf bndr `seq` rnf e
    IfaceRec binds -> rnf binds

instance NFData IfaceTopBndrInfo where
  rnf (IfGblTopBndr n) = n `seq` ()
  rnf (IfLclTopBndr fs ty info dets) = rnf fs `seq` rnf ty `seq` rnf info `seq` rnf dets `seq` ()

instance NFData IfaceMaybeRhs where
  rnf IfUseUnfoldingRhs = ()
  rnf (IfRhs ce) = rnf ce `seq` ()

instance NFData IfaceLetBndr where
  rnf (IfLetBndr nm ty id_info join_info) =
    rnf nm `seq` rnf ty `seq` rnf id_info `seq` rnf join_info

instance NFData IfaceFamTyConFlav where
  rnf = \case
    IfaceDataFamilyTyCon -> ()
    IfaceOpenSynFamilyTyCon -> ()
    IfaceClosedSynFamilyTyCon f1 -> rnf f1
    IfaceAbstractClosedSynFamilyTyCon -> ()
    IfaceBuiltInSynFamTyCon -> ()

instance NFData IfaceTickish where
  rnf = \case
    IfaceHpcTick m i -> rnf m `seq` rnf i
    IfaceSCC cc b1 b2 -> cc `seq` rnf b1 `seq` rnf b2
    IfaceSource src str -> src `seq` rnf str
    IfaceBreakpoint m i fvs -> rnf m `seq` rnf i `seq` rnf fvs

instance NFData IfaceConAlt where
  rnf = \case
    IfaceDefaultAlt -> ()
    IfaceDataAlt nm -> rnf nm
    IfaceLitAlt lit -> lit `seq` ()

instance NFData IfaceCompleteMatch where
  rnf (IfaceCompleteMatch f1 mtc) = rnf f1 `seq` rnf mtc

instance NFData IfaceRule where
  rnf (IfaceRule f1 f2 f3 f4 f5 f6 f7 f8) =
    rnf f1 `seq` f2 `seq` rnf f3 `seq` rnf f4 `seq` rnf f5 `seq` rnf f6 `seq` rnf f7 `seq` f8 `seq` ()

instance NFData IfaceDefault where
  rnf (IfaceDefault f1 f2 f3) =
    rnf f1 `seq` rnf f2 `seq` rnf f3

instance NFData IfaceFamInst where
  rnf (IfaceFamInst f1 f2 f3 f4) =
    rnf f1 `seq` rnf f2 `seq` rnf f3 `seq` f4 `seq` ()

instance NFData IfaceClsInst where
  rnf (IfaceClsInst f1 f2 f3 f4 f5 f6) =
    f1 `seq` rnf f2 `seq` rnf f3 `seq` f4 `seq` f5 `seq` rnf f6

instance NFData IfaceWarnings where
  rnf = \case
      IfWarnAll txt   -> rnf txt
      IfWarnSome vs ds -> rnf vs `seq` rnf ds

instance NFData IfaceWarningTxt where
    rnf = \case
        IfWarningTxt f1 f2 f3 -> rnf f1 `seq` rnf f2 `seq` rnf f3
        IfDeprecatedTxt f1 f2 -> rnf f1 `seq` rnf f2

instance NFData IfaceStringLiteral where
    rnf (IfStringLiteral f1 f2) = rnf f1 `seq` rnf f2

instance NFData IfaceAnnotation where
  rnf (IfaceAnnotation f1 f2) = f1 `seq` f2 `seq` ()
