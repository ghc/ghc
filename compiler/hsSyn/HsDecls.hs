{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable,
             DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Abstract syntax of global declarations.
--
-- Definitions for: @SynDecl@ and @ConDecl@, @ClassDecl@,
-- @InstDecl@, @DefaultDecl@ and @ForeignDecl@.
module HsDecls (
  -- * Toplevel declarations
  HsDecl(..), LHsDecl, HsDataDefn(..), HsDeriving, LHsFunDep,
  HsDerivingClause(..), LHsDerivingClause, NewOrData(..), newOrDataToFlavour,

  -- ** Class or type declarations
  TyClDecl(..), LTyClDecl, DataDeclRn(..),
  TyClGroup(..), mkTyClGroup, emptyTyClGroup,
  tyClGroupTyClDecls, tyClGroupInstDecls, tyClGroupRoleDecls,
  isClassDecl, isDataDecl, isSynDecl, tcdName,
  isFamilyDecl, isTypeFamilyDecl, isDataFamilyDecl,
  isOpenTypeFamilyInfo, isClosedTypeFamilyInfo,
  tyFamInstDeclName, tyFamInstDeclLName,
  countTyClDecls, pprTyClDeclFlavour,
  tyClDeclLName, tyClDeclTyVars,
  hsDeclHasCusk, famDeclHasCusk,
  FamilyDecl(..), LFamilyDecl,

  -- ** Instance declarations
  InstDecl(..), LInstDecl, FamilyInfo(..),
  TyFamInstDecl(..), LTyFamInstDecl, instDeclDataFamInsts,
  DataFamInstDecl(..), LDataFamInstDecl,
  pprDataFamInstFlavour, pprHsFamInstLHS,
  FamInstEqn, LFamInstEqn, FamEqn(..),
  TyFamInstEqn, LTyFamInstEqn, TyFamDefltEqn, LTyFamDefltEqn,
  HsTyPats,
  LClsInstDecl, ClsInstDecl(..),

  -- ** Standalone deriving declarations
  DerivDecl(..), LDerivDecl,
  -- ** Deriving strategies
  DerivStrategy(..), LDerivStrategy, derivStrategyName,
  -- ** @RULE@ declarations
  LRuleDecls,RuleDecls(..),RuleDecl(..),LRuleDecl,HsRuleRn(..),
  RuleBndr(..),LRuleBndr,
  collectRuleBndrSigTys,
  flattenRuleDecls, pprFullRuleName,
  -- ** @default@ declarations
  DefaultDecl(..), LDefaultDecl,
  -- ** Template haskell declaration splice
  SpliceExplicitFlag(..),
  SpliceDecl(..), LSpliceDecl,
  -- ** Foreign function interface declarations
  ForeignDecl(..), LForeignDecl, ForeignImport(..), ForeignExport(..),
  CImportSpec(..),
  -- ** Data-constructor declarations
  ConDecl(..), LConDecl,
  HsConDeclDetails, hsConDeclArgTys, hsConDeclTheta,
  getConNames, getConArgs,
  -- ** Document comments
  DocDecl(..), LDocDecl, docDeclDoc,
  -- ** Deprecations
  WarnDecl(..),  LWarnDecl,
  WarnDecls(..), LWarnDecls,
  -- ** Annotations
  AnnDecl(..), LAnnDecl,
  AnnProvenance(..), annProvenanceName_maybe,
  -- ** Role annotations
  RoleAnnotDecl(..), LRoleAnnotDecl, roleAnnotDeclName,
  -- ** Injective type families
  FamilyResultSig(..), LFamilyResultSig, InjectivityAnn(..), LInjectivityAnn,
  resultVariableName,

  -- * Grouping
  HsGroup(..),  emptyRdrGroup, emptyRnGroup, appendGroups, hsGroupInstDecls

    ) where

-- friends:
import GhcPrelude

import {-# SOURCE #-}   HsExpr( HsExpr, HsSplice, pprExpr,
                                pprSpliceDecl )
        -- Because Expr imports Decls via HsBracket

import HsBinds
import HsTypes
import HsDoc
import TyCon
import BasicTypes
import Coercion
import ForeignCall
import HsExtension
import NameSet

-- others:
import Class
import Outputable
import Util
import SrcLoc
import Type

import Bag
import Maybes
import Data.Data        hiding (TyCon,Fixity, Infix)

{-
************************************************************************
*                                                                      *
\subsection[HsDecl]{Declarations}
*                                                                      *
************************************************************************
-}

type LHsDecl p = Located (HsDecl p)
        -- ^ When in a list this may have
        --
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'
        --

-- For details on above see note [Api annotations] in ApiAnnotation

-- | A Haskell Declaration
data HsDecl p
  = TyClD      (XTyClD p)      (TyClDecl p)      -- ^ Type or Class Declaration
  | InstD      (XInstD p)      (InstDecl  p)     -- ^ Instance declaration
  | DerivD     (XDerivD p)     (DerivDecl p)     -- ^ Deriving declaration
  | ValD       (XValD p)       (HsBind p)        -- ^ Value declaration
  | SigD       (XSigD p)       (Sig p)           -- ^ Signature declaration
  | DefD       (XDefD p)       (DefaultDecl p)   -- ^ 'default' declaration
  | ForD       (XForD p)       (ForeignDecl p)   -- ^ Foreign declaration
  | WarningD   (XWarningD p)   (WarnDecls p)     -- ^ Warning declaration
  | AnnD       (XAnnD p)       (AnnDecl p)       -- ^ Annotation declaration
  | RuleD      (XRuleD p)      (RuleDecls p)     -- ^ Rule declaration
  | SpliceD    (XSpliceD p)    (SpliceDecl p)    -- ^ Splice declaration
                                                 -- (Includes quasi-quotes)
  | DocD       (XDocD p)       (DocDecl)  -- ^ Documentation comment declaration
  | RoleAnnotD (XRoleAnnotD p) (RoleAnnotDecl p) -- ^Role annotation declaration
  | XHsDecl    (XXHsDecl p)

type instance XTyClD      (GhcPass _) = NoExt
type instance XInstD      (GhcPass _) = NoExt
type instance XDerivD     (GhcPass _) = NoExt
type instance XValD       (GhcPass _) = NoExt
type instance XSigD       (GhcPass _) = NoExt
type instance XDefD       (GhcPass _) = NoExt
type instance XForD       (GhcPass _) = NoExt
type instance XWarningD   (GhcPass _) = NoExt
type instance XAnnD       (GhcPass _) = NoExt
type instance XRuleD      (GhcPass _) = NoExt
type instance XSpliceD    (GhcPass _) = NoExt
type instance XDocD       (GhcPass _) = NoExt
type instance XRoleAnnotD (GhcPass _) = NoExt
type instance XXHsDecl    (GhcPass _) = NoExt

-- NB: all top-level fixity decls are contained EITHER
-- EITHER SigDs
-- OR     in the ClassDecls in TyClDs
--
-- The former covers
--      a) data constructors
--      b) class methods (but they can be also done in the
--              signatures of class decls)
--      c) imported functions (that have an IfacSig)
--      d) top level decls
--
-- The latter is for class methods only

-- | Haskell Group
--
-- A 'HsDecl' is categorised into a 'HsGroup' before being
-- fed to the renamer.
data HsGroup p
  = HsGroup {
        hs_ext    :: XCHsGroup p,
        hs_valds  :: HsValBinds p,
        hs_splcds :: [LSpliceDecl p],

        hs_tyclds :: [TyClGroup p],
                -- A list of mutually-recursive groups;
                -- This includes `InstDecl`s as well;
                -- Parser generates a singleton list;
                -- renamer does dependency analysis

        hs_derivds :: [LDerivDecl p],

        hs_fixds  :: [LFixitySig p],
                -- Snaffled out of both top-level fixity signatures,
                -- and those in class declarations

        hs_defds  :: [LDefaultDecl p],
        hs_fords  :: [LForeignDecl p],
        hs_warnds :: [LWarnDecls p],
        hs_annds  :: [LAnnDecl p],
        hs_ruleds :: [LRuleDecls p],

        hs_docs   :: [LDocDecl]
    }
  | XHsGroup (XXHsGroup p)

type instance XCHsGroup (GhcPass _) = NoExt
type instance XXHsGroup (GhcPass _) = NoExt


emptyGroup, emptyRdrGroup, emptyRnGroup :: HsGroup (GhcPass p)
emptyRdrGroup = emptyGroup { hs_valds = emptyValBindsIn }
emptyRnGroup  = emptyGroup { hs_valds = emptyValBindsOut }

hsGroupInstDecls :: HsGroup id -> [LInstDecl id]
hsGroupInstDecls = (=<<) group_instds . hs_tyclds

emptyGroup = HsGroup { hs_ext = noExt,
                       hs_tyclds = [],
                       hs_derivds = [],
                       hs_fixds = [], hs_defds = [], hs_annds = [],
                       hs_fords = [], hs_warnds = [], hs_ruleds = [],
                       hs_valds = error "emptyGroup hs_valds: Can't happen",
                       hs_splcds = [],
                       hs_docs = [] }

appendGroups :: HsGroup (GhcPass p) -> HsGroup (GhcPass p)
             -> HsGroup (GhcPass p)
appendGroups
    HsGroup {
        hs_valds  = val_groups1,
        hs_splcds = spliceds1,
        hs_tyclds = tyclds1,
        hs_derivds = derivds1,
        hs_fixds  = fixds1,
        hs_defds  = defds1,
        hs_annds  = annds1,
        hs_fords  = fords1,
        hs_warnds = warnds1,
        hs_ruleds = rulds1,
        hs_docs   = docs1 }
    HsGroup {
        hs_valds  = val_groups2,
        hs_splcds = spliceds2,
        hs_tyclds = tyclds2,
        hs_derivds = derivds2,
        hs_fixds  = fixds2,
        hs_defds  = defds2,
        hs_annds  = annds2,
        hs_fords  = fords2,
        hs_warnds = warnds2,
        hs_ruleds = rulds2,
        hs_docs   = docs2 }
  =
    HsGroup {
        hs_ext    = noExt,
        hs_valds  = val_groups1 `plusHsValBinds` val_groups2,
        hs_splcds = spliceds1 ++ spliceds2,
        hs_tyclds = tyclds1 ++ tyclds2,
        hs_derivds = derivds1 ++ derivds2,
        hs_fixds  = fixds1 ++ fixds2,
        hs_annds  = annds1 ++ annds2,
        hs_defds  = defds1 ++ defds2,
        hs_fords  = fords1 ++ fords2,
        hs_warnds = warnds1 ++ warnds2,
        hs_ruleds = rulds1 ++ rulds2,
        hs_docs   = docs1  ++ docs2 }
appendGroups _ _ = panic "appendGroups"

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (HsDecl p) where
    ppr (TyClD _ dcl)             = ppr dcl
    ppr (ValD _ binds)            = ppr binds
    ppr (DefD _ def)              = ppr def
    ppr (InstD _ inst)            = ppr inst
    ppr (DerivD _ deriv)          = ppr deriv
    ppr (ForD _ fd)               = ppr fd
    ppr (SigD _ sd)               = ppr sd
    ppr (RuleD _ rd)              = ppr rd
    ppr (WarningD _ wd)           = ppr wd
    ppr (AnnD _ ad)               = ppr ad
    ppr (SpliceD _ dd)            = ppr dd
    ppr (DocD _ doc)              = ppr doc
    ppr (RoleAnnotD _ ra)         = ppr ra
    ppr (XHsDecl x)               = ppr x

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (HsGroup p) where
    ppr (HsGroup { hs_valds  = val_decls,
                   hs_tyclds = tycl_decls,
                   hs_derivds = deriv_decls,
                   hs_fixds  = fix_decls,
                   hs_warnds = deprec_decls,
                   hs_annds  = ann_decls,
                   hs_fords  = foreign_decls,
                   hs_defds  = default_decls,
                   hs_ruleds = rule_decls })
        = vcat_mb empty
            [ppr_ds fix_decls, ppr_ds default_decls,
             ppr_ds deprec_decls, ppr_ds ann_decls,
             ppr_ds rule_decls,
             if isEmptyValBinds val_decls
                then Nothing
                else Just (ppr val_decls),
             ppr_ds (tyClGroupTyClDecls tycl_decls),
             ppr_ds (tyClGroupInstDecls tycl_decls),
             ppr_ds deriv_decls,
             ppr_ds foreign_decls]
        where
          ppr_ds :: Outputable a => [a] -> Maybe SDoc
          ppr_ds [] = Nothing
          ppr_ds ds = Just (vcat (map ppr ds))

          vcat_mb :: SDoc -> [Maybe SDoc] -> SDoc
          -- Concatenate vertically with white-space between non-blanks
          vcat_mb _    []             = empty
          vcat_mb gap (Nothing : ds) = vcat_mb gap ds
          vcat_mb gap (Just d  : ds) = gap $$ d $$ vcat_mb blankLine ds
    ppr (XHsGroup x) = ppr x

-- | Located Splice Declaration
type LSpliceDecl pass = Located (SpliceDecl pass)

-- | Splice Declaration
data SpliceDecl p
  = SpliceDecl                  -- Top level splice
        (XSpliceDecl p)
        (Located (HsSplice p))
        SpliceExplicitFlag
  | XSpliceDecl (XXSpliceDecl p)

type instance XSpliceDecl      (GhcPass _) = NoExt
type instance XXSpliceDecl     (GhcPass _) = NoExt

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (SpliceDecl p) where
   ppr (SpliceDecl _ (L _ e) f) = pprSpliceDecl e f
   ppr (XSpliceDecl x) = ppr x

{-
************************************************************************
*                                                                      *
            Type and class declarations
*                                                                      *
************************************************************************

Note [The Naming story]
~~~~~~~~~~~~~~~~~~~~~~~
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
   the ClassOpSig (in HsBinds), in the DefMethInfo field.
   (DefMethInfo is defined in Class.hs)

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
   The Class for Foo has a Nothing for op2 and
                         a Just ($dm_op1, VanillaDM) for op1.
   The Name for $dmop2 is simply discarded.

In *interface-file* class declarations:
  - When parsing, we see if there's an explicit programmer-supplied default method
    because there's an '=' sign to indicate it:
        class Foo a where
          op1 = :: <type>       -- NB the '='
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
-}

-- | Located Declaration of a Type or Class
type LTyClDecl pass = Located (TyClDecl pass)

-- | A type or class declaration.
data TyClDecl pass
  = -- | @type/data family T :: *->*@
    --
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
    --             'ApiAnnotation.AnnData',
    --             'ApiAnnotation.AnnFamily','ApiAnnotation.AnnDcolon',
    --             'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpenP',
    --             'ApiAnnotation.AnnDcolon','ApiAnnotation.AnnCloseP',
    --             'ApiAnnotation.AnnEqual','ApiAnnotation.AnnRarrow',
    --             'ApiAnnotation.AnnVbar'

    -- For details on above see note [Api annotations] in ApiAnnotation
    FamDecl { tcdFExt :: XFamDecl pass, tcdFam :: FamilyDecl pass }

  | -- | @type@ declaration
    --
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
    --             'ApiAnnotation.AnnEqual',

    -- For details on above see note [Api annotations] in ApiAnnotation
    SynDecl { tcdSExt   :: XSynDecl pass          -- ^ Post renameer, FVs
            , tcdLName  :: Located (IdP pass)     -- ^ Type constructor
            , tcdTyVars :: LHsQTyVars pass        -- ^ Type variables; for an
                                                  -- associated type these
                                                  -- include outer binders
            , tcdFixity :: LexicalFixity    -- ^ Fixity used in the declaration
            , tcdRhs    :: LHsType pass }         -- ^ RHS of type declaration

  | -- | @data@ declaration
    --
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnData',
    --              'ApiAnnotation.AnnFamily',
    --              'ApiAnnotation.AnnNewType',
    --              'ApiAnnotation.AnnNewType','ApiAnnotation.AnnDcolon'
    --              'ApiAnnotation.AnnWhere',

    -- For details on above see note [Api annotations] in ApiAnnotation
    DataDecl { tcdDExt     :: XDataDecl pass       -- ^ Post renamer, CUSK flag, FVs
             , tcdLName    :: Located (IdP pass)   -- ^ Type constructor
             , tcdTyVars   :: LHsQTyVars pass      -- ^ Type variables
                              -- See Note [TyVar binders for associated declarations]
             , tcdFixity   :: LexicalFixity        -- ^ Fixity used in the declaration
             , tcdDataDefn :: HsDataDefn pass }

  | ClassDecl { tcdCExt    :: XClassDecl pass,         -- ^ Post renamer, FVs
                tcdCtxt    :: LHsContext pass,         -- ^ Context...
                tcdLName   :: Located (IdP pass),      -- ^ Name of the class
                tcdTyVars  :: LHsQTyVars pass,         -- ^ Class type variables
                tcdFixity  :: LexicalFixity, -- ^ Fixity used in the declaration
                tcdFDs     :: [LHsFunDep pass],         -- ^ Functional deps
                tcdSigs    :: [LSig pass],              -- ^ Methods' signatures
                tcdMeths   :: LHsBinds pass,            -- ^ Default methods
                tcdATs     :: [LFamilyDecl pass],       -- ^ Associated types;
                tcdATDefs  :: [LTyFamDefltEqn pass],    -- ^ Associated type defaults
                tcdDocs    :: [LDocDecl]                -- ^ Haddock docs
    }
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnClass',
        --           'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnClose'
        --   - The tcdFDs will have 'ApiAnnotation.AnnVbar',
        --                          'ApiAnnotation.AnnComma'
        --                          'ApiAnnotation.AnnRarrow'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | XTyClDecl (XXTyClDecl pass)

type LHsFunDep pass = Located (FunDep (Located (IdP pass)))

data DataDeclRn = DataDeclRn
             { tcdDataCusk :: Bool    -- ^ does this have a CUSK?
             , tcdFVs      :: NameSet }
  deriving Data

{- Note [TyVar binders for associated decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For an /associated/ data, newtype, or type-family decl, the LHsQTyVars
/includes/ outer binders.  For example
    class T a where
       data D a c
       type F a b :: *
       type F a b = a -> a
Here the data decl for 'D', and type-family decl for 'F', both include 'a'
in their LHsQTyVars (tcdTyVars and fdTyVars resp).

Ditto any implicit binders in the hsq_implicit field of the LHSQTyVars.

The idea is that the associated type is really a top-level decl in its
own right.  However we are careful to use the same name 'a', so that
we can match things up.

c.f. Note [Associated type tyvar names] in Class.hs
     Note [Family instance declaration binders]
-}

type instance XFamDecl      (GhcPass _) = NoExt

type instance XSynDecl      GhcPs = NoExt
type instance XSynDecl      GhcRn = NameSet -- FVs
type instance XSynDecl      GhcTc = NameSet -- FVs

type instance XDataDecl     GhcPs = NoExt
type instance XDataDecl     GhcRn = DataDeclRn
type instance XDataDecl     GhcTc = DataDeclRn

type instance XClassDecl    GhcPs = NoExt
type instance XClassDecl    GhcRn = NameSet -- FVs
type instance XClassDecl    GhcTc = NameSet -- FVs

type instance XXTyClDecl    (GhcPass _) = NoExt

-- Simple classifiers for TyClDecl
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | @True@ <=> argument is a @data@\/@newtype@
-- declaration.
isDataDecl :: TyClDecl pass -> Bool
isDataDecl (DataDecl {}) = True
isDataDecl _other        = False

-- | type or type instance declaration
isSynDecl :: TyClDecl pass -> Bool
isSynDecl (SynDecl {})   = True
isSynDecl _other        = False

-- | type class
isClassDecl :: TyClDecl pass -> Bool
isClassDecl (ClassDecl {}) = True
isClassDecl _              = False

-- | type/data family declaration
isFamilyDecl :: TyClDecl pass -> Bool
isFamilyDecl (FamDecl {})  = True
isFamilyDecl _other        = False

-- | type family declaration
isTypeFamilyDecl :: TyClDecl pass -> Bool
isTypeFamilyDecl (FamDecl _ (FamilyDecl { fdInfo = info })) = case info of
  OpenTypeFamily      -> True
  ClosedTypeFamily {} -> True
  _                   -> False
isTypeFamilyDecl _ = False

-- | open type family info
isOpenTypeFamilyInfo :: FamilyInfo pass -> Bool
isOpenTypeFamilyInfo OpenTypeFamily = True
isOpenTypeFamilyInfo _              = False

-- | closed type family info
isClosedTypeFamilyInfo :: FamilyInfo pass -> Bool
isClosedTypeFamilyInfo (ClosedTypeFamily {}) = True
isClosedTypeFamilyInfo _                     = False

-- | data family declaration
isDataFamilyDecl :: TyClDecl pass -> Bool
isDataFamilyDecl (FamDecl _ (FamilyDecl { fdInfo = DataFamily })) = True
isDataFamilyDecl _other      = False

-- Dealing with names

tyFamInstDeclName :: TyFamInstDecl pass -> (IdP pass)
tyFamInstDeclName = unLoc . tyFamInstDeclLName

tyFamInstDeclLName :: TyFamInstDecl pass -> Located (IdP pass)
tyFamInstDeclLName (TyFamInstDecl { tfid_eqn =
                     (HsIB { hsib_body = FamEqn { feqn_tycon = ln }}) })
  = ln
tyFamInstDeclLName (TyFamInstDecl (HsIB _ (XFamEqn _)))
  = panic "tyFamInstDeclLName"
tyFamInstDeclLName (TyFamInstDecl (XHsImplicitBndrs _))
  = panic "tyFamInstDeclLName"

tyClDeclLName :: TyClDecl pass -> Located (IdP pass)
tyClDeclLName (FamDecl { tcdFam = FamilyDecl { fdLName = ln } }) = ln
tyClDeclLName decl = tcdLName decl

tcdName :: TyClDecl pass -> (IdP pass)
tcdName = unLoc . tyClDeclLName

tyClDeclTyVars :: TyClDecl pass -> LHsQTyVars pass
tyClDeclTyVars (FamDecl { tcdFam = FamilyDecl { fdTyVars = tvs } }) = tvs
tyClDeclTyVars d = tcdTyVars d

countTyClDecls :: [TyClDecl pass] -> (Int, Int, Int, Int, Int)
        -- class, synonym decls, data, newtype, family decls
countTyClDecls decls
 = (count isClassDecl    decls,
    count isSynDecl      decls,  -- excluding...
    count isDataTy       decls,  -- ...family...
    count isNewTy        decls,  -- ...instances
    count isFamilyDecl   decls)
 where
   isDataTy DataDecl{ tcdDataDefn = HsDataDefn { dd_ND = DataType } } = True
   isDataTy _                                                       = False

   isNewTy DataDecl{ tcdDataDefn = HsDataDefn { dd_ND = NewType } } = True
   isNewTy _                                                      = False

-- | Does this declaration have a complete, user-supplied kind signature?
-- See Note [CUSKs: complete user-supplied kind signatures]
hsDeclHasCusk :: TyClDecl GhcRn -> Bool
hsDeclHasCusk (FamDecl { tcdFam = fam_decl }) = famDeclHasCusk Nothing fam_decl
hsDeclHasCusk (SynDecl { tcdTyVars = tyvars, tcdRhs = rhs })
  -- NB: Keep this synchronized with 'getInitialKind'
  = hsTvbAllKinded tyvars && rhs_annotated rhs
  where
    rhs_annotated (L _ ty) = case ty of
      HsParTy _ lty  -> rhs_annotated lty
      HsKindSig {}   -> True
      _              -> False
hsDeclHasCusk (DataDecl { tcdDExt = DataDeclRn { tcdDataCusk = cusk }}) = cusk
hsDeclHasCusk (ClassDecl { tcdTyVars = tyvars }) = hsTvbAllKinded tyvars
hsDeclHasCusk (XTyClDecl _) = panic "hsDeclHasCusk"

-- Pretty-printing TyClDecl
-- ~~~~~~~~~~~~~~~~~~~~~~~~

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (TyClDecl p) where

    ppr (FamDecl { tcdFam = decl }) = ppr decl
    ppr (SynDecl { tcdLName = ltycon, tcdTyVars = tyvars, tcdFixity = fixity
                 , tcdRhs = rhs })
      = hang (text "type" <+>
              pp_vanilla_decl_head ltycon tyvars fixity noLHsContext <+> equals)
          4 (ppr rhs)

    ppr (DataDecl { tcdLName = ltycon, tcdTyVars = tyvars, tcdFixity = fixity
                  , tcdDataDefn = defn })
      = pp_data_defn (pp_vanilla_decl_head ltycon tyvars fixity) defn

    ppr (ClassDecl {tcdCtxt = context, tcdLName = lclas, tcdTyVars = tyvars,
                    tcdFixity = fixity,
                    tcdFDs  = fds,
                    tcdSigs = sigs, tcdMeths = methods,
                    tcdATs = ats, tcdATDefs = at_defs})
      | null sigs && isEmptyBag methods && null ats && null at_defs -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = vcat [ top_matter <+> text "where"
             , nest 2 $ pprDeclList (map (pprFamilyDecl NotTopLevel . unLoc) ats ++
                                     map ppr_fam_deflt_eqn at_defs ++
                                     pprLHsBindsForUser methods sigs) ]
      where
        top_matter = text "class"
                    <+> pp_vanilla_decl_head lclas tyvars fixity context
                    <+> pprFundeps (map unLoc fds)

    ppr (XTyClDecl x) = ppr x

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (TyClGroup p) where
  ppr (TyClGroup { group_tyclds = tyclds
                 , group_roles = roles
                 , group_instds = instds
                 }
      )
    = ppr tyclds $$
      ppr roles $$
      ppr instds
  ppr (XTyClGroup x) = ppr x

pp_vanilla_decl_head :: (OutputableBndrId (GhcPass p))
   => Located (IdP (GhcPass p))
   -> LHsQTyVars (GhcPass p)
   -> LexicalFixity
   -> LHsContext (GhcPass p)
   -> SDoc
pp_vanilla_decl_head thing (HsQTvs { hsq_explicit = tyvars }) fixity context
 = hsep [pprLHsContext context, pp_tyvars tyvars]
  where
    pp_tyvars (varl:varsr)
      | fixity == Infix && length varsr > 1
         = hsep [char '(',ppr (unLoc varl), pprInfixOcc (unLoc thing)
                , (ppr.unLoc) (head varsr), char ')'
                , hsep (map (ppr.unLoc) (tail varsr))]
      | fixity == Infix
         = hsep [ppr (unLoc varl), pprInfixOcc (unLoc thing)
         , hsep (map (ppr.unLoc) varsr)]
      | otherwise = hsep [ pprPrefixOcc (unLoc thing)
                  , hsep (map (ppr.unLoc) (varl:varsr))]
    pp_tyvars [] = pprPrefixOcc (unLoc thing)
pp_vanilla_decl_head _ (XLHsQTyVars x) _ _ = ppr x

pprTyClDeclFlavour :: TyClDecl (GhcPass p) -> SDoc
pprTyClDeclFlavour (ClassDecl {})   = text "class"
pprTyClDeclFlavour (SynDecl {})     = text "type"
pprTyClDeclFlavour (FamDecl { tcdFam = FamilyDecl { fdInfo = info }})
  = pprFlavour info <+> text "family"
pprTyClDeclFlavour (FamDecl { tcdFam = XFamilyDecl x})
  = ppr x
pprTyClDeclFlavour (DataDecl { tcdDataDefn = HsDataDefn { dd_ND = nd } })
  = ppr nd
pprTyClDeclFlavour (DataDecl { tcdDataDefn = XHsDataDefn x })
  = ppr x
pprTyClDeclFlavour (XTyClDecl x) = ppr x


{- Note [CUSKs: complete user-supplied kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We kind-check declarations differently if they have a complete, user-supplied
kind signature (CUSK). This is because we can safely generalise a CUSKed
declaration before checking all of the others, supporting polymorphic recursion.
See ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindInference#Proposednewstrategy
and #9200 for lots of discussion of how we got here.

PRINCIPLE:
  a type declaration has a CUSK iff we could produce a separate kind signature
  for it, just like a type signature for a function,
  looking only at the header of the declaration.

Examples:
  * data T1 (a :: *->*) (b :: *) = ....
    -- Has CUSK; equivalant to   T1 :: (*->*) -> * -> *

 * data T2 a b = ...
   -- No CUSK; we do not want to guess T2 :: * -> * -> *
   -- because the full decl might be   data T a b = MkT (a b)

  * data T3 (a :: k -> *) (b :: *) = ...
    -- CUSK; equivalent to   T3 :: (k -> *) -> * -> *
    -- We lexically generalise over k to get
    --    T3 :: forall k. (k -> *) -> * -> *
    -- The generalisation is here is purely lexical, just like
    --    f3 :: a -> a
    -- means
    --    f3 :: forall a. a -> a

  * data T4 (a :: j k) = ...
     -- CUSK; equivalent to   T4 :: j k -> *
     -- which we lexically generalise to  T4 :: forall j k. j k -> *
     -- and then, if PolyKinds is on, we further generalise to
     --   T4 :: forall kk (j :: kk -> *) (k :: kk). j k -> *
     -- Again this is exactly like what happens as the term level
     -- when you write
     --    f4 :: forall a b. a b -> Int

NOTE THAT
  * A CUSK does /not/ mean that everything about the kind signature is
    fully specified by the user.  Look at T4 and f4: we had do do kind
    inference to figure out the kind-quantification.  But in both cases
    (T4 and f4) that inference is done looking /only/ at the header of T4
    (or signature for f4), not at the definition thereof.

  * The CUSK completely fixes the kind of the type constructor, forever.

  * The precise rules, for each declaration form, for whethher a declaration
    has a CUSK are given in the user manual section "Complete user-supplied
    kind signatures and polymorphic recursion".  BUt they simply implement
    PRINCIPLE above.

  * Open type families are interesting:
      type family T5 a b :: *
    There simply /is/ no accompanying declaration, so that info is all
    we'll ever get.  So we it has a CUSK by definition, and we default
    any un-fixed kind variables to *.

  * Associated types are a bit tricker:
      class C6 a where
         type family T6 a b :: *
         op :: a Int -> Int
    Here C6 does not have a CUSK (in fact we ultimately discover that
    a :: * -> *).  And hence neither does T6, the associated family,
    because we can't fix its kind until we have settled C6.  Another
    way to say it: unlike a top-level, we /may/ discover more about
    a's kind from C6's definition.

  * A data definition with a top-level :: must explicitly bind all
    kind variables to the right of the ::. See test
    dependent/should_compile/KindLevels, which requires this
    case. (Naturally, any kind variable mentioned before the :: should
    not be bound after it.)

    This last point is much more debatable than the others; see
    Trac #15142 comment:22
-}


{- *********************************************************************
*                                                                      *
                         TyClGroup
        Strongly connected components of
      type, class, instance, and role declarations
*                                                                      *
********************************************************************* -}

{- Note [TyClGroups and dependency analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A TyClGroup represents a strongly connected components of type/class/instance
decls, together with the role annotations for the type/class declarations.

The hs_tyclds :: [TyClGroup] field of a HsGroup is a dependency-order
sequence of strongly-connected components.

Invariants
 * The type and class declarations, group_tyclds, may depend on each
   other, or earlier TyClGroups, but not on later ones

 * The role annotations, group_roles, are role-annotations for some or
   all of the types and classes in group_tyclds (only).

 * The instance declarations, group_instds, may (and usually will)
   depend on group_tyclds, or on earlier TyClGroups, but not on later
   ones.

See Note [Dependency analsis of type, class, and instance decls]
in RnSource for more info.
-}

-- | Type or Class Group
data TyClGroup pass  -- See Note [TyClGroups and dependency analysis]
  = TyClGroup { group_ext    :: XCTyClGroup pass
              , group_tyclds :: [LTyClDecl pass]
              , group_roles  :: [LRoleAnnotDecl pass]
              , group_instds :: [LInstDecl pass] }
  | XTyClGroup (XXTyClGroup pass)

type instance XCTyClGroup (GhcPass _) = NoExt
type instance XXTyClGroup (GhcPass _) = NoExt


emptyTyClGroup :: TyClGroup (GhcPass p)
emptyTyClGroup = TyClGroup noExt [] [] []

tyClGroupTyClDecls :: [TyClGroup pass] -> [LTyClDecl pass]
tyClGroupTyClDecls = concatMap group_tyclds

tyClGroupInstDecls :: [TyClGroup pass] -> [LInstDecl pass]
tyClGroupInstDecls = concatMap group_instds

tyClGroupRoleDecls :: [TyClGroup pass] -> [LRoleAnnotDecl pass]
tyClGroupRoleDecls = concatMap group_roles

mkTyClGroup :: [LTyClDecl (GhcPass p)] -> [LInstDecl (GhcPass p)]
            -> TyClGroup (GhcPass p)
mkTyClGroup decls instds = TyClGroup
  { group_ext = noExt
  , group_tyclds = decls
  , group_roles = []
  , group_instds = instds
  }



{- *********************************************************************
*                                                                      *
               Data and type family declarations
*                                                                      *
********************************************************************* -}

{- Note [FamilyResultSig]
~~~~~~~~~~~~~~~~~~~~~~~~~

This data type represents the return signature of a type family.  Possible
values are:

 * NoSig - the user supplied no return signature:
      type family Id a where ...

 * KindSig - the user supplied the return kind:
      type family Id a :: * where ...

 * TyVarSig - user named the result with a type variable and possibly
   provided a kind signature for that variable:
      type family Id a = r where ...
      type family Id a = (r :: *) where ...

   Naming result of a type family is required if we want to provide
   injectivity annotation for a type family:
      type family Id a = r | r -> a where ...

See also: Note [Injectivity annotation]

Note [Injectivity annotation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A user can declare a type family to be injective:

   type family Id a = r | r -> a where ...

 * The part after the "|" is called "injectivity annotation".
 * "r -> a" part is called "injectivity condition"; at the moment terms
   "injectivity annotation" and "injectivity condition" are synonymous
   because we only allow a single injectivity condition.
 * "r" is the "LHS of injectivity condition". LHS can only contain the
   variable naming the result of a type family.

 * "a" is the "RHS of injectivity condition". RHS contains space-separated
   type and kind variables representing the arguments of a type
   family. Variables can be omitted if a type family is not injective in
   these arguments. Example:
         type family Foo a b c = d | d -> a c where ...

Note that:
 (a) naming of type family result is required to provide injectivity
     annotation
 (b) for associated types if the result was named then injectivity annotation
     is mandatory. Otherwise result type variable is indistinguishable from
     associated type default.

It is possible that in the future this syntax will be extended to support
more complicated injectivity annotations. For example we could declare that
if we know the result of Plus and one of its arguments we can determine the
other argument:

   type family Plus a b = (r :: Nat) | r a -> b, r b -> a where ...

Here injectivity annotation would consist of two comma-separated injectivity
conditions.

See also Note [Injective type families] in TyCon
-}

-- | Located type Family Result Signature
type LFamilyResultSig pass = Located (FamilyResultSig pass)

-- | type Family Result Signature
data FamilyResultSig pass = -- see Note [FamilyResultSig]
    NoSig (XNoSig pass)
  -- ^ - 'ApiAnnotation.AnnKeywordId' :

  -- For details on above see note [Api annotations] in ApiAnnotation

  | KindSig  (XCKindSig pass) (LHsKind pass)
  -- ^ - 'ApiAnnotation.AnnKeywordId' :
  --             'ApiAnnotation.AnnOpenP','ApiAnnotation.AnnDcolon',
  --             'ApiAnnotation.AnnCloseP'

  -- For details on above see note [Api annotations] in ApiAnnotation

  | TyVarSig (XTyVarSig pass) (LHsTyVarBndr pass)
  -- ^ - 'ApiAnnotation.AnnKeywordId' :
  --             'ApiAnnotation.AnnOpenP','ApiAnnotation.AnnDcolon',
  --             'ApiAnnotation.AnnCloseP', 'ApiAnnotation.AnnEqual'
  | XFamilyResultSig (XXFamilyResultSig pass)

  -- For details on above see note [Api annotations] in ApiAnnotation

type instance XNoSig            (GhcPass _) = NoExt
type instance XCKindSig         (GhcPass _) = NoExt
type instance XTyVarSig         (GhcPass _) = NoExt
type instance XXFamilyResultSig (GhcPass _) = NoExt


-- | Located type Family Declaration
type LFamilyDecl pass = Located (FamilyDecl pass)

-- | type Family Declaration
data FamilyDecl pass = FamilyDecl
  { fdExt            :: XCFamilyDecl pass
  , fdInfo           :: FamilyInfo pass              -- type/data, closed/open
  , fdLName          :: Located (IdP pass)           -- type constructor
  , fdTyVars         :: LHsQTyVars pass              -- type variables
                       -- See Note [TyVar binders for associated declarations]
  , fdFixity         :: LexicalFixity                -- Fixity used in the declaration
  , fdResultSig      :: LFamilyResultSig pass        -- result signature
  , fdInjectivityAnn :: Maybe (LInjectivityAnn pass) -- optional injectivity ann
  }
  | XFamilyDecl (XXFamilyDecl pass)
  -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
  --             'ApiAnnotation.AnnData', 'ApiAnnotation.AnnFamily',
  --             'ApiAnnotation.AnnWhere', 'ApiAnnotation.AnnOpenP',
  --             'ApiAnnotation.AnnDcolon', 'ApiAnnotation.AnnCloseP',
  --             'ApiAnnotation.AnnEqual', 'ApiAnnotation.AnnRarrow',
  --             'ApiAnnotation.AnnVbar'

  -- For details on above see note [Api annotations] in ApiAnnotation

type instance XCFamilyDecl    (GhcPass _) = NoExt
type instance XXFamilyDecl    (GhcPass _) = NoExt


-- | Located Injectivity Annotation
type LInjectivityAnn pass = Located (InjectivityAnn pass)

-- | If the user supplied an injectivity annotation it is represented using
-- InjectivityAnn. At the moment this is a single injectivity condition - see
-- Note [Injectivity annotation]. `Located name` stores the LHS of injectivity
-- condition. `[Located name]` stores the RHS of injectivity condition. Example:
--
--   type family Foo a b c = r | r -> a c where ...
--
-- This will be represented as "InjectivityAnn `r` [`a`, `c`]"
data InjectivityAnn pass
  = InjectivityAnn (Located (IdP pass)) [Located (IdP pass)]
  -- ^ - 'ApiAnnotation.AnnKeywordId' :
  --             'ApiAnnotation.AnnRarrow', 'ApiAnnotation.AnnVbar'

  -- For details on above see note [Api annotations] in ApiAnnotation

data FamilyInfo pass
  = DataFamily
  | OpenTypeFamily
     -- | 'Nothing' if we're in an hs-boot file and the user
     -- said "type family Foo x where .."
  | ClosedTypeFamily (Maybe [LTyFamInstEqn pass])

-- | Does this family declaration have a complete, user-supplied kind signature?
-- See Note [CUSKs: complete user-supplied kind signatures]
famDeclHasCusk :: Maybe Bool
                   -- ^ if associated, does the enclosing class have a CUSK?
               -> FamilyDecl pass -> Bool
famDeclHasCusk _ (FamilyDecl { fdInfo      = ClosedTypeFamily _
                             , fdTyVars    = tyvars
                             , fdResultSig = L _ resultSig })
  = hsTvbAllKinded tyvars && hasReturnKindSignature resultSig
famDeclHasCusk mb_class_cusk _ = mb_class_cusk `orElse` True
        -- all un-associated open families have CUSKs

-- | Does this family declaration have user-supplied return kind signature?
hasReturnKindSignature :: FamilyResultSig a -> Bool
hasReturnKindSignature (NoSig _)                        = False
hasReturnKindSignature (TyVarSig _ (L _ (UserTyVar{}))) = False
hasReturnKindSignature _                                = True

-- | Maybe return name of the result type variable
resultVariableName :: FamilyResultSig a -> Maybe (IdP a)
resultVariableName (TyVarSig _ sig) = Just $ hsLTyVarName sig
resultVariableName _                = Nothing

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (FamilyDecl p) where
  ppr = pprFamilyDecl TopLevel

pprFamilyDecl :: (OutputableBndrId (GhcPass p))
              => TopLevelFlag -> FamilyDecl (GhcPass p) -> SDoc
pprFamilyDecl top_level (FamilyDecl { fdInfo = info, fdLName = ltycon
                                    , fdTyVars = tyvars
                                    , fdFixity = fixity
                                    , fdResultSig = L _ result
                                    , fdInjectivityAnn = mb_inj })
  = vcat [ pprFlavour info <+> pp_top_level <+>
           pp_vanilla_decl_head ltycon tyvars fixity noLHsContext <+>
           pp_kind <+> pp_inj <+> pp_where
         , nest 2 $ pp_eqns ]
  where
    pp_top_level = case top_level of
                     TopLevel    -> text "family"
                     NotTopLevel -> empty

    pp_kind = case result of
                NoSig    _         -> empty
                KindSig  _ kind    -> dcolon <+> ppr kind
                TyVarSig _ tv_bndr -> text "=" <+> ppr tv_bndr
                XFamilyResultSig x -> ppr x
    pp_inj = case mb_inj of
               Just (L _ (InjectivityAnn lhs rhs)) ->
                 hsep [ vbar, ppr lhs, text "->", hsep (map ppr rhs) ]
               Nothing -> empty
    (pp_where, pp_eqns) = case info of
      ClosedTypeFamily mb_eqns ->
        ( text "where"
        , case mb_eqns of
            Nothing   -> text ".."
            Just eqns -> vcat $ map (ppr_fam_inst_eqn . unLoc) eqns )
      _ -> (empty, empty)
pprFamilyDecl _ (XFamilyDecl x) = ppr x

pprFlavour :: FamilyInfo pass -> SDoc
pprFlavour DataFamily            = text "data"
pprFlavour OpenTypeFamily        = text "type"
pprFlavour (ClosedTypeFamily {}) = text "type"

instance Outputable (FamilyInfo pass) where
  ppr info = pprFlavour info <+> text "family"



{- *********************************************************************
*                                                                      *
               Data types and data constructors
*                                                                      *
********************************************************************* -}

-- | Haskell Data type Definition
data HsDataDefn pass   -- The payload of a data type defn
                       -- Used *both* for vanilla data declarations,
                       --       *and* for data family instances
  = -- | Declares a data type or newtype, giving its constructors
    -- @
    --  data/newtype T a = <constrs>
    --  data/newtype instance T [a] = <constrs>
    -- @
    HsDataDefn { dd_ext    :: XCHsDataDefn pass,
                 dd_ND     :: NewOrData,
                 dd_ctxt   :: LHsContext pass,           -- ^ Context
                 dd_cType  :: Maybe (Located CType),
                 dd_kindSig:: Maybe (LHsKind pass),
                     -- ^ Optional kind signature.
                     --
                     -- @(Just k)@ for a GADT-style @data@,
                     -- or @data instance@ decl, with explicit kind sig
                     --
                     -- Always @Nothing@ for H98-syntax decls

                 dd_cons   :: [LConDecl pass],
                     -- ^ Data constructors
                     --
                     -- For @data T a = T1 | T2 a@
                     --   the 'LConDecl's all have 'ConDeclH98'.
                     -- For @data T a where { T1 :: T a }@
                     --   the 'LConDecls' all have 'ConDeclGADT'.

                 dd_derivs :: HsDeriving pass  -- ^ Optional 'deriving' claues

             -- For details on above see note [Api annotations] in ApiAnnotation
   }
  | XHsDataDefn (XXHsDataDefn pass)

type instance XCHsDataDefn    (GhcPass _) = NoExt
type instance XXHsDataDefn    (GhcPass _) = NoExt

-- | Haskell Deriving clause
type HsDeriving pass = Located [LHsDerivingClause pass]
  -- ^ The optional @deriving@ clauses of a data declaration. "Clauses" is
  -- plural because one can specify multiple deriving clauses using the
  -- @-XDerivingStrategies@ language extension.
  --
  -- The list of 'LHsDerivingClause's corresponds to exactly what the user
  -- requested to derive, in order. If no deriving clauses were specified,
  -- the list is empty.

type LHsDerivingClause pass = Located (HsDerivingClause pass)

-- | A single @deriving@ clause of a data declaration.
--
--  - 'ApiAnnotation.AnnKeywordId' :
--       'ApiAnnotation.AnnDeriving', 'ApiAnnotation.AnnStock',
--       'ApiAnnotation.AnnAnyClass', 'Api.AnnNewtype',
--       'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'
data HsDerivingClause pass
  -- See Note [Deriving strategies] in TcDeriv
  = HsDerivingClause
    { deriv_clause_ext :: XCHsDerivingClause pass
    , deriv_clause_strategy :: Maybe (LDerivStrategy pass)
      -- ^ The user-specified strategy (if any) to use when deriving
      -- 'deriv_clause_tys'.
    , deriv_clause_tys :: Located [LHsSigType pass]
      -- ^ The types to derive.
      --
      -- It uses 'LHsSigType's because, with @-XGeneralizedNewtypeDeriving@,
      -- we can mention type variables that aren't bound by the datatype, e.g.
      --
      -- > data T b = ... deriving (C [a])
      --
      -- should produce a derived instance for @C [a] (T b)@.
    }
  | XHsDerivingClause (XXHsDerivingClause pass)

type instance XCHsDerivingClause    (GhcPass _) = NoExt
type instance XXHsDerivingClause    (GhcPass _) = NoExt

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (HsDerivingClause p) where
  ppr (HsDerivingClause { deriv_clause_strategy = dcs
                        , deriv_clause_tys      = L _ dct })
    = hsep [ text "deriving"
           , pp_strat_before
           , pp_dct dct
           , pp_strat_after ]
      where
        -- This complexity is to distinguish between
        --    deriving Show
        --    deriving (Show)
        pp_dct [HsIB { hsib_body = ty }]
                 = ppr (parenthesizeHsType appPrec ty)
        pp_dct _ = parens (interpp'SP dct)

        -- @via@ is unique in that in comes /after/ the class being derived,
        -- so we must special-case it.
        (pp_strat_before, pp_strat_after) =
          case dcs of
            Just (L _ via@ViaStrategy{}) -> (empty, ppr via)
            _                            -> (ppDerivStrategy dcs, empty)
  ppr (XHsDerivingClause x) = ppr x

data NewOrData
  = NewType                     -- ^ @newtype Blah ...@
  | DataType                    -- ^ @data Blah ...@
  deriving( Eq, Data )                -- Needed because Demand derives Eq

-- | Convert a 'NewOrData' to a 'TyConFlavour'
newOrDataToFlavour :: NewOrData -> TyConFlavour
newOrDataToFlavour NewType  = NewtypeFlavour
newOrDataToFlavour DataType = DataTypeFlavour

-- | Located data Constructor Declaration
type LConDecl pass = Located (ConDecl pass)
      -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi' when
      --   in a GADT constructor list

  -- For details on above see note [Api annotations] in ApiAnnotation

-- |
--
-- @
-- data T b = forall a. Eq a => MkT a b
--   MkT :: forall b a. Eq a => MkT a b
--
-- data T b where
--      MkT1 :: Int -> T Int
--
-- data T = Int `MkT` Int
--        | MkT2
--
-- data T a where
--      Int `MkT` Int :: T Int
-- @
--
-- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
--            'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnCLose',
--            'ApiAnnotation.AnnEqual','ApiAnnotation.AnnVbar',
--            'ApiAnnotation.AnnDarrow','ApiAnnotation.AnnDarrow',
--            'ApiAnnotation.AnnForall','ApiAnnotation.AnnDot'

-- For details on above see note [Api annotations] in ApiAnnotation

-- | data Constructor Declaration
data ConDecl pass
  = ConDeclGADT
      { con_g_ext   :: XConDeclGADT pass
      , con_names   :: [Located (IdP pass)]

      -- The next four fields describe the type after the '::'
      -- See Note [GADT abstract syntax]
      -- The following field is Located to anchor API Annotations,
      -- AnnForall and AnnDot.
      , con_forall  :: Located Bool      -- ^ True <=> explicit forall
                                         --   False => hsq_explicit is empty
      , con_qvars   :: LHsQTyVars pass
                       -- Whether or not there is an /explicit/ forall, we still
                       -- need to capture the implicitly-bound type/kind variables

      , con_mb_cxt  :: Maybe (LHsContext pass) -- ^ User-written context (if any)
      , con_args    :: HsConDeclDetails pass   -- ^ Arguments; never InfixCon
      , con_res_ty  :: LHsType pass            -- ^ Result type

      , con_doc     :: Maybe LHsDocString
          -- ^ A possible Haddock comment.
      }

  | ConDeclH98
      { con_ext     :: XConDeclH98 pass
      , con_name    :: Located (IdP pass)

      , con_forall  :: Located Bool
                              -- ^ True <=> explicit user-written forall
                              --     e.g. data T a = forall b. MkT b (b->a)
                              --     con_ex_tvs = {b}
                              -- False => con_ex_tvs is empty
      , con_ex_tvs :: [LHsTyVarBndr pass]      -- ^ Existentials only
      , con_mb_cxt :: Maybe (LHsContext pass)  -- ^ User-written context (if any)
      , con_args   :: HsConDeclDetails pass    -- ^ Arguments; can be InfixCon

      , con_doc       :: Maybe LHsDocString
          -- ^ A possible Haddock comment.
      }
  | XConDecl (XXConDecl pass)

type instance XConDeclGADT (GhcPass _) = NoExt
type instance XConDeclH98  (GhcPass _) = NoExt
type instance XXConDecl    (GhcPass _) = NoExt

{- Note [GADT abstract syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's a wrinkle in ConDeclGADT

* For record syntax, it's all uniform.  Given:
      data T a where
        K :: forall a. Ord a => { x :: [a], ... } -> T a
    we make the a ConDeclGADT for K with
       con_qvars  = {a}
       con_mb_cxt = Just [Ord a]
       con_args   = RecCon <the record fields>
       con_res_ty = T a

  We need the RecCon before the reanmer, so we can find the record field
  binders in HsUtils.hsConDeclsBinders.

* However for a GADT constr declaration which is not a record, it can
  be hard parse until we know operator fixities. Consider for example
     C :: a :*: b -> a :*: b -> a :+: b
  Initially this type will parse as
      a :*: (b -> (a :*: (b -> (a :+: b))))
  so it's hard to split up the arguments until we've done the precedence
  resolution (in the renamer).

  So:  - In the parser (RdrHsSyn.mkGadtDecl), we put the whole constr
         type into the res_ty for a ConDeclGADT for now, and use
         PrefixCon []
            con_args   = PrefixCon []
            con_res_ty = a :*: (b -> (a :*: (b -> (a :+: b))))

       - In the renamer (RnSource.rnConDecl), we unravel it afer
         operator fixities are sorted. So we generate. So we end
         up with
            con_args   = PrefixCon [ a :*: b, a :*: b ]
            con_res_ty = a :+: b
-}

-- | Haskell data Constructor Declaration Details
type HsConDeclDetails pass
   = HsConDetails (LBangType pass) (Located [LConDeclField pass])

getConNames :: ConDecl pass -> [Located (IdP pass)]
getConNames ConDeclH98  {con_name  = name}  = [name]
getConNames ConDeclGADT {con_names = names} = names
getConNames XConDecl {} = panic "getConNames"

getConArgs :: ConDecl pass -> HsConDeclDetails pass
getConArgs d = con_args d

hsConDeclArgTys :: HsConDeclDetails pass -> [LBangType pass]
hsConDeclArgTys (PrefixCon tys)    = tys
hsConDeclArgTys (InfixCon ty1 ty2) = [ty1,ty2]
hsConDeclArgTys (RecCon flds)      = map (cd_fld_type . unLoc) (unLoc flds)

hsConDeclTheta :: Maybe (LHsContext pass) -> [LHsType pass]
hsConDeclTheta Nothing            = []
hsConDeclTheta (Just (L _ theta)) = theta

pp_data_defn :: (OutputableBndrId (GhcPass p))
                  => (LHsContext (GhcPass p) -> SDoc)   -- Printing the header
                  -> HsDataDefn (GhcPass p)
                  -> SDoc
pp_data_defn pp_hdr (HsDataDefn { dd_ND = new_or_data, dd_ctxt = context
                                , dd_cType = mb_ct
                                , dd_kindSig = mb_sig
                                , dd_cons = condecls, dd_derivs = derivings })
  | null condecls
  = ppr new_or_data <+> pp_ct <+> pp_hdr context <+> pp_sig
    <+> pp_derivings derivings

  | otherwise
  = hang (ppr new_or_data <+> pp_ct  <+> pp_hdr context <+> pp_sig)
       2 (pp_condecls condecls $$ pp_derivings derivings)
  where
    pp_ct = case mb_ct of
               Nothing   -> empty
               Just ct -> ppr ct
    pp_sig = case mb_sig of
               Nothing   -> empty
               Just kind -> dcolon <+> ppr kind
    pp_derivings (L _ ds) = vcat (map ppr ds)
pp_data_defn _ (XHsDataDefn x) = ppr x

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (HsDataDefn p) where
   ppr d = pp_data_defn (\_ -> text "Naked HsDataDefn") d

instance Outputable NewOrData where
  ppr NewType  = text "newtype"
  ppr DataType = text "data"

pp_condecls :: (OutputableBndrId (GhcPass p)) => [LConDecl (GhcPass p)] -> SDoc
pp_condecls cs@(L _ ConDeclGADT{} : _) -- In GADT syntax
  = hang (text "where") 2 (vcat (map ppr cs))
pp_condecls cs                    -- In H98 syntax
  = equals <+> sep (punctuate (text " |") (map ppr cs))

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (ConDecl p) where
    ppr = pprConDecl

pprConDecl :: (OutputableBndrId (GhcPass p)) => ConDecl (GhcPass p) -> SDoc
pprConDecl (ConDeclH98 { con_name = L _ con
                       , con_ex_tvs = ex_tvs
                       , con_mb_cxt = mcxt
                       , con_args = args
                       , con_doc = doc })
  = sep [ppr_mbDoc doc, pprHsForAll ex_tvs cxt, ppr_details args]
  where
    ppr_details (InfixCon t1 t2) = hsep [ppr t1, pprInfixOcc con, ppr t2]
    ppr_details (PrefixCon tys)  = hsep (pprPrefixOcc con
                                   : map (pprHsType . unLoc) tys)
    ppr_details (RecCon fields)  = pprPrefixOcc con
                                 <+> pprConDeclFields (unLoc fields)
    cxt = fromMaybe noLHsContext mcxt

pprConDecl (ConDeclGADT { con_names = cons, con_qvars = qvars
                        , con_mb_cxt = mcxt, con_args = args
                        , con_res_ty = res_ty, con_doc = doc })
  = ppr_mbDoc doc <+> ppr_con_names cons <+> dcolon
    <+> (sep [pprHsForAll (hsq_explicit qvars) cxt,
              ppr_arrow_chain (get_args args ++ [ppr res_ty]) ])
  where
    get_args (PrefixCon args) = map ppr args
    get_args (RecCon fields)  = [pprConDeclFields (unLoc fields)]
    get_args (InfixCon {})    = pprPanic "pprConDecl:GADT" (ppr cons)

    cxt = fromMaybe noLHsContext mcxt

    ppr_arrow_chain (a:as) = sep (a : map (arrow <+>) as)
    ppr_arrow_chain []     = empty

pprConDecl (XConDecl x) = ppr x

ppr_con_names :: (OutputableBndr a) => [Located a] -> SDoc
ppr_con_names = pprWithCommas (pprPrefixOcc . unLoc)

{-
************************************************************************
*                                                                      *
                Instance declarations
*                                                                      *
************************************************************************

Note [Type family instance declarations in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The data type FamEqn represents one equation of a type family instance.
Aside from the pass, it is also parameterised over two fields:
feqn_pats and feqn_rhs.

feqn_pats is either LHsTypes (for ordinary data/type family instances) or
LHsQTyVars (for associated type family default instances). In particular:

 * An ordinary type family instance declaration looks like this in source Haskell
      type instance T [a] Int = a -> a
   (or something similar for a closed family)
   It is represented by a FamInstEqn, with a *type* (LHsType) in the feqn_pats
   field.

 * On the other hand, the *default instance* of an associated type looks like
   this in source Haskell
      class C a where
        type T a b
        type T a b = a -> b   -- The default instance
   It is represented by a TyFamDefltEqn, with *type variables* (LHsQTyVars) in
   the feqn_pats field.

feqn_rhs is either an HsDataDefn (for data family instances) or an LHsType
(for type family instances).
-}

----------------- Type synonym family instances -------------

-- | Located Type Family Instance Equation
type LTyFamInstEqn pass = Located (TyFamInstEqn pass)
  -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'
  --   when in a list

-- For details on above see note [Api annotations] in ApiAnnotation

-- | Located Type Family Default Equation
type LTyFamDefltEqn pass = Located (TyFamDefltEqn pass)

-- | Haskell Type Patterns
type HsTyPats pass = [LHsTypeArg pass]

{- Note [Family instance declaration binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For ordinary data/type family instances, the feqn_pats field of FamEqn stores
the LHS type (and kind) patterns. Any type (and kind) variables contained
in these type patterns are bound in the hsib_vars field of the HsImplicitBndrs
in FamInstEqn depending on whether or not an explicit forall is present. In
the case of an explicit forall, the hsib_vars only includes kind variables not
bound in the forall. Otherwise, all type (and kind) variables are bound in
the hsib_vars. In the latter case, note that in particular

* The hsib_vars *includes* any anonymous wildcards.  For example
     type instance F a _ = a
  The hsib_vars will be {a, _}.  Remember that each separate wildcard
  '_' gets its own unique.  In this context wildcards behave just like
  an ordinary type variable, only anonymous.

* The hsib_vars *includes* type variables that are already in scope

   Eg   class C s t where
          type F t p :: *
        instance C w (a,b) where
          type F (a,b) x = x->a
   The hsib_vars of the F decl are {a,b,x}, even though the F decl
   is nested inside the 'instance' decl.

   However after the renamer, the uniques will match up:
        instance C w7 (a8,b9) where
          type F (a8,b9) x10 = x10->a8
   so that we can compare the type pattern in the 'instance' decl and
   in the associated 'type' decl

For associated type family default instances (TyFamDefltEqn), instead of using
type patterns with binders in a surrounding HsImplicitBndrs, we use raw type
variables (LHsQTyVars) in the feqn_pats field of FamEqn.

c.f. Note [TyVar binders for associated declarations]
-}

-- | Type Family Instance Equation
type TyFamInstEqn pass = FamInstEqn pass (LHsType pass)

-- | Type Family Default Equation
type TyFamDefltEqn pass = FamEqn pass (LHsQTyVars pass) (LHsType pass)
  -- See Note [Type family instance declarations in HsSyn]

-- | Located Type Family Instance Declaration
type LTyFamInstDecl pass = Located (TyFamInstDecl pass)

-- | Type Family Instance Declaration
newtype TyFamInstDecl pass = TyFamInstDecl { tfid_eqn :: TyFamInstEqn pass }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
    --           'ApiAnnotation.AnnInstance',

    -- For details on above see note [Api annotations] in ApiAnnotation

----------------- Data family instances -------------

-- | Located Data Family Instance Declaration
type LDataFamInstDecl pass = Located (DataFamInstDecl pass)

-- | Data Family Instance Declaration
newtype DataFamInstDecl pass
  = DataFamInstDecl { dfid_eqn :: FamInstEqn pass (HsDataDefn pass) }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnData',
    --           'ApiAnnotation.AnnNewType','ApiAnnotation.AnnInstance',
    --           'ApiAnnotation.AnnDcolon'
    --           'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpen',
    --           'ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation

----------------- Family instances (common types) -------------

-- | Located Family Instance Equation
type LFamInstEqn pass rhs = Located (FamInstEqn pass rhs)

-- | Family Instance Equation
type FamInstEqn pass rhs
  = HsImplicitBndrs pass (FamEqn pass (HsTyPats pass) rhs)
            -- ^ Here, the @pats@ are type patterns (with kind and type bndrs).
            -- See Note [Family instance declaration binders]

-- | Family Equation
--
-- One equation in a type family instance declaration, data family instance
-- declaration, or type family default.
-- See Note [Type family instance declarations in HsSyn]
-- See Note [Family instance declaration binders]
data FamEqn pass pats rhs
  = FamEqn
       { feqn_ext    :: XCFamEqn pass pats rhs
       , feqn_tycon  :: Located (IdP pass)
       , feqn_bndrs  :: Maybe [LHsTyVarBndr pass] -- ^ Optional quantified type vars
       , feqn_pats   :: pats
       , feqn_fixity :: LexicalFixity -- ^ Fixity used in the declaration
       , feqn_rhs    :: rhs
       }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnEqual'
  | XFamEqn (XXFamEqn pass pats rhs)

    -- For details on above see note [Api annotations] in ApiAnnotation

type instance XCFamEqn    (GhcPass _) p r = NoExt
type instance XXFamEqn    (GhcPass _) p r = NoExt

----------------- Class instances -------------

-- | Located Class Instance Declaration
type LClsInstDecl pass = Located (ClsInstDecl pass)

-- | Class Instance Declaration
data ClsInstDecl pass
  = ClsInstDecl
      { cid_ext     :: XCClsInstDecl pass
      , cid_poly_ty :: LHsSigType pass    -- Context => Class Instance-type
                                          -- Using a polytype means that the renamer conveniently
                                          -- figures out the quantified type variables for us.
      , cid_binds         :: LHsBinds pass       -- Class methods
      , cid_sigs          :: [LSig pass]         -- User-supplied pragmatic info
      , cid_tyfam_insts   :: [LTyFamInstDecl pass]   -- Type family instances
      , cid_datafam_insts :: [LDataFamInstDecl pass] -- Data family instances
      , cid_overlap_mode  :: Maybe (Located OverlapMode)
         -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
         --                                    'ApiAnnotation.AnnClose',

        -- For details on above see note [Api annotations] in ApiAnnotation
      }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnInstance',
    --           'ApiAnnotation.AnnWhere',
    --           'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose',

    -- For details on above see note [Api annotations] in ApiAnnotation
  | XClsInstDecl (XXClsInstDecl pass)

type instance XCClsInstDecl    (GhcPass _) = NoExt
type instance XXClsInstDecl    (GhcPass _) = NoExt

----------------- Instances of all kinds -------------

-- | Located Instance Declaration
type LInstDecl pass = Located (InstDecl pass)

-- | Instance Declaration
data InstDecl pass  -- Both class and family instances
  = ClsInstD
      { cid_d_ext :: XClsInstD pass
      , cid_inst  :: ClsInstDecl pass }
  | DataFamInstD              -- data family instance
      { dfid_ext  :: XDataFamInstD pass
      , dfid_inst :: DataFamInstDecl pass }
  | TyFamInstD              -- type family instance
      { tfid_ext  :: XTyFamInstD pass
      , tfid_inst :: TyFamInstDecl pass }
  | XInstDecl (XXInstDecl pass)

type instance XClsInstD     (GhcPass _) = NoExt
type instance XDataFamInstD (GhcPass _) = NoExt
type instance XTyFamInstD   (GhcPass _) = NoExt
type instance XXInstDecl    (GhcPass _) = NoExt

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (TyFamInstDecl p) where
  ppr = pprTyFamInstDecl TopLevel

pprTyFamInstDecl :: (OutputableBndrId (GhcPass p))
                 => TopLevelFlag -> TyFamInstDecl (GhcPass p) -> SDoc
pprTyFamInstDecl top_lvl (TyFamInstDecl { tfid_eqn = eqn })
   = text "type" <+> ppr_instance_keyword top_lvl <+> ppr_fam_inst_eqn eqn

ppr_instance_keyword :: TopLevelFlag -> SDoc
ppr_instance_keyword TopLevel    = text "instance"
ppr_instance_keyword NotTopLevel = empty

ppr_fam_inst_eqn :: (OutputableBndrId (GhcPass p))
                 => TyFamInstEqn (GhcPass p) -> SDoc
ppr_fam_inst_eqn (HsIB { hsib_body = FamEqn { feqn_tycon  = L _ tycon
                                            , feqn_bndrs  = bndrs
                                            , feqn_pats   = pats
                                            , feqn_fixity = fixity
                                            , feqn_rhs    = rhs }})
    = pprHsFamInstLHS tycon bndrs pats fixity noLHsContext <+> equals <+> ppr rhs
ppr_fam_inst_eqn (HsIB { hsib_body = XFamEqn x }) = ppr x
ppr_fam_inst_eqn (XHsImplicitBndrs x) = ppr x

ppr_fam_deflt_eqn :: (OutputableBndrId (GhcPass p))
                  => LTyFamDefltEqn (GhcPass p) -> SDoc
ppr_fam_deflt_eqn (L _ (FamEqn { feqn_tycon  = tycon
                               , feqn_pats   = tvs
                               , feqn_fixity = fixity
                               , feqn_rhs    = rhs }))
    = text "type" <+> pp_vanilla_decl_head tycon tvs fixity noLHsContext
                  <+> equals <+> ppr rhs
ppr_fam_deflt_eqn (L _ (XFamEqn x)) = ppr x

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (DataFamInstDecl p) where
  ppr = pprDataFamInstDecl TopLevel

pprDataFamInstDecl :: (OutputableBndrId (GhcPass p))
                   => TopLevelFlag -> DataFamInstDecl (GhcPass p) -> SDoc
pprDataFamInstDecl top_lvl (DataFamInstDecl { dfid_eqn = HsIB { hsib_body =
                             FamEqn { feqn_tycon  = L _ tycon
                                    , feqn_bndrs  = bndrs
                                    , feqn_pats   = pats
                                    , feqn_fixity = fixity
                                    , feqn_rhs    = defn }}})
  = pp_data_defn pp_hdr defn
  where
    pp_hdr ctxt = ppr_instance_keyword top_lvl
              <+> pprHsFamInstLHS tycon bndrs pats fixity ctxt
                  -- pp_data_defn pretty-prints the kind sig. See #14817.

pprDataFamInstDecl _ (DataFamInstDecl (HsIB _ (XFamEqn x)))
  = ppr x
pprDataFamInstDecl _ (DataFamInstDecl (XHsImplicitBndrs x))
  = ppr x

pprDataFamInstFlavour :: DataFamInstDecl (GhcPass p) -> SDoc
pprDataFamInstFlavour (DataFamInstDecl { dfid_eqn = HsIB { hsib_body =
                        FamEqn { feqn_rhs = HsDataDefn { dd_ND = nd }}}})
  = ppr nd
pprDataFamInstFlavour (DataFamInstDecl { dfid_eqn = HsIB { hsib_body =
                        FamEqn { feqn_rhs = XHsDataDefn x}}})
  = ppr x
pprDataFamInstFlavour (DataFamInstDecl (HsIB _ (XFamEqn x)))
  = ppr x
pprDataFamInstFlavour (DataFamInstDecl (XHsImplicitBndrs x))
  = ppr x

pprHsFamInstLHS :: (OutputableBndrId (GhcPass p))
   => IdP (GhcPass p)
   -> Maybe [LHsTyVarBndr (GhcPass p)]
   -> HsTyPats (GhcPass p)
   -> LexicalFixity
   -> LHsContext (GhcPass p)
   -> SDoc
pprHsFamInstLHS thing bndrs typats fixity mb_ctxt
   = hsep [ pprHsExplicitForAll bndrs
          , pprLHsContext mb_ctxt
          , pp_pats typats ]
   where
     pp_pats (patl:patr:pats)
       | Infix <- fixity
       = let pp_op_app = hsep [ ppr patl, pprInfixOcc thing, ppr patr ] in
         case pats of
           [] -> pp_op_app
           _  -> hsep (parens pp_op_app : map ppr pats)

     pp_pats pats = hsep [ pprPrefixOcc thing
                         , hsep (map ppr pats)]

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (ClsInstDecl p) where
    ppr (ClsInstDecl { cid_poly_ty = inst_ty, cid_binds = binds
                     , cid_sigs = sigs, cid_tyfam_insts = ats
                     , cid_overlap_mode = mbOverlap
                     , cid_datafam_insts = adts })
      | null sigs, null ats, null adts, isEmptyBag binds  -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = vcat [ top_matter <+> text "where"
             , nest 2 $ pprDeclList $
               map (pprTyFamInstDecl NotTopLevel . unLoc)   ats ++
               map (pprDataFamInstDecl NotTopLevel . unLoc) adts ++
               pprLHsBindsForUser binds sigs ]
      where
        top_matter = text "instance" <+> ppOverlapPragma mbOverlap
                                             <+> ppr inst_ty
    ppr (XClsInstDecl x) = ppr x

ppDerivStrategy :: (p ~ GhcPass pass, OutputableBndrId p)
                => Maybe (LDerivStrategy p) -> SDoc
ppDerivStrategy mb =
  case mb of
    Nothing       -> empty
    Just (L _ ds) -> ppr ds

ppOverlapPragma :: Maybe (Located OverlapMode) -> SDoc
ppOverlapPragma mb =
  case mb of
    Nothing           -> empty
    Just (L _ (NoOverlap s))    -> maybe_stext s "{-# NO_OVERLAP #-}"
    Just (L _ (Overlappable s)) -> maybe_stext s "{-# OVERLAPPABLE #-}"
    Just (L _ (Overlapping s))  -> maybe_stext s "{-# OVERLAPPING #-}"
    Just (L _ (Overlaps s))     -> maybe_stext s "{-# OVERLAPS #-}"
    Just (L _ (Incoherent s))   -> maybe_stext s "{-# INCOHERENT #-}"
  where
    maybe_stext NoSourceText     alt = text alt
    maybe_stext (SourceText src) _   = text src <+> text "#-}"


instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (InstDecl p) where
    ppr (ClsInstD     { cid_inst  = decl }) = ppr decl
    ppr (TyFamInstD   { tfid_inst = decl }) = ppr decl
    ppr (DataFamInstD { dfid_inst = decl }) = ppr decl
    ppr (XInstDecl x) = ppr x

-- Extract the declarations of associated data types from an instance

instDeclDataFamInsts :: [LInstDecl pass] -> [DataFamInstDecl pass]
instDeclDataFamInsts inst_decls
  = concatMap do_one inst_decls
  where
    do_one (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = fam_insts } }))
      = map unLoc fam_insts
    do_one (L _ (DataFamInstD { dfid_inst = fam_inst }))      = [fam_inst]
    do_one (L _ (TyFamInstD {}))                              = []
    do_one (L _ (ClsInstD _ (XClsInstDecl _))) = panic "instDeclDataFamInsts"
    do_one (L _ (XInstDecl _))                 = panic "instDeclDataFamInsts"

{-
************************************************************************
*                                                                      *
\subsection[DerivDecl]{A stand-alone instance deriving declaration}
*                                                                      *
************************************************************************
-}

-- | Located stand-alone 'deriving instance' declaration
type LDerivDecl pass = Located (DerivDecl pass)

-- | Stand-alone 'deriving instance' declaration
data DerivDecl pass = DerivDecl
        { deriv_ext          :: XCDerivDecl pass
        , deriv_type         :: LHsSigWcType pass
          -- ^ The instance type to derive.
          --
          -- It uses an 'LHsSigWcType' because the context is allowed to be a
          -- single wildcard:
          --
          -- > deriving instance _ => Eq (Foo a)
          --
          -- Which signifies that the context should be inferred.

          -- See Note [Inferring the instance context] in TcDerivInfer.

        , deriv_strategy     :: Maybe (LDerivStrategy pass)
        , deriv_overlap_mode :: Maybe (Located OverlapMode)
         -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDeriving',
         --        'ApiAnnotation.AnnInstance', 'ApiAnnotation.AnnStock',
         --        'ApiAnnotation.AnnAnyClass', 'Api.AnnNewtype',
         --        'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
        }
  | XDerivDecl (XXDerivDecl pass)

type instance XCDerivDecl    (GhcPass _) = NoExt
type instance XXDerivDecl    (GhcPass _) = NoExt

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (DerivDecl p) where
    ppr (DerivDecl { deriv_type = ty
                   , deriv_strategy = ds
                   , deriv_overlap_mode = o })
        = hsep [ text "deriving"
               , ppDerivStrategy ds
               , text "instance"
               , ppOverlapPragma o
               , ppr ty ]
    ppr (XDerivDecl x) = ppr x

{-
************************************************************************
*                                                                      *
                Deriving strategies
*                                                                      *
************************************************************************
-}

-- | A 'Located' 'DerivStrategy'.
type LDerivStrategy pass = Located (DerivStrategy pass)

-- | Which technique the user explicitly requested when deriving an instance.
data DerivStrategy pass
  -- See Note [Deriving strategies] in TcDeriv
  = StockStrategy    -- ^ GHC's \"standard\" strategy, which is to implement a
                     --   custom instance for the data type. This only works
                     --   for certain types that GHC knows about (e.g., 'Eq',
                     --   'Show', 'Functor' when @-XDeriveFunctor@ is enabled,
                     --   etc.)
  | AnyclassStrategy -- ^ @-XDeriveAnyClass@
  | NewtypeStrategy  -- ^ @-XGeneralizedNewtypeDeriving@
  | ViaStrategy (XViaStrategy pass)
                     -- ^ @-XDerivingVia@

type instance XViaStrategy GhcPs = LHsSigType GhcPs
type instance XViaStrategy GhcRn = LHsSigType GhcRn
type instance XViaStrategy GhcTc = Type

instance (p ~ GhcPass pass, OutputableBndrId p)
        => Outputable (DerivStrategy p) where
    ppr StockStrategy    = text "stock"
    ppr AnyclassStrategy = text "anyclass"
    ppr NewtypeStrategy  = text "newtype"
    ppr (ViaStrategy ty) = text "via" <+> ppr ty

-- | A short description of a @DerivStrategy'@.
derivStrategyName :: DerivStrategy a -> SDoc
derivStrategyName = text . go
  where
    go StockStrategy    = "stock"
    go AnyclassStrategy = "anyclass"
    go NewtypeStrategy  = "newtype"
    go (ViaStrategy {}) = "via"

{-
************************************************************************
*                                                                      *
\subsection[DefaultDecl]{A @default@ declaration}
*                                                                      *
************************************************************************

There can only be one default declaration per module, but it is hard
for the parser to check that; we pass them all through in the abstract
syntax, and that restriction must be checked in the front end.
-}

-- | Located Default Declaration
type LDefaultDecl pass = Located (DefaultDecl pass)

-- | Default Declaration
data DefaultDecl pass
  = DefaultDecl (XCDefaultDecl pass) [LHsType pass]
        -- ^ - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnDefault',
        --          'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | XDefaultDecl (XXDefaultDecl pass)

type instance XCDefaultDecl    (GhcPass _) = NoExt
type instance XXDefaultDecl    (GhcPass _) = NoExt

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (DefaultDecl p) where
    ppr (DefaultDecl _ tys)
      = text "default" <+> parens (interpp'SP tys)
    ppr (XDefaultDecl x) = ppr x

{-
************************************************************************
*                                                                      *
\subsection{Foreign function interface declaration}
*                                                                      *
************************************************************************
-}

-- foreign declarations are distinguished as to whether they define or use a
-- Haskell name
--
--  * the Boolean value indicates whether the pre-standard deprecated syntax
--   has been used

-- | Located Foreign Declaration
type LForeignDecl pass = Located (ForeignDecl pass)

-- | Foreign Declaration
data ForeignDecl pass
  = ForeignImport
      { fd_i_ext  :: XForeignImport pass   -- Post typechecker, rep_ty ~ sig_ty
      , fd_name   :: Located (IdP pass)    -- defines this name
      , fd_sig_ty :: LHsSigType pass       -- sig_ty
      , fd_fi     :: ForeignImport }

  | ForeignExport
      { fd_e_ext  :: XForeignExport pass   -- Post typechecker, rep_ty ~ sig_ty
      , fd_name   :: Located (IdP pass)    -- uses this name
      , fd_sig_ty :: LHsSigType pass       -- sig_ty
      , fd_fe     :: ForeignExport }
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnForeign',
        --           'ApiAnnotation.AnnImport','ApiAnnotation.AnnExport',
        --           'ApiAnnotation.AnnDcolon'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | XForeignDecl (XXForeignDecl pass)

{-
    In both ForeignImport and ForeignExport:
        sig_ty is the type given in the Haskell code
        rep_ty is the representation for this type, i.e. with newtypes
               coerced away and type functions evaluated.
    Thus if the declaration is valid, then rep_ty will only use types
    such as Int and IO that we know how to make foreign calls with.
-}

type instance XForeignImport   GhcPs = NoExt
type instance XForeignImport   GhcRn = NoExt
type instance XForeignImport   GhcTc = Coercion

type instance XForeignExport   GhcPs = NoExt
type instance XForeignExport   GhcRn = NoExt
type instance XForeignExport   GhcTc = Coercion

type instance XXForeignDecl    (GhcPass _) = NoExt

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
                     CImport  (Located CCallConv) -- ccall or stdcall
                              (Located Safety)  -- interruptible, safe or unsafe
                              (Maybe Header)       -- name of C header
                              CImportSpec          -- details of the C entity
                              (Located SourceText) -- original source text for
                                                   -- the C entity
  deriving Data

-- details of an external C entity
--
data CImportSpec = CLabel    CLabelString     -- import address of a C label
                 | CFunction CCallTarget      -- static or dynamic function
                 | CWrapper                   -- wrapper to expose closures
                                              -- (former f.e.d.)
  deriving Data

-- specification of an externally exported entity in dependence on the calling
-- convention
--
data ForeignExport = CExport  (Located CExportSpec) -- contains the calling
                                                    -- convention
                              (Located SourceText)  -- original source text for
                                                    -- the C entity
  deriving Data

-- pretty printing of foreign declarations
--

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (ForeignDecl p) where
  ppr (ForeignImport { fd_name = n, fd_sig_ty = ty, fd_fi = fimport })
    = hang (text "foreign import" <+> ppr fimport <+> ppr n)
         2 (dcolon <+> ppr ty)
  ppr (ForeignExport { fd_name = n, fd_sig_ty = ty, fd_fe = fexport }) =
    hang (text "foreign export" <+> ppr fexport <+> ppr n)
       2 (dcolon <+> ppr ty)
  ppr (XForeignDecl x) = ppr x

instance Outputable ForeignImport where
  ppr (CImport  cconv safety mHeader spec (L _ srcText)) =
    ppr cconv <+> ppr safety
      <+> pprWithSourceText srcText (pprCEntity spec "")
    where
      pp_hdr = case mHeader of
               Nothing -> empty
               Just (Header _ header) -> ftext header

      pprCEntity (CLabel lbl) _ =
        doubleQuotes $ text "static" <+> pp_hdr <+> char '&' <> ppr lbl
      pprCEntity (CFunction (StaticTarget st _lbl _ isFun)) src =
        if dqNeeded then doubleQuotes ce else empty
          where
            dqNeeded = (take 6 src == "static")
                    || isJust mHeader
                    || not isFun
                    || st /= NoSourceText
            ce =
                  -- We may need to drop leading spaces first
                  (if take 6 src == "static" then text "static" else empty)
              <+> pp_hdr
              <+> (if isFun then empty else text "value")
              <+> (pprWithSourceText st empty)
      pprCEntity (CFunction DynamicTarget) _ =
        doubleQuotes $ text "dynamic"
      pprCEntity CWrapper _ = doubleQuotes $ text "wrapper"

instance Outputable ForeignExport where
  ppr (CExport  (L _ (CExportStatic _ lbl cconv)) _) =
    ppr cconv <+> char '"' <> ppr lbl <> char '"'

{-
************************************************************************
*                                                                      *
\subsection{Transformation rules}
*                                                                      *
************************************************************************
-}

-- | Located Rule Declarations
type LRuleDecls pass = Located (RuleDecls pass)

  -- Note [Pragma source text] in BasicTypes
-- | Rule Declarations
data RuleDecls pass = HsRules { rds_ext   :: XCRuleDecls pass
                              , rds_src   :: SourceText
                              , rds_rules :: [LRuleDecl pass] }
  | XRuleDecls (XXRuleDecls pass)

type instance XCRuleDecls    (GhcPass _) = NoExt
type instance XXRuleDecls    (GhcPass _) = NoExt

-- | Located Rule Declaration
type LRuleDecl pass = Located (RuleDecl pass)

-- | Rule Declaration
data RuleDecl pass
  = HsRule -- Source rule
       { rd_ext  :: XHsRule pass
           -- ^ After renamer, free-vars from the LHS and RHS
       , rd_name :: Located (SourceText,RuleName)
           -- ^ Note [Pragma source text] in BasicTypes
       , rd_act  :: Activation
       , rd_tyvs :: Maybe [LHsTyVarBndr (NoGhcTc pass)]
           -- ^ Forall'd type vars
       , rd_tmvs :: [LRuleBndr pass]
           -- ^ Forall'd term vars, before typechecking; after typechecking
           --    this includes all forall'd vars
       , rd_lhs  :: Located (HsExpr pass)
       , rd_rhs  :: Located (HsExpr pass)
       }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' :
    --           'ApiAnnotation.AnnOpen','ApiAnnotation.AnnTilde',
    --           'ApiAnnotation.AnnVal',
    --           'ApiAnnotation.AnnClose',
    --           'ApiAnnotation.AnnForall','ApiAnnotation.AnnDot',
    --           'ApiAnnotation.AnnEqual',
  | XRuleDecl (XXRuleDecl pass)

data HsRuleRn = HsRuleRn NameSet NameSet -- Free-vars from the LHS and RHS
  deriving Data

type instance XHsRule       GhcPs = NoExt
type instance XHsRule       GhcRn = HsRuleRn
type instance XHsRule       GhcTc = HsRuleRn

type instance XXRuleDecl    (GhcPass _) = NoExt

flattenRuleDecls :: [LRuleDecls pass] -> [LRuleDecl pass]
flattenRuleDecls decls = concatMap (rds_rules . unLoc) decls

-- | Located Rule Binder
type LRuleBndr pass = Located (RuleBndr pass)

-- | Rule Binder
data RuleBndr pass
  = RuleBndr (XCRuleBndr pass)  (Located (IdP pass))
  | RuleBndrSig (XRuleBndrSig pass) (Located (IdP pass)) (LHsSigWcType pass)
  | XRuleBndr (XXRuleBndr pass)
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --     'ApiAnnotation.AnnDcolon','ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation

type instance XCRuleBndr    (GhcPass _) = NoExt
type instance XRuleBndrSig  (GhcPass _) = NoExt
type instance XXRuleBndr    (GhcPass _) = NoExt

collectRuleBndrSigTys :: [RuleBndr pass] -> [LHsSigWcType pass]
collectRuleBndrSigTys bndrs = [ty | RuleBndrSig _ _ ty <- bndrs]

pprFullRuleName :: Located (SourceText, RuleName) -> SDoc
pprFullRuleName (L _ (st, n)) = pprWithSourceText st (doubleQuotes $ ftext n)

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (RuleDecls p) where
  ppr (HsRules { rds_src = st
               , rds_rules = rules })
    = pprWithSourceText st (text "{-# RULES")
          <+> vcat (punctuate semi (map ppr rules)) <+> text "#-}"
  ppr (XRuleDecls x) = ppr x

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (RuleDecl p) where
  ppr (HsRule { rd_name = name
              , rd_act  = act
              , rd_tyvs = tys
              , rd_tmvs = tms
              , rd_lhs  = lhs
              , rd_rhs  = rhs })
        = sep [pprFullRuleName name <+> ppr act,
               nest 4 (pp_forall_ty tys <+> pp_forall_tm tys
                                        <+> pprExpr (unLoc lhs)),
               nest 6 (equals <+> pprExpr (unLoc rhs)) ]
        where
          pp_forall_ty Nothing     = empty
          pp_forall_ty (Just qtvs) = forAllLit <+> fsep (map ppr qtvs) <> dot
          pp_forall_tm Nothing | null tms = empty
          pp_forall_tm _ = forAllLit <+> fsep (map ppr tms) <> dot
  ppr (XRuleDecl x) = ppr x

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (RuleBndr p) where
   ppr (RuleBndr _ name) = ppr name
   ppr (RuleBndrSig _ name ty) = parens (ppr name <> dcolon <> ppr ty)
   ppr (XRuleBndr x) = ppr x

{-
************************************************************************
*                                                                      *
\subsection[DocDecl]{Document comments}
*                                                                      *
************************************************************************
-}

-- | Located Documentation comment Declaration
type LDocDecl = Located (DocDecl)

-- | Documentation comment Declaration
data DocDecl
  = DocCommentNext HsDocString
  | DocCommentPrev HsDocString
  | DocCommentNamed String HsDocString
  | DocGroup Int HsDocString
  deriving Data

-- Okay, I need to reconstruct the document comments, but for now:
instance Outputable DocDecl where
  ppr _ = text "<document comment>"

docDeclDoc :: DocDecl -> HsDocString
docDeclDoc (DocCommentNext d) = d
docDeclDoc (DocCommentPrev d) = d
docDeclDoc (DocCommentNamed _ d) = d
docDeclDoc (DocGroup _ d) = d

{-
************************************************************************
*                                                                      *
\subsection[DeprecDecl]{Deprecations}
*                                                                      *
************************************************************************

We use exported entities for things to deprecate.
-}

-- | Located Warning Declarations
type LWarnDecls pass = Located (WarnDecls pass)

 -- Note [Pragma source text] in BasicTypes
-- | Warning pragma Declarations
data WarnDecls pass = Warnings { wd_ext      :: XWarnings pass
                               , wd_src      :: SourceText
                               , wd_warnings :: [LWarnDecl pass]
                               }
  | XWarnDecls (XXWarnDecls pass)

type instance XWarnings      (GhcPass _) = NoExt
type instance XXWarnDecls    (GhcPass _) = NoExt

-- | Located Warning pragma Declaration
type LWarnDecl pass = Located (WarnDecl pass)

-- | Warning pragma Declaration
data WarnDecl pass = Warning (XWarning pass) [Located (IdP pass)] WarningTxt
                   | XWarnDecl (XXWarnDecl pass)

type instance XWarning      (GhcPass _) = NoExt
type instance XXWarnDecl    (GhcPass _) = NoExt


instance (p ~ GhcPass pass,OutputableBndr (IdP p))
        => Outputable (WarnDecls p) where
    ppr (Warnings _ (SourceText src) decls)
      = text src <+> vcat (punctuate comma (map ppr decls)) <+> text "#-}"
    ppr (Warnings _ NoSourceText _decls) = panic "WarnDecls"
    ppr (XWarnDecls x) = ppr x

instance (p ~ GhcPass pass, OutputableBndr (IdP p))
       => Outputable (WarnDecl p) where
    ppr (Warning _ thing txt)
      = hsep ( punctuate comma (map ppr thing))
              <+> ppr txt
    ppr (XWarnDecl x) = ppr x

{-
************************************************************************
*                                                                      *
\subsection[AnnDecl]{Annotations}
*                                                                      *
************************************************************************
-}

-- | Located Annotation Declaration
type LAnnDecl pass = Located (AnnDecl pass)

-- | Annotation Declaration
data AnnDecl pass = HsAnnotation
                      (XHsAnnotation pass)
                      SourceText -- Note [Pragma source text] in BasicTypes
                      (AnnProvenance (IdP pass)) (Located (HsExpr pass))
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
      --           'ApiAnnotation.AnnType'
      --           'ApiAnnotation.AnnModule'
      --           'ApiAnnotation.AnnClose'

      -- For details on above see note [Api annotations] in ApiAnnotation
  | XAnnDecl (XXAnnDecl pass)

type instance XHsAnnotation (GhcPass _) = NoExt
type instance XXAnnDecl     (GhcPass _) = NoExt

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (AnnDecl p) where
    ppr (HsAnnotation _ _ provenance expr)
      = hsep [text "{-#", pprAnnProvenance provenance, pprExpr (unLoc expr), text "#-}"]
    ppr (XAnnDecl x) = ppr x

-- | Annotation Provenance
data AnnProvenance name = ValueAnnProvenance (Located name)
                        | TypeAnnProvenance (Located name)
                        | ModuleAnnProvenance
deriving instance Functor     AnnProvenance
deriving instance Foldable    AnnProvenance
deriving instance Traversable AnnProvenance
deriving instance (Data pass) => Data (AnnProvenance pass)

annProvenanceName_maybe :: AnnProvenance name -> Maybe name
annProvenanceName_maybe (ValueAnnProvenance (L _ name)) = Just name
annProvenanceName_maybe (TypeAnnProvenance (L _ name))  = Just name
annProvenanceName_maybe ModuleAnnProvenance       = Nothing

pprAnnProvenance :: OutputableBndr name => AnnProvenance name -> SDoc
pprAnnProvenance ModuleAnnProvenance       = text "ANN module"
pprAnnProvenance (ValueAnnProvenance (L _ name))
  = text "ANN" <+> ppr name
pprAnnProvenance (TypeAnnProvenance (L _ name))
  = text "ANN type" <+> ppr name

{-
************************************************************************
*                                                                      *
\subsection[RoleAnnot]{Role annotations}
*                                                                      *
************************************************************************
-}

-- | Located Role Annotation Declaration
type LRoleAnnotDecl pass = Located (RoleAnnotDecl pass)

-- See #8185 for more info about why role annotations are
-- top-level declarations
-- | Role Annotation Declaration
data RoleAnnotDecl pass
  = RoleAnnotDecl (XCRoleAnnotDecl pass)
                  (Located (IdP pass))   -- type constructor
                  [Located (Maybe Role)] -- optional annotations
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
      --           'ApiAnnotation.AnnRole'

      -- For details on above see note [Api annotations] in ApiAnnotation
  | XRoleAnnotDecl (XXRoleAnnotDecl pass)

type instance XCRoleAnnotDecl (GhcPass _) = NoExt
type instance XXRoleAnnotDecl (GhcPass _) = NoExt

instance (p ~ GhcPass pass, OutputableBndr (IdP p))
       => Outputable (RoleAnnotDecl p) where
  ppr (RoleAnnotDecl _ ltycon roles)
    = text "type role" <+> pprPrefixOcc (unLoc ltycon) <+>
      hsep (map (pp_role . unLoc) roles)
    where
      pp_role Nothing  = underscore
      pp_role (Just r) = ppr r
  ppr (XRoleAnnotDecl x) = ppr x

roleAnnotDeclName :: RoleAnnotDecl pass -> (IdP pass)
roleAnnotDeclName (RoleAnnotDecl _ (L _ name) _) = name
roleAnnotDeclName (XRoleAnnotDecl _) = panic "roleAnnotDeclName"
