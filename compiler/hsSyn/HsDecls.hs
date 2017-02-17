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
{-# LANGUAGE FlexibleInstances #-}

-- | Abstract syntax of global declarations.
--
-- Definitions for: @SynDecl@ and @ConDecl@, @ClassDecl@,
-- @InstDecl@, @DefaultDecl@ and @ForeignDecl@.
module HsDecls (
  -- * Toplevel declarations
  HsDecl(..), LHsDecl, HsDataDefn(..), HsDeriving,
  HsDerivingClause(..), LHsDerivingClause,

  -- ** Class or type declarations
  TyClDecl(..), LTyClDecl,
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
  InstDecl(..), LInstDecl, NewOrData(..), FamilyInfo(..),
  TyFamInstDecl(..), LTyFamInstDecl, instDeclDataFamInsts,
  DataFamInstDecl(..), LDataFamInstDecl, pprDataFamInstFlavour,
  TyFamEqn(..), TyFamInstEqn, LTyFamInstEqn, TyFamDefltEqn, LTyFamDefltEqn,
  HsTyPats,
  LClsInstDecl, ClsInstDecl(..),

  -- ** Standalone deriving declarations
  DerivDecl(..), LDerivDecl,
  -- ** @RULE@ declarations
  LRuleDecls,RuleDecls(..),RuleDecl(..), LRuleDecl, RuleBndr(..),LRuleBndr,
  collectRuleBndrSigTys,
  flattenRuleDecls, pprFullRuleName,
  -- ** @VECTORISE@ declarations
  VectDecl(..), LVectDecl,
  lvectDeclName, lvectInstDecl,
  -- ** @default@ declarations
  DefaultDecl(..), LDefaultDecl,
  -- ** Template haskell declaration splice
  SpliceExplicitFlag(..),
  SpliceDecl(..), LSpliceDecl,
  -- ** Foreign function interface declarations
  ForeignDecl(..), LForeignDecl, ForeignImport(..), ForeignExport(..),
  noForeignImportCoercionYet, noForeignExportCoercionYet,
  CImportSpec(..),
  -- ** Data-constructor declarations
  ConDecl(..), LConDecl,
  HsConDeclDetails, hsConDeclArgTys,
  getConNames,
  getConDetails,
  gadtDeclDetails,
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
import {-# SOURCE #-}   HsExpr( LHsExpr, HsExpr, HsSplice, pprExpr,
                                pprSpliceDecl )
        -- Because Expr imports Decls via HsBracket

import HsBinds
import HsTypes
import HsDoc
import TyCon
import Name
import BasicTypes
import Coercion
import ForeignCall
import PlaceHolder ( PostTc,PostRn,PlaceHolder(..),DataId, OutputableBndrId )
import NameSet
import HsEmbellished

-- others:
import InstEnv
import Class
import Outputable
import Util
import SrcLoc

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

type LHsDecl id = Located (HsDecl id)
        -- ^ When in a list this may have
        --
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'
        --

-- For details on above see note [Api annotations] in ApiAnnotation

-- | A Haskell Declaration
data HsDecl id
  = TyClD       (TyClDecl id)      -- ^ Type or Class Declaration
  | InstD       (InstDecl  id)     -- ^ Instance declaration
  | DerivD      (DerivDecl id)     -- ^ Deriving declaration
  | ValD        (HsBind id)        -- ^ Value declaration
  | SigD        (Sig id)           -- ^ Signature declaration
  | DefD        (DefaultDecl id)   -- ^ 'default' declaration
  | ForD        (ForeignDecl id)   -- ^ Foreign declaration
  | WarningD    (WarnDecls id)     -- ^ Warning declaration
  | AnnD        (AnnDecl id)       -- ^ Annotation declaration
  | RuleD       (RuleDecls id)     -- ^ Rule declaration
  | VectD       (VectDecl id)      -- ^ Vectorise declaration
  | SpliceD     (SpliceDecl id)    -- ^ Splice declaration
                                   -- (Includes quasi-quotes)
  | DocD        (DocDecl)          -- ^ Documentation comment declaration
  | RoleAnnotD  (RoleAnnotDecl id) -- ^ Role annotation declaration
deriving instance (DataId id) => Data (HsDecl id)


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
data HsGroup id
  = HsGroup {
        hs_valds  :: HsValBinds id,
        hs_splcds :: [LSpliceDecl id],

        hs_tyclds :: [TyClGroup id],
                -- A list of mutually-recursive groups;
                -- This includes `InstDecl`s as well;
                -- Parser generates a singleton list;
                -- renamer does dependency analysis

        hs_derivds :: [LDerivDecl id],

        hs_fixds  :: [LFixitySig id],
                -- Snaffled out of both top-level fixity signatures,
                -- and those in class declarations

        hs_defds  :: [LDefaultDecl id],
        hs_fords  :: [LForeignDecl id],
        hs_warnds :: [LWarnDecls id],
        hs_annds  :: [LAnnDecl id],
        hs_ruleds :: [LRuleDecls id],
        hs_vects  :: [LVectDecl id],

        hs_docs   :: [LDocDecl]
  }
deriving instance (DataId id) => Data (HsGroup id)

emptyGroup, emptyRdrGroup, emptyRnGroup :: HsGroup a
emptyRdrGroup = emptyGroup { hs_valds = emptyValBindsIn }
emptyRnGroup  = emptyGroup { hs_valds = emptyValBindsOut }

hsGroupInstDecls :: HsGroup id -> [LInstDecl id]
hsGroupInstDecls = (=<<) group_instds . hs_tyclds

emptyGroup = HsGroup { hs_tyclds = [],
                       hs_derivds = [],
                       hs_fixds = [], hs_defds = [], hs_annds = [],
                       hs_fords = [], hs_warnds = [], hs_ruleds = [], hs_vects = [],
                       hs_valds = error "emptyGroup hs_valds: Can't happen",
                       hs_splcds = [],
                       hs_docs = [] }

appendGroups :: HsGroup a -> HsGroup a -> HsGroup a
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
        hs_vects = vects1,
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
        hs_vects  = vects2,
        hs_docs   = docs2 }
  =
    HsGroup {
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
        hs_vects  = vects1 ++ vects2,
        hs_docs   = docs1  ++ docs2 }

instance (OutputableBndrId name) => Outputable (HsDecl name) where
    ppr (TyClD dcl)             = ppr dcl
    ppr (ValD binds)            = ppr binds
    ppr (DefD def)              = ppr def
    ppr (InstD inst)            = ppr inst
    ppr (DerivD deriv)          = ppr deriv
    ppr (ForD fd)               = ppr fd
    ppr (SigD sd)               = ppr sd
    ppr (RuleD rd)              = ppr rd
    ppr (VectD vect)            = ppr vect
    ppr (WarningD wd)           = ppr wd
    ppr (AnnD ad)               = ppr ad
    ppr (SpliceD dd)            = ppr dd
    ppr (DocD doc)              = ppr doc
    ppr (RoleAnnotD ra)         = ppr ra

instance (OutputableBndrId name) => Outputable (HsGroup name) where
    ppr (HsGroup { hs_valds  = val_decls,
                   hs_tyclds = tycl_decls,
                   hs_derivds = deriv_decls,
                   hs_fixds  = fix_decls,
                   hs_warnds = deprec_decls,
                   hs_annds  = ann_decls,
                   hs_fords  = foreign_decls,
                   hs_defds  = default_decls,
                   hs_ruleds = rule_decls,
                   hs_vects  = vect_decls })
        = vcat_mb empty
            [ppr_ds fix_decls, ppr_ds default_decls,
             ppr_ds deprec_decls, ppr_ds ann_decls,
             ppr_ds rule_decls,
             ppr_ds vect_decls,
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

-- | Located Splice Declaration
type LSpliceDecl name = Located (SpliceDecl name)

-- | Splice Declaration
data SpliceDecl id
  = SpliceDecl                  -- Top level splice
        (Located (HsSplice id))
        SpliceExplicitFlag
deriving instance (DataId id) => Data (SpliceDecl id)

instance (OutputableBndrId name) => Outputable (SpliceDecl name) where
   ppr (SpliceDecl (L _ e) f) = pprSpliceDecl e f

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
type LTyClDecl name = Located (TyClDecl name)

-- | A type or class declaration.
data TyClDecl name
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
    FamDecl { tcdFam :: FamilyDecl name }

  | -- | @type@ declaration
    --
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
    --             'ApiAnnotation.AnnEqual',

    -- For details on above see note [Api annotations] in ApiAnnotation
    SynDecl { tcdLName  :: Located name           -- ^ Type constructor
            , tcdTyVars :: LHsQTyVars name        -- ^ Type variables; for an associated type
                                                  --   these include outer binders
            , tcdFixity :: LexicalFixity    -- ^ Fixity used in the declaration
            , tcdRhs    :: LHsType name           -- ^ RHS of type declaration
            , tcdFVs    :: PostRn name NameSet }

  | -- | @data@ declaration
    --
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnData',
    --              'ApiAnnotation.AnnFamily',
    --              'ApiAnnotation.AnnNewType',
    --              'ApiAnnotation.AnnNewType','ApiAnnotation.AnnDcolon'
    --              'ApiAnnotation.AnnWhere',

    -- For details on above see note [Api annotations] in ApiAnnotation
    DataDecl { tcdLName    :: Located name        -- ^ Type constructor
             , tcdTyVars   :: LHsQTyVars name  -- ^ Type variables; for an associated type
                                                  --   these include outer binders
                                                  -- Eg  class T a where
                                                  --       type F a :: *
                                                  --       type F a = a -> a
                                                  -- Here the type decl for 'f' includes 'a'
                                                  -- in its tcdTyVars
             , tcdFixity  :: LexicalFixity -- ^ Fixity used in the declaration
             , tcdDataDefn :: HsDataDefn name
             , tcdDataCusk :: PostRn name Bool    -- ^ does this have a CUSK?
             , tcdFVs      :: PostRn name NameSet }

  | ClassDecl { tcdCtxt    :: LHsContext name,          -- ^ Context...
                tcdLName   :: Located name,             -- ^ Name of the class
                tcdTyVars  :: LHsQTyVars name,          -- ^ Class type variables
                tcdFixity  :: LexicalFixity, -- ^ Fixity used in the declaration
                tcdFDs     :: [Located (FunDep (Located name))],
                                                        -- ^ Functional deps
                tcdSigs    :: [LSig name],              -- ^ Methods' signatures
                tcdMeths   :: LHsBinds name,            -- ^ Default methods
                tcdATs     :: [LFamilyDecl name],       -- ^ Associated types;
                tcdATDefs  :: [LTyFamDefltEqn name],    -- ^ Associated type defaults
                tcdDocs    :: [LDocDecl],               -- ^ Haddock docs
                tcdFVs     :: PostRn name NameSet
    }
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnClass',
        --           'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnClose'
        --   - The tcdFDs will have 'ApiAnnotation.AnnVbar',
        --                          'ApiAnnotation.AnnComma'
        --                          'ApiAnnotation.AnnRarrow'

        -- For details on above see note [Api annotations] in ApiAnnotation

deriving instance (DataId id) => Data (TyClDecl id)


-- Simple classifiers for TyClDecl
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | @True@ <=> argument is a @data@\/@newtype@
-- declaration.
isDataDecl :: TyClDecl name -> Bool
isDataDecl (DataDecl {}) = True
isDataDecl _other        = False

-- | type or type instance declaration
isSynDecl :: TyClDecl name -> Bool
isSynDecl (SynDecl {})   = True
isSynDecl _other        = False

-- | type class
isClassDecl :: TyClDecl name -> Bool
isClassDecl (ClassDecl {}) = True
isClassDecl _              = False

-- | type/data family declaration
isFamilyDecl :: TyClDecl name -> Bool
isFamilyDecl (FamDecl {})  = True
isFamilyDecl _other        = False

-- | type family declaration
isTypeFamilyDecl :: TyClDecl name -> Bool
isTypeFamilyDecl (FamDecl (FamilyDecl { fdInfo = info })) = case info of
  OpenTypeFamily      -> True
  ClosedTypeFamily {} -> True
  _                   -> False
isTypeFamilyDecl _ = False

-- | open type family info
isOpenTypeFamilyInfo :: FamilyInfo name -> Bool
isOpenTypeFamilyInfo OpenTypeFamily = True
isOpenTypeFamilyInfo _              = False

-- | closed type family info
isClosedTypeFamilyInfo :: FamilyInfo name -> Bool
isClosedTypeFamilyInfo (ClosedTypeFamily {}) = True
isClosedTypeFamilyInfo _                     = False

-- | data family declaration
isDataFamilyDecl :: TyClDecl name -> Bool
isDataFamilyDecl (FamDecl (FamilyDecl { fdInfo = DataFamily })) = True
isDataFamilyDecl _other      = False

-- Dealing with names

tyFamInstDeclName :: TyFamInstDecl name -> name
tyFamInstDeclName = unLoc . tyFamInstDeclLName

tyFamInstDeclLName :: TyFamInstDecl name -> Located name
tyFamInstDeclLName (TyFamInstDecl { tfid_eqn =
                     (L _ (TyFamEqn { tfe_tycon = ln })) })
  = ln

tyClDeclLName :: TyClDecl name -> Located name
tyClDeclLName (FamDecl { tcdFam = FamilyDecl { fdLName = ln } }) = ln
tyClDeclLName decl = tcdLName decl

tcdName :: TyClDecl name -> name
tcdName = unLoc . tyClDeclLName

tyClDeclTyVars :: TyClDecl name -> LHsQTyVars name
tyClDeclTyVars (FamDecl { tcdFam = FamilyDecl { fdTyVars = tvs } }) = tvs
tyClDeclTyVars d = tcdTyVars d

countTyClDecls :: [TyClDecl name] -> (Int, Int, Int, Int, Int)
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
-- See Note [Complete user-supplied kind signatures]
hsDeclHasCusk :: TyClDecl Name -> Bool
hsDeclHasCusk (FamDecl { tcdFam = fam_decl }) = famDeclHasCusk Nothing fam_decl
hsDeclHasCusk (SynDecl { tcdTyVars = tyvars, tcdRhs = rhs })
  -- NB: Keep this synchronized with 'getInitialKind'
  = hsTvbAllKinded tyvars && rhs_annotated rhs
  where
    rhs_annotated (L _ ty) = case ty of
      HsParTy lty  -> rhs_annotated lty
      HsKindSig {} -> True
      _            -> False
hsDeclHasCusk (DataDecl { tcdDataCusk = cusk }) = cusk
hsDeclHasCusk (ClassDecl { tcdTyVars = tyvars }) = hsTvbAllKinded tyvars

-- Pretty-printing TyClDecl
-- ~~~~~~~~~~~~~~~~~~~~~~~~

instance (OutputableBndrId name) => Outputable (TyClDecl name) where

    ppr (FamDecl { tcdFam = decl }) = ppr decl
    ppr (SynDecl { tcdLName = ltycon, tcdTyVars = tyvars, tcdFixity = fixity
                 , tcdRhs = rhs })
      = hang (text "type" <+>
              pp_vanilla_decl_head ltycon tyvars fixity [] <+> equals)
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
                    <+> pp_vanilla_decl_head lclas tyvars fixity (unLoc context)
                    <+> pprFundeps (map unLoc fds)

instance (OutputableBndrId name) => Outputable (TyClGroup name) where
  ppr (TyClGroup { group_tyclds = tyclds
                 , group_roles = roles
                 , group_instds = instds
                 }
      )
    = ppr tyclds $$
      ppr roles $$
      ppr instds

pp_vanilla_decl_head :: (OutputableBndrId name) => Located name
   -> LHsQTyVars name
   -> LexicalFixity
   -> HsContext name
   -> SDoc
pp_vanilla_decl_head thing (HsQTvs { hsq_explicit = tyvars }) fixity context
 = hsep [pprHsContext context, pp_tyvars tyvars]
  where
    pp_tyvars (varl:varsr)
      | fixity == Infix
         = hsep [ppr (unLoc varl), pprInfixOcc (unLoc thing)
         , hsep (map (ppr.unLoc) varsr)]
      | otherwise = hsep [ pprPrefixOcc (unLoc thing)
                  , hsep (map (ppr.unLoc) (varl:varsr))]
    pp_tyvars [] = ppr thing

pprTyClDeclFlavour :: TyClDecl a -> SDoc
pprTyClDeclFlavour (ClassDecl {})   = text "class"
pprTyClDeclFlavour (SynDecl {})     = text "type"
pprTyClDeclFlavour (FamDecl { tcdFam = FamilyDecl { fdInfo = info }})
  = pprFlavour info <+> text "family"
pprTyClDeclFlavour (DataDecl { tcdDataDefn = HsDataDefn { dd_ND = nd } })
  = ppr nd


{- Note [Complete user-supplied kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We kind-check declarations differently if they have a complete, user-supplied
kind signature (CUSK). This is because we can safely generalise a CUSKed
declaration before checking all of the others, supporting polymorphic recursion.
See ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindInference#Proposednewstrategy
and #9200 for lots of discussion of how we got here.

A declaration has a CUSK if we can know its complete kind without doing any
inference, at all. Here are the rules:

 - A class or datatype is said to have a CUSK if and only if all of its type
variables are annotated. Its result kind is, by construction, Constraint or *
respectively.

 - A type synonym has a CUSK if and only if all of its type variables and its
RHS are annotated with kinds.

 - A closed type family is said to have a CUSK if and only if all of its type
variables and its return type are annotated.

 - An open type family always has a CUSK -- unannotated type variables (and
return type) default to *.

 - Additionally, if -XTypeInType is on, then a data definition with a top-level
   :: must explicitly bind all kind variables to the right of the ::.
   See test dependent/should_compile/KindLevels, which requires this case.
   (Naturally, any kind variable mentioned before the :: should not be bound
   after it.)
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
data TyClGroup name  -- See Note [TyClGroups and dependency analysis]
  = TyClGroup { group_tyclds :: [LTyClDecl name]
              , group_roles  :: [LRoleAnnotDecl name]
              , group_instds :: [LInstDecl name] }
deriving instance (DataId id) => Data (TyClGroup id)

emptyTyClGroup :: TyClGroup name
emptyTyClGroup = TyClGroup [] [] []

tyClGroupTyClDecls :: [TyClGroup name] -> [LTyClDecl name]
tyClGroupTyClDecls = concatMap group_tyclds

tyClGroupInstDecls :: [TyClGroup name] -> [LInstDecl name]
tyClGroupInstDecls = concatMap group_instds

tyClGroupRoleDecls :: [TyClGroup name] -> [LRoleAnnotDecl name]
tyClGroupRoleDecls = concatMap group_roles

mkTyClGroup :: [LTyClDecl name] -> [LInstDecl name] -> TyClGroup name
mkTyClGroup decls instds = TyClGroup
  { group_tyclds = decls
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
type LFamilyResultSig name = Located (FamilyResultSig name)

-- | type Family Result Signature
data FamilyResultSig name = -- see Note [FamilyResultSig]
    NoSig
  -- ^ - 'ApiAnnotation.AnnKeywordId' :

  -- For details on above see note [Api annotations] in ApiAnnotation

  | KindSig  (LHsKind name)
  -- ^ - 'ApiAnnotation.AnnKeywordId' :
  --             'ApiAnnotation.AnnOpenP','ApiAnnotation.AnnDcolon',
  --             'ApiAnnotation.AnnCloseP'

  -- For details on above see note [Api annotations] in ApiAnnotation

  | TyVarSig (LHsTyVarBndr name)
  -- ^ - 'ApiAnnotation.AnnKeywordId' :
  --             'ApiAnnotation.AnnOpenP','ApiAnnotation.AnnDcolon',
  --             'ApiAnnotation.AnnCloseP', 'ApiAnnotation.AnnEqual'

  -- For details on above see note [Api annotations] in ApiAnnotation

deriving instance (DataId name) => Data (FamilyResultSig name)

-- | Located type Family Declaration
type LFamilyDecl name = Located (FamilyDecl name)

-- | type Family Declaration
data FamilyDecl name = FamilyDecl
  { fdInfo           :: FamilyInfo name              -- type/data, closed/open
  , fdLName          :: Located name                 -- type constructor
  , fdTyVars         :: LHsQTyVars name              -- type variables
  , fdFixity         :: LexicalFixity         -- Fixity used in the declaration
  , fdResultSig      :: LFamilyResultSig name        -- result signature
  , fdInjectivityAnn :: Maybe (LInjectivityAnn name) -- optional injectivity ann
  }
  -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
  --             'ApiAnnotation.AnnData', 'ApiAnnotation.AnnFamily',
  --             'ApiAnnotation.AnnWhere', 'ApiAnnotation.AnnOpenP',
  --             'ApiAnnotation.AnnDcolon', 'ApiAnnotation.AnnCloseP',
  --             'ApiAnnotation.AnnEqual', 'ApiAnnotation.AnnRarrow',
  --             'ApiAnnotation.AnnVbar'

  -- For details on above see note [Api annotations] in ApiAnnotation

deriving instance (DataId id) => Data (FamilyDecl id)

-- | Located Injectivity Annotation
type LInjectivityAnn name = Located (InjectivityAnn name)

-- | If the user supplied an injectivity annotation it is represented using
-- InjectivityAnn. At the moment this is a single injectivity condition - see
-- Note [Injectivity annotation]. `Located name` stores the LHS of injectivity
-- condition. `[Located name]` stores the RHS of injectivity condition. Example:
--
--   type family Foo a b c = r | r -> a c where ...
--
-- This will be represented as "InjectivityAnn `r` [`a`, `c`]"
data InjectivityAnn name
  = InjectivityAnn (Located name) [Located name]
  -- ^ - 'ApiAnnotation.AnnKeywordId' :
  --             'ApiAnnotation.AnnRarrow', 'ApiAnnotation.AnnVbar'

  -- For details on above see note [Api annotations] in ApiAnnotation
  deriving Data

data FamilyInfo name
  = DataFamily
  | OpenTypeFamily
     -- | 'Nothing' if we're in an hs-boot file and the user
     -- said "type family Foo x where .."
  | ClosedTypeFamily (Maybe [LTyFamInstEqn name])
deriving instance (DataId name) => Data (FamilyInfo name)

-- | Does this family declaration have a complete, user-supplied kind signature?
famDeclHasCusk :: Maybe Bool
                   -- ^ if associated, does the enclosing class have a CUSK?
               -> FamilyDecl name -> Bool
famDeclHasCusk _ (FamilyDecl { fdInfo      = ClosedTypeFamily _
                             , fdTyVars    = tyvars
                             , fdResultSig = L _ resultSig })
  = hsTvbAllKinded tyvars && hasReturnKindSignature resultSig
famDeclHasCusk mb_class_cusk _ = mb_class_cusk `orElse` True
        -- all un-associated open families have CUSKs!

-- | Does this family declaration have user-supplied return kind signature?
hasReturnKindSignature :: FamilyResultSig a -> Bool
hasReturnKindSignature NoSig                          = False
hasReturnKindSignature (TyVarSig (L _ (UserTyVar _))) = False
hasReturnKindSignature _                              = True

-- | Maybe return name of the result type variable
resultVariableName :: FamilyResultSig a -> Maybe a
resultVariableName (TyVarSig sig) = Just $ hsLTyVarName sig
resultVariableName _              = Nothing

instance (OutputableBndrId name) => Outputable (FamilyDecl name) where
  ppr = pprFamilyDecl TopLevel

pprFamilyDecl :: (OutputableBndrId name)
              => TopLevelFlag -> FamilyDecl name -> SDoc
pprFamilyDecl top_level (FamilyDecl { fdInfo = info, fdLName = ltycon
                                    , fdTyVars = tyvars
                                    , fdFixity = fixity
                                    , fdResultSig = L _ result
                                    , fdInjectivityAnn = mb_inj })
  = vcat [ pprFlavour info <+> pp_top_level <+>
           pp_vanilla_decl_head ltycon tyvars fixity [] <+>
           pp_kind <+> pp_inj <+> pp_where
         , nest 2 $ pp_eqns ]
  where
    pp_top_level = case top_level of
                     TopLevel    -> text "family"
                     NotTopLevel -> empty

    pp_kind = case result of
                NoSig            -> empty
                KindSig  kind    -> dcolon <+> ppr kind
                TyVarSig tv_bndr -> text "=" <+> ppr tv_bndr
    pp_inj = case mb_inj of
               Just (L _ (InjectivityAnn lhs rhs)) ->
                 hsep [ vbar, ppr lhs, text "->", hsep (map ppr rhs) ]
               Nothing -> empty
    (pp_where, pp_eqns) = case info of
      ClosedTypeFamily mb_eqns ->
        ( text "where"
        , case mb_eqns of
            Nothing   -> text ".."
            Just eqns -> vcat $ map ppr_fam_inst_eqn eqns )
      _ -> (empty, empty)

pprFlavour :: FamilyInfo name -> SDoc
pprFlavour DataFamily            = text "data"
pprFlavour OpenTypeFamily        = text "type"
pprFlavour (ClosedTypeFamily {}) = text "type"

instance Outputable (FamilyInfo name) where
  ppr info = pprFlavour info <+> text "family"



{- *********************************************************************
*                                                                      *
               Data types and data constructors
*                                                                      *
********************************************************************* -}

-- | Haskell Data type Definition
data HsDataDefn name   -- The payload of a data type defn
                       -- Used *both* for vanilla data declarations,
                       --       *and* for data family instances
  = -- | Declares a data type or newtype, giving its constructors
    -- @
    --  data/newtype T a = <constrs>
    --  data/newtype instance T [a] = <constrs>
    -- @
    HsDataDefn { dd_ND     :: NewOrData,
                 dd_ctxt   :: LHsContext name,           -- ^ Context
                 dd_cType  :: Maybe (Located CType),
                 dd_kindSig:: Maybe (LHsKind name),
                     -- ^ Optional kind signature.
                     --
                     -- @(Just k)@ for a GADT-style @data@,
                     -- or @data instance@ decl, with explicit kind sig
                     --
                     -- Always @Nothing@ for H98-syntax decls

                 dd_cons   :: [LConDecl name],
                     -- ^ Data constructors
                     --
                     -- For @data T a = T1 | T2 a@
                     --   the 'LConDecl's all have 'ConDeclH98'.
                     -- For @data T a where { T1 :: T a }@
                     --   the 'LConDecls' all have 'ConDeclGADT'.

                 dd_derivs :: HsDeriving name  -- ^ Optional 'deriving' claues

             -- For details on above see note [Api annotations] in ApiAnnotation
   }
deriving instance (DataId id) => Data (HsDataDefn id)

-- | Haskell Deriving clause
type HsDeriving name = Located [LHsDerivingClause name]
  -- ^ The optional @deriving@ clauses of a data declaration. "Clauses" is
  -- plural because one can specify multiple deriving clauses using the
  -- @-XDerivingStrategies@ language extension.
  --
  -- The list of 'LHsDerivingClause's corresponds to exactly what the user
  -- requested to derive, in order. If no deriving clauses were specified,
  -- the list is empty.

type LHsDerivingClause name = Located (HsDerivingClause name)

-- | A single @deriving@ clause of a data declaration.
--
--  - 'ApiAnnotation.AnnKeywordId' :
--       'ApiAnnotation.AnnDeriving', 'ApiAnnotation.AnnStock',
--       'ApiAnnotation.AnnAnyClass', 'Api.AnnNewtype',
--       'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'
data HsDerivingClause name
  -- See Note [Deriving strategies] in TcDeriv
  = HsDerivingClause
    { deriv_clause_strategy :: Maybe (Located DerivStrategy)
      -- ^ The user-specified strategy (if any) to use when deriving
      -- 'deriv_clause_tys'.
    , deriv_clause_tys :: Located [LHsSigType name]
      -- ^ The types to derive.
      --
      -- It uses 'LHsSigType's because, with @-XGeneralizedNewtypeDeriving@,
      -- we can mention type variables that aren't bound by the datatype, e.g.
      --
      -- > data T b = ... deriving (C [a])
      --
      -- should produce a derived instance for @C [a] (T b)@.
    }
deriving instance (DataId id) => Data (HsDerivingClause id)

instance (OutputableBndrId name)
       => Outputable (HsDerivingClause name) where
  ppr (HsDerivingClause { deriv_clause_strategy = dcs
                        , deriv_clause_tys      = L _ dct })
    = hsep [ text "deriving"
           , ppDerivStrategy dcs
           , pp_dct dct ]
      where
        -- This complexity is to distinguish between
        --    deriving Show
        --    deriving (Show)
        pp_dct [a@(HsIB _ (L _ HsAppsTy{}))] = parens (ppr a)
        pp_dct [a] = ppr a
        pp_dct _   = parens (interpp'SP dct)

data NewOrData
  = NewType                     -- ^ @newtype Blah ...@
  | DataType                    -- ^ @data Blah ...@
  deriving( Eq, Data )                -- Needed because Demand derives Eq

-- | Located data Constructor Declaration
type LConDecl name = Located (ConDecl name)
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
data ConDecl name
  = ConDeclGADT
      { con_names   :: [LEmbellished name]
      , con_type    :: LHsSigType name
        -- ^ The type after the ‘::’
      , con_doc     :: Maybe LHsDocString
          -- ^ A possible Haddock comment.
      }

  | ConDeclH98
      { con_name    :: LEmbellished name

      , con_qvars     :: Maybe (LHsQTyVars name)
        -- User-written forall (if any), and its implicit
        -- kind variables
        -- Non-Nothing needs -XExistentialQuantification
        --               e.g. data T a = forall b. MkT b (b->a)
        --               con_qvars = {b}

      , con_cxt       :: Maybe (LHsContext name)
        -- ^ User-written context (if any)

      , con_details   :: HsConDeclDetails name
          -- ^ Arguments

      , con_doc       :: Maybe LHsDocString
          -- ^ A possible Haddock comment.
      }
deriving instance (DataId name) => Data (ConDecl name)

-- | Haskell data Constructor Declaration Details
type HsConDeclDetails name
   = HsConDetails (LBangType name) (Located [LConDeclField name])

getConNames :: ConDecl name -> [LEmbellished name]
getConNames ConDeclH98  {con_name  = name}  = [name]
getConNames ConDeclGADT {con_names = names} = names

-- don't call with RdrNames, because it can't deal with HsAppsTy
getConDetails :: ConDecl name -> HsConDeclDetails name
getConDetails ConDeclH98  {con_details  = details} = details
getConDetails ConDeclGADT {con_type     = ty     } = details
  where
    (details,_,_,_) = gadtDeclDetails ty

-- don't call with RdrNames, because it can't deal with HsAppsTy
gadtDeclDetails :: LHsSigType name
                -> ( HsConDeclDetails name
                   , LHsType name
                   , LHsContext name
                   , [LHsTyVarBndr name] )
gadtDeclDetails HsIB {hsib_body = lbody_ty} = (details,res_ty,cxt,tvs)
  where
    (tvs, cxt, tau) = splitLHsSigmaTy lbody_ty
    (details, res_ty)           -- See Note [Sorting out the result type]
      = case tau of
          L _ (HsFunTy (L l (HsRecTy flds)) res_ty')
                  -> (RecCon (L l flds), res_ty')
          _other  -> (PrefixCon [], tau)

hsConDeclArgTys :: HsConDeclDetails name -> [LBangType name]
hsConDeclArgTys (PrefixCon tys)    = tys
hsConDeclArgTys (InfixCon ty1 ty2) = [ty1,ty2]
hsConDeclArgTys (RecCon flds)      = map (cd_fld_type . unLoc) (unLoc flds)

pp_data_defn :: (OutputableBndrId name)
                  => (HsContext name -> SDoc)   -- Printing the header
                  -> HsDataDefn name
                  -> SDoc
pp_data_defn pp_hdr (HsDataDefn { dd_ND = new_or_data, dd_ctxt = L _ context
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

instance (OutputableBndrId name) => Outputable (HsDataDefn name) where
   ppr d = pp_data_defn (\_ -> text "Naked HsDataDefn") d

instance Outputable NewOrData where
  ppr NewType  = text "newtype"
  ppr DataType = text "data"

pp_condecls :: (OutputableBndrId name) => [LConDecl name] -> SDoc
pp_condecls cs@(L _ ConDeclGADT{} : _) -- In GADT syntax
  = hang (text "where") 2 (vcat (map ppr cs))
pp_condecls cs                    -- In H98 syntax
  = equals <+> sep (punctuate (text " |") (map ppr cs))

instance (OutputableBndrId name) => Outputable (ConDecl name) where
    ppr = pprConDecl

pprConDecl :: (OutputableBndrId name) => ConDecl name -> SDoc
pprConDecl (ConDeclH98 { con_name = L _ con
                       , con_qvars = mtvs
                       , con_cxt = mcxt
                       , con_details = details
                       , con_doc = doc })
  = sep [ppr_mbDoc doc, pprHsForAll tvs cxt,         ppr_details details]
  where
    ppr_details (InfixCon t1 t2) = hsep [ppr t1, pprInfixOcc con, ppr t2]
    ppr_details (PrefixCon tys)  = hsep (pprPrefixOcc con
                                   : map (pprParendHsType . unLoc) tys)
    ppr_details (RecCon fields)  = pprPrefixOcc con
                                 <+> pprConDeclFields (unLoc fields)
    tvs = case mtvs of
      Nothing -> []
      Just (HsQTvs { hsq_explicit = tvs }) -> tvs

    cxt = fromMaybe (noLoc []) mcxt

pprConDecl (ConDeclGADT { con_names = cons, con_type = res_ty, con_doc = doc })
  = sep [ppr_mbDoc doc <+> ppr_con_names cons <+> dcolon
         <+> ppr res_ty]

ppr_con_names :: (OutputableBndr name) => [Located name] -> SDoc
ppr_con_names = pprWithCommas (pprPrefixOcc . unLoc)

{-
************************************************************************
*                                                                      *
                Instance declarations
*                                                                      *
************************************************************************

Note [Type family instance declarations in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The data type TyFamEqn represents one equation of a type family instance.
It is parameterised over its tfe_pats field:

 * An ordinary type family instance declaration looks like this in source Haskell
      type instance T [a] Int = a -> a
   (or something similar for a closed family)
   It is represented by a TyFamInstEqn, with *type* in the tfe_pats field.

 * On the other hand, the *default instance* of an associated type looks like
   this in source Haskell
      class C a where
        type T a b
        type T a b = a -> b   -- The default instance
   It is represented by a TyFamDefltEqn, with *type variables* in the tfe_pats
   field.
-}

----------------- Type synonym family instances -------------

-- | Located Type Family Instance Equation
type LTyFamInstEqn  name = Located (TyFamInstEqn  name)
  -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'
  --   when in a list

-- For details on above see note [Api annotations] in ApiAnnotation

-- | Located Type Family Default Equation
type LTyFamDefltEqn name = Located (TyFamDefltEqn name)

-- | Haskell Type Patterns
type HsTyPats name = HsImplicitBndrs name [LHsType name]
            -- ^ Type patterns (with kind and type bndrs)
            -- See Note [Family instance declaration binders]

{- Note [Family instance declaration binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The HsTyPats field is LHS patterns or a type/data family instance.

The hsib_vars of the HsImplicitBndrs are the template variables of the
type patterns, i.e. fv(pat_tys).  Note in particular

* The hsib_vars *includes* any anonymous wildcards.  For example
     type instance F a _ = a
  The hsib_vars will be {a, _}.  Remember that each separate wildcard
  '_' gets its own unique.  In this context wildcards behave just like
  an ordinary type variable, only anonymous.

* The hsib_vars *including* type variables that are already in scope

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
-}

-- | Type Family Instance Equation
type TyFamInstEqn  name = TyFamEqn name (HsTyPats name)

-- | Type Family Default Equation
type TyFamDefltEqn name = TyFamEqn name (LHsQTyVars name)
  -- See Note [Type family instance declarations in HsSyn]

-- | Type Family Equation
--
-- One equation in a type family instance declaration
-- See Note [Type family instance declarations in HsSyn]
data TyFamEqn name pats
  = TyFamEqn
       { tfe_tycon  :: Located name
       , tfe_pats   :: pats
       , tfe_fixity :: LexicalFixity    -- ^ Fixity used in the declaration
       , tfe_rhs    :: LHsType name }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnEqual'

    -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (DataId name, Data pats) => Data (TyFamEqn name pats)

-- | Located Type Family Instance Declaration
type LTyFamInstDecl name = Located (TyFamInstDecl name)

-- | Type Family Instance Declaration
data TyFamInstDecl name
  = TyFamInstDecl
       { tfid_eqn  :: LTyFamInstEqn name
       , tfid_fvs  :: PostRn name NameSet }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
    --           'ApiAnnotation.AnnInstance',

    -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (DataId name) => Data (TyFamInstDecl name)

----------------- Data family instances -------------

-- | Located Data Family Instance Declaration
type LDataFamInstDecl name = Located (DataFamInstDecl name)

-- | Data Family Instance Declaration
data DataFamInstDecl name
  = DataFamInstDecl
       { dfid_tycon     :: Located name
       , dfid_pats      :: HsTyPats   name       -- LHS
       , dfid_fixity    :: LexicalFixity    -- ^ Fixity used in the declaration
       , dfid_defn      :: HsDataDefn name       -- RHS
       , dfid_fvs       :: PostRn name NameSet } -- Free vars for dependency analysis
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnData',
    --           'ApiAnnotation.AnnNewType','ApiAnnotation.AnnInstance',
    --           'ApiAnnotation.AnnDcolon'
    --           'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpen',
    --           'ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (DataId name) => Data (DataFamInstDecl name)


----------------- Class instances -------------

-- | Located Class Instance Declaration
type LClsInstDecl name = Located (ClsInstDecl name)

-- | Class Instance Declaration
data ClsInstDecl name
  = ClsInstDecl
      { cid_poly_ty :: LHsSigType name    -- Context => Class Instance-type
                                          -- Using a polytype means that the renamer conveniently
                                          -- figures out the quantified type variables for us.
      , cid_binds         :: LHsBinds name           -- Class methods
      , cid_sigs          :: [LSig name]             -- User-supplied pragmatic info
      , cid_tyfam_insts   :: [LTyFamInstDecl name]   -- Type family instances
      , cid_datafam_insts :: [LDataFamInstDecl name] -- Data family instances
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
deriving instance (DataId id) => Data (ClsInstDecl id)


----------------- Instances of all kinds -------------

-- | Located Instance Declaration
type LInstDecl name = Located (InstDecl name)

-- | Instance Declaration
data InstDecl name  -- Both class and family instances
  = ClsInstD
      { cid_inst  :: ClsInstDecl name }
  | DataFamInstD              -- data family instance
      { dfid_inst :: DataFamInstDecl name }
  | TyFamInstD              -- type family instance
      { tfid_inst :: TyFamInstDecl name }
deriving instance (DataId id) => Data (InstDecl id)

instance (OutputableBndrId name) => Outputable (TyFamInstDecl name) where
  ppr = pprTyFamInstDecl TopLevel

pprTyFamInstDecl :: (OutputableBndrId name)
                 => TopLevelFlag -> TyFamInstDecl name -> SDoc
pprTyFamInstDecl top_lvl (TyFamInstDecl { tfid_eqn = eqn })
   = text "type" <+> ppr_instance_keyword top_lvl <+> ppr_fam_inst_eqn eqn

ppr_instance_keyword :: TopLevelFlag -> SDoc
ppr_instance_keyword TopLevel    = text "instance"
ppr_instance_keyword NotTopLevel = empty

ppr_fam_inst_eqn :: (OutputableBndrId name) => LTyFamInstEqn name -> SDoc
ppr_fam_inst_eqn (L _ (TyFamEqn { tfe_tycon = tycon
                                , tfe_pats  = pats
                                , tfe_fixity = fixity
                                , tfe_rhs   = rhs }))
    = pp_fam_inst_lhs tycon pats fixity [] <+> equals <+> ppr rhs

ppr_fam_deflt_eqn :: (OutputableBndrId name) => LTyFamDefltEqn name -> SDoc
ppr_fam_deflt_eqn (L _ (TyFamEqn { tfe_tycon = tycon
                                 , tfe_pats  = tvs
                                 , tfe_fixity = fixity
                                 , tfe_rhs   = rhs }))
    = text "type" <+> pp_vanilla_decl_head tycon tvs fixity []
                  <+> equals <+> ppr rhs

instance (OutputableBndrId name) => Outputable (DataFamInstDecl name) where
  ppr = pprDataFamInstDecl TopLevel

pprDataFamInstDecl :: (OutputableBndrId name)
                   => TopLevelFlag -> DataFamInstDecl name -> SDoc
pprDataFamInstDecl top_lvl (DataFamInstDecl { dfid_tycon = tycon
                                            , dfid_pats  = pats
                                            , dfid_fixity = fixity
                                            , dfid_defn  = defn })
  = pp_data_defn pp_hdr defn
  where
    pp_hdr ctxt = ppr_instance_keyword top_lvl
              <+> pp_fam_inst_lhs tycon pats fixity ctxt

pprDataFamInstFlavour :: DataFamInstDecl name -> SDoc
pprDataFamInstFlavour (DataFamInstDecl { dfid_defn = (HsDataDefn { dd_ND = nd }) })
  = ppr nd

pp_fam_inst_lhs :: (OutputableBndrId name) => Located name
   -> HsTyPats name
   -> LexicalFixity
   -> HsContext name
   -> SDoc
pp_fam_inst_lhs thing (HsIB { hsib_body = typats }) fixity context
                                              -- explicit type patterns
   = hsep [ pprHsContext context, pp_pats typats]
   where
     pp_pats (patl:patsr)
       | fixity == Infix
          = hsep [pprParendHsType (unLoc patl), pprInfixOcc (unLoc thing)
          , hsep (map (pprParendHsType.unLoc) patsr)]
       | otherwise = hsep [ pprPrefixOcc (unLoc thing)
                   , hsep (map (pprParendHsType.unLoc) (patl:patsr))]
     pp_pats [] = empty

instance (OutputableBndrId name) => Outputable (ClsInstDecl name) where
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

ppDerivStrategy :: Maybe (Located DerivStrategy) -> SDoc
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


instance (OutputableBndrId name) => Outputable (InstDecl name) where
    ppr (ClsInstD     { cid_inst  = decl }) = ppr decl
    ppr (TyFamInstD   { tfid_inst = decl }) = ppr decl
    ppr (DataFamInstD { dfid_inst = decl }) = ppr decl

-- Extract the declarations of associated data types from an instance

instDeclDataFamInsts :: [LInstDecl name] -> [DataFamInstDecl name]
instDeclDataFamInsts inst_decls
  = concatMap do_one inst_decls
  where
    do_one (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = fam_insts } }))
      = map unLoc fam_insts
    do_one (L _ (DataFamInstD { dfid_inst = fam_inst }))      = [fam_inst]
    do_one (L _ (TyFamInstD {}))                              = []

{-
************************************************************************
*                                                                      *
\subsection[DerivDecl]{A stand-alone instance deriving declaration}
*                                                                      *
************************************************************************
-}

-- | Located Deriving Declaration
type LDerivDecl name = Located (DerivDecl name)

-- | Deriving Declaration
data DerivDecl name = DerivDecl
        { deriv_type         :: LHsSigType name
        , deriv_strategy     :: Maybe (Located DerivStrategy)
        , deriv_overlap_mode :: Maybe (Located OverlapMode)
         -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDeriving',
         --        'ApiAnnotation.AnnInstance', 'ApiAnnotation.AnnStock',
         --        'ApiAnnotation.AnnAnyClass', 'Api.AnnNewtype',
         --        'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
        }
deriving instance (DataId name) => Data (DerivDecl name)

instance (OutputableBndrId name) => Outputable (DerivDecl name) where
    ppr (DerivDecl { deriv_type = ty
                   , deriv_strategy = ds
                   , deriv_overlap_mode = o })
        = hsep [ text "deriving"
               , ppDerivStrategy ds
               , text "instance"
               , ppOverlapPragma o
               , ppr ty ]

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
type LDefaultDecl name = Located (DefaultDecl name)

-- | Default Declaration
data DefaultDecl name
  = DefaultDecl [LHsType name]
        -- ^ - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnDefault',
        --          'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (DataId name) => Data (DefaultDecl name)

instance (OutputableBndrId name) => Outputable (DefaultDecl name) where

    ppr (DefaultDecl tys)
      = text "default" <+> parens (interpp'SP tys)

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
type LForeignDecl name = Located (ForeignDecl name)

-- | Foreign Declaration
data ForeignDecl name
  = ForeignImport
      { fd_name   :: Located name          -- defines this name
      , fd_sig_ty :: LHsSigType name       -- sig_ty
      , fd_co     :: PostTc name Coercion  -- rep_ty ~ sig_ty
      , fd_fi     :: ForeignImport }

  | ForeignExport
      { fd_name   :: Located name          -- uses this name
      , fd_sig_ty :: LHsSigType name       -- sig_ty
      , fd_co     :: PostTc name Coercion  -- rep_ty ~ sig_ty
      , fd_fe     :: ForeignExport }
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnForeign',
        --           'ApiAnnotation.AnnImport','ApiAnnotation.AnnExport',
        --           'ApiAnnotation.AnnDcolon'

        -- For details on above see note [Api annotations] in ApiAnnotation

deriving instance (DataId name) => Data (ForeignDecl name)
{-
    In both ForeignImport and ForeignExport:
        sig_ty is the type given in the Haskell code
        rep_ty is the representation for this type, i.e. with newtypes
               coerced away and type functions evaluated.
    Thus if the declaration is valid, then rep_ty will only use types
    such as Int and IO that we know how to make foreign calls with.
-}

noForeignImportCoercionYet :: PlaceHolder
noForeignImportCoercionYet = PlaceHolder

noForeignExportCoercionYet :: PlaceHolder
noForeignExportCoercionYet = PlaceHolder

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

instance (OutputableBndrId name) => Outputable (ForeignDecl name) where
  ppr (ForeignImport { fd_name = n, fd_sig_ty = ty, fd_fi = fimport })
    = hang (text "foreign import" <+> ppr fimport <+> ppr n)
         2 (dcolon <+> ppr ty)
  ppr (ForeignExport { fd_name = n, fd_sig_ty = ty, fd_fe = fexport }) =
    hang (text "foreign export" <+> ppr fexport <+> ppr n)
       2 (dcolon <+> ppr ty)

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
type LRuleDecls name = Located (RuleDecls name)

  -- Note [Pragma source text] in BasicTypes
-- | Rule Declarations
data RuleDecls name = HsRules { rds_src   :: SourceText
                              , rds_rules :: [LRuleDecl name] }
deriving instance (DataId name) => Data (RuleDecls name)

-- | Located Rule Declaration
type LRuleDecl name = Located (RuleDecl name)

-- | Rule Declaration
data RuleDecl name
  = HsRule                             -- Source rule
        (Located (SourceText,RuleName)) -- Rule name
               -- Note [Pragma source text] in BasicTypes
        Activation
        [LRuleBndr name]        -- Forall'd vars; after typechecking this
                                --   includes tyvars
        (Located (HsExpr name)) -- LHS
        (PostRn name NameSet)   -- Free-vars from the LHS
        (Located (HsExpr name)) -- RHS
        (PostRn name NameSet)   -- Free-vars from the RHS
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' :
        --           'ApiAnnotation.AnnOpen','ApiAnnotation.AnnTilde',
        --           'ApiAnnotation.AnnVal',
        --           'ApiAnnotation.AnnClose',
        --           'ApiAnnotation.AnnForall','ApiAnnotation.AnnDot',
        --           'ApiAnnotation.AnnEqual',

        -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (DataId name) => Data (RuleDecl name)

flattenRuleDecls :: [LRuleDecls name] -> [LRuleDecl name]
flattenRuleDecls decls = concatMap (rds_rules . unLoc) decls

-- | Located Rule Binder
type LRuleBndr name = Located (RuleBndr name)

-- | Rule Binder
data RuleBndr name
  = RuleBndr (Located name)
  | RuleBndrSig (Located name) (LHsSigWcType name)
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --     'ApiAnnotation.AnnDcolon','ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (DataId name) => Data (RuleBndr name)

collectRuleBndrSigTys :: [RuleBndr name] -> [LHsSigWcType name]
collectRuleBndrSigTys bndrs = [ty | RuleBndrSig _ ty <- bndrs]

pprFullRuleName :: Located (SourceText, RuleName) -> SDoc
pprFullRuleName (L _ (st, n)) = pprWithSourceText st (doubleQuotes $ ftext n)

instance (OutputableBndrId name) => Outputable (RuleDecls name) where
  ppr (HsRules st rules)
    = pprWithSourceText st (text "{-# RULES")
          <+> vcat (punctuate semi (map ppr rules)) <+> text "#-}"

instance (OutputableBndrId name) => Outputable (RuleDecl name) where
  ppr (HsRule name act ns lhs _fv_lhs rhs _fv_rhs)
        = sep [pprFullRuleName name <+> ppr act,
               nest 4 (pp_forall <+> pprExpr (unLoc lhs)),
               nest 6 (equals <+> pprExpr (unLoc rhs)) ]
        where
          pp_forall | null ns   = empty
                    | otherwise = forAllLit <+> fsep (map ppr ns) <> dot

instance (OutputableBndrId name) => Outputable (RuleBndr name) where
   ppr (RuleBndr name) = ppr name
   ppr (RuleBndrSig name ty) = parens (ppr name <> dcolon <> ppr ty)

{-
************************************************************************
*                                                                      *
\subsection{Vectorisation declarations}
*                                                                      *
************************************************************************

A vectorisation pragma, one of

  {-# VECTORISE f = closure1 g (scalar_map g) #-}
  {-# VECTORISE SCALAR f #-}
  {-# NOVECTORISE f #-}

  {-# VECTORISE type T = ty #-}
  {-# VECTORISE SCALAR type T #-}
-}

-- | Located Vectorise Declaration
type LVectDecl name = Located (VectDecl name)

-- | Vectorise Declaration
data VectDecl name
  = HsVect
      SourceText   -- Note [Pragma source text] in BasicTypes
      (LEmbellished name)
      (LHsExpr name)
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnEqual','ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | HsNoVect
      SourceText   -- Note [Pragma source text] in BasicTypes
      (LEmbellished name)
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --                                    'ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | HsVectTypeIn                -- pre type-checking
      SourceText                -- Note [Pragma source text] in BasicTypes
      Bool                      -- 'TRUE' => SCALAR declaration
      (LEmbellished name)
      (Maybe (LEmbellished name))    -- 'Nothing' => no right-hand side
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnType','ApiAnnotation.AnnClose',
        --           'ApiAnnotation.AnnEqual'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | HsVectTypeOut               -- post type-checking
      Bool                      -- 'TRUE' => SCALAR declaration
      TyCon
      (Maybe TyCon)             -- 'Nothing' => no right-hand side
  | HsVectClassIn               -- pre type-checking
      SourceText                -- Note [Pragma source text] in BasicTypes
      (LEmbellished name)
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnClass','ApiAnnotation.AnnClose',

       -- For details on above see note [Api annotations] in ApiAnnotation
  | HsVectClassOut              -- post type-checking
      Class
  | HsVectInstIn                -- pre type-checking (always SCALAR)  !!!FIXME: should be superfluous now
      (LHsSigType name)
  | HsVectInstOut               -- post type-checking (always SCALAR) !!!FIXME: should be superfluous now
      ClsInst
deriving instance (DataId name) => Data (VectDecl name)

lvectDeclName :: NamedThing name => LVectDecl name -> Name
lvectDeclName (L _ (HsVect _       (L _ name) _))    = getName $ unEmb name
lvectDeclName (L _ (HsNoVect _     (L _ name)))      = getName $ unEmb name
lvectDeclName (L _ (HsVectTypeIn _  _ (L _ name) _)) = getName $ unEmb name
lvectDeclName (L _ (HsVectTypeOut  _ tycon _))       = getName tycon
lvectDeclName (L _ (HsVectClassIn _ (L _ name)))     = getName $ unEmb name
lvectDeclName (L _ (HsVectClassOut cls))             = getName cls
lvectDeclName (L _ (HsVectInstIn _))
  = panic "HsDecls.lvectDeclName: HsVectInstIn"
lvectDeclName (L _ (HsVectInstOut  _))
  = panic "HsDecls.lvectDeclName: HsVectInstOut"

lvectInstDecl :: LVectDecl name -> Bool
lvectInstDecl (L _ (HsVectInstIn _))  = True
lvectInstDecl (L _ (HsVectInstOut _)) = True
lvectInstDecl _                       = False

instance (OutputableBndrId name) => Outputable (VectDecl name) where
  ppr (HsVect _ v rhs)
    = sep [text "{-# VECTORISE" <+> ppr v,
           nest 4 $
             pprExpr (unLoc rhs) <+> text "#-}" ]
  ppr (HsNoVect _ v)
    = sep [text "{-# NOVECTORISE" <+> ppr v <+> text "#-}" ]
  ppr (HsVectTypeIn _ False t Nothing)
    = sep [text "{-# VECTORISE type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeIn _ False t (Just t'))
    = sep [text "{-# VECTORISE type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeIn _ True t Nothing)
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeIn _ True t (Just t'))
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeOut False t Nothing)
    = sep [text "{-# VECTORISE type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeOut False t (Just t'))
    = sep [text "{-# VECTORISE type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeOut True t Nothing)
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeOut True t (Just t'))
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectClassIn _ c)
    = sep [text "{-# VECTORISE class" <+> ppr c <+> text "#-}" ]
  ppr (HsVectClassOut c)
    = sep [text "{-# VECTORISE class" <+> ppr c <+> text "#-}" ]
  ppr (HsVectInstIn ty)
    = sep [text "{-# VECTORISE SCALAR instance" <+> ppr ty <+> text "#-}" ]
  ppr (HsVectInstOut i)
    = sep [text "{-# VECTORISE SCALAR instance" <+> ppr i <+> text "#-}" ]

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
type LWarnDecls name = Located (WarnDecls name)

 -- Note [Pragma source text] in BasicTypes
-- | Warning pragma Declarations
data WarnDecls name = Warnings { wd_src :: SourceText
                               , wd_warnings :: [LWarnDecl name]
                               }
  deriving Data

-- | Located Warning pragma Declaration
type LWarnDecl name = Located (WarnDecl name)

-- | Warning pragma Declaration
data WarnDecl name = Warning [LEmbellished name] WarningTxt
  deriving Data

instance OutputableBndr name => Outputable (WarnDecls name) where
    ppr (Warnings (SourceText src) decls)
      = text src <+> vcat (punctuate comma (map ppr decls)) <+> text "#-}"
    ppr (Warnings NoSourceText _decls) = panic "WarnDecls"

instance OutputableBndr name => Outputable (WarnDecl name) where
    ppr (Warning thing txt)
      = hsep ( punctuate comma (map ppr thing))
              <+> ppr txt

{-
************************************************************************
*                                                                      *
\subsection[AnnDecl]{Annotations}
*                                                                      *
************************************************************************
-}

-- | Located Annotation Declaration
type LAnnDecl name = Located (AnnDecl name)

-- | Annotation Declaration
data AnnDecl name = HsAnnotation
                      SourceText -- Note [Pragma source text] in BasicTypes
                      (AnnProvenance name) (Located (HsExpr name))
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
      --           'ApiAnnotation.AnnType'
      --           'ApiAnnotation.AnnModule'
      --           'ApiAnnotation.AnnClose'

      -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (DataId name) => Data (AnnDecl name)

instance (OutputableBndrId name) => Outputable (AnnDecl name) where
    ppr (HsAnnotation _ provenance expr)
      = hsep [text "{-#", pprAnnProvenance provenance, pprExpr (unLoc expr), text "#-}"]

-- | Annotation Provenance
data AnnProvenance name = ValueAnnProvenance (LEmbellished name)
                        | TypeAnnProvenance (Located name)
                        | ModuleAnnProvenance
  deriving (Data, Functor)
deriving instance Foldable    AnnProvenance
deriving instance Traversable AnnProvenance

annProvenanceName_maybe :: AnnProvenance name -> Maybe name
annProvenanceName_maybe (ValueAnnProvenance (L _ name)) = Just $ unEmb name
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
type LRoleAnnotDecl name = Located (RoleAnnotDecl name)

-- See #8185 for more info about why role annotations are
-- top-level declarations
-- | Role Annotation Declaration
data RoleAnnotDecl name
  = RoleAnnotDecl (LEmbellished name)         -- type constructor
                  [Located (Maybe Role)] -- optional annotations
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
      --           'ApiAnnotation.AnnRole'

      -- For details on above see note [Api annotations] in ApiAnnotation
  deriving Data

instance OutputableBndr name => Outputable (RoleAnnotDecl name) where
  ppr (RoleAnnotDecl ltycon roles)
    = text "type role" <+> ppr ltycon <+>
      hsep (map (pp_role . unLoc) roles)
    where
      pp_role Nothing  = underscore
      pp_role (Just r) = ppr r

roleAnnotDeclName :: RoleAnnotDecl name -> name
roleAnnotDeclName (RoleAnnotDecl (L _ name) _) = unEmb name
