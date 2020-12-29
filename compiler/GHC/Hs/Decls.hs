{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}


{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Abstract syntax of global declarations.
--
-- Definitions for: @SynDecl@ and @ConDecl@, @ClassDecl@,
-- @InstDecl@, @DefaultDecl@ and @ForeignDecl@.
module GHC.Hs.Decls (
  -- * Toplevel declarations
  HsDecl(..), LHsDecl, HsDataDefn(..), HsDeriving, LHsFunDep,
  HsDerivingClause(..), LHsDerivingClause, DerivClauseTys(..), LDerivClauseTys,
  NewOrData(..), newOrDataToFlavour,
  StandaloneKindSig(..), LStandaloneKindSig, standaloneKindSigName,

  -- ** Class or type declarations
  TyClDecl(..), LTyClDecl, DataDeclRn(..),
  TyClGroup(..),
  tyClGroupTyClDecls, tyClGroupInstDecls, tyClGroupRoleDecls,
  tyClGroupKindSigs,
  isClassDecl, isDataDecl, isSynDecl, tcdName,
  isFamilyDecl, isTypeFamilyDecl, isDataFamilyDecl,
  isOpenTypeFamilyInfo, isClosedTypeFamilyInfo,
  tyFamInstDeclName, tyFamInstDeclLName,
  countTyClDecls, pprTyClDeclFlavour,
  tyClDeclLName, tyClDeclTyVars,
  hsDeclHasCusk, famResultKindSignature,
  FamilyDecl(..), LFamilyDecl,
  FunDep(..),

  -- ** Instance declarations
  InstDecl(..), LInstDecl, FamilyInfo(..),
  TyFamInstDecl(..), LTyFamInstDecl, instDeclDataFamInsts,
  TyFamDefltDecl, LTyFamDefltDecl,
  DataFamInstDecl(..), LDataFamInstDecl,
  pprDataFamInstFlavour, pprTyFamInstDecl, pprHsFamInstLHS,
  FamEqn(..), TyFamInstEqn, LTyFamInstEqn, HsTyPats,
  LClsInstDecl, ClsInstDecl(..),

  -- ** Standalone deriving declarations
  DerivDecl(..), LDerivDecl,
  -- ** Deriving strategies
  DerivStrategy(..), LDerivStrategy,
  derivStrategyName, foldDerivStrategy, mapDerivStrategy,
  XViaStrategyPs(..),
  -- ** @RULE@ declarations
  LRuleDecls,RuleDecls(..),RuleDecl(..),LRuleDecl,HsRuleRn(..),
  HsRuleAnn(..),
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
  HsConDeclH98Details, HsConDeclGADTDetails(..), hsConDeclTheta,
  getConNames, getRecConArgs_maybe,
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
  resultVariableName, familyDeclLName, familyDeclName,

  -- * Grouping
  HsGroup(..),  emptyRdrGroup, emptyRnGroup, appendGroups, hsGroupInstDecls,
  hsGroupTopLevelFixitySigs,

  partitionBindsAndSigs,
    ) where

-- friends:
import GHC.Prelude

import Language.Haskell.Syntax.Decls

import {-# SOURCE #-} GHC.Hs.Expr ( pprExpr, pprSpliceDecl )
        -- Because Expr imports Decls via HsBracket

import GHC.Hs.Binds
import GHC.Hs.Type
import GHC.Hs.Doc
import GHC.Types.Basic
import GHC.Core.Coercion
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension
import GHC.Parser.Annotation
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Fixity

-- others:
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.SrcLoc
import GHC.Types.SourceText
import GHC.Core.Type
import GHC.Types.ForeignCall

import GHC.Data.Bag
import GHC.Data.Maybe
import Data.Data (Data)

{-
************************************************************************
*                                                                      *
\subsection[HsDecl]{Declarations}
*                                                                      *
************************************************************************
-}

type instance XTyClD      (GhcPass _) = NoExtField
type instance XInstD      (GhcPass _) = NoExtField
type instance XDerivD     (GhcPass _) = NoExtField
type instance XValD       (GhcPass _) = NoExtField
type instance XSigD       (GhcPass _) = NoExtField
type instance XKindSigD   (GhcPass _) = NoExtField
type instance XDefD       (GhcPass _) = NoExtField
type instance XForD       (GhcPass _) = NoExtField
type instance XWarningD   (GhcPass _) = NoExtField
type instance XAnnD       (GhcPass _) = NoExtField
type instance XRuleD      (GhcPass _) = NoExtField
type instance XSpliceD    (GhcPass _) = NoExtField
type instance XDocD       (GhcPass _) = NoExtField
type instance XRoleAnnotD (GhcPass _) = NoExtField
type instance XXHsDecl    (GhcPass _) = NoExtCon

-- | Partition a list of HsDecls into function/pattern bindings, signatures,
-- type family declarations, type family instances, and documentation comments.
--
-- Panics when given a declaration that cannot be put into any of the output
-- groups.
--
-- The primary use of this function is to implement
-- 'GHC.Parser.PostProcess.cvBindsAndSigs'.
partitionBindsAndSigs
  :: [LHsDecl GhcPs]
  -> (LHsBinds GhcPs, [LSig GhcPs], [LFamilyDecl GhcPs],
      [LTyFamInstDecl GhcPs], [LDataFamInstDecl GhcPs], [LDocDecl GhcPs])
partitionBindsAndSigs = go
  where
    go [] = (emptyBag, [], [], [], [], [])
    go ((L l decl) : ds) =
      let (bs, ss, ts, tfis, dfis, docs) = go ds in
      case decl of
        ValD _ b
          -> (L l b `consBag` bs, ss, ts, tfis, dfis, docs)
        SigD _ s
          -> (bs, L l s : ss, ts, tfis, dfis, docs)
        TyClD _ (FamDecl _ t)
          -> (bs, ss, L l t : ts, tfis, dfis, docs)
        InstD _ (TyFamInstD { tfid_inst = tfi })
          -> (bs, ss, ts, L l tfi : tfis, dfis, docs)
        InstD _ (DataFamInstD { dfid_inst = dfi })
          -> (bs, ss, ts, tfis, L l dfi : dfis, docs)
        DocD _ d
          -> (bs, ss, ts, tfis, dfis, L l d : docs)
        _ -> pprPanic "partitionBindsAndSigs" (ppr decl)

type instance XCHsGroup (GhcPass _) = NoExtField
type instance XXHsGroup (GhcPass _) = NoExtCon


emptyGroup, emptyRdrGroup, emptyRnGroup :: HsGroup (GhcPass p)
emptyRdrGroup = emptyGroup { hs_valds = emptyValBindsIn }
emptyRnGroup  = emptyGroup { hs_valds = emptyValBindsOut }

emptyGroup = HsGroup { hs_ext = noExtField,
                       hs_tyclds = [],
                       hs_derivds = [],
                       hs_fixds = [], hs_defds = [], hs_annds = [],
                       hs_fords = [], hs_warnds = [], hs_ruleds = [],
                       hs_valds = error "emptyGroup hs_valds: Can't happen",
                       hs_splcds = [],
                       hs_docs = [] }

-- | The fixity signatures for each top-level declaration and class method
-- in an 'HsGroup'.
-- See Note [Top-level fixity signatures in an HsGroup]
hsGroupTopLevelFixitySigs :: HsGroup (GhcPass p) -> [LFixitySig (GhcPass p)]
hsGroupTopLevelFixitySigs (HsGroup{ hs_fixds = fixds, hs_tyclds = tyclds }) =
    fixds ++ cls_fixds
  where
    cls_fixds = [ L loc sig
                | L _ ClassDecl{tcdSigs = sigs} <- tyClGroupTyClDecls tyclds
                , L loc (FixSig _ sig) <- sigs
                ]

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
        hs_ext    = noExtField,
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

instance (OutputableBndrId p) => Outputable (HsDecl (GhcPass p)) where
    ppr (TyClD _ dcl)             = ppr dcl
    ppr (ValD _ binds)            = ppr binds
    ppr (DefD _ def)              = ppr def
    ppr (InstD _ inst)            = ppr inst
    ppr (DerivD _ deriv)          = ppr deriv
    ppr (ForD _ fd)               = ppr fd
    ppr (SigD _ sd)               = ppr sd
    ppr (KindSigD _ ksd)          = ppr ksd
    ppr (RuleD _ rd)              = ppr rd
    ppr (WarningD _ wd)           = ppr wd
    ppr (AnnD _ ad)               = ppr ad
    ppr (SpliceD _ dd)            = ppr dd
    ppr (DocD _ doc)              = ppr doc
    ppr (RoleAnnotD _ ra)         = ppr ra

instance (OutputableBndrId p) => Outputable (HsGroup (GhcPass p)) where
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
             ppr_ds (tyClGroupRoleDecls tycl_decls),
             ppr_ds (tyClGroupKindSigs  tycl_decls),
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

type instance XSpliceDecl      (GhcPass _) = NoExtField
type instance XXSpliceDecl     (GhcPass _) = NoExtCon

instance OutputableBndrId p
       => Outputable (SpliceDecl (GhcPass p)) where
   ppr (SpliceDecl _ (L _ e) f) = pprSpliceDecl e f

{-
************************************************************************
*                                                                      *
            Type and class declarations
*                                                                      *
************************************************************************
-}

type instance XFamDecl      (GhcPass _) = NoExtField

type instance XSynDecl      GhcPs = EpAnn [AddEpAnn]
type instance XSynDecl      GhcRn = NameSet -- FVs
type instance XSynDecl      GhcTc = NameSet -- FVs

type instance XDataDecl     GhcPs = EpAnn [AddEpAnn]
type instance XDataDecl     GhcRn = DataDeclRn
type instance XDataDecl     GhcTc = DataDeclRn

type instance XClassDecl    GhcPs = (EpAnn [AddEpAnn], AnnSortKey, LayoutInfo)  -- See Note [Class LayoutInfo]
  -- TODO:AZ:tidy up AnnSortKey above
type instance XClassDecl    GhcRn = NameSet -- FVs
type instance XClassDecl    GhcTc = NameSet -- FVs

type instance XXTyClDecl    (GhcPass _) = NoExtCon

type instance XCTyFamInstDecl (GhcPass _) = EpAnn [AddEpAnn]
type instance XXTyFamInstDecl (GhcPass _) = NoExtCon

-- Dealing with names

tyFamInstDeclName :: Anno (IdGhcP p) ~ SrcSpanAnnN
                  => TyFamInstDecl (GhcPass p) -> IdP (GhcPass p)
tyFamInstDeclName = unLoc . tyFamInstDeclLName

tyFamInstDeclLName :: Anno (IdGhcP p) ~ SrcSpanAnnN
                   => TyFamInstDecl (GhcPass p) -> LocatedN (IdP (GhcPass p))
tyFamInstDeclLName (TyFamInstDecl { tfid_eqn = FamEqn { feqn_tycon = ln }})
  = ln

tyClDeclLName :: Anno (IdGhcP p) ~ SrcSpanAnnN
              => TyClDecl (GhcPass p) -> LocatedN (IdP (GhcPass p))
tyClDeclLName (FamDecl { tcdFam = fd })     = familyDeclLName fd
tyClDeclLName (SynDecl { tcdLName = ln })   = ln
tyClDeclLName (DataDecl { tcdLName = ln })  = ln
tyClDeclLName (ClassDecl { tcdLName = ln }) = ln

-- FIXME: tcdName is commonly used by both GHC and third-party tools, so it
-- needs to be polymorphic in the pass
tcdName :: Anno (IdGhcP p) ~ SrcSpanAnnN
        => TyClDecl (GhcPass p) -> IdP (GhcPass p)
tcdName = unLoc . tyClDeclLName

-- | Does this declaration have a complete, user-supplied kind signature?
-- See Note [CUSKs: complete user-supplied kind signatures]
hsDeclHasCusk :: TyClDecl GhcRn -> Bool
hsDeclHasCusk (FamDecl { tcdFam =
    FamilyDecl { fdInfo      = fam_info
               , fdTyVars    = tyvars
               , fdResultSig = L _ resultSig } }) =
    case fam_info of
      ClosedTypeFamily {} -> hsTvbAllKinded tyvars
                          && isJust (famResultKindSignature resultSig)
      _ -> True -- Un-associated open type/data families have CUSKs
hsDeclHasCusk (SynDecl { tcdTyVars = tyvars, tcdRhs = rhs })
  = hsTvbAllKinded tyvars && isJust (hsTyKindSig rhs)
hsDeclHasCusk (DataDecl { tcdDExt = DataDeclRn { tcdDataCusk = cusk }}) = cusk
hsDeclHasCusk (ClassDecl { tcdTyVars = tyvars }) = hsTvbAllKinded tyvars

-- Pretty-printing TyClDecl
-- ~~~~~~~~~~~~~~~~~~~~~~~~

instance (OutputableBndrId p) => Outputable (TyClDecl (GhcPass p)) where

    ppr (FamDecl { tcdFam = decl }) = ppr decl
    ppr (SynDecl { tcdLName = ltycon, tcdTyVars = tyvars, tcdFixity = fixity
                 , tcdRhs = rhs })
      = hang (text "type" <+>
              pp_vanilla_decl_head ltycon tyvars fixity Nothing <+> equals)
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
             , nest 2 $ pprDeclList (map (ppr . unLoc) ats ++
                                     map (pprTyFamDefltDecl . unLoc) at_defs ++
                                     pprLHsBindsForUser methods sigs) ]
      where
        top_matter = text "class"
                    <+> pp_vanilla_decl_head lclas tyvars fixity context
                    <+> pprFundeps (map unLoc fds)

instance OutputableBndrId p
       => Outputable (TyClGroup (GhcPass p)) where
  ppr (TyClGroup { group_tyclds = tyclds
                 , group_roles = roles
                 , group_kisigs = kisigs
                 , group_instds = instds
                 }
      )
    = hang (text "TyClGroup") 2 $
      ppr kisigs $$
      ppr tyclds $$
      ppr roles $$
      ppr instds

pp_vanilla_decl_head :: (OutputableBndrId p)
   => XRec (GhcPass p) (IdP (GhcPass p))
   -> LHsQTyVars (GhcPass p)
   -> LexicalFixity
   -> Maybe (LHsContext (GhcPass p))
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

pprTyClDeclFlavour :: TyClDecl (GhcPass p) -> SDoc
pprTyClDeclFlavour (ClassDecl {})   = text "class"
pprTyClDeclFlavour (SynDecl {})     = text "type"
pprTyClDeclFlavour (FamDecl { tcdFam = FamilyDecl { fdInfo = info }})
  = pprFlavour info <+> text "family"
pprTyClDeclFlavour (DataDecl { tcdDataDefn = HsDataDefn { dd_ND = nd } })
  = ppr nd

instance OutputableBndrId p => Outputable (FunDep (GhcPass p)) where
  ppr = pprFunDep

type instance XCFunDep    (GhcPass _) = EpAnn [AddEpAnn]
type instance XXFunDep    (GhcPass _) = NoExtCon

pprFundeps :: OutputableBndrId p => [FunDep (GhcPass p)] -> SDoc
pprFundeps []  = empty
pprFundeps fds = hsep (vbar : punctuate comma (map pprFunDep fds))

pprFunDep :: OutputableBndrId p => FunDep (GhcPass p) -> SDoc
pprFunDep (FunDep _ us vs) = hsep [interppSP us, arrow, interppSP vs]

{- *********************************************************************
*                                                                      *
                         TyClGroup
        Strongly connected components of
      type, class, instance, and role declarations
*                                                                      *
********************************************************************* -}

type instance XCTyClGroup (GhcPass _) = NoExtField
type instance XXTyClGroup (GhcPass _) = NoExtCon


{- *********************************************************************
*                                                                      *
               Data and type family declarations
*                                                                      *
********************************************************************* -}

type instance XNoSig            (GhcPass _) = NoExtField
type instance XCKindSig         (GhcPass _) = NoExtField

type instance XTyVarSig         (GhcPass _) = NoExtField
type instance XXFamilyResultSig (GhcPass _) = NoExtCon

type instance XCFamilyDecl    (GhcPass _) = EpAnn [AddEpAnn]
type instance XXFamilyDecl    (GhcPass _) = NoExtCon


------------- Functions over FamilyDecls -----------

familyDeclLName :: FamilyDecl (GhcPass p) -> XRec (GhcPass p) (IdP (GhcPass p))
familyDeclLName (FamilyDecl { fdLName = n }) = n

familyDeclName :: FamilyDecl (GhcPass p) -> IdP (GhcPass p)
familyDeclName = unLoc . familyDeclLName

famResultKindSignature :: FamilyResultSig (GhcPass p) -> Maybe (LHsKind (GhcPass p))
famResultKindSignature (NoSig _) = Nothing
famResultKindSignature (KindSig _ ki) = Just ki
famResultKindSignature (TyVarSig _ bndr) =
  case unLoc bndr of
    UserTyVar _ _ _ -> Nothing
    KindedTyVar _ _ _ ki -> Just ki

-- | Maybe return name of the result type variable
resultVariableName :: FamilyResultSig (GhcPass a) -> Maybe (IdP (GhcPass a))
resultVariableName (TyVarSig _ sig) = Just $ hsLTyVarName sig
resultVariableName _                = Nothing

------------- Pretty printing FamilyDecls -----------

type instance XCInjectivityAnn  (GhcPass _) = EpAnn [AddEpAnn]
type instance XXInjectivityAnn  (GhcPass _) = NoExtCon

instance OutputableBndrId p
       => Outputable (FamilyDecl (GhcPass p)) where
  ppr (FamilyDecl { fdInfo = info, fdLName = ltycon
                  , fdTopLevel = top_level
                  , fdTyVars = tyvars
                  , fdFixity = fixity
                  , fdResultSig = L _ result
                  , fdInjectivityAnn = mb_inj })
    = vcat [ pprFlavour info <+> pp_top_level <+>
             pp_vanilla_decl_head ltycon tyvars fixity Nothing <+>
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
      pp_inj = case mb_inj of
                 Just (L _ (InjectivityAnn _ lhs rhs)) ->
                   hsep [ vbar, ppr lhs, text "->", hsep (map ppr rhs) ]
                 Nothing -> empty
      (pp_where, pp_eqns) = case info of
        ClosedTypeFamily mb_eqns ->
          ( text "where"
          , case mb_eqns of
              Nothing   -> text ".."
              Just eqns -> vcat $ map (ppr_fam_inst_eqn . unLoc) eqns )
        _ -> (empty, empty)



{- *********************************************************************
*                                                                      *
               Data types and data constructors
*                                                                      *
********************************************************************* -}

type instance XCHsDataDefn    (GhcPass _) = NoExtField
type instance XXHsDataDefn    (GhcPass _) = NoExtCon

type instance XCHsDerivingClause    (GhcPass _) = EpAnn [AddEpAnn]
type instance XXHsDerivingClause    (GhcPass _) = NoExtCon

instance OutputableBndrId p
       => Outputable (HsDerivingClause (GhcPass p)) where
  ppr (HsDerivingClause { deriv_clause_strategy = dcs
                        , deriv_clause_tys      = L _ dct })
    = hsep [ text "deriving"
           , pp_strat_before
           , ppr dct
           , pp_strat_after ]
      where
        -- @via@ is unique in that in comes /after/ the class being derived,
        -- so we must special-case it.
        (pp_strat_before, pp_strat_after) =
          case dcs of
            Just (L _ via@ViaStrategy{}) -> (empty, ppr via)
            _                            -> (ppDerivStrategy dcs, empty)

type instance XDctSingle (GhcPass _) = NoExtField
type instance XDctMulti  (GhcPass _) = NoExtField
type instance XXDerivClauseTys (GhcPass _) = NoExtCon

instance OutputableBndrId p => Outputable (DerivClauseTys (GhcPass p)) where
  ppr (DctSingle _ ty) = ppr ty
  ppr (DctMulti _ tys) = parens (interpp'SP tys)

type instance XStandaloneKindSig GhcPs = EpAnn [AddEpAnn]
type instance XStandaloneKindSig GhcRn = NoExtField
type instance XStandaloneKindSig GhcTc = NoExtField

type instance XXStandaloneKindSig (GhcPass p) = NoExtCon

standaloneKindSigName :: StandaloneKindSig (GhcPass p) -> IdP (GhcPass p)
standaloneKindSigName (StandaloneKindSig _ lname _) = unLoc lname

type instance XConDeclGADT (GhcPass _) = EpAnn [AddEpAnn]
type instance XConDeclH98  (GhcPass _) = EpAnn [AddEpAnn]

type instance XXConDecl (GhcPass _) = NoExtCon

getConNames :: ConDecl GhcRn -> [LocatedN Name]
getConNames ConDeclH98  {con_name  = name}  = [name]
getConNames ConDeclGADT {con_names = names} = names

-- | Return @'Just' fields@ if a data constructor declaration uses record
-- syntax (i.e., 'RecCon'), where @fields@ are the field selectors.
-- Otherwise, return 'Nothing'.
getRecConArgs_maybe :: ConDecl GhcRn -> Maybe (LocatedL [LConDeclField GhcRn])
getRecConArgs_maybe (ConDeclH98{con_args = args}) = case args of
  PrefixCon{} -> Nothing
  RecCon flds -> Just flds
  InfixCon{}  -> Nothing
getRecConArgs_maybe (ConDeclGADT{con_g_args = args}) = case args of
  PrefixConGADT{} -> Nothing
  RecConGADT flds -> Just flds

hsConDeclTheta :: Maybe (LHsContext (GhcPass p)) -> [LHsType (GhcPass p)]
hsConDeclTheta Nothing            = []
hsConDeclTheta (Just (L _ theta)) = theta

pp_data_defn :: (OutputableBndrId p)
                  => (Maybe (LHsContext (GhcPass p)) -> SDoc)   -- Printing the header
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
    pp_derivings ds = vcat (map ppr ds)

instance OutputableBndrId p
       => Outputable (HsDataDefn (GhcPass p)) where
   ppr d = pp_data_defn (\_ -> text "Naked HsDataDefn") d

instance OutputableBndrId p
       => Outputable (StandaloneKindSig (GhcPass p)) where
  ppr (StandaloneKindSig _ v ki)
    = text "type" <+> pprPrefixOcc (unLoc v) <+> text "::" <+> ppr ki

pp_condecls :: forall p. OutputableBndrId p => [LConDecl (GhcPass p)] -> SDoc
pp_condecls cs
  | gadt_syntax                  -- In GADT syntax
  = hang (text "where") 2 (vcat (map ppr cs))
  | otherwise                    -- In H98 syntax
  = equals <+> sep (punctuate (text " |") (map ppr cs))
  where
    gadt_syntax = case cs of
      []                      -> False
      (L _ ConDeclH98{}  : _) -> False
      (L _ ConDeclGADT{} : _) -> True

instance (OutputableBndrId p) => Outputable (ConDecl (GhcPass p)) where
    ppr = pprConDecl

pprConDecl :: forall p. OutputableBndrId p => ConDecl (GhcPass p) -> SDoc
pprConDecl (ConDeclH98 { con_name = L _ con
                       , con_ex_tvs = ex_tvs
                       , con_mb_cxt = mcxt
                       , con_args = args
                       , con_doc = doc })
  = sep [ ppr_mbDoc doc
        , pprHsForAll (mkHsForAllInvisTele noAnn ex_tvs) mcxt
        , ppr_details args ]
  where
    -- In ppr_details: let's not print the multiplicities (they are always 1, by
    -- definition) as they do not appear in an actual declaration.
    ppr_details (InfixCon t1 t2) = hsep [ppr (hsScaledThing t1),
                                         pprInfixOcc con,
                                         ppr (hsScaledThing t2)]
    ppr_details (PrefixCon _ tys) = hsep (pprPrefixOcc con
                                    : map (pprHsType . unLoc . hsScaledThing) tys)
    ppr_details (RecCon fields)  = pprPrefixOcc con
                                 <+> pprConDeclFields (unLoc fields)

pprConDecl (ConDeclGADT { con_names = cons, con_bndrs = L _ outer_bndrs
                        , con_mb_cxt = mcxt, con_g_args = args
                        , con_res_ty = res_ty, con_doc = doc })
  = ppr_mbDoc doc <+> ppr_con_names cons <+> dcolon
    <+> (sep [pprHsOuterSigTyVarBndrs outer_bndrs <+> pprLHsContext mcxt,
              ppr_arrow_chain (get_args args ++ [ppr res_ty]) ])
  where
    get_args (PrefixConGADT args) = map ppr args
    get_args (RecConGADT fields)  = [pprConDeclFields (unLoc fields)]

    ppr_arrow_chain (a:as) = sep (a : map (arrow <+>) as)
    ppr_arrow_chain []     = empty

ppr_con_names :: (OutputableBndr a) => [GenLocated l a] -> SDoc
ppr_con_names = pprWithCommas (pprPrefixOcc . unLoc)

{-
************************************************************************
*                                                                      *
                Instance declarations
*                                                                      *
************************************************************************
-}

type instance XCFamEqn    (GhcPass _) r = EpAnn [AddEpAnn]
type instance XXFamEqn    (GhcPass _) r = NoExtCon

type instance Anno (FamEqn (GhcPass p) _) = SrcSpanAnnA

----------------- Class instances -------------

type instance XCClsInstDecl    GhcPs = (EpAnn [AddEpAnn], AnnSortKey) -- TODO:AZ:tidy up
type instance XCClsInstDecl    GhcRn = NoExtField
type instance XCClsInstDecl    GhcTc = NoExtField

type instance XXClsInstDecl    (GhcPass _) = NoExtCon

----------------- Instances of all kinds -------------

type instance XClsInstD     (GhcPass _) = NoExtField

type instance XDataFamInstD GhcPs = EpAnn [AddEpAnn]
type instance XDataFamInstD GhcRn = NoExtField
type instance XDataFamInstD GhcTc = NoExtField

type instance XTyFamInstD   GhcPs = NoExtField
type instance XTyFamInstD   GhcRn = NoExtField
type instance XTyFamInstD   GhcTc = NoExtField

type instance XXInstDecl    (GhcPass _) = NoExtCon

instance OutputableBndrId p
       => Outputable (TyFamInstDecl (GhcPass p)) where
  ppr = pprTyFamInstDecl TopLevel

pprTyFamInstDecl :: (OutputableBndrId p)
                 => TopLevelFlag -> TyFamInstDecl (GhcPass p) -> SDoc
pprTyFamInstDecl top_lvl (TyFamInstDecl { tfid_eqn = eqn })
   = text "type" <+> ppr_instance_keyword top_lvl <+> ppr_fam_inst_eqn eqn

ppr_instance_keyword :: TopLevelFlag -> SDoc
ppr_instance_keyword TopLevel    = text "instance"
ppr_instance_keyword NotTopLevel = empty

pprTyFamDefltDecl :: (OutputableBndrId p)
                  => TyFamDefltDecl (GhcPass p) -> SDoc
pprTyFamDefltDecl = pprTyFamInstDecl NotTopLevel

ppr_fam_inst_eqn :: (OutputableBndrId p)
                 => TyFamInstEqn (GhcPass p) -> SDoc
ppr_fam_inst_eqn (FamEqn { feqn_tycon  = L _ tycon
                         , feqn_bndrs  = bndrs
                         , feqn_pats   = pats
                         , feqn_fixity = fixity
                         , feqn_rhs    = rhs })
    = pprHsFamInstLHS tycon bndrs pats fixity Nothing <+> equals <+> ppr rhs

instance OutputableBndrId p
       => Outputable (DataFamInstDecl (GhcPass p)) where
  ppr = pprDataFamInstDecl TopLevel

pprDataFamInstDecl :: (OutputableBndrId p)
                   => TopLevelFlag -> DataFamInstDecl (GhcPass p) -> SDoc
pprDataFamInstDecl top_lvl (DataFamInstDecl { dfid_eqn =
                            (FamEqn { feqn_tycon  = L _ tycon
                                    , feqn_bndrs  = bndrs
                                    , feqn_pats   = pats
                                    , feqn_fixity = fixity
                                    , feqn_rhs    = defn })})
  = pp_data_defn pp_hdr defn
  where
    pp_hdr mctxt = ppr_instance_keyword top_lvl
              <+> pprHsFamInstLHS tycon bndrs pats fixity mctxt
                  -- pp_data_defn pretty-prints the kind sig. See #14817.

pprDataFamInstFlavour :: DataFamInstDecl (GhcPass p) -> SDoc
pprDataFamInstFlavour (DataFamInstDecl { dfid_eqn =
                       (FamEqn { feqn_rhs = HsDataDefn { dd_ND = nd }})})
  = ppr nd

pprHsFamInstLHS :: (OutputableBndrId p)
   => IdP (GhcPass p)
   -> HsOuterFamEqnTyVarBndrs (GhcPass p)
   -> HsTyPats (GhcPass p)
   -> LexicalFixity
   -> Maybe (LHsContext (GhcPass p))
   -> SDoc
pprHsFamInstLHS thing bndrs typats fixity mb_ctxt
   = hsep [ pprHsOuterFamEqnTyVarBndrs bndrs
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

instance OutputableBndrId p
       => Outputable (ClsInstDecl (GhcPass p)) where
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

ppDerivStrategy :: OutputableBndrId p
                => Maybe (LDerivStrategy (GhcPass p)) -> SDoc
ppDerivStrategy mb =
  case mb of
    Nothing       -> empty
    Just (L _ ds) -> ppr ds

ppOverlapPragma :: Maybe (LocatedP OverlapMode) -> SDoc
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


instance (OutputableBndrId p) => Outputable (InstDecl (GhcPass p)) where
    ppr (ClsInstD     { cid_inst  = decl }) = ppr decl
    ppr (TyFamInstD   { tfid_inst = decl }) = ppr decl
    ppr (DataFamInstD { dfid_inst = decl }) = ppr decl

-- Extract the declarations of associated data types from an instance

instDeclDataFamInsts :: [LInstDecl (GhcPass p)] -> [DataFamInstDecl (GhcPass p)]
instDeclDataFamInsts inst_decls
  = concatMap do_one inst_decls
  where
    do_one :: LInstDecl (GhcPass p) -> [DataFamInstDecl (GhcPass p)]
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

type instance XCDerivDecl    (GhcPass _) = EpAnn [AddEpAnn]
type instance XXDerivDecl    (GhcPass _) = NoExtCon

type instance Anno OverlapMode = SrcSpanAnnP

instance OutputableBndrId p
       => Outputable (DerivDecl (GhcPass p)) where
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
                Deriving strategies
*                                                                      *
************************************************************************
-}

type instance XStockStrategy    GhcPs = EpAnn [AddEpAnn]
type instance XStockStrategy    GhcRn = NoExtField
type instance XStockStrategy    GhcTc = NoExtField

type instance XAnyClassStrategy GhcPs = EpAnn [AddEpAnn]
type instance XAnyClassStrategy GhcRn = NoExtField
type instance XAnyClassStrategy GhcTc = NoExtField

type instance XNewtypeStrategy  GhcPs = EpAnn [AddEpAnn]
type instance XNewtypeStrategy  GhcRn = NoExtField
type instance XNewtypeStrategy  GhcTc = NoExtField

type instance XViaStrategy GhcPs = XViaStrategyPs
type instance XViaStrategy GhcRn = LHsSigType GhcRn
type instance XViaStrategy GhcTc = Type

data XViaStrategyPs = XViaStrategyPs (EpAnn [AddEpAnn]) (LHsSigType GhcPs)

instance OutputableBndrId p
        => Outputable (DerivStrategy (GhcPass p)) where
    ppr (StockStrategy    _) = text "stock"
    ppr (AnyclassStrategy _) = text "anyclass"
    ppr (NewtypeStrategy  _) = text "newtype"
    ppr (ViaStrategy ty)     = text "via" <+> case ghcPass @p of
                                                GhcPs -> ppr ty
                                                GhcRn -> ppr ty
                                                GhcTc -> ppr ty

instance Outputable XViaStrategyPs where
    ppr (XViaStrategyPs _ t) = ppr t


-- | Eliminate a 'DerivStrategy'.
foldDerivStrategy :: (p ~ GhcPass pass)
                  => r -> (XViaStrategy p -> r) -> DerivStrategy p -> r
foldDerivStrategy other _   (StockStrategy    _) = other
foldDerivStrategy other _   (AnyclassStrategy _) = other
foldDerivStrategy other _   (NewtypeStrategy  _) = other
foldDerivStrategy _     via (ViaStrategy t)  = via t

-- | Map over the @via@ type if dealing with 'ViaStrategy'. Otherwise,
-- return the 'DerivStrategy' unchanged.
mapDerivStrategy :: (p ~ GhcPass pass)
                 => (XViaStrategy p -> XViaStrategy p)
                 -> DerivStrategy p -> DerivStrategy p
mapDerivStrategy f ds = foldDerivStrategy ds (ViaStrategy . f) ds

{-
************************************************************************
*                                                                      *
\subsection[DefaultDecl]{A @default@ declaration}
*                                                                      *
************************************************************************
-}

type instance XCDefaultDecl    GhcPs = EpAnn [AddEpAnn]
type instance XCDefaultDecl    GhcRn = NoExtField
type instance XCDefaultDecl    GhcTc = NoExtField

type instance XXDefaultDecl    (GhcPass _) = NoExtCon

instance OutputableBndrId p
       => Outputable (DefaultDecl (GhcPass p)) where
    ppr (DefaultDecl _ tys)
      = text "default" <+> parens (interpp'SP tys)

{-
************************************************************************
*                                                                      *
\subsection{Foreign function interface declaration}
*                                                                      *
************************************************************************
-}

type instance XForeignImport   GhcPs = EpAnn [AddEpAnn]
type instance XForeignImport   GhcRn = NoExtField
type instance XForeignImport   GhcTc = Coercion

type instance XForeignExport   GhcPs = EpAnn [AddEpAnn]
type instance XForeignExport   GhcRn = NoExtField
type instance XForeignExport   GhcTc = Coercion

type instance XXForeignDecl    (GhcPass _) = NoExtCon

instance OutputableBndrId p
       => Outputable (ForeignDecl (GhcPass p)) where
  ppr (ForeignImport { fd_name = n, fd_sig_ty = ty, fd_fi = fimport })
    = hang (text "foreign import" <+> ppr fimport <+> ppr n)
         2 (dcolon <+> ppr ty)
  ppr (ForeignExport { fd_name = n, fd_sig_ty = ty, fd_fe = fexport }) =
    hang (text "foreign export" <+> ppr fexport <+> ppr n)
       2 (dcolon <+> ppr ty)

{-
************************************************************************
*                                                                      *
\subsection{Rewrite rules}
*                                                                      *
************************************************************************
-}

type instance XCRuleDecls    GhcPs = EpAnn [AddEpAnn]
type instance XCRuleDecls    GhcRn = NoExtField
type instance XCRuleDecls    GhcTc = NoExtField

type instance XXRuleDecls    (GhcPass _) = NoExtCon

type instance XHsRule       GhcPs = EpAnn HsRuleAnn
type instance XHsRule       GhcRn = HsRuleRn
type instance XHsRule       GhcTc = HsRuleRn

type instance XXRuleDecl    (GhcPass _) = NoExtCon

type instance Anno (SourceText, RuleName) = SrcSpan

data HsRuleAnn
  = HsRuleAnn
       { ra_tyanns :: Maybe (AddEpAnn, AddEpAnn)
                 -- ^ The locations of 'forall' and '.' for forall'd type vars
                 -- Using AddEpAnn to capture possible unicode variants
       , ra_tmanns :: Maybe (AddEpAnn, AddEpAnn)
                 -- ^ The locations of 'forall' and '.' for forall'd term vars
                 -- Using AddEpAnn to capture possible unicode variants
       , ra_rest :: [AddEpAnn]
       } deriving (Data, Eq)

flattenRuleDecls :: [LRuleDecls (GhcPass p)] -> [LRuleDecl (GhcPass p)]
flattenRuleDecls decls = concatMap (rds_rules . unLoc) decls

type instance XCRuleBndr    (GhcPass _) = EpAnn [AddEpAnn]
type instance XRuleBndrSig  (GhcPass _) = EpAnn [AddEpAnn]
type instance XXRuleBndr    (GhcPass _) = NoExtCon

instance (OutputableBndrId p) => Outputable (RuleDecls (GhcPass p)) where
  ppr (HsRules { rds_src = st
               , rds_rules = rules })
    = pprWithSourceText st (text "{-# RULES")
          <+> vcat (punctuate semi (map ppr rules)) <+> text "#-}"

instance (OutputableBndrId p) => Outputable (RuleDecl (GhcPass p)) where
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

instance (OutputableBndrId p) => Outputable (RuleBndr (GhcPass p)) where
   ppr (RuleBndr _ name) = ppr name
   ppr (RuleBndrSig _ name ty) = parens (ppr name <> dcolon <> ppr ty)

{-
************************************************************************
*                                                                      *
\subsection[DeprecDecl]{Deprecations}
*                                                                      *
************************************************************************
-}

type instance XWarnings      GhcPs = EpAnn [AddEpAnn]
type instance XWarnings      GhcRn = NoExtField
type instance XWarnings      GhcTc = NoExtField

type instance XXWarnDecls    (GhcPass _) = NoExtCon

type instance XWarning      (GhcPass _) = EpAnn [AddEpAnn]
type instance XXWarnDecl    (GhcPass _) = NoExtCon


instance OutputableBndrId p
        => Outputable (WarnDecls (GhcPass p)) where
    ppr (Warnings _ (SourceText src) decls)
      = text src <+> vcat (punctuate comma (map ppr decls)) <+> text "#-}"
    ppr (Warnings _ NoSourceText _decls) = panic "WarnDecls"

instance OutputableBndrId p
       => Outputable (WarnDecl (GhcPass p)) where
    ppr (Warning _ thing txt)
      = hsep ( punctuate comma (map ppr thing))
              <+> ppr txt

{-
************************************************************************
*                                                                      *
\subsection[AnnDecl]{Annotations}
*                                                                      *
************************************************************************
-}

type instance XHsAnnotation (GhcPass _) = EpAnn AnnPragma
type instance XXAnnDecl     (GhcPass _) = NoExtCon

instance (OutputableBndrId p) => Outputable (AnnDecl (GhcPass p)) where
    ppr (HsAnnotation _ _ provenance expr)
      = hsep [text "{-#", pprAnnProvenance provenance, pprExpr (unLoc expr), text "#-}"]

pprAnnProvenance :: OutputableBndrId p => AnnProvenance (GhcPass p) -> SDoc
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

type instance XCRoleAnnotDecl GhcPs = EpAnn [AddEpAnn]
type instance XCRoleAnnotDecl GhcRn = NoExtField
type instance XCRoleAnnotDecl GhcTc = NoExtField

type instance XXRoleAnnotDecl (GhcPass _) = NoExtCon

type instance Anno (Maybe Role) = SrcSpan

instance OutputableBndr (IdP (GhcPass p))
       => Outputable (RoleAnnotDecl (GhcPass p)) where
  ppr (RoleAnnotDecl _ ltycon roles)
    = text "type role" <+> pprPrefixOcc (unLoc ltycon) <+>
      hsep (map (pp_role . unLoc) roles)
    where
      pp_role Nothing  = underscore
      pp_role (Just r) = ppr r

roleAnnotDeclName :: RoleAnnotDecl (GhcPass p) -> IdP (GhcPass p)
roleAnnotDeclName (RoleAnnotDecl _ (L _ name) _) = name

{-
************************************************************************
*                                                                      *
\subsection{Anno instances}
*                                                                      *
************************************************************************
-}

type instance Anno (HsDecl (GhcPass _)) = SrcSpanAnnA
type instance Anno (SpliceDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno (TyClDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno (FunDep (GhcPass p)) = SrcSpanAnnA
type instance Anno (FamilyResultSig (GhcPass p)) = SrcSpan
type instance Anno (FamilyDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno (InjectivityAnn (GhcPass p)) = SrcSpan
type instance Anno CType = SrcSpanAnnP
type instance Anno (HsDerivingClause (GhcPass p)) = SrcSpan
type instance Anno (DerivClauseTys (GhcPass _)) = SrcSpanAnnC
type instance Anno (StandaloneKindSig (GhcPass p)) = SrcSpanAnnA
type instance Anno (ConDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno Bool = SrcSpan
type instance Anno [LocatedA (ConDeclField (GhcPass _))] = SrcSpanAnnL
type instance Anno (FamEqn p (LocatedA (HsType p))) = SrcSpanAnnA
type instance Anno (TyFamInstDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno (DataFamInstDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno (FamEqn (GhcPass p) _) = SrcSpanAnnA
type instance Anno (ClsInstDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno (InstDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno DocDecl = SrcSpanAnnA
type instance Anno (DerivDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno OverlapMode = SrcSpanAnnP
type instance Anno (DerivStrategy (GhcPass p)) = SrcSpan
type instance Anno (DefaultDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno (ForeignDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno (RuleDecls (GhcPass p)) = SrcSpanAnnA
type instance Anno (RuleDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno (SourceText, RuleName) = SrcSpan
type instance Anno (RuleBndr (GhcPass p)) = SrcSpan
type instance Anno (WarnDecls (GhcPass p)) = SrcSpanAnnA
type instance Anno (WarnDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno (AnnDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno (RoleAnnotDecl (GhcPass p)) = SrcSpanAnnA
type instance Anno (Maybe Role) = SrcSpan
