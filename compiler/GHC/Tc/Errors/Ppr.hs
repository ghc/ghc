{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic TcRnMessage
{-# LANGUAGE InstanceSigs #-}

module GHC.Tc.Errors.Ppr
  ( pprTypeDoesNotHaveFixedRuntimeRep
  , pprScopeError
  --
  , tidySkolemInfo
  , tidySkolemInfoAnon
  --
  , pprHsDocContext
  , inHsDocContext
  , TcRnMessageOpts(..)
  , pprTyThingUsedWrong
  )
  where

import GHC.Prelude

import GHC.Builtin.Names
import GHC.Builtin.Types ( boxedRepDataConTyCon, tYPETyCon, filterCTuple )

import GHC.Types.Name.Reader
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Warnings

import GHC.Core.Coercion
import GHC.Core.Unify     ( tcMatchTys )
import GHC.Core.TyCon
import GHC.Core.Class
import GHC.Core.DataCon
import GHC.Core.Coercion.Axiom (coAxiomTyCon, coAxiomSingleBranch)
import GHC.Core.ConLike
import GHC.Core.FamInstEnv ( FamInst(..), famInstAxiom, pprFamInst )
import GHC.Core.InstEnv
import GHC.Core.TyCo.Rep (Type(..))
import GHC.Core.TyCo.Ppr (pprWithExplicitKindsWhen,
                          pprSourceTyCon, pprTyVars, pprWithTYPE, pprTyVar, pprTidiedType)
import GHC.Core.PatSyn ( patSynName, pprPatSynType )
import GHC.Core.Predicate
import GHC.Core.Type
import GHC.Core.FVs( orphNamesOfTypes )

import GHC.Driver.Flags
import GHC.Driver.Backend
import GHC.Hs

import GHC.Tc.Errors.Types
import GHC.Tc.Types.Constraint
import {-# SOURCE #-} GHC.Tc.Types( getLclEnvLoc, lclEnvInGeneratedCode, TcTyThing )
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Rank (Rank(..))
import GHC.Tc.Utils.TcType

import GHC.Types.Error
import GHC.Types.Hint
import GHC.Types.Hint.Ppr () -- Outputable GhcHint
import GHC.Types.Basic
import GHC.Types.Error.Codes ( constructorCode )
import GHC.Types.Id
import GHC.Types.Id.Info ( RecSelParent(..) )
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.SrcLoc
import GHC.Types.TyThing
import GHC.Types.Unique.Set ( nonDetEltsUniqSet )
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Fixity (defaultFixity)

import GHC.Unit.State
import GHC.Unit.Module

import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.List.SetOps ( nubOrdBy )
import GHC.Data.Maybe
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.BooleanFormula (pprBooleanFormulaNice)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Function (on)
import Data.List ( groupBy, sortBy, tails
                 , partition, unfoldr )
import Data.Ord ( comparing )
import Data.Bifunctor
import qualified Language.Haskell.TH as TH
import {-# SOURCE #-} GHC.Tc.Types (pprTcTyThingCategory)
import GHC.Iface.Errors.Types
import GHC.Iface.Errors.Ppr

data TcRnMessageOpts = TcRnMessageOpts { tcOptsShowContext :: !Bool -- ^ Whether we show the error context or not
                                       , tcOptsIfaceOpts   :: !IfaceMessageOpts
                                       }

defaultTcRnMessageOpts :: TcRnMessageOpts
defaultTcRnMessageOpts = TcRnMessageOpts { tcOptsShowContext = True
                                         , tcOptsIfaceOpts = defaultDiagnosticOpts @IfaceMessage }

instance Diagnostic TcRnMessage where
  type DiagnosticOpts TcRnMessage = TcRnMessageOpts
  defaultDiagnosticOpts = defaultTcRnMessageOpts
  diagnosticMessage opts = \case
    TcRnUnknownMessage (UnknownDiagnostic @e m)
      -> diagnosticMessage (defaultDiagnosticOpts @e) m
    TcRnMessageWithInfo unit_state msg_with_info
      -> case msg_with_info of
           TcRnMessageDetailed err_info msg
             -> messageWithInfoDiagnosticMessage unit_state err_info
                  (tcOptsShowContext opts)
                  (diagnosticMessage opts msg)
    TcRnWithHsDocContext ctxt msg
      -> if tcOptsShowContext opts
         then main_msg `unionDecoratedSDoc` ctxt_msg
         else main_msg
      where
        main_msg = diagnosticMessage opts msg
        ctxt_msg = mkSimpleDecorated (inHsDocContext ctxt)
    TcRnSolverReport msg _ _
      -> mkSimpleDecorated $ pprSolverReportWithCtxt msg
    TcRnRedundantConstraints redundants (info, show_info)
      -> mkSimpleDecorated $
         text "Redundant constraint" <> plural redundants <> colon
           <+> pprEvVarTheta redundants
         $$ if show_info then text "In" <+> ppr info else empty
    TcRnInaccessibleCode implic contra
      -> mkSimpleDecorated $
         hang (text "Inaccessible code in")
           2 (ppr (ic_info implic))
         $$ pprSolverReportWithCtxt contra
    TcRnTypeDoesNotHaveFixedRuntimeRep ty prov (ErrInfo extra supplementary)
      -> mkDecorated [pprTypeDoesNotHaveFixedRuntimeRep ty prov, extra, supplementary]
    TcRnImplicitLift id_or_name ErrInfo{..}
      -> mkDecorated $
           ( text "The variable" <+> quotes (ppr id_or_name) <+>
             text "is implicitly lifted in the TH quotation"
           ) : [errInfoContext, errInfoSupplementary]
    TcRnUnusedPatternBinds bind
      -> mkDecorated [hang (text "This pattern-binding binds no variables:") 2 (ppr bind)]
    TcRnDodgyImports (DodgyImportsEmptyParent gre)
      -> mkDecorated [dodgy_msg (text "import") gre (dodgy_msg_insert gre)]
    TcRnDodgyImports (DodgyImportsHiding reason)
      -> mkSimpleDecorated $
         pprImportLookup reason
    TcRnDodgyExports gre
      -> mkDecorated [dodgy_msg (text "export") gre (dodgy_msg_insert gre)]
    TcRnMissingImportList ie
      -> mkDecorated [ text "The import item" <+> quotes (ppr ie) <+>
                       text "does not have an explicit import list"
                     ]
    TcRnUnsafeDueToPlugin
      -> mkDecorated [text "Use of plugins makes the module unsafe"]
    TcRnModMissingRealSrcSpan mod
      -> mkDecorated [text "Module does not have a RealSrcSpan:" <+> ppr mod]
    TcRnIdNotExportedFromModuleSig name mod
      -> mkDecorated [ text "The identifier" <+> ppr (occName name) <+>
                       text "does not exist in the signature for" <+> ppr mod
                     ]
    TcRnIdNotExportedFromLocalSig name
      -> mkDecorated [ text "The identifier" <+> ppr (occName name) <+>
                       text "does not exist in the local signature."
                     ]
    TcRnShadowedName occ provenance
      -> let shadowed_locs = case provenance of
               ShadowedNameProvenanceLocal n     -> [text "bound at" <+> ppr n]
               ShadowedNameProvenanceGlobal gres -> map pprNameProvenance gres
         in mkSimpleDecorated $
            sep [text "This binding for" <+> quotes (ppr occ)
             <+> text "shadows the existing binding" <> plural shadowed_locs,
                   nest 2 (vcat shadowed_locs)]
    TcRnInvalidWarningCategory cat
      -> mkSimpleDecorated $
           vcat [text "Warning category" <+> quotes (ppr cat) <+> text "is not valid",
                 text "(user-defined category names must begin with" <+> quotes (text "x-"),
                 text "and contain only letters, numbers, apostrophes and dashes)" ]
    TcRnDuplicateWarningDecls d rdr_name
      -> mkSimpleDecorated $
           vcat [text "Multiple warning declarations for" <+> quotes (ppr rdr_name),
                 text "also at " <+> ppr (getLocA d)]
    TcRnSimplifierTooManyIterations simples limit wc
      -> mkSimpleDecorated $
           hang (text "solveWanteds: too many iterations"
                   <+> parens (text "limit =" <+> ppr limit))
                2 (vcat [ text "Unsolved:" <+> ppr wc
                        , text "Simples:"  <+> ppr simples
                        ])
    TcRnIllegalPatSynDecl rdrname
      -> mkSimpleDecorated $
           hang (text "Illegal pattern synonym declaration for" <+> quotes (ppr rdrname))
              2 (text "Pattern synonym declarations are only valid at top level")
    TcRnLinearPatSyn ty
      -> mkSimpleDecorated $
           hang (text "Pattern synonyms do not support linear fields (GHC #18806):") 2 (ppr ty)
    TcRnEmptyRecordUpdate
      -> mkSimpleDecorated $ text "Empty record update"
    TcRnIllegalFieldPunning fld
      -> mkSimpleDecorated $ text "Illegal use of punning for field" <+> quotes (ppr fld)
    TcRnIllegalWildcardsInRecord fld_part
      -> mkSimpleDecorated $ text "Illegal `..' in record" <+> pprRecordFieldPart fld_part
    TcRnIllegalWildcardInType mb_name bad
      -> mkSimpleDecorated $ case bad of
          WildcardNotLastInConstraint ->
            hang notAllowed 2 constraint_hint_msg
          ExtraConstraintWildcardNotAllowed allow_sole ->
            case allow_sole of
              SoleExtraConstraintWildcardNotAllowed ->
                notAllowed
              SoleExtraConstraintWildcardAllowed ->
                hang notAllowed 2 sole_msg
          WildcardsNotAllowedAtAll ->
            notAllowed
      where
        notAllowed, what, wildcard, how :: SDoc
        notAllowed = what <+> quotes wildcard <+> how
        wildcard = case mb_name of
          Nothing   -> pprAnonWildCard
          Just name -> ppr name
        what
          | Just _ <- mb_name
          = text "Named wildcard"
          | ExtraConstraintWildcardNotAllowed {} <- bad
          = text "Extra-constraint wildcard"
          | otherwise
          = text "Wildcard"
        how = case bad of
          WildcardNotLastInConstraint
            -> text "not allowed in a constraint"
          _ -> text "not allowed"
        constraint_hint_msg :: SDoc
        constraint_hint_msg
          | Just _ <- mb_name
          = vcat [ text "Extra-constraint wildcards must be anonymous"
                 , nest 2 (text "e.g  f :: (Eq a, _) => blah") ]
          | otherwise
          = vcat [ text "except as the last top-level constraint of a type signature"
                 , nest 2 (text "e.g  f :: (Eq a, _) => blah") ]
        sole_msg :: SDoc
        sole_msg =
          vcat [ text "except as the sole constraint"
               , nest 2 (text "e.g., deriving instance _ => Eq (Foo a)") ]
    TcRnDuplicateFieldName fld_part dups
      -> mkSimpleDecorated $
           hsep [ text "Duplicate field name"
                , quotes (ppr (rdrNameOcc $ NE.head dups))
                , text "in record", pprRecordFieldPart fld_part ]
    TcRnIllegalViewPattern pat
      -> mkSimpleDecorated $ vcat [text "Illegal view pattern: " <+> ppr pat]
    TcRnCharLiteralOutOfRange c
      -> mkSimpleDecorated $ text "character literal out of range: '\\" <> char c  <> char '\''
    TcRnIllegalWildcardsInConstructor con
      -> mkSimpleDecorated $
           vcat [ text "Illegal `{..}' notation for constructor" <+> quotes (ppr con)
                , nest 2 (text "Record wildcards may not be used for constructors with unlabelled fields.")
                , nest 2 (text "Possible fix: Remove the `{..}' and add a match for each field of the constructor.")
                ]
    TcRnIgnoringAnnotations anns
      -> mkSimpleDecorated $
           text "Ignoring ANN annotation" <> plural anns <> comma
           <+> text "because this is a stage-1 compiler without -fexternal-interpreter or doesn't support GHCi"
    TcRnAnnotationInSafeHaskell
      -> mkSimpleDecorated $
           vcat [ text "Annotations are not compatible with Safe Haskell."
                , text "See https://gitlab.haskell.org/ghc/ghc/issues/10826" ]
    TcRnInvalidTypeApplication fun_ty hs_ty
      -> mkSimpleDecorated $
           text "Cannot apply expression of type" <+> quotes (ppr fun_ty) $$
           text "to a visible type argument" <+> quotes (ppr hs_ty)
    TcRnTagToEnumMissingValArg
      -> mkSimpleDecorated $
           text "tagToEnum# must appear applied to one value argument"
    TcRnTagToEnumUnspecifiedResTy ty
      -> mkSimpleDecorated $
           hang (text "Bad call to tagToEnum# at type" <+> ppr ty)
              2 (vcat [ text "Specify the type by giving a type signature"
                      , text "e.g. (tagToEnum# x) :: Bool" ])
    TcRnTagToEnumResTyNotAnEnum ty
      -> mkSimpleDecorated $
           hang (text "Bad call to tagToEnum# at type" <+> ppr ty)
              2 (text "Result type must be an enumeration type")
    TcRnTagToEnumResTyTypeData ty
      -> mkSimpleDecorated $
           hang (text "Bad call to tagToEnum# at type" <+> ppr ty)
              2 (text "Result type cannot be headed by a `type data` type")
    TcRnArrowIfThenElsePredDependsOnResultTy
      -> mkSimpleDecorated $
           text "Predicate type of `ifThenElse' depends on result type"
    TcRnIllegalHsBootFileDecl
      -> mkSimpleDecorated $
           text "Illegal declarations in an hs-boot file"
    TcRnRecursivePatternSynonym binds
      -> mkSimpleDecorated $
            hang (text "Recursive pattern synonym definition with following bindings:")
               2 (vcat $ map pprLBind . bagToList $ binds)
          where
            pprLoc loc = parens (text "defined at" <+> ppr loc)
            pprLBind :: CollectPass GhcRn => GenLocated (SrcSpanAnn' a) (HsBindLR GhcRn idR) -> SDoc
            pprLBind (L loc bind) = pprWithCommas ppr (collectHsBindBinders CollNoDictBinders bind)
                                        <+> pprLoc (locA loc)
    TcRnPartialTypeSigTyVarMismatch n1 n2 fn_name hs_ty
      -> mkSimpleDecorated $
           hang (text "Couldn't match" <+> quotes (ppr n1)
                   <+> text "with" <+> quotes (ppr n2))
                2 (hang (text "both bound by the partial type signature:")
                        2 (ppr fn_name <+> dcolon <+> ppr hs_ty))
    TcRnPartialTypeSigBadQuantifier n fn_name m_unif_ty hs_ty
      -> mkSimpleDecorated $
           hang (text "Can't quantify over" <+> quotes (ppr n))
                2 (vcat [ hang (text "bound by the partial type signature:")
                             2 (ppr fn_name <+> dcolon <+> ppr hs_ty)
                        , extra ])
      where
        extra | Just rhs_ty <- m_unif_ty
              = sep [ quotes (ppr n), text "should really be", quotes (ppr rhs_ty) ]
              | otherwise
              = empty
    TcRnMissingSignature what _ _ ->
      mkSimpleDecorated $
      case what of
        MissingPatSynSig p ->
          hang (text "Pattern synonym with no type signature:")
            2 (text "pattern" <+> pprPrefixName (patSynName p) <+> dcolon <+> pprPatSynType p)
        MissingTopLevelBindingSig name ty ->
          hang (text "Top-level binding with no type signature:")
            2 (pprPrefixName name <+> dcolon <+> pprSigmaType ty)
        MissingTyConKindSig tc cusks_enabled ->
          hang msg
            2 (text "type" <+> pprPrefixName (tyConName tc) <+> dcolon <+> pprKind (tyConKind tc))
          where
            msg | cusks_enabled
                = text "Top-level type constructor with no standalone kind signature or CUSK:"
                | otherwise
                = text "Top-level type constructor with no standalone kind signature:"

    TcRnPolymorphicBinderMissingSig n ty
      -> mkSimpleDecorated $
           sep [ text "Polymorphic local binding with no type signature:"
               , nest 2 $ pprPrefixName n <+> dcolon <+> ppr ty ]
    TcRnOverloadedSig sig
      -> mkSimpleDecorated $
           hang (text "Overloaded signature conflicts with monomorphism restriction")
              2 (ppr sig)
    TcRnTupleConstraintInst _
      -> mkSimpleDecorated $ text "You can't specify an instance for a tuple constraint"
    TcRnAbstractClassInst clas
      -> mkSimpleDecorated $
           text "Cannot define instance for abstract class" <+>
           quotes (ppr (className clas))
    TcRnNoClassInstHead tau
      -> mkSimpleDecorated $
           hang (text "Instance head is not headed by a class:") 2 (pprType tau)
    TcRnUserTypeError ty
      -> mkSimpleDecorated (pprUserTypeErrorTy ty)
    TcRnConstraintInKind ty
      -> mkSimpleDecorated $
           text "Illegal constraint in a kind:" <+> pprType ty
    TcRnUnboxedTupleOrSumTypeFuncArg tuple_or_sum ty
      -> mkSimpleDecorated $
           sep [ text "Illegal unboxed" <+> what <+> text "type as function argument:"
               , pprType ty ]
        where
          what = case tuple_or_sum of
            UnboxedTupleType -> text "tuple"
            UnboxedSumType   -> text "sum"
    TcRnLinearFuncInKind ty
      -> mkSimpleDecorated $
           text "Illegal linear function in a kind:" <+> pprType ty
    TcRnForAllEscapeError ty kind
      -> mkSimpleDecorated $ vcat
           [ hang (text "Quantified type's kind mentions quantified type variable")
                2 (text "type:" <+> quotes (ppr ty))
           , hang (text "where the body of the forall has this kind:")
                2 (quotes (pprKind kind)) ]
    TcRnVDQInTermType mb_ty
      -> mkSimpleDecorated $ vcat
           [ case mb_ty of
               Nothing -> main_msg
               Just ty -> hang (main_msg <> char ':') 2 (pprType ty)
           , text "(GHC does not yet support this)" ]
      where
        main_msg =
          text "Illegal visible, dependent quantification" <+>
          text "in the type of a term"
    TcRnBadQuantPredHead ty
      -> mkSimpleDecorated $
           hang (text "Quantified predicate must have a class or type variable head:")
              2 (pprType ty)
    TcRnIllegalTupleConstraint ty
      -> mkSimpleDecorated $
           text "Illegal tuple constraint:" <+> pprType ty
    TcRnNonTypeVarArgInConstraint ty
      -> mkSimpleDecorated $
           hang (text "Non type-variable argument")
              2 (text "in the constraint:" <+> pprType ty)
    TcRnIllegalImplicitParam ty
      -> mkSimpleDecorated $
           text "Illegal implicit parameter" <+> quotes (pprType ty)
    TcRnIllegalConstraintSynonymOfKind kind
      -> mkSimpleDecorated $
           text "Illegal constraint synonym of kind:" <+> quotes (pprKind kind)
    TcRnIllegalClassInst tcf
      -> mkSimpleDecorated $
           vcat [ text "Illegal instance for a" <+> ppr tcf
                , text "A class instance must be for a class" ]
    TcRnOversaturatedVisibleKindArg ty
      -> mkSimpleDecorated $
           text "Illegal oversaturated visible kind argument:" <+>
           quotes (char '@' <> pprParendType ty)
    TcRnBadAssociatedType clas tc
      -> mkSimpleDecorated $
           hsep [ text "Class", quotes (ppr clas)
                , text "does not have an associated type", quotes (ppr tc) ]
    TcRnForAllRankErr rank ty
      -> let herald = case tcSplitForAllTyVars ty of
               ([], _) -> text "Illegal qualified type:"
               _       -> text "Illegal polymorphic type:"
             extra = case rank of
               MonoTypeConstraint -> text "A constraint must be a monotype"
               _                  -> empty
         in mkSimpleDecorated $ vcat [hang herald 2 (pprType ty), extra]
    TcRnMonomorphicBindings bindings
      -> let pp_bndrs = pprBindings bindings
         in mkSimpleDecorated $
              sep [ text "The Monomorphism Restriction applies to the binding"
                  <> plural bindings
                  , text "for" <+> pp_bndrs ]
    TcRnOrphanInstance (Left cls_inst)
      -> mkSimpleDecorated $
           hang (text "Orphan class instance:")
              2 (pprInstanceHdr cls_inst)
    TcRnOrphanInstance (Right fam_inst)
      -> mkSimpleDecorated $
           hang (text "Orphan family instance:")
              2 (pprFamInst fam_inst)
    TcRnFunDepConflict unit_state sorted
      -> let herald = text "Functional dependencies conflict between instance declarations:"
         in mkSimpleDecorated $
              pprWithUnitState unit_state $ (hang herald 2 (pprInstances $ NE.toList sorted))
    TcRnDupInstanceDecls unit_state sorted
      -> let herald = text "Duplicate instance declarations:"
         in mkSimpleDecorated $
              pprWithUnitState unit_state $ (hang herald 2 (pprInstances $ NE.toList sorted))
    TcRnConflictingFamInstDecls sortedNE
      -> let sorted = NE.toList sortedNE
         in mkSimpleDecorated $
              hang (text "Conflicting family instance declarations:")
                 2 (vcat [ pprCoAxBranchUser (coAxiomTyCon ax) (coAxiomSingleBranch ax)
                         | fi <- sorted
                         , let ax = famInstAxiom fi ])
    TcRnFamInstNotInjective rea fam_tc (eqn1 NE.:| rest_eqns)
      -> let (herald, show_kinds) = case rea of
               InjErrRhsBareTyVar tys ->
                 (injectivityErrorHerald $$
                  text "RHS of injective type family equation is a bare" <+>
                  text "type variable" $$
                  text "but these LHS type and kind patterns are not bare" <+>
                  text "variables:" <+> pprQuotedList tys, False)
               InjErrRhsCannotBeATypeFam ->
                 (injectivityErrorHerald $$
                   text "RHS of injective type family equation cannot" <+>
                   text "be a type family:", False)
               InjErrRhsOverlap ->
                  (text "Type family equation right-hand sides overlap; this violates" $$
                   text "the family's injectivity annotation:", False)
               InjErrCannotInferFromRhs tvs has_kinds _ ->
                 let show_kinds = has_kinds == YesHasKinds
                     what = if show_kinds then text "Type/kind" else text "Type"
                     body = sep [ what <+> text "variable" <>
                                  pluralVarSet tvs <+> pprVarSet tvs (pprQuotedList . scopedSort)
                                , text "cannot be inferred from the right-hand side." ]
                     in (injectivityErrorHerald $$ body $$ text "In the type family equation:", show_kinds)

         in mkSimpleDecorated $ pprWithExplicitKindsWhen show_kinds $
              hang herald
                2 (vcat (map (pprCoAxBranchUser fam_tc) (eqn1 : rest_eqns)))
    TcRnBangOnUnliftedType ty
      -> mkSimpleDecorated $
           text "Strictness flag has no effect on unlifted type" <+> quotes (ppr ty)
    TcRnLazyBangOnUnliftedType ty
      -> mkSimpleDecorated $
           text "Lazy flag has no effect on unlifted type" <+> quotes (ppr ty)
    TcRnMultipleDefaultDeclarations dup_things
      -> mkSimpleDecorated $
           hang (text "Multiple default declarations")
              2 (vcat (map pp dup_things))
         where
           pp :: LDefaultDecl GhcRn -> SDoc
           pp (L locn (DefaultDecl _ _))
             = text "here was another default declaration" <+> ppr (locA locn)
    TcRnBadDefaultType ty deflt_clss
      -> mkSimpleDecorated $
           hang (text "The default type" <+> quotes (ppr ty) <+> text "is not an instance of")
              2 (foldr1 (\a b -> a <+> text "or" <+> b) (map (quotes. ppr) deflt_clss))
    TcRnPatSynBundledWithNonDataCon
      -> mkSimpleDecorated $
           text "Pattern synonyms can be bundled only with datatypes."
    TcRnPatSynBundledWithWrongType expected_res_ty res_ty
      -> mkSimpleDecorated $
           text "Pattern synonyms can only be bundled with matching type constructors"
               $$ text "Couldn't match expected type of"
               <+> quotes (ppr expected_res_ty)
               <+> text "with actual type of"
               <+> quotes (ppr res_ty)
    TcRnDupeModuleExport mod
      -> mkSimpleDecorated $
           hsep [ text "Duplicate"
                , quotes (text "Module" <+> ppr mod)
                , text "in export list" ]
    TcRnExportedModNotImported mod
      -> mkSimpleDecorated
       $ formatExportItemError
           (text "module" <+> ppr mod)
           "is not imported"
    TcRnNullExportedModule mod
      -> mkSimpleDecorated
       $ formatExportItemError
           (text "module" <+> ppr mod)
           "exports nothing"
    TcRnMissingExportList mod
      -> mkSimpleDecorated
       $ formatExportItemError
           (text "module" <+> ppr mod)
           "is missing an export list"
    TcRnExportHiddenComponents export_item
      -> mkSimpleDecorated
       $ formatExportItemError
           (ppr export_item)
           "attempts to export constructors or class methods that are not visible here"
    TcRnDuplicateExport gre ie1 ie2
      -> mkSimpleDecorated $
           hsep [ quotes (ppr $ greName gre)
                , text "is exported by", quotes (ppr ie1)
                , text "and",            quotes (ppr ie2) ]
    TcRnExportedParentChildMismatch parent_name ty_thing child parent_names
      -> mkSimpleDecorated $
           text "The type constructor" <+> quotes (ppr parent_name)
                 <+> text "is not the parent of the" <+> text what_is
                 <+> quotes thing <> char '.'
                 $$ text (capitalise what_is)
                    <> text "s can only be exported with their parent type constructor."
                 $$ (case parents of
                       [] -> empty
                       [_] -> text "Parent:"
                       _  -> text "Parents:") <+> fsep (punctuate comma parents)
      where
        pp_category :: TyThing -> String
        pp_category (AnId i)
          | isRecordSelector i = "record selector"
        pp_category i = tyThingCategory i
        what_is = pp_category ty_thing
        thing = ppr $ greOccName child
        parents = map ppr parent_names
    TcRnConflictingExports occ child_gre1 ie1 child_gre2 ie2
      -> mkSimpleDecorated $
           vcat [ text "Conflicting exports for" <+> quotes (ppr occ) <> colon
                , ppr_export child_gre1 ie1
                , ppr_export child_gre2 ie2
                ]
      where
        ppr_export gre ie =
          nest 3 $
            hang (quotes (ppr ie) <+> text "exports" <+> quotes (ppr $ greName gre))
               2 (pprNameProvenance gre)
    TcRnDuplicateFieldExport (gre, ie1) gres_ies ->
      mkSimpleDecorated $
           vcat ( hsep [ text "Duplicate record field"
                       , quotes (ppr $ greOccName gre)
                       , text "in export list" <> colon ]
                : map ppr_export ((gre,ie1) : NE.toList gres_ies)
                )
      where
        ppr_export (gre,ie) =
          nest 3 $
            hang (sep [ quotes (ppr ie) <+> text "exports the field" <+> quotes (ppr $ greName gre)
                       , text "belonging to the constructor" <> plural fld_cons <+> pprQuotedList fld_cons ])
               2 (pprNameProvenance gre)
          where
            fld_cons :: [ConLikeName]
            fld_cons = nonDetEltsUniqSet $ recFieldCons $ fieldGREInfo gre
    TcRnAmbiguousFieldInUpdate (gre1, gre2, gres)
      -> mkSimpleDecorated $
          vcat [ text "Ambiguous record field" <+> fld <> dot
               , hang (text "It could refer to any of the following:")
                  2 $ vcat (map pprSugg (gre1 : gre2 : gres))
               ]
        where
          fld = quotes $ ppr (occNameFS $ greOccName gre1)
          pprSugg gre = vcat [ bullet <+> pprGRE gre <> comma
                             , nest 2 (pprNameProvenance gre) ]
          pprGRE gre = case gre_info gre of
            IAmRecField {}
              -> let parent = par_is $ gre_par gre
                 in text "record field" <+> fld <+> text "of" <+> quotes (ppr parent)
            _ -> text "variable" <+> fld
    TcRnAmbiguousRecordUpdate _rupd tc
      -> mkSimpleDecorated $
          vcat [ text "Ambiguous record update with parent" <+> what <> dot
               , hsep [ text "This type-directed disambiguation mechanism"
                      , text "will not be supported by -XDuplicateRecordFields in future releases of GHC." ]
               , text "Consider disambiguating using module qualification instead."
               ]
        where
          what :: SDoc
          what = text "type constructor" <+> quotes (ppr $ RecSelData tc)
    TcRnMissingFields con fields
      -> mkSimpleDecorated $ vcat [header, nest 2 rest]
         where
           rest | null fields = empty
                | otherwise   = vcat (fmap pprField fields)
           header = text "Fields of" <+> quotes (ppr con) <+>
                    text "not initialised" <>
                    if null fields then empty else colon
    TcRnFieldUpdateInvalidType prs
      -> mkSimpleDecorated $
           hang (text "Record update for insufficiently polymorphic field"
                   <> plural prs <> colon)
              2 (vcat [ ppr f <+> dcolon <+> ppr ty | (f,ty) <- prs ])
    TcRnMissingStrictFields con fields
      -> mkSimpleDecorated $ vcat [header, nest 2 rest]
         where
           rest | null fields = empty  -- Happens for non-record constructors
                                       -- with strict fields
                | otherwise   = vcat (fmap pprField fields)

           header = text "Constructor" <+> quotes (ppr con) <+>
                    text "does not have the required strict field(s)" <>
                    if null fields then empty else colon
    TcRnBadRecordUpdate upd_flds reason
      -> case reason of
          NoConstructorHasAllFields { conflictingFields = conflicts }
            | [fld] <- conflicts
            -> mkSimpleDecorated $
                vcat [ header
                     , text "No constructor in scope has the field" <+> quotes (ppr fld) ]
            | otherwise
            ->
              mkSimpleDecorated $
                vcat [ header
                     , hang (text "No constructor in scope has all of the following fields:")
                        2 (pprQuotedList conflicts) ]
            where
              header :: SDoc
              header = text "Invalid record update."
          MultiplePossibleParents (par1, par2, pars) ->
            mkSimpleDecorated $
              vcat [ hang (text "Ambiguous record update with field" <> plural upd_flds)
                       2 ppr_flds
                   , hang (thisOrThese upd_flds <+> text "field" <> plural upd_flds <+> what_parent)
                       2 (quotedListWithAnd (map ppr (par1:par2:pars))) ]
            where
              ppr_flds, what_parent, which :: SDoc
              ppr_flds = quotedListWithAnd $ map ppr upd_flds
              what_parent = case par1 of
                RecSelData   {} -> text "appear" <> singular upd_flds
                                <+> text "in" <+> which <+> text "datatypes"
                RecSelPatSyn {} -> isOrAre upd_flds <+> text "associated with"
                                <+> which <+> text "pattern synonyms"
              which = case pars of
                [] -> text "both"
                _  -> text "all of the"
          InvalidTyConParent tc pars ->
            mkSimpleDecorated $
              vcat [ hang (text "No data constructor of" <+> what $$ text "has all of the fields:")
                      2 (pprQuotedList upd_flds)
                   , pat_syn_msg ]
            where
              what = text "type constructor" <+> quotes (ppr (RecSelData tc))
              pat_syn_msg
                | any (\case { RecSelPatSyn {} -> True; _ -> False}) pars
                = text "NB: type-directed disambiguation is not supported for pattern synonym record fields."
                | otherwise
                = empty
    TcRnStaticFormNotClosed name reason
      -> mkSimpleDecorated $
           quotes (ppr name)
             <+> text "is used in a static form but it is not closed"
             <+> text "because it"
             $$ sep (causes reason)
         where
          causes :: NotClosedReason -> [SDoc]
          causes NotLetBoundReason = [text "is not let-bound."]
          causes (NotTypeClosed vs) =
            [ text "has a non-closed type because it contains the"
            , text "type variables:" <+>
              pprVarSet vs (hsep . punctuate comma . map (quotes . ppr))
            ]
          causes (NotClosed n reason) =
            let msg = text "uses" <+> quotes (ppr n) <+> text "which"
             in case reason of
                  NotClosed _ _ -> msg : causes reason
                  _   -> let (xs0, xs1) = splitAt 1 $ causes reason
                          in fmap (msg <+>) xs0 ++ xs1
    TcRnUselessTypeable
      -> mkSimpleDecorated $
           text "Deriving" <+> quotes (ppr typeableClassName) <+>
           text "has no effect: all types now auto-derive Typeable"
    TcRnDerivingDefaults cls
      -> mkSimpleDecorated $ sep
                     [ text "Both DeriveAnyClass and"
                       <+> text "GeneralizedNewtypeDeriving are enabled"
                     , text "Defaulting to the DeriveAnyClass strategy"
                       <+> text "for instantiating" <+> ppr cls
                     ]
    TcRnNonUnaryTypeclassConstraint ct
      -> mkSimpleDecorated $
           quotes (ppr ct)
           <+> text "is not a unary constraint, as expected by a deriving clause"
    TcRnPartialTypeSignatures _ theta
      -> mkSimpleDecorated $
           text "Found type wildcard" <+> quotes (char '_')
                       <+> text "standing for" <+> quotes (pprTheta theta)
    TcRnCannotDeriveInstance cls cls_tys mb_strat newtype_deriving reason
      -> mkSimpleDecorated $
           derivErrDiagnosticMessage cls cls_tys mb_strat newtype_deriving True reason
    TcRnLookupInstance cls tys reason
      -> mkSimpleDecorated $
          text "Couldn't match instance:" <+>
           lookupInstanceErrDiagnosticMessage cls tys reason
    TcRnLazyGADTPattern
      -> mkSimpleDecorated $
           hang (text "An existential or GADT data constructor cannot be used")
              2 (text "inside a lazy (~) pattern")
    TcRnArrowProcGADTPattern
      -> mkSimpleDecorated $
           text "Proc patterns cannot use existential or GADT data constructors"

    TcRnSpecialClassInst cls because_safeHaskell
      -> mkSimpleDecorated $
            text "Class" <+> quotes (ppr $ className cls)
                   <+> text "does not support user-specified instances"
                   <> safeHaskell_msg
          where
            safeHaskell_msg
              | because_safeHaskell
              = text " when Safe Haskell is enabled."
              | otherwise
              = dot
    TcRnForallIdentifier rdr_name
      -> mkSimpleDecorated $
            fsep [ text "The use of" <+> quotes (ppr rdr_name)
                                     <+> text "as an identifier",
                   text "will become an error in a future GHC release." ]
    TcRnTypeEqualityOutOfScope
      -> mkDecorated
           [ text "The" <+> quotes (text "~") <+> text "operator is out of scope." $$
             text "Assuming it to stand for an equality constraint."
           , text "NB:" <+> (quotes (text "~") <+> text "used to be built-in syntax but now is a regular type operator" $$
                             text "exported from Data.Type.Equality and Prelude.") $$
             text "If you are using a custom Prelude, consider re-exporting it."
           , text "This will become an error in a future GHC release." ]
    TcRnTypeEqualityRequiresOperators
      -> mkSimpleDecorated $
            fsep [ text "The use of" <+> quotes (text "~")
                                     <+> text "without TypeOperators",
                   text "will become an error in a future GHC release." ]
    TcRnIllegalTypeOperator overall_ty op
      -> mkSimpleDecorated $
           text "Illegal operator" <+> quotes (ppr op) <+>
           text "in type" <+> quotes (ppr overall_ty)
    TcRnIllegalTypeOperatorDecl name
      -> mkSimpleDecorated $
        text "Illegal declaration of a type or class operator" <+> quotes (ppr name)
    TcRnGADTMonoLocalBinds
      -> mkSimpleDecorated $
            fsep [ text "Pattern matching on GADTs without MonoLocalBinds"
                 , text "is fragile." ]
    TcRnIncorrectNameSpace name _
      -> mkSimpleDecorated $ msg
        where
          msg
            -- We are in a type-level namespace,
            -- and the name is incorrectly at the term-level.
            | isValNameSpace ns
            = text "The" <+> what <+> text "does not live in the type-level namespace"

            -- We are in a term-level namespace,
            -- and the name is incorrectly at the type-level.
            | otherwise
            = text "Illegal term-level use of the" <+> what
          ns = nameNameSpace name
          what = pprNameSpace ns <+> quotes (ppr name)
    TcRnNotInScope err name imp_errs _
      -> mkSimpleDecorated $
           pprScopeError name err $$ vcat (map ppr imp_errs)
    TcRnTermNameInType name _
      -> mkSimpleDecorated $
           quotes (ppr name) <+>
             (text "is a term-level binding") $+$
             (text " and can not be used at the type level.")
    TcRnUntickedPromotedThing thing
      -> mkSimpleDecorated $
         text "Unticked promoted" <+> what
           where
             what :: SDoc
             what = case thing of
               UntickedExplicitList -> text "list" <> dot
               UntickedConstructor fixity nm ->
                 let con      = pprUntickedConstructor fixity nm
                     bare_sym = isBareSymbol fixity nm
                 in text "constructor:" <+> con <> if bare_sym then empty else dot
    TcRnIllegalBuiltinSyntax what rdr_name
      -> mkSimpleDecorated $
           hsep [text "Illegal", what, text "of built-in syntax:", ppr rdr_name]
    TcRnWarnDefaulting tidy_wanteds tidy_tv default_ty
      -> mkSimpleDecorated $
           hang (hsep $ [ text "Defaulting" ]
                     ++
                     (case tidy_tv of
                         Nothing -> []
                         Just tv -> [text "the type variable"
                                    , quotes (ppr tv)])
                     ++
                     [ text "to type"
                     , quotes (ppr default_ty)
                     , text "in the following constraint" <> plural tidy_wanteds ])
             2
             (pprWithArising tidy_wanteds)


    TcRnForeignImportPrimExtNotSet _decl
      -> mkSimpleDecorated $
           text "`foreign import prim' requires GHCForeignImportPrim."

    TcRnForeignImportPrimSafeAnn _decl
      -> mkSimpleDecorated $
           text "The safe/unsafe annotation should not be used with `foreign import prim'."

    TcRnForeignFunctionImportAsValue _decl
      -> mkSimpleDecorated $
           text "`value' imports cannot have function types"

    TcRnFunPtrImportWithoutAmpersand _decl
      -> mkSimpleDecorated $
           text "possible missing & in foreign import of FunPtr"

    TcRnIllegalForeignDeclBackend _decl _backend expectedBknds
      -> mkSimpleDecorated $
         fsep (text "Illegal foreign declaration: requires one of these back ends:" :
               commafyWith (text "or") (map (text . backendDescription) expectedBknds))

    TcRnUnsupportedCallConv _decl unsupportedCC
      -> mkSimpleDecorated $
           case unsupportedCC of
             StdCallConvUnsupported ->
               text "the 'stdcall' calling convention is unsupported on this platform,"
               $$ text "treating as ccall"
             PrimCallConvUnsupported ->
               text "The `prim' calling convention can only be used with `foreign import'"
             JavaScriptCallConvUnsupported ->
               text "The `javascript' calling convention is unsupported on this platform"

    TcRnIllegalForeignType mArgOrResult reason
      -> mkSimpleDecorated $ hang msg 2 extra
      where
        arg_or_res = case mArgOrResult of
          Nothing -> empty
          Just Arg -> text "argument"
          Just Result -> text "result"
        msg = hsep [ text "Unacceptable", arg_or_res
                   , text "type in foreign declaration:"]
        extra =
          case reason of
            TypeCannotBeMarshaled ty why ->
              let innerMsg = quotes (ppr ty) <+> text "cannot be marshalled in a foreign call"
               in case why of
                NotADataType ->
                  quotes (ppr ty) <+> text "is not a data type"
                NewtypeDataConNotInScope _ [] ->
                  hang innerMsg 2 $ text "because its data constructor is not in scope"
                NewtypeDataConNotInScope tc _ ->
                  hang innerMsg 2 $
                    text "because the data constructor for"
                    <+> quotes (ppr tc) <+> text "is not in scope"
                UnliftedFFITypesNeeded ->
                  innerMsg $$ text "UnliftedFFITypes is required to marshal unlifted types"
                NotABoxedMarshalableTyCon -> innerMsg
                ForeignLabelNotAPtr ->
                  innerMsg $$ text "A foreign-imported address (via &foo) must have type (Ptr a) or (FunPtr a)"
                NotSimpleUnliftedType ->
                  innerMsg $$ text "foreign import prim only accepts simple unlifted types"
                NotBoxedKindAny ->
                  text "Expected kind" <+> quotes (text "Type") <+> text "or" <+> quotes (text "UnliftedType") <> comma $$
                  text "but" <+> quotes (ppr ty) <+> text "has kind" <+> quotes (ppr (typeKind ty))
            ForeignDynNotPtr expected ty ->
              vcat [ text "Expected: Ptr/FunPtr" <+> pprParendType expected <> comma, text "  Actual:" <+> ppr ty ]
            SafeHaskellMustBeInIO ->
              text "Safe Haskell is on, all FFI imports must be in the IO monad"
            IOResultExpected ->
              text "IO result type expected"
            UnexpectedNestedForall ->
              text "Unexpected nested forall"
            LinearTypesNotAllowed ->
              text "Linear types are not supported in FFI declarations, see #18472"
            OneArgExpected ->
              text "One argument expected"
            AtLeastOneArgExpected ->
              text "At least one argument expected"
    TcRnInvalidCIdentifier target
      -> mkSimpleDecorated $
           sep [quotes (ppr target) <+> text "is not a valid C identifier"]
    TcRnExpectedValueId thing
      -> mkSimpleDecorated $
           ppr thing <+> text "used where a value identifier was expected"
    TcRnRecSelectorEscapedTyVar lbl
      -> mkSimpleDecorated $
           text "Cannot use record selector" <+> quotes (ppr lbl) <+>
           text "as a function due to escaped type variables"
    TcRnPatSynNotBidirectional name
      -> mkSimpleDecorated $
           text "non-bidirectional pattern synonym"
           <+> quotes (ppr name) <+> text "used in an expression"
    TcRnSplicePolymorphicLocalVar ident
      -> mkSimpleDecorated $
           text "Can't splice the polymorphic local variable" <+> quotes (ppr ident)
    TcRnIllegalDerivingItem hs_ty
      -> mkSimpleDecorated $
           text "Illegal deriving item" <+> quotes (ppr hs_ty)
    TcRnUnexpectedAnnotation ty bang
      -> mkSimpleDecorated $
           let err = case bang of
                 HsSrcBang _ SrcUnpack _           -> "UNPACK"
                 HsSrcBang _ SrcNoUnpack _         -> "NOUNPACK"
                 HsSrcBang _ NoSrcUnpack SrcLazy   -> "laziness"
                 HsSrcBang _ _ _                   -> "strictness"
            in text "Unexpected" <+> text err <+> text "annotation:" <+> ppr ty $$
               text err <+> text "annotation cannot appear nested inside a type"
    TcRnIllegalRecordSyntax either_ty_ty
      -> mkSimpleDecorated $
           text "Record syntax is illegal here:" <+> either ppr ppr either_ty_ty
    TcRnUnexpectedTypeSplice ty
      -> mkSimpleDecorated $
           text "Unexpected type splice:" <+> ppr ty
    TcRnInvalidVisibleKindArgument arg ty
      -> mkSimpleDecorated $
           text "Cannot apply function of kind" <+> quotes (ppr ty)
             $$ text "to visible kind argument" <+> quotes (ppr arg)
    TcRnTooManyBinders ki bndrs
      -> mkSimpleDecorated $
           hang (text "Not a function kind:")
              4 (ppr ki) $$
           hang (text "but extra binders found:")
              4 (fsep (map ppr bndrs))
    TcRnDifferentNamesForTyVar n1 n2
      -> mkSimpleDecorated $
           hang (text "Different names for the same type variable:") 2 info
         where
           info | nameOccName n1 /= nameOccName n2
                = quotes (ppr n1) <+> text "and" <+> quotes (ppr n2)
                | otherwise -- Same OccNames! See C2 in
                            -- Note [Swizzling the tyvars before generaliseTcTyCon]
                = vcat [ quotes (ppr n1) <+> text "bound at" <+> ppr (getSrcLoc n1)
                       , quotes (ppr n2) <+> text "bound at" <+> ppr (getSrcLoc n2) ]
    TcRnInvalidReturnKind data_sort allowed_kind kind _suggested_ext
      -> mkSimpleDecorated $
           sep [ ppDataSort data_sort <+>
                 text "has non-" <>
                 allowed_kind_tycon
               , (if is_data_family then text "and non-variable" else empty) <+>
                 text "return kind" <+> quotes (ppr kind)
               ]
         where
          is_data_family =
            case data_sort of
              DataDeclSort{}     -> False
              DataInstanceSort{} -> False
              DataFamilySort     -> True
          allowed_kind_tycon =
            case allowed_kind of
              AnyTYPEKind  -> ppr tYPETyCon
              AnyBoxedKind -> ppr boxedRepDataConTyCon
              LiftedKind   -> ppr liftedTypeKind
    TcRnClassKindNotConstraint _kind
      -> mkSimpleDecorated $
           text "Kind signature on a class must end with" <+> ppr constraintKind $$
           text "unobscured by type families"
    TcRnUnpromotableThing name err
      -> mkSimpleDecorated $
           (hang (pprPECategory err <+> quotes (ppr name) <+> text "cannot be used here")
                        2 (parens reason))
        where
          reason = case err of
                     ConstrainedDataConPE theta
                                    -> text "it has an unpromotable context"
                                       <+> quotes (pprTheta theta)

                     FamDataConPE   -> text "it comes from a data family instance"
                     NoDataKindsDC  -> text "perhaps you intended to use DataKinds"
                     PatSynPE       -> text "pattern synonyms cannot be promoted"
                     RecDataConPE   -> same_rec_group_msg
                     ClassPE        -> same_rec_group_msg
                     TyConPE        -> same_rec_group_msg
                     TermVariablePE -> text "term variables cannot be promoted"
          same_rec_group_msg = text "it is defined and used in the same recursive group"
    TcRnMatchesHaveDiffNumArgs argsContext (MatchArgMatches match1 bad_matches)
      -> mkSimpleDecorated $
           (vcat [ pprMatchContextNouns argsContext <+>
                   text "have different numbers of arguments"
                 , nest 2 (ppr (getLocA match1))
                 , nest 2 (ppr (getLocA (NE.head bad_matches)))])
    TcRnCannotBindScopedTyVarInPatSig sig_tvs
      -> mkSimpleDecorated $
           hang (text "You cannot bind scoped type variable"
                  <> plural (NE.toList sig_tvs)
                 <+> pprQuotedList (map fst $ NE.toList sig_tvs))
              2 (text "in a pattern binding signature")
    TcRnCannotBindTyVarsInPatBind _offenders
      -> mkSimpleDecorated $
           text "Binding type variables is not allowed in pattern bindings"
    TcRnTooManyTyArgsInConPattern con_like expected_number actual_number
      -> mkSimpleDecorated $
           text "Too many type arguments in constructor pattern for" <+> quotes (ppr con_like) $$
           text "Expected no more than" <+> ppr expected_number <> semi <+> text "got" <+> ppr actual_number
    TcRnMultipleInlinePragmas poly_id fst_inl_prag inl_prags
      -> mkSimpleDecorated $
           hang (text "Multiple INLINE pragmas for" <+> ppr poly_id)
             2 (vcat (text "Ignoring all but the first"
                      : map pp_inl (fst_inl_prag : NE.toList inl_prags)))
         where
           pp_inl (L loc prag) = ppr prag <+> parens (ppr loc)
    TcRnUnexpectedPragmas poly_id bad_sigs
      -> mkSimpleDecorated $
           hang (text "Discarding unexpected pragmas for" <+> ppr poly_id)
              2 (vcat (map (ppr . getLoc) $ NE.toList bad_sigs))
    TcRnNonOverloadedSpecialisePragma fun_name
       -> mkSimpleDecorated $
            text "SPECIALISE pragma for non-overloaded function"
              <+> quotes (ppr fun_name)
    TcRnSpecialiseNotVisible name
      -> mkSimpleDecorated $
         text "You cannot SPECIALISE" <+> quotes (ppr name)
           <+> text "because its definition is not visible in this module"
    TcRnPragmaWarning {pragma_warning_occ, pragma_warning_msg, pragma_warning_import_mod, pragma_warning_defined_mod}
      -> mkSimpleDecorated $
        sep [ sep [ text "In the use of"
                <+> pprNonVarNameSpace (occNameSpace pragma_warning_occ)
                <+> quotes (ppr pragma_warning_occ)
                , parens impMsg <> colon ]
          , pprWarningTxtForMsg pragma_warning_msg ]
          where
            impMsg  = text "imported from" <+> ppr pragma_warning_import_mod <> extra
            extra | pragma_warning_import_mod == pragma_warning_defined_mod = empty
                  | otherwise = text ", but defined in" <+> ppr pragma_warning_defined_mod
    TcRnIllegalHsigDefaultMethods name meths
      -> mkSimpleDecorated $
        text "Illegal default method" <> plural (NE.toList meths) <+> text "in class definition of" <+> ppr name <+> text "in hsig file"
    TcRnHsigFixityMismatch real_thing real_fixity sig_fixity
      ->
      let ppr_fix f = ppr f <+> if f == defaultFixity then parens (text "default") else empty
      in mkSimpleDecorated $
        vcat [ppr real_thing <+> text "has conflicting fixities in the module",
              text "and its hsig file",
              text "Main module:" <+> ppr_fix real_fixity,
              text "Hsig file:" <+> ppr_fix sig_fixity]
    TcRnHsigShapeMismatch (HsigShapeSortMismatch info1 info2)
      -> mkSimpleDecorated $
            text "While merging export lists, could not combine"
            <+> ppr info1 <+> text "with" <+> ppr info2
            <+> parens (text "one is a type, the other is a plain identifier")
    TcRnHsigShapeMismatch (HsigShapeNotUnifiable name1 name2 notHere)
      ->
      let extra = if notHere
                  then text "Neither name variable originates from the current signature."
                  else empty
      in mkSimpleDecorated $
        text "While merging export lists, could not unify"
        <+> ppr name1 <+> text "with" <+> ppr name2 $$ extra
    TcRnHsigMissingModuleExport occ unit_state impl_mod
      -> mkSimpleDecorated $
            quotes (ppr occ)
        <+> text "is exported by the hsig file, but not exported by the implementing module"
        <+> quotes (pprWithUnitState unit_state $ ppr impl_mod)
    TcRnBadGenericMethod clas op
      -> mkSimpleDecorated $
        hsep [text "Class", quotes (ppr clas),
          text "has a generic-default signature without a binding", quotes (ppr op)]
    TcRnWarningMinimalDefIncomplete mindef
      -> mkSimpleDecorated $
        vcat [ text "The MINIMAL pragma does not require:"
          , nest 2 (pprBooleanFormulaNice mindef)
          , text "but there is no default implementation." ]
    TcRnDefaultMethodForPragmaLacksBinding sel_id prag
      -> mkSimpleDecorated $
        text "The" <+> hsSigDoc prag <+> text "for default method"
          <+> quotes (ppr sel_id)
          <+> text "lacks an accompanying binding"
    TcRnIgnoreSpecialisePragmaOnDefMethod sel_name
      -> mkSimpleDecorated $
        text "Ignoring SPECIALISE pragmas on default method"
          <+> quotes (ppr sel_name)
    TcRnBadMethodErr{badMethodErrClassName, badMethodErrMethodName}
      -> mkSimpleDecorated $
        hsep [text "Class", quotes (ppr badMethodErrClassName),
          text "does not have a method", quotes (ppr badMethodErrMethodName)]
    TcRnNoExplicitAssocTypeOrDefaultDeclaration name
      -> mkSimpleDecorated $
        text "No explicit" <+> text "associated type"
          <+> text "or default declaration for"
          <+> quotes (ppr name)
    TcRnIllegalTypeData
      -> mkSimpleDecorated $
        text "Illegal type-level data declaration"
    TcRnTypeDataForbids feature
      -> mkSimpleDecorated $
        ppr feature <+> text "are not allowed in type data declarations."

    TcRnIllegalNewtype con show_linear_types reason
      -> mkSimpleDecorated $
        vcat [msg, additional]
        where
          (msg,additional) =
            case reason of
              DoesNotHaveSingleField n_flds ->
                (sep [
                  text "A newtype constructor must have exactly one field",
                  nest 2 $
                    text "but" <+> quotes (ppr con) <+> text "has" <+> speakN n_flds
                ],
                ppr con <+> dcolon <+> ppr (dataConDisplayType show_linear_types con))
              IsNonLinear ->
                (text "A newtype constructor must be linear",
                ppr con <+> dcolon <+> ppr (dataConDisplayType True con))
              IsGADT ->
                (text "A newtype must not be a GADT",
                ppr con <+> dcolon <+> pprWithExplicitKindsWhen sneaky_eq_spec
                                       (ppr $ dataConDisplayType show_linear_types con))
              HasConstructorContext ->
                (text "A newtype constructor must not have a context in its type",
                ppr con <+> dcolon <+> ppr (dataConDisplayType show_linear_types con))
              HasExistentialTyVar ->
                (text "A newtype constructor must not have existential type variables",
                ppr con <+> dcolon <+> ppr (dataConDisplayType show_linear_types con))
              HasStrictnessAnnotation ->
                (text "A newtype constructor must not have a strictness annotation", empty)

          -- Is the data con a "covert" GADT?  See Note [isCovertGadtDataCon]
          -- in GHC.Core.DataCon
          sneaky_eq_spec = isCovertGadtDataCon con

    TcRnTypedTHWithPolyType ty
      -> mkSimpleDecorated $
        vcat [ text "Illegal polytype:" <+> ppr ty
             , text "The type of a Typed Template Haskell expression must" <+>
               text "not have any quantification." ]
    TcRnSpliceThrewException phase _exn exn_msg expr show_code
      -> mkSimpleDecorated $
           vcat [ text "Exception when trying to" <+> text phaseStr <+> text "compile-time code:"
                , nest 2 (text exn_msg)
                , if show_code then text "Code:" <+> ppr expr else empty]
         where phaseStr =
                 case phase of
                   SplicePhase_Run -> "run"
                   SplicePhase_CompileAndLink -> "compile and link"
    TcRnInvalidTopDecl _decl
      -> mkSimpleDecorated $
         text "Only function, value, annotation, and foreign import declarations may be added with addTopDecls"
    TcRnNonExactName name
      -> mkSimpleDecorated $
         hang (text "The binder" <+> quotes (ppr name) <+> text "is not a NameU.")
            2 (text "Probable cause: you used mkName instead of newName to generate a binding.")
    TcRnAddInvalidCorePlugin plugin
      -> mkSimpleDecorated $
         hang
           (text "addCorePlugin: invalid plugin module "
              <+> text (show plugin)
           )
           2
           (text "Plugins in the current package can't be specified.")
    TcRnAddDocToNonLocalDefn doc_loc
      -> mkSimpleDecorated $
         text "Can't add documentation to" <+> ppr_loc doc_loc <+>
         text "as it isn't inside the current module"
      where
        ppr_loc (TH.DeclDoc n) = text $ TH.pprint n
        ppr_loc (TH.ArgDoc n _) = text $ TH.pprint n
        ppr_loc (TH.InstDoc t) = text $ TH.pprint t
        ppr_loc TH.ModuleDoc = text "the module header"

    TcRnFailedToLookupThInstName th_type reason
      -> mkSimpleDecorated $
         case reason of
           NoMatchesFound ->
             text "Couldn't find any instances of"
               <+> text (TH.pprint th_type)
               <+> text "to add documentation to"
           CouldNotDetermineInstance ->
             text "Couldn't work out what instance"
               <+> text (TH.pprint th_type)
               <+> text "is supposed to be"
    TcRnCannotReifyInstance ty
      -> mkSimpleDecorated $
         hang (text "reifyInstances:" <+> quotes (ppr ty))
            2 (text "is not a class constraint or type family application")
    TcRnCannotReifyOutOfScopeThing th_name
      -> mkSimpleDecorated $
         quotes (text (TH.pprint th_name)) <+>
                 text "is not in scope at a reify"
               -- Ugh! Rather an indirect way to display the name
    TcRnCannotReifyThingNotInTypeEnv name
      -> mkSimpleDecorated $
         quotes (ppr name) <+> text "is not in the type environment at a reify"
    TcRnNoRolesAssociatedWithThing thing
      -> mkSimpleDecorated $
         text "No roles associated with" <+> (ppr thing)
    TcRnCannotRepresentType sort ty
      -> mkSimpleDecorated $
         hsep [text "Can't represent" <+> sort_doc <+>
               text "in Template Haskell:",
                 nest 2 (ppr ty)]
       where
         sort_doc = text $
           case sort of
             LinearInvisibleArgument -> "linear invisible argument"
             CoercionsInTypes -> "coercions in types"
    TcRnRunSpliceFailure mCallingFnName (ConversionFail what reason)
      -> mkSimpleDecorated
           . addCallingFn
           . addSpliceInfo
           $ pprConversionFailReason reason
      where
        addCallingFn rest =
          case mCallingFnName of
            Nothing -> rest
            Just callingFn ->
              hang (text ("Error in a declaration passed to " ++ callingFn ++ ":"))
                 2 rest
        addSpliceInfo = case what of
          ConvDec d -> addSliceInfo' "declaration" d
          ConvExp e -> addSliceInfo' "expression" e
          ConvPat p -> addSliceInfo' "pattern" p
          ConvType t -> addSliceInfo' "type" t
        addSliceInfo' what item reasonErr = reasonErr $$ descr
          where
                -- Show the item in pretty syntax normally,
                -- but with all its constructors if you say -dppr-debug
            descr = hang (text "When splicing a TH" <+> text what <> colon)
                       2 (getPprDebug $ \case
                           True  -> text (show item)
                           False -> text (TH.pprint item))
    TcRnReportCustomQuasiError _ msg -> mkSimpleDecorated $ text msg
    TcRnUnsatisfiedMinimalDef mindef
      -> mkSimpleDecorated $
        vcat [text "No explicit implementation for"
              ,nest 2 $ pprBooleanFormulaNice mindef
             ]
    TcRnMisplacedInstSig name hs_ty
      -> mkSimpleDecorated $
        vcat [ hang (text "Illegal type signature in instance declaration:")
                  2 (hang (pprPrefixName name)
                        2 (dcolon <+> ppr hs_ty))
             ]
    TcRnBadBootFamInstDecl {}
      -> mkSimpleDecorated $
        text "Illegal family instance in hs-boot file"
    TcRnIllegalFamilyInstance tycon
      -> mkSimpleDecorated $
        vcat [ text "Illegal family instance for" <+> quotes (ppr tycon)
             , nest 2 $ parens (ppr tycon <+> text "is not an indexed type family")]
    TcRnMissingClassAssoc name
      -> mkSimpleDecorated $
        text "Associated type" <+> quotes (ppr name) <+>
        text "must be inside a class instance"
    TcRnNotOpenFamily tc
      -> mkSimpleDecorated $
        text "Illegal instance for closed family" <+> quotes (ppr tc)
    TcRnNoRebindableSyntaxRecordDot -> mkSimpleDecorated $
      text "RebindableSyntax is required if OverloadedRecordUpdate is enabled."
    TcRnNoFieldPunsRecordDot -> mkSimpleDecorated $
      text "For this to work enable NamedFieldPuns"
    TcRnIllegalStaticExpression e -> mkSimpleDecorated $
        text "Illegal static expression:" <+> ppr e
    TcRnIllegalStaticFormInSplice e -> mkSimpleDecorated $
      sep [ text "static forms cannot be used in splices:"
          , nest 2 $ ppr e
          ]
    TcRnListComprehensionDuplicateBinding n -> mkSimpleDecorated $
        (text "Duplicate binding in parallel list comprehension for:"
          <+> quotes (ppr n))
    TcRnEmptyStmtsGroup cause -> mkSimpleDecorated  $ case cause of
      EmptyStmtsGroupInParallelComp ->
        text "Empty statement group in parallel comprehension"
      EmptyStmtsGroupInTransformListComp ->
        text "Empty statement group preceding 'group' or 'then'"
      EmptyStmtsGroupInDoNotation ctxt ->
        text "Empty" <+> pprHsDoFlavour ctxt
      EmptyStmtsGroupInArrowNotation ->
        text "Empty 'do' block in an arrow command"
    TcRnLastStmtNotExpr ctxt (UnexpectedStatement stmt) ->
      mkSimpleDecorated $ hang last_error 2 (ppr stmt)
      where
        last_error =
          text "The last statement in" <+> pprAStmtContext ctxt
          <+> text "must be an expression"
    TcRnUnexpectedStatementInContext ctxt (UnexpectedStatement stmt) _ -> mkSimpleDecorated $
       sep [ text "Unexpected" <+> pprStmtCat stmt <+> text "statement"
                       , text "in" <+> pprAStmtContext ctxt ]
    TcRnIllegalTupleSection -> mkSimpleDecorated $
      text "Illegal tuple section"
    TcRnIllegalImplicitParameterBindings eBinds -> mkSimpleDecorated $
        either msg msg eBinds
      where
        msg binds = hang
          (text "Implicit-parameter bindings illegal in an mdo expression")
          2 (ppr binds)
    TcRnSectionWithoutParentheses expr -> mkSimpleDecorated $
      hang (text "A section must be enclosed in parentheses")
         2 (text "thus:" <+> (parens (ppr expr)))

    TcRnCapturedTermName tv_name shadowed_term_names
      -> mkSimpleDecorated $
        text "The type variable" <+> quotes (ppr tv_name) <+>
          text "is implicitly quantified," $+$
          text "even though another variable of the same name is in scope:" $+$
          nest 2 var_names $+$
          text "This is not forward-compatible with a planned GHC extension, RequiredTypeArguments."
        where
          var_names = case shadowed_term_names of
              Left gbl_names -> vcat (map (\name -> quotes (ppr $ greName name) <+> pprNameProvenance name) gbl_names)
              Right lcl_name -> quotes (ppr lcl_name) <+> text "defined at"
                <+> ppr (nameSrcLoc lcl_name)
    TcRnBindingOfExistingName name -> mkSimpleDecorated $
      text "Illegal binding of an existing name:" <+> ppr (filterCTuple name)
    TcRnMultipleFixityDecls loc rdr_name -> mkSimpleDecorated $
      vcat [text "Multiple fixity declarations for" <+> quotes (ppr rdr_name),
            text "also at " <+> ppr loc]
    TcRnIllegalPatternSynonymDecl -> mkSimpleDecorated $
      text "Illegal pattern synonym declaration"
    TcRnIllegalClassBinding dsort bind -> mkSimpleDecorated $
      vcat [ what <+> text "not allowed in" <+> decl_sort
              , nest 2 (ppr bind) ]
      where
        decl_sort = case dsort of
          ClassDeclSort -> text "class declaration:"
          InstanceDeclSort -> text "instance declaration:"
        what = case bind of
                  PatBind {}    -> text "Pattern bindings (except simple variables)"
                  PatSynBind {} -> text "Pattern synonyms"
                                   -- Associated pattern synonyms are not implemented yet
                  _ -> pprPanic "rnMethodBind" (ppr bind)
    TcRnOrphanCompletePragma -> mkSimpleDecorated $
      text "Orphan COMPLETE pragmas not supported" $$
      text "A COMPLETE pragma must mention at least one data constructor" $$
      text "or pattern synonym defined in the same module."
    TcRnEmptyCase ctxt -> mkSimpleDecorated message
      where
        pp_ctxt = case ctxt of
          CaseAlt                                  -> text "case expression"
          LamCaseAlt LamCase                       -> text "\\case expression"
          ArrowMatchCtxt (ArrowLamCaseAlt LamCase) -> text "\\case command"
          ArrowMatchCtxt ArrowCaseAlt              -> text "case command"
          ArrowMatchCtxt KappaExpr                 -> text "kappa abstraction"
          _                                        -> text "(unexpected)"
                                                      <+> pprMatchContextNoun ctxt

        message = case ctxt of
          LamCaseAlt LamCases -> lcases_msg <+> text "expression"
          ArrowMatchCtxt (ArrowLamCaseAlt LamCases) -> lcases_msg <+> text "command"
          _ -> text "Empty list of alternatives in" <+> pp_ctxt

        lcases_msg =
          text "Empty list of alternatives is not allowed in \\cases"
    TcRnNonStdGuards (NonStandardGuards guards) -> mkSimpleDecorated $
      text "accepting non-standard pattern guards" $$
      nest 4 (interpp'SP guards)
    TcRnDuplicateSigDecl pairs@((L _ name, sig) :| _) -> mkSimpleDecorated $
      vcat [ text "Duplicate" <+> what_it_is
            <> text "s for" <+> quotes (ppr name)
          , text "at" <+> vcat (map ppr $ sortBy leftmost_smallest
                                        $ map (getLocA . fst)
                                        $ NE.toList pairs)
          ]
      where
        what_it_is = hsSigDoc sig
    TcRnMisplacedSigDecl sig -> mkSimpleDecorated $
      sep [text "Misplaced" <+> hsSigDoc sig <> colon, ppr sig]
    TcRnUnexpectedDefaultSig sig -> mkSimpleDecorated $
      hang (text "Unexpected default signature:")
         2 (ppr sig)
    TcRnBindInBootFile -> mkSimpleDecorated $
      text "Bindings in hs-boot files are not allowed"
    TcRnDuplicateMinimalSig sig1 sig2 otherSigs -> mkSimpleDecorated $
      vcat [ text "Multiple minimal complete definitions"
           , text "at" <+> vcat (map ppr $ sortBy leftmost_smallest $ map getLocA sigs)
           , text "Combine alternative minimal complete definitions with `|'" ]
      where
        sigs = sig1 : sig2 : otherSigs
    TcRnLoopySuperclassSolve wtd_loc wtd_pty ->
      mkSimpleDecorated $ vcat [ header, warning, user_manual ]
      where
        header, warning, user_manual :: SDoc
        header
          = vcat [ text "I am solving the constraint" <+> quotes (ppr wtd_pty) <> comma
                 , nest 2 $ pprCtOrigin (ctLocOrigin wtd_loc) <> comma
                 , text "in a way that might turn out to loop at runtime." ]
        warning
          = vcat [ text "Starting from GHC 9.10, this warning will turn into an error." ]
        user_manual =
          vcat [ text "See the user manual, § Undecidable instances and loopy superclasses." ]
    TcRnIllegalInstanceHeadDecl head_ty -> mkSimpleDecorated $
      hang (text "Illegal head of an instance declaration:"
              <+> quotes (ppr head_ty))
        2 (vcat [ text "Instance heads must be of the form"
                , nest 2 $ text "C ty_1 ... ty_n"
                , text "where" <+> quotes (char 'C')
                  <+> text "is a class"
                ])
    TcRnUnexpectedStandaloneDerivingDecl -> mkSimpleDecorated $
      text "Illegal standalone deriving declaration"
    TcRnUnusedVariableInRuleDecl name var -> mkSimpleDecorated $
      sep [text "Rule" <+> doubleQuotes (ftext name) <> colon,
          text "Forall'd variable" <+> quotes (ppr var) <+>
                  text "does not appear on left hand side"]
    TcRnUnexpectedStandaloneKindSig -> mkSimpleDecorated $
      text "Illegal standalone kind signature"
    TcRnIllegalRuleLhs errReason name lhs bad_e -> mkSimpleDecorated $
      sep [text "Rule" <+> pprRuleName name <> colon,
           nest 2 (vcat [err,
                         text "in left-hand side:" <+> ppr lhs])]
      $$
      text "LHS must be of form (f e1 .. en) where f is not forall'd"
      where
        err = case errReason of
          UnboundVariable uv nis -> pprScopeError uv nis
          IllegalExpression -> text "Illegal expression:" <+> ppr bad_e
    TcRnBadAssocRhs ns -> mkSimpleDecorated $
      hang (text "The RHS of an associated type declaration mentions"
                <+> text "out-of-scope variable" <> plural ns
                <+> pprWithCommas (quotes . ppr) ns)
              2 (text "All such variables must be bound on the LHS")
    TcRnDuplicateRoleAnnot list -> mkSimpleDecorated $
      hang (text "Duplicate role annotations for" <+>
            quotes (ppr $ roleAnnotDeclName first_decl) <> colon)
        2 (vcat $ map pp_role_annot $ NE.toList sorted_list)
      where
        sorted_list = NE.sortBy cmp_loc list
        ((L _ first_decl) :| _) = sorted_list

        pp_role_annot (L loc decl) = hang (ppr decl)
                                        4 (text "-- written at" <+> ppr (locA loc))

        cmp_loc = leftmost_smallest `on` getLocA
    TcRnDuplicateKindSig list -> mkSimpleDecorated $
      hang (text "Duplicate standalone kind signatures for" <+>
            quotes (ppr $ standaloneKindSigName first_decl) <> colon)
        2 (vcat $ map pp_kisig $ NE.toList sorted_list)
      where
        sorted_list = NE.sortBy cmp_loc list
        ((L _ first_decl) :| _) = sorted_list

        pp_kisig (L loc decl) =
          hang (ppr decl) 4 (text "-- written at" <+> ppr (locA loc))

        cmp_loc = leftmost_smallest `on` getLocA
    TcRnIllegalDerivStrategy ds -> mkSimpleDecorated $
      text "Illegal deriving strategy" <> colon <+> derivStrategyName ds
    TcRnIllegalMultipleDerivClauses -> mkSimpleDecorated $
      text "Illegal use of multiple, consecutive deriving clauses"
    TcRnNoDerivStratSpecified{} -> mkSimpleDecorated $ text
      "No deriving strategy specified. Did you want stock, newtype, or anyclass?"
    TcRnStupidThetaInGadt{} -> mkSimpleDecorated $
      vcat [text "No context is allowed on a GADT-style data declaration",
            text "(You can put a context on each constructor, though.)"]
    TcRnBadImplicitSplice -> mkSimpleDecorated $
         text "Parse error: module header, import declaration"
      $$ text "or top-level declaration expected."
    TcRnShadowedTyVarNameInFamResult resName -> mkSimpleDecorated $
       hsep [ text "Type variable", quotes (ppr resName) <> comma
            , text "naming a type family result,"
            ] $$
      text "shadows an already bound type variable"
    TcRnIncorrectTyVarOnLhsOfInjCond resName injFrom -> mkSimpleDecorated $
        vcat [ text $ "Incorrect type variable on the LHS of "
                   ++ "injectivity condition"
      , nest 5
      ( vcat [ text "Expected :" <+> ppr resName
             , text "Actual   :" <+> ppr injFrom ])]
    TcRnUnknownTyVarsOnRhsOfInjCond errorVars -> mkSimpleDecorated $
      hsep [ text "Unknown type variable" <> plural errorVars
           , text "on the RHS of injectivity condition:"
           , interpp'SP errorVars ]
    TcRnBadlyStaged reason bind_lvl use_lvl
      -> mkSimpleDecorated $
         text "Stage error:" <+> pprStageCheckReason reason <+>
         hsep [text "is bound at stage" <+> ppr bind_lvl,
               text "but used at stage" <+> ppr use_lvl]
    TcRnStageRestriction reason
      -> mkSimpleDecorated $
         sep [ text "GHC stage restriction:"
             , nest 2 (vcat [ pprStageCheckReason reason <+>
                              text "is used in a top-level splice, quasi-quote, or annotation,"
                            , text "and must be imported, not defined locally"])]
    TcRnTyThingUsedWrong sort thing name
      -> mkSimpleDecorated $
         pprTyThingUsedWrong sort thing name
    TcRnCannotDefaultKindVar var knd ->
      mkSimpleDecorated $
      (vcat [ text "Cannot default kind variable" <+> quotes (ppr var)
            , text "of kind:" <+> ppr knd
            , text "Perhaps enable PolyKinds or add a kind signature" ])
    TcRnUninferrableTyVar tidied_tvs context ->
      mkSimpleDecorated $
      pprWithExplicitKindsWhen True $
      vcat [ text "Uninferrable type variable"
              <> plural tidied_tvs
              <+> pprWithCommas pprTyVar tidied_tvs
              <+> text "in"
            , pprUninferrableTyVarCtx context ]
    TcRnSkolemEscape escapees tv orig_ty ->
      mkSimpleDecorated $
      pprWithExplicitKindsWhen True $
      vcat [ sep [ text "Cannot generalise type; skolem" <> plural escapees
                , quotes $ pprTyVars escapees
                , text "would escape" <+> itsOrTheir escapees <+> text "scope"
                ]
          , sep [ text "if I tried to quantify"
                , pprTyVar tv
                , text "in this type:"
                ]
          , nest 2 (pprTidiedType orig_ty)
          , text "(Indeed, I sometimes struggle even printing this correctly,"
          , text " due to its ill-scoped nature.)"
          ]
    TcRnPatSynEscapedCoercion arg bad_co_ne -> mkSimpleDecorated $
      vcat [ text "Iceland Jack!  Iceland Jack! Stop torturing me!"
           , hang (text "Pattern-bound variable")
                2 (ppr arg <+> dcolon <+> ppr (idType arg))
           , nest 2 $
             hang (text "has a type that mentions pattern-bound coercion"
                   <> plural bad_co_list <> colon)
                2 (pprWithCommas ppr bad_co_list)
           , text "Hint: use -fprint-explicit-coercions to see the coercions"
           , text "Probable fix: add a pattern signature" ]
      where
        bad_co_list = NE.toList bad_co_ne
    TcRnPatSynExistentialInResult name pat_ty bad_tvs -> mkSimpleDecorated $
      hang (sep [ text "The result type of the signature for" <+> quotes (ppr name) <> comma
                , text "namely" <+> quotes (ppr pat_ty) ])
        2 (text "mentions existential type variable" <> plural bad_tvs
           <+> pprQuotedList bad_tvs)
    TcRnPatSynArityMismatch name decl_arity missing -> mkSimpleDecorated $
      hang (text "Pattern synonym" <+> quotes (ppr name) <+> text "has"
            <+> speakNOf decl_arity (text "argument"))
         2 (text "but its type signature has" <+> int missing <+> text "fewer arrows")
    TcRnPatSynInvalidRhs ps_name lpat _ reason -> mkSimpleDecorated $
      vcat [ hang (text "Invalid right-hand side of bidirectional pattern synonym"
                   <+> quotes (ppr ps_name) <> colon)
                2 (pprPatSynInvalidRhsReason reason)
           , text "RHS pattern:" <+> ppr lpat ]
    TcRnMultiAssocTyFamDefaults name -> mkSimpleDecorated $
      text "More than one default declaration for"
      <+> ppr name
    TcRnTyFamDepsDisabled -> mkSimpleDecorated $
      text "Illegal injectivity annotation"
    TcRnAbstractClosedTyFamDecl -> mkSimpleDecorated $
      text "You may define an abstract closed type family" $$
      text "only in a .hs-boot file"
    TcRnPartialFieldSelector fld -> mkSimpleDecorated $
      sep [text "Use of partial record field selector" <> colon,
           nest 2 $ quotes (ppr (occName fld))]
    TcRnBadFieldAnnotation n con reason -> mkSimpleDecorated $
      hang (pprBadFieldAnnotationReason reason)
         2 (text "on the" <+> speakNth n
            <+> text "argument of" <+> quotes (ppr con))
    TcRnSuperclassCycle (MkSuperclassCycle cls definite details) ->
      let herald | definite  = text "Superclass cycle for"
                 | otherwise = text "Potential superclass cycle for"
      in mkSimpleDecorated $
       vcat [ herald <+> quotes (ppr cls), nest 2 (vcat (pprSuperclassCycleDetail <$> details))]
    TcRnDefaultSigMismatch sel_id dm_ty -> mkSimpleDecorated $
      hang (text "The default type signature for"
            <+> ppr sel_id <> colon)
         2 (ppr dm_ty)
      $$ (text "does not match its corresponding"
          <+> text "non-default type signature")
    TcRnTyFamsDisabled reason -> mkSimpleDecorated $
      text "Illegal family" <+> text sort <+> text "for" <+> quotes name
      where
        (sort, name) = case reason of
          TyFamsDisabledFamily n -> ("declaration", ppr n)
          TyFamsDisabledInstance n -> ("instance", ppr n)
    TcRnTyFamResultDisabled tc_name tvb -> mkSimpleDecorated $
      text "Illegal result type variable" <+> ppr tvb <+> text "for" <+> quotes (ppr tc_name)
    TcRnRoleValidationFailed role reason -> mkSimpleDecorated $
      vcat [text "Internal error in role inference:",
            pprRoleValidationFailedReason role reason,
            text "Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug"]
    TcRnCommonFieldResultTypeMismatch con1 con2 field_name -> mkSimpleDecorated $
      vcat [sep [text "Constructors" <+> ppr con1 <+> text "and" <+> ppr con2,
                 text "have a common field" <+> quotes (ppr field_name) <> comma],
            nest 2 $ text "but have different result types"]
    TcRnCommonFieldTypeMismatch con1 con2 field_name -> mkSimpleDecorated $
      sep [text "Constructors" <+> ppr con1 <+> text "and" <+> ppr con2,
           text "give different types for field", quotes (ppr field_name)]
    TcRnClassExtensionDisabled cls reason -> mkSimpleDecorated $
      pprDisabledClassExtension cls reason
    TcRnAssocNoClassTyVar cls fam_tc -> mkSimpleDecorated $
      sep [ text "The associated type" <+> quotes (ppr fam_tc <+> hsep (map ppr (tyConTyVars fam_tc)))
          , text "mentions none of the type or kind variables of the class" <+>
                  quotes (ppr cls <+> hsep (map ppr (classTyVars cls)))]
    TcRnDataConParentTypeMismatch data_con res_ty_tmpl -> mkSimpleDecorated $
      hang (text "Data constructor" <+> quotes (ppr data_con) <+>
            text "returns type" <+> quotes (ppr actual_res_ty))
         2 (text "instead of an instance of its parent type" <+> quotes (ppr res_ty_tmpl))
      where
        actual_res_ty = dataConOrigResTy data_con
    TcRnGADTsDisabled tc_name -> mkSimpleDecorated $
      text "Illegal generalised algebraic data declaration for" <+> quotes (ppr tc_name)
    TcRnExistentialQuantificationDisabled con -> mkSimpleDecorated $
      sdocOption sdocLinearTypes (\show_linear_types ->
        hang (text "Data constructor" <+> quotes (ppr con) <+>
              text "has existential type variables, a context, or a specialised result type")
           2 (ppr con <+> dcolon <+> ppr (dataConDisplayType show_linear_types con)))
    TcRnGADTDataContext tc_name -> mkSimpleDecorated $
      text "A data type declared in GADT style cannot have a context:" <+> quotes (ppr tc_name)
    TcRnMultipleConForNewtype tycon n -> mkSimpleDecorated $
      sep [text "A newtype must have exactly one constructor,",
           nest 2 $ text "but" <+> quotes (ppr tycon) <+> text "has" <+> speakN n]
    TcRnKindSignaturesDisabled thing -> mkSimpleDecorated $
      text "Illegal kind signature" <+> quotes (either ppr with_sig thing)
      where
        with_sig (tc_name, ksig) = ppr tc_name <+> dcolon <+> ppr ksig
    TcRnEmptyDataDeclsDisabled tycon -> mkSimpleDecorated $
      quotes (ppr tycon) <+> text "has no constructors"
    TcRnFamilyCategoryMismatch family -> mkSimpleDecorated $
      text "Wrong category of family instance; declaration was for a"
      <+> kindOfFamily
      where
        kindOfFamily | isTypeFamilyTyCon family = text "type family"
                     | isDataFamilyTyCon family = text "data family"
                     | otherwise = pprPanic "wrongKindOfFamily" (ppr family)
    TcRnFamilyArityMismatch _ max_args -> mkSimpleDecorated $
      text "Number of parameters must match family declaration; expected"
      <+> ppr max_args
    TcRnRoleMismatch var annot inferred -> mkSimpleDecorated $
      hang (text "Role mismatch on variable" <+> ppr var <> colon)
         2 (sep [ text "Annotation says", ppr annot
                , text "but role", ppr inferred
                , text "is required" ])
    TcRnRoleCountMismatch tyvars d@(L _ (RoleAnnotDecl _ _ annots)) -> mkSimpleDecorated $
      hang (text "Wrong number of roles listed in role annotation;" $$
            text "Expected" <+> (ppr tyvars) <> comma <+>
            text "got" <+> (ppr $ length annots) <> colon)
         2 (ppr d)
    TcRnIllegalRoleAnnotation (RoleAnnotDecl _ tycon _) -> mkSimpleDecorated $
      (text "Illegal role annotation for" <+> ppr tycon <> char ';' $$
       text "they are allowed only for datatypes and classes.")
    TcRnRoleAnnotationsDisabled  tc -> mkSimpleDecorated $
      text "Illegal role annotation for" <+> ppr tc
    TcRnIncoherentRoles _ -> mkSimpleDecorated $
      (text "Roles other than" <+> quotes (text "nominal") <+>
      text "for class parameters can lead to incoherence.")
    TcRnTyFamNameMismatch fam_tc_name eqn_tc_name -> mkSimpleDecorated $
      hang (text "Mismatched type name in type family instance.")
         2 (vcat [ text "Expected:" <+> ppr fam_tc_name
                 , text "  Actual:" <+> ppr eqn_tc_name ])

    TcRnBindVarAlreadyInScope tv_names_in_scope
      -> mkSimpleDecorated $
        vcat
          [ text "Type variable" <> plural tv_names_in_scope
            <+> hcat (punctuate (text ",") (map (quotes . ppr) tv_names_in_scope))
            <+> isOrAre tv_names_in_scope
            <+> text "already in scope."
          , text "Type applications in patterns must bind fresh variables, without shadowing."
          ]

    TcRnBindMultipleVariables ctx tv_name_w_loc
      -> mkSimpleDecorated $
        text "Variable" <+> text "`" <> ppr tv_name_w_loc <> text "'" <+> text "would be bound multiple times by" <+> pprHsDocContext ctx <> text "."

    TcRnUnexpectedKindVar tv_name
      -> mkSimpleDecorated $ text "Unexpected kind variable" <+> quotes (ppr tv_name)

    TcRnNegativeNumTypeLiteral tyLit
      -> mkSimpleDecorated $ text "Illegal literal in type (type literals must not be negative):" <+> ppr tyLit

    TcRnIllegalKind ty_thing _
      -> mkSimpleDecorated $ text "Illegal kind:" <+> (ppr ty_thing)

    TcRnPrecedenceParsingError op1 op2
      -> mkSimpleDecorated $
           hang (text "Precedence parsing error")
           4 (hsep [text "cannot mix", ppr_opfix op1, text "and",
           ppr_opfix op2,
           text "in the same infix expression"])

    TcRnSectionPrecedenceError op arg_op section
      -> mkSimpleDecorated $
           vcat [text "The operator" <+> ppr_opfix op <+> text "of a section",
             nest 4 (sep [text "must have lower precedence than that of the operand,",
                          nest 2 (text "namely" <+> ppr_opfix arg_op)]),
             nest 4 (text "in the section:" <+> quotes (ppr section))]

    TcRnUnexpectedPatSigType ty
      -> mkSimpleDecorated $
           hang (text "Illegal type signature:" <+> quotes (ppr ty))
              2 (text "Type signatures are only allowed in patterns with ScopedTypeVariables")

    TcRnIllegalKindSignature ty
      -> mkSimpleDecorated $ text "Illegal kind signature:" <+> quotes (ppr ty)

    TcRnUnusedQuantifiedTypeVar doc tyVar
      -> mkSimpleDecorated $
           vcat [ text "Unused quantified type variable" <+> quotes (ppr tyVar)
                , inHsDocContext doc ]

    TcRnDataKindsError typeOrKind thing
      -> mkSimpleDecorated $
           text "Illegal" <+> (text $ levelString typeOrKind) <> colon <+> quotes (ppr thing)

    TcRnTypeSynonymCycle decl_or_tcs
      -> mkSimpleDecorated $
           sep [ text "Cycle in type synonym declarations:"
               , nest 2 (vcat (map ppr_decl decl_or_tcs)) ]
      where
        ppr_decl = \case
          Right (L loc decl) -> ppr (locA loc) <> colon <+> ppr decl
          Left tc ->
            let n = tyConName tc
            in ppr (getSrcSpan n) <> colon <+> ppr (tyConName tc)
                   <+> text "from external module"

    TcRnCannotDefaultConcrete frr
      -> mkSimpleDecorated $
         ppr (frr_context frr) $$
         text "cannot be assigned a fixed runtime representation," <+>
         text "not even by defaulting."
    TcRnInterfaceError reason
      -> diagnosticMessage (tcOptsIfaceOpts opts) reason
    TcRnSelfImport imp_mod_name
      -> mkSimpleDecorated $
         text "A module cannot import itself:" <+> ppr imp_mod_name
    TcRnNoExplicitImportList mod
      -> mkSimpleDecorated $
         text "The module" <+> quotes (ppr mod) <+> text "does not have an explicit import list"
    TcRnSafeImportsDisabled _
      -> mkSimpleDecorated $
         text "safe import can't be used as Safe Haskell isn't on!"
    TcRnDeprecatedModule mod txt
      -> mkSimpleDecorated $
         sep [ text "Module" <+> quotes (ppr mod) <> text extra <> colon,
               nest 2 (vcat (map (ppr . hsDocString . unLoc) msg)) ]
         where
          (extra, msg) = case txt of
            WarningTxt _ _ msg -> ("", msg)
            DeprecatedTxt _ msg -> (" is deprecated", msg)
    TcRnCompatUnqualifiedImport decl
      -> mkSimpleDecorated $
         vcat
         [ text "To ensure compatibility with future core libraries changes"
         , text "imports to" <+> ppr (ideclName decl) <+> text "should be"
         , text "either qualified or have an explicit import list."
         ]
    TcRnRedundantSourceImport mod_name
      -> mkSimpleDecorated $
         text "Unnecessary {-# SOURCE #-} in the import of module" <+> quotes (ppr mod_name)
    TcRnImportLookup reason
      -> mkSimpleDecorated $
         pprImportLookup reason
    TcRnUnusedImport decl reason
      -> mkSimpleDecorated $
         pprUnusedImport decl reason
    TcRnDuplicateDecls name sorted_names
      -> mkSimpleDecorated $
         vcat [text "Multiple declarations of" <+>
               quotes (ppr name),
                -- NB. print the OccName, not the Name, because the
                -- latter might not be in scope in the RdrEnv and so will
                -- be printed qualified.
               text "Declared at:" <+>
               vcat (NE.toList $ ppr . nameSrcLoc <$> sorted_names)]
    TcRnPackageImportsDisabled
      -> mkSimpleDecorated $
         text "Package-qualified imports are not enabled"
    TcRnIllegalDataCon name
      -> mkSimpleDecorated $
         hsep [text "Illegal data constructor name", quotes (ppr name)]

  diagnosticReason = \case
    TcRnUnknownMessage m
      -> diagnosticReason m
    TcRnMessageWithInfo _ msg_with_info
      -> case msg_with_info of
           TcRnMessageDetailed _ m -> diagnosticReason m
    TcRnWithHsDocContext _ msg
      -> diagnosticReason msg
    TcRnSolverReport _ reason _
      -> reason -- Error, or a Warning if we are deferring type errors
    TcRnRedundantConstraints {}
      -> WarningWithFlag Opt_WarnRedundantConstraints
    TcRnInaccessibleCode {}
      -> WarningWithFlag Opt_WarnInaccessibleCode
    TcRnTypeDoesNotHaveFixedRuntimeRep{}
      -> ErrorWithoutFlag
    TcRnImplicitLift{}
      -> WarningWithFlag Opt_WarnImplicitLift
    TcRnUnusedPatternBinds{}
      -> WarningWithFlag Opt_WarnUnusedPatternBinds
    TcRnDodgyImports{}
      -> WarningWithFlag Opt_WarnDodgyImports
    TcRnDodgyExports{}
      -> WarningWithFlag Opt_WarnDodgyExports
    TcRnMissingImportList{}
      -> WarningWithFlag Opt_WarnMissingImportList
    TcRnUnsafeDueToPlugin{}
      -> WarningWithoutFlag
    TcRnModMissingRealSrcSpan{}
      -> ErrorWithoutFlag
    TcRnIdNotExportedFromModuleSig{}
      -> ErrorWithoutFlag
    TcRnIdNotExportedFromLocalSig{}
      -> ErrorWithoutFlag
    TcRnShadowedName{}
      -> WarningWithFlag Opt_WarnNameShadowing
    TcRnInvalidWarningCategory{}
      -> ErrorWithoutFlag
    TcRnDuplicateWarningDecls{}
      -> ErrorWithoutFlag
    TcRnSimplifierTooManyIterations{}
      -> ErrorWithoutFlag
    TcRnIllegalPatSynDecl{}
      -> ErrorWithoutFlag
    TcRnLinearPatSyn{}
      -> ErrorWithoutFlag
    TcRnEmptyRecordUpdate
      -> ErrorWithoutFlag
    TcRnIllegalFieldPunning{}
      -> ErrorWithoutFlag
    TcRnIllegalWildcardsInRecord{}
      -> ErrorWithoutFlag
    TcRnIllegalWildcardInType{}
      -> ErrorWithoutFlag
    TcRnDuplicateFieldName{}
      -> ErrorWithoutFlag
    TcRnIllegalViewPattern{}
      -> ErrorWithoutFlag
    TcRnCharLiteralOutOfRange{}
      -> ErrorWithoutFlag
    TcRnIllegalWildcardsInConstructor{}
      -> ErrorWithoutFlag
    TcRnIgnoringAnnotations{}
      -> WarningWithoutFlag
    TcRnAnnotationInSafeHaskell
      -> ErrorWithoutFlag
    TcRnInvalidTypeApplication{}
      -> ErrorWithoutFlag
    TcRnTagToEnumMissingValArg
      -> ErrorWithoutFlag
    TcRnTagToEnumUnspecifiedResTy{}
      -> ErrorWithoutFlag
    TcRnTagToEnumResTyNotAnEnum{}
      -> ErrorWithoutFlag
    TcRnTagToEnumResTyTypeData{}
      -> ErrorWithoutFlag
    TcRnArrowIfThenElsePredDependsOnResultTy
      -> ErrorWithoutFlag
    TcRnIllegalHsBootFileDecl
      -> ErrorWithoutFlag
    TcRnRecursivePatternSynonym{}
      -> ErrorWithoutFlag
    TcRnPartialTypeSigTyVarMismatch{}
      -> ErrorWithoutFlag
    TcRnPartialTypeSigBadQuantifier{}
      -> ErrorWithoutFlag
    TcRnMissingSignature what exported overridden
      -> WarningWithFlag $ missingSignatureWarningFlag what exported overridden
    TcRnPolymorphicBinderMissingSig{}
      -> WarningWithFlag Opt_WarnMissingLocalSignatures
    TcRnOverloadedSig{}
      -> ErrorWithoutFlag
    TcRnTupleConstraintInst{}
      -> ErrorWithoutFlag
    TcRnAbstractClassInst{}
      -> ErrorWithoutFlag
    TcRnNoClassInstHead{}
      -> ErrorWithoutFlag
    TcRnUserTypeError{}
      -> ErrorWithoutFlag
    TcRnConstraintInKind{}
      -> ErrorWithoutFlag
    TcRnUnboxedTupleOrSumTypeFuncArg{}
      -> ErrorWithoutFlag
    TcRnLinearFuncInKind{}
      -> ErrorWithoutFlag
    TcRnForAllEscapeError{}
      -> ErrorWithoutFlag
    TcRnVDQInTermType{}
      -> ErrorWithoutFlag
    TcRnBadQuantPredHead{}
      -> ErrorWithoutFlag
    TcRnIllegalTupleConstraint{}
      -> ErrorWithoutFlag
    TcRnNonTypeVarArgInConstraint{}
      -> ErrorWithoutFlag
    TcRnIllegalImplicitParam{}
      -> ErrorWithoutFlag
    TcRnIllegalConstraintSynonymOfKind{}
      -> ErrorWithoutFlag
    TcRnIllegalClassInst{}
      -> ErrorWithoutFlag
    TcRnOversaturatedVisibleKindArg{}
      -> ErrorWithoutFlag
    TcRnBadAssociatedType{}
      -> ErrorWithoutFlag
    TcRnForAllRankErr{}
      -> ErrorWithoutFlag
    TcRnMonomorphicBindings{}
      -> WarningWithFlag Opt_WarnMonomorphism
    TcRnOrphanInstance{}
      -> WarningWithFlag Opt_WarnOrphans
    TcRnFunDepConflict{}
      -> ErrorWithoutFlag
    TcRnDupInstanceDecls{}
      -> ErrorWithoutFlag
    TcRnConflictingFamInstDecls{}
      -> ErrorWithoutFlag
    TcRnFamInstNotInjective{}
      -> ErrorWithoutFlag
    TcRnBangOnUnliftedType{}
      -> WarningWithFlag Opt_WarnRedundantStrictnessFlags
    TcRnLazyBangOnUnliftedType{}
      -> WarningWithFlag Opt_WarnRedundantStrictnessFlags
    TcRnMultipleDefaultDeclarations{}
      -> ErrorWithoutFlag
    TcRnBadDefaultType{}
      -> ErrorWithoutFlag
    TcRnPatSynBundledWithNonDataCon{}
      -> ErrorWithoutFlag
    TcRnPatSynBundledWithWrongType{}
      -> ErrorWithoutFlag
    TcRnDupeModuleExport{}
      -> WarningWithFlag Opt_WarnDuplicateExports
    TcRnExportedModNotImported{}
      -> ErrorWithoutFlag
    TcRnNullExportedModule{}
      -> WarningWithFlag Opt_WarnDodgyExports
    TcRnMissingExportList{}
      -> WarningWithFlag Opt_WarnMissingExportList
    TcRnExportHiddenComponents{}
      -> ErrorWithoutFlag
    TcRnDuplicateExport{}
      -> WarningWithFlag Opt_WarnDuplicateExports
    TcRnExportedParentChildMismatch{}
      -> ErrorWithoutFlag
    TcRnConflictingExports{}
      -> ErrorWithoutFlag
    TcRnDuplicateFieldExport {}
      -> ErrorWithoutFlag
    TcRnAmbiguousFieldInUpdate {}
      -> ErrorWithoutFlag
    TcRnAmbiguousRecordUpdate{}
      -> WarningWithFlag Opt_WarnAmbiguousFields
    TcRnMissingFields{}
      -> WarningWithFlag Opt_WarnMissingFields
    TcRnFieldUpdateInvalidType{}
      -> ErrorWithoutFlag
    TcRnMissingStrictFields{}
      -> ErrorWithoutFlag
    TcRnBadRecordUpdate{}
      -> ErrorWithoutFlag
    TcRnStaticFormNotClosed{}
      -> ErrorWithoutFlag
    TcRnUselessTypeable
      -> WarningWithFlag Opt_WarnDerivingTypeable
    TcRnDerivingDefaults{}
      -> WarningWithFlag Opt_WarnDerivingDefaults
    TcRnNonUnaryTypeclassConstraint{}
      -> ErrorWithoutFlag
    TcRnPartialTypeSignatures{}
      -> WarningWithFlag Opt_WarnPartialTypeSignatures
    TcRnCannotDeriveInstance _ _ _ _ rea
      -> case rea of
           DerivErrNotWellKinded{}                 -> ErrorWithoutFlag
           DerivErrSafeHaskellGenericInst          -> ErrorWithoutFlag
           DerivErrDerivingViaWrongKind{}          -> ErrorWithoutFlag
           DerivErrNoEtaReduce{}                   -> ErrorWithoutFlag
           DerivErrBootFileFound                   -> ErrorWithoutFlag
           DerivErrDataConsNotAllInScope{}         -> ErrorWithoutFlag
           DerivErrGNDUsedOnData                   -> ErrorWithoutFlag
           DerivErrNullaryClasses                  -> ErrorWithoutFlag
           DerivErrLastArgMustBeApp                -> ErrorWithoutFlag
           DerivErrNoFamilyInstance{}              -> ErrorWithoutFlag
           DerivErrNotStockDeriveable{}            -> ErrorWithoutFlag
           DerivErrHasAssociatedDatatypes{}        -> ErrorWithoutFlag
           DerivErrNewtypeNonDeriveableClass       -> ErrorWithoutFlag
           DerivErrCannotEtaReduceEnough{}         -> ErrorWithoutFlag
           DerivErrOnlyAnyClassDeriveable{}        -> ErrorWithoutFlag
           DerivErrNotDeriveable{}                 -> ErrorWithoutFlag
           DerivErrNotAClass{}                     -> ErrorWithoutFlag
           DerivErrNoConstructors{}                -> ErrorWithoutFlag
           DerivErrLangExtRequired{}               -> ErrorWithoutFlag
           DerivErrDunnoHowToDeriveForType{}       -> ErrorWithoutFlag
           DerivErrMustBeEnumType{}                -> ErrorWithoutFlag
           DerivErrMustHaveExactlyOneConstructor{} -> ErrorWithoutFlag
           DerivErrMustHaveSomeParameters{}        -> ErrorWithoutFlag
           DerivErrMustNotHaveClassContext{}       -> ErrorWithoutFlag
           DerivErrBadConstructor{}                -> ErrorWithoutFlag
           DerivErrGenerics{}                      -> ErrorWithoutFlag
           DerivErrEnumOrProduct{}                 -> ErrorWithoutFlag
    TcRnLookupInstance _ _ _
      -> ErrorWithoutFlag
    TcRnLazyGADTPattern
      -> ErrorWithoutFlag
    TcRnArrowProcGADTPattern
      -> ErrorWithoutFlag
    TcRnSpecialClassInst {}
      -> ErrorWithoutFlag
    TcRnForallIdentifier {}
      -> WarningWithFlag Opt_WarnForallIdentifier
    TcRnTypeEqualityOutOfScope
      -> WarningWithFlag Opt_WarnTypeEqualityOutOfScope
    TcRnTypeEqualityRequiresOperators
      -> WarningWithFlag Opt_WarnTypeEqualityRequiresOperators
    TcRnIllegalTypeOperator {}
      -> ErrorWithoutFlag
    TcRnIllegalTypeOperatorDecl {}
      -> ErrorWithoutFlag
    TcRnGADTMonoLocalBinds {}
      -> WarningWithFlag Opt_WarnGADTMonoLocalBinds
    TcRnIncorrectNameSpace {}
      -> ErrorWithoutFlag
    TcRnNotInScope {}
      -> ErrorWithoutFlag
    TcRnTermNameInType {}
      -> ErrorWithoutFlag
    TcRnUntickedPromotedThing {}
      -> WarningWithFlag Opt_WarnUntickedPromotedConstructors
    TcRnIllegalBuiltinSyntax {}
      -> ErrorWithoutFlag
    TcRnWarnDefaulting {}
      -> WarningWithFlag Opt_WarnTypeDefaults
    TcRnForeignImportPrimExtNotSet{}
      -> ErrorWithoutFlag
    TcRnForeignImportPrimSafeAnn{}
      -> ErrorWithoutFlag
    TcRnForeignFunctionImportAsValue{}
      -> ErrorWithoutFlag
    TcRnFunPtrImportWithoutAmpersand{}
      -> WarningWithFlag Opt_WarnDodgyForeignImports
    TcRnIllegalForeignDeclBackend{}
      -> ErrorWithoutFlag
    TcRnUnsupportedCallConv _ unsupportedCC
      -> case unsupportedCC of
           StdCallConvUnsupported -> WarningWithFlag Opt_WarnUnsupportedCallingConventions
           _ -> ErrorWithoutFlag
    TcRnIllegalForeignType{}
      -> ErrorWithoutFlag
    TcRnInvalidCIdentifier{}
      -> ErrorWithoutFlag
    TcRnExpectedValueId{}
      -> ErrorWithoutFlag
    TcRnRecSelectorEscapedTyVar{}
      -> ErrorWithoutFlag
    TcRnPatSynNotBidirectional{}
      -> ErrorWithoutFlag
    TcRnSplicePolymorphicLocalVar{}
      -> ErrorWithoutFlag
    TcRnIllegalDerivingItem{}
      -> ErrorWithoutFlag
    TcRnUnexpectedAnnotation{}
      -> ErrorWithoutFlag
    TcRnIllegalRecordSyntax{}
      -> ErrorWithoutFlag
    TcRnUnexpectedTypeSplice{}
      -> ErrorWithoutFlag
    TcRnInvalidVisibleKindArgument{}
      -> ErrorWithoutFlag
    TcRnTooManyBinders{}
      -> ErrorWithoutFlag
    TcRnDifferentNamesForTyVar{}
      -> ErrorWithoutFlag
    TcRnInvalidReturnKind{}
      -> ErrorWithoutFlag
    TcRnClassKindNotConstraint{}
      -> ErrorWithoutFlag
    TcRnUnpromotableThing{}
      -> ErrorWithoutFlag
    TcRnMatchesHaveDiffNumArgs{}
      -> ErrorWithoutFlag
    TcRnCannotBindScopedTyVarInPatSig{}
      -> ErrorWithoutFlag
    TcRnCannotBindTyVarsInPatBind{}
      -> ErrorWithoutFlag
    TcRnTooManyTyArgsInConPattern{}
      -> ErrorWithoutFlag
    TcRnMultipleInlinePragmas{}
      -> WarningWithoutFlag
    TcRnUnexpectedPragmas{}
      -> WarningWithoutFlag
    TcRnNonOverloadedSpecialisePragma{}
      -> WarningWithoutFlag
    TcRnSpecialiseNotVisible{}
      -> WarningWithoutFlag
    TcRnPragmaWarning{pragma_warning_msg}
      -> WarningWithCategory (warningTxtCategory pragma_warning_msg)
    TcRnIllegalHsigDefaultMethods{}
      -> ErrorWithoutFlag
    TcRnHsigFixityMismatch{}
      -> ErrorWithoutFlag
    TcRnHsigShapeMismatch{}
      -> ErrorWithoutFlag
    TcRnHsigMissingModuleExport{}
      -> ErrorWithoutFlag
    TcRnBadGenericMethod{}
      -> ErrorWithoutFlag
    TcRnWarningMinimalDefIncomplete{}
      -> WarningWithoutFlag
    TcRnDefaultMethodForPragmaLacksBinding{}
      -> ErrorWithoutFlag
    TcRnIgnoreSpecialisePragmaOnDefMethod{}
      -> WarningWithoutFlag
    TcRnBadMethodErr{}
      -> ErrorWithoutFlag
    TcRnNoExplicitAssocTypeOrDefaultDeclaration{}
      -> WarningWithFlag (Opt_WarnMissingMethods)
    TcRnIllegalTypeData
      -> ErrorWithoutFlag
    TcRnTypeDataForbids{}
      -> ErrorWithoutFlag
    TcRnIllegalNewtype{}
      -> ErrorWithoutFlag
    TcRnTypedTHWithPolyType{}
      -> ErrorWithoutFlag
    TcRnSpliceThrewException{}
      -> ErrorWithoutFlag
    TcRnInvalidTopDecl{}
      -> ErrorWithoutFlag
    TcRnNonExactName{}
      -> ErrorWithoutFlag
    TcRnAddInvalidCorePlugin{}
      -> ErrorWithoutFlag
    TcRnAddDocToNonLocalDefn{}
      -> ErrorWithoutFlag
    TcRnFailedToLookupThInstName{}
      -> ErrorWithoutFlag
    TcRnCannotReifyInstance{}
      -> ErrorWithoutFlag
    TcRnCannotReifyOutOfScopeThing{}
      -> ErrorWithoutFlag
    TcRnCannotReifyThingNotInTypeEnv{}
      -> ErrorWithoutFlag
    TcRnNoRolesAssociatedWithThing{}
      -> ErrorWithoutFlag
    TcRnCannotRepresentType{}
      -> ErrorWithoutFlag
    TcRnRunSpliceFailure{}
      -> ErrorWithoutFlag
    TcRnReportCustomQuasiError isError _
      -> if isError then ErrorWithoutFlag else WarningWithoutFlag
    TcRnUnsatisfiedMinimalDef{}
      -> WarningWithFlag (Opt_WarnMissingMethods)
    TcRnMisplacedInstSig{}
      -> ErrorWithoutFlag
    TcRnBadBootFamInstDecl{}
      -> ErrorWithoutFlag
    TcRnIllegalFamilyInstance{}
      -> ErrorWithoutFlag
    TcRnMissingClassAssoc{}
      -> ErrorWithoutFlag
    TcRnNotOpenFamily{}
      -> ErrorWithoutFlag
    TcRnNoRebindableSyntaxRecordDot{}
      -> ErrorWithoutFlag
    TcRnNoFieldPunsRecordDot{}
      -> ErrorWithoutFlag
    TcRnIllegalStaticExpression{}
      -> ErrorWithoutFlag
    TcRnIllegalStaticFormInSplice{}
      -> ErrorWithoutFlag
    TcRnListComprehensionDuplicateBinding{}
      -> ErrorWithoutFlag
    TcRnEmptyStmtsGroup{}
      -> ErrorWithoutFlag
    TcRnLastStmtNotExpr{}
      -> ErrorWithoutFlag
    TcRnUnexpectedStatementInContext{}
      -> ErrorWithoutFlag
    TcRnSectionWithoutParentheses{}
      -> ErrorWithoutFlag
    TcRnIllegalImplicitParameterBindings{}
      -> ErrorWithoutFlag
    TcRnIllegalTupleSection{}
      -> ErrorWithoutFlag
    TcRnCapturedTermName{}
      -> WarningWithFlag Opt_WarnTermVariableCapture
    TcRnBindingOfExistingName{}
      -> ErrorWithoutFlag
    TcRnMultipleFixityDecls{}
      -> ErrorWithoutFlag
    TcRnIllegalPatternSynonymDecl{}
      -> ErrorWithoutFlag
    TcRnIllegalClassBinding{}
      -> ErrorWithoutFlag
    TcRnOrphanCompletePragma{}
      -> ErrorWithoutFlag
    TcRnEmptyCase{}
      -> ErrorWithoutFlag
    TcRnNonStdGuards{}
      -> WarningWithoutFlag
    TcRnDuplicateSigDecl{}
      -> ErrorWithoutFlag
    TcRnMisplacedSigDecl{}
      -> ErrorWithoutFlag
    TcRnUnexpectedDefaultSig{}
      -> ErrorWithoutFlag
    TcRnBindInBootFile{}
      -> ErrorWithoutFlag
    TcRnDuplicateMinimalSig{}
      -> ErrorWithoutFlag
    TcRnLoopySuperclassSolve{}
      -> WarningWithFlag Opt_WarnLoopySuperclassSolve
    TcRnIllegalInstanceHeadDecl{}
      -> ErrorWithoutFlag
    TcRnUnexpectedStandaloneDerivingDecl{}
      -> ErrorWithoutFlag
    TcRnUnusedVariableInRuleDecl{}
      -> ErrorWithoutFlag
    TcRnUnexpectedStandaloneKindSig{}
      -> ErrorWithoutFlag
    TcRnIllegalRuleLhs{}
      -> ErrorWithoutFlag
    TcRnBadAssocRhs{}
      -> ErrorWithoutFlag
    TcRnDuplicateRoleAnnot{}
      -> ErrorWithoutFlag
    TcRnDuplicateKindSig{}
      -> ErrorWithoutFlag
    TcRnIllegalDerivStrategy{}
      -> ErrorWithoutFlag
    TcRnIllegalMultipleDerivClauses{}
      -> ErrorWithoutFlag
    TcRnNoDerivStratSpecified{}
      -> WarningWithFlag Opt_WarnMissingDerivingStrategies
    TcRnStupidThetaInGadt{}
      -> ErrorWithoutFlag
    TcRnBadImplicitSplice{}
      -> ErrorWithoutFlag
    TcRnShadowedTyVarNameInFamResult{}
      -> ErrorWithoutFlag
    TcRnIncorrectTyVarOnLhsOfInjCond{}
      -> ErrorWithoutFlag
    TcRnUnknownTyVarsOnRhsOfInjCond{}
      -> ErrorWithoutFlag
    TcRnBadlyStaged{}
      -> ErrorWithoutFlag
    TcRnStageRestriction{}
      -> ErrorWithoutFlag
    TcRnTyThingUsedWrong{}
      -> ErrorWithoutFlag
    TcRnCannotDefaultKindVar{}
      -> ErrorWithoutFlag
    TcRnUninferrableTyVar{}
      -> ErrorWithoutFlag
    TcRnSkolemEscape{}
      -> ErrorWithoutFlag
    TcRnPatSynEscapedCoercion{}
      -> ErrorWithoutFlag
    TcRnPatSynExistentialInResult{}
      -> ErrorWithoutFlag
    TcRnPatSynArityMismatch{}
      -> ErrorWithoutFlag
    TcRnPatSynInvalidRhs{}
      -> ErrorWithoutFlag
    TcRnMultiAssocTyFamDefaults{}
      -> ErrorWithoutFlag
    TcRnTyFamDepsDisabled{}
      -> ErrorWithoutFlag
    TcRnAbstractClosedTyFamDecl{}
      -> ErrorWithoutFlag
    TcRnPartialFieldSelector{}
      -> WarningWithFlag Opt_WarnPartialFields
    TcRnBadFieldAnnotation _ _ LazyFieldsDisabled
      -> ErrorWithoutFlag
    TcRnBadFieldAnnotation{}
      -> WarningWithoutFlag
    TcRnSuperclassCycle{}
      -> ErrorWithoutFlag
    TcRnDefaultSigMismatch{}
      -> ErrorWithoutFlag
    TcRnTyFamsDisabled{}
      -> ErrorWithoutFlag
    TcRnTyFamResultDisabled{}
      -> ErrorWithoutFlag
    TcRnRoleValidationFailed{}
      -> ErrorWithoutFlag
    TcRnCommonFieldResultTypeMismatch{}
      -> ErrorWithoutFlag
    TcRnCommonFieldTypeMismatch{}
      -> ErrorWithoutFlag
    TcRnClassExtensionDisabled{}
      -> ErrorWithoutFlag
    TcRnAssocNoClassTyVar{}
      -> ErrorWithoutFlag
    TcRnDataConParentTypeMismatch{}
      -> ErrorWithoutFlag
    TcRnGADTsDisabled{}
      -> ErrorWithoutFlag
    TcRnExistentialQuantificationDisabled{}
      -> ErrorWithoutFlag
    TcRnGADTDataContext{}
      -> ErrorWithoutFlag
    TcRnMultipleConForNewtype{}
      -> ErrorWithoutFlag
    TcRnKindSignaturesDisabled{}
      -> ErrorWithoutFlag
    TcRnEmptyDataDeclsDisabled{}
      -> ErrorWithoutFlag
    TcRnFamilyCategoryMismatch{}
      -> ErrorWithoutFlag
    TcRnFamilyArityMismatch{}
      -> ErrorWithoutFlag
    TcRnRoleMismatch{}
      -> ErrorWithoutFlag
    TcRnRoleCountMismatch{}
      -> ErrorWithoutFlag
    TcRnIllegalRoleAnnotation{}
      -> ErrorWithoutFlag
    TcRnRoleAnnotationsDisabled{}
      -> ErrorWithoutFlag
    TcRnIncoherentRoles{}
      -> ErrorWithoutFlag
    TcRnTyFamNameMismatch{}
      -> ErrorWithoutFlag
    TcRnBindVarAlreadyInScope{}
      -> ErrorWithoutFlag
    TcRnBindMultipleVariables{}
      -> ErrorWithoutFlag
    TcRnUnexpectedKindVar{}
      -> ErrorWithoutFlag
    TcRnNegativeNumTypeLiteral{}
      -> ErrorWithoutFlag
    TcRnIllegalKind{}
      -> ErrorWithoutFlag
    TcRnPrecedenceParsingError{}
      -> ErrorWithoutFlag
    TcRnSectionPrecedenceError{}
      -> ErrorWithoutFlag
    TcRnUnexpectedPatSigType{}
      -> ErrorWithoutFlag
    TcRnIllegalKindSignature{}
      -> ErrorWithoutFlag
    TcRnUnusedQuantifiedTypeVar{}
      -> WarningWithFlag Opt_WarnUnusedForalls
    TcRnDataKindsError{}
      -> ErrorWithoutFlag
    TcRnTypeSynonymCycle{}
      -> ErrorWithoutFlag
    TcRnCannotDefaultConcrete{}
      -> ErrorWithoutFlag
    TcRnInterfaceError err
      -> interfaceErrorReason err
    TcRnSelfImport{}
      -> ErrorWithoutFlag
    TcRnNoExplicitImportList{}
      -> WarningWithFlag Opt_WarnMissingImportList
    TcRnSafeImportsDisabled{}
      -> ErrorWithoutFlag
    TcRnDeprecatedModule _ txt
      -> WarningWithCategory (warningTxtCategory txt)
    TcRnCompatUnqualifiedImport{}
      -> WarningWithFlag Opt_WarnCompatUnqualifiedImports
    TcRnRedundantSourceImport{}
      -> WarningWithoutFlag
    TcRnImportLookup{}
      -> ErrorWithoutFlag
    TcRnUnusedImport{}
      -> WarningWithFlag Opt_WarnUnusedImports
    TcRnDuplicateDecls{}
      -> ErrorWithoutFlag
    TcRnPackageImportsDisabled
      -> ErrorWithoutFlag
    TcRnIllegalDataCon{}
      -> ErrorWithoutFlag


  diagnosticHints = \case
    TcRnUnknownMessage m
      -> diagnosticHints m
    TcRnMessageWithInfo _ msg_with_info
      -> case msg_with_info of
           TcRnMessageDetailed _ m -> diagnosticHints m
    TcRnWithHsDocContext _ msg
      -> diagnosticHints msg
    TcRnSolverReport _ _ hints
      -> hints
    TcRnRedundantConstraints{}
      -> noHints
    TcRnInaccessibleCode{}
      -> noHints
    TcRnTypeDoesNotHaveFixedRuntimeRep{}
      -> noHints
    TcRnImplicitLift{}
      -> noHints
    TcRnUnusedPatternBinds{}
      -> noHints
    TcRnDodgyImports{}
      -> noHints
    TcRnDodgyExports{}
      -> noHints
    TcRnMissingImportList{}
      -> noHints
    TcRnUnsafeDueToPlugin{}
      -> noHints
    TcRnModMissingRealSrcSpan{}
      -> noHints
    TcRnIdNotExportedFromModuleSig name mod
      -> [SuggestAddToHSigExportList name $ Just mod]
    TcRnIdNotExportedFromLocalSig name
      -> [SuggestAddToHSigExportList name Nothing]
    TcRnShadowedName{}
      -> noHints
    TcRnInvalidWarningCategory{}
      -> noHints
    TcRnDuplicateWarningDecls{}
      -> noHints
    TcRnSimplifierTooManyIterations{}
      -> [SuggestIncreaseSimplifierIterations]
    TcRnIllegalPatSynDecl{}
      -> noHints
    TcRnLinearPatSyn{}
      -> noHints
    TcRnEmptyRecordUpdate{}
      -> noHints
    TcRnIllegalFieldPunning{}
      -> [suggestExtension LangExt.NamedFieldPuns]
    TcRnIllegalWildcardsInRecord{}
      -> [suggestExtension LangExt.RecordWildCards]
    TcRnIllegalWildcardInType{}
      -> noHints
    TcRnDuplicateFieldName{}
      -> noHints
    TcRnIllegalViewPattern{}
      -> [suggestExtension LangExt.ViewPatterns]
    TcRnCharLiteralOutOfRange{}
      -> noHints
    TcRnIllegalWildcardsInConstructor{}
      -> noHints
    TcRnIgnoringAnnotations{}
      -> noHints
    TcRnAnnotationInSafeHaskell
      -> noHints
    TcRnInvalidTypeApplication{}
      -> noHints
    TcRnTagToEnumMissingValArg
      -> noHints
    TcRnTagToEnumUnspecifiedResTy{}
      -> noHints
    TcRnTagToEnumResTyNotAnEnum{}
      -> noHints
    TcRnTagToEnumResTyTypeData{}
      -> noHints
    TcRnArrowIfThenElsePredDependsOnResultTy
      -> noHints
    TcRnIllegalHsBootFileDecl
      -> noHints
    TcRnRecursivePatternSynonym{}
      -> noHints
    TcRnPartialTypeSigTyVarMismatch{}
      -> noHints
    TcRnPartialTypeSigBadQuantifier{}
      -> noHints
    TcRnMissingSignature {}
      -> noHints
    TcRnPolymorphicBinderMissingSig{}
      -> noHints
    TcRnOverloadedSig{}
      -> noHints
    TcRnTupleConstraintInst{}
      -> noHints
    TcRnAbstractClassInst{}
      -> noHints
    TcRnNoClassInstHead{}
      -> noHints
    TcRnUserTypeError{}
      -> noHints
    TcRnConstraintInKind{}
      -> noHints
    TcRnUnboxedTupleOrSumTypeFuncArg tuple_or_sum _
      -> [suggestExtension $ unboxedTupleOrSumExtension tuple_or_sum]
    TcRnLinearFuncInKind{}
      -> noHints
    TcRnForAllEscapeError{}
      -> noHints
    TcRnVDQInTermType{}
      -> noHints
    TcRnBadQuantPredHead{}
      -> noHints
    TcRnIllegalTupleConstraint{}
      -> [suggestExtension LangExt.ConstraintKinds]
    TcRnNonTypeVarArgInConstraint{}
      -> [suggestExtension LangExt.FlexibleContexts]
    TcRnIllegalImplicitParam{}
      -> noHints
    TcRnIllegalConstraintSynonymOfKind{}
      -> [suggestExtension LangExt.ConstraintKinds]
    TcRnIllegalClassInst{}
      -> noHints
    TcRnOversaturatedVisibleKindArg{}
      -> noHints
    TcRnBadAssociatedType{}
      -> noHints
    TcRnForAllRankErr rank _
      -> case rank of
           LimitedRank{}      -> [suggestExtension LangExt.RankNTypes]
           MonoTypeRankZero   -> [suggestExtension LangExt.RankNTypes]
           MonoTypeTyConArg   -> [suggestExtension LangExt.ImpredicativeTypes]
           MonoTypeSynArg     -> [suggestExtension LangExt.LiberalTypeSynonyms]
           MonoTypeConstraint -> [suggestExtension LangExt.QuantifiedConstraints]
           _                  -> noHints
    TcRnMonomorphicBindings bindings
      -> case bindings of
          []     -> noHints
          (x:xs) -> [SuggestAddTypeSignatures $ NamedBindings (x NE.:| xs)]
    TcRnOrphanInstance clsOrFamInst
      -> [SuggestFixOrphanInst { isFamilyInstance = isFam }]
        where
          isFam = case clsOrFamInst :: Either ClsInst FamInst of
            Left  _clsInst -> Nothing
            Right famInst  -> Just $ fi_flavor famInst
    TcRnFunDepConflict{}
      -> noHints
    TcRnDupInstanceDecls{}
      -> noHints
    TcRnConflictingFamInstDecls{}
      -> noHints
    TcRnFamInstNotInjective rea _ _
      -> case rea of
           InjErrRhsBareTyVar{}      -> noHints
           InjErrRhsCannotBeATypeFam -> noHints
           InjErrRhsOverlap          -> noHints
           InjErrCannotInferFromRhs _ _ suggestUndInst
             | YesSuggestUndecidableInstaces <- suggestUndInst
             -> [suggestExtension LangExt.UndecidableInstances]
             | otherwise
             -> noHints
    TcRnBangOnUnliftedType{}
      -> noHints
    TcRnLazyBangOnUnliftedType{}
      -> noHints
    TcRnMultipleDefaultDeclarations{}
      -> noHints
    TcRnBadDefaultType{}
      -> noHints
    TcRnPatSynBundledWithNonDataCon{}
      -> noHints
    TcRnPatSynBundledWithWrongType{}
      -> noHints
    TcRnDupeModuleExport{}
      -> noHints
    TcRnExportedModNotImported{}
      -> noHints
    TcRnNullExportedModule{}
      -> noHints
    TcRnMissingExportList{}
      -> noHints
    TcRnExportHiddenComponents{}
      -> noHints
    TcRnDuplicateExport{}
      -> noHints
    TcRnExportedParentChildMismatch{}
      -> noHints
    TcRnConflictingExports{}
      -> noHints
    TcRnDuplicateFieldExport {}
      -> [suggestExtension LangExt.DuplicateRecordFields]
    TcRnAmbiguousFieldInUpdate {}
      -> [suggestExtension LangExt.DisambiguateRecordFields]
    TcRnAmbiguousRecordUpdate{}
      -> noHints
    TcRnMissingFields{}
      -> noHints
    TcRnFieldUpdateInvalidType{}
      -> noHints
    TcRnMissingStrictFields{}
      -> noHints
    TcRnBadRecordUpdate{}
      -> noHints
    TcRnStaticFormNotClosed{}
      -> noHints
    TcRnUselessTypeable
      -> noHints
    TcRnDerivingDefaults{}
      -> [useDerivingStrategies]
    TcRnNonUnaryTypeclassConstraint{}
      -> noHints
    TcRnPartialTypeSignatures suggestParSig _
      -> case suggestParSig of
           YesSuggestPartialTypeSignatures
             -> let info = text "to use the inferred type"
                in [suggestExtensionWithInfo info LangExt.PartialTypeSignatures]
           NoSuggestPartialTypeSignatures
             -> noHints
    TcRnCannotDeriveInstance cls _ _ newtype_deriving rea
      -> deriveInstanceErrReasonHints cls newtype_deriving rea
    TcRnLookupInstance _ _ _
      -> noHints
    TcRnLazyGADTPattern
      -> noHints
    TcRnArrowProcGADTPattern
      -> noHints
    TcRnSpecialClassInst {}
      -> noHints
    TcRnForallIdentifier {}
      -> [SuggestRenameForall]
    TcRnTypeEqualityOutOfScope
      -> noHints
    TcRnTypeEqualityRequiresOperators
      -> [suggestExtension LangExt.TypeOperators]
    TcRnIllegalTypeOperator {}
      -> [suggestExtension LangExt.TypeOperators]
    TcRnIllegalTypeOperatorDecl {}
      -> [suggestExtension LangExt.TypeOperators]
    TcRnGADTMonoLocalBinds {}
      -> [suggestAnyExtension [LangExt.GADTs, LangExt.TypeFamilies]]
    TcRnIncorrectNameSpace nm is_th_use
      | is_th_use
      -> [SuggestAppropriateTHTick $ nameNameSpace nm]
      | otherwise
      -> noHints
    TcRnNotInScope err _ _ hints
      -> scopeErrorHints err ++ hints
    TcRnTermNameInType _ hints
      -> hints
    TcRnUntickedPromotedThing thing
      -> [SuggestAddTick thing]
    TcRnIllegalBuiltinSyntax {}
      -> noHints
    TcRnWarnDefaulting {}
      -> noHints
    TcRnForeignImportPrimExtNotSet{}
      -> [suggestExtension LangExt.GHCForeignImportPrim]
    TcRnForeignImportPrimSafeAnn{}
      -> noHints
    TcRnForeignFunctionImportAsValue{}
      -> noHints
    TcRnFunPtrImportWithoutAmpersand{}
      -> noHints
    TcRnIllegalForeignDeclBackend{}
      -> noHints
    TcRnUnsupportedCallConv{}
      -> noHints
    TcRnIllegalForeignType _ reason
      -> case reason of
           TypeCannotBeMarshaled _ why
             | NewtypeDataConNotInScope tc _ <- why
             -> let tc_nm = tyConName tc
                    dc = dataConName $ head $ tyConDataCons tc
                in [ ImportSuggestion (occName dc)
                   $ ImportDataCon Nothing (nameOccName tc_nm) ]
             | UnliftedFFITypesNeeded <- why
             -> [suggestExtension LangExt.UnliftedFFITypes]
           _ -> noHints
    TcRnInvalidCIdentifier{}
      -> noHints
    TcRnExpectedValueId{}
      -> noHints
    TcRnRecSelectorEscapedTyVar{}
      -> [SuggestPatternMatchingSyntax]
    TcRnPatSynNotBidirectional{}
      -> noHints
    TcRnSplicePolymorphicLocalVar{}
      -> noHints
    TcRnIllegalDerivingItem{}
      -> noHints
    TcRnUnexpectedAnnotation{}
      -> noHints
    TcRnIllegalRecordSyntax{}
      -> noHints
    TcRnUnexpectedTypeSplice{}
      -> noHints
    TcRnInvalidVisibleKindArgument{}
      -> noHints
    TcRnTooManyBinders{}
      -> noHints
    TcRnDifferentNamesForTyVar{}
      -> noHints
    TcRnInvalidReturnKind _ _ _ mb_suggest_unlifted_ext
      -> case mb_suggest_unlifted_ext of
           Nothing -> noHints
           Just SuggestUnliftedNewtypes -> [suggestExtension LangExt.UnliftedNewtypes]
           Just SuggestUnliftedDatatypes -> [suggestExtension LangExt.UnliftedDatatypes]
    TcRnClassKindNotConstraint{}
      -> noHints
    TcRnUnpromotableThing{}
      -> noHints
    TcRnMatchesHaveDiffNumArgs{}
      -> noHints
    TcRnCannotBindScopedTyVarInPatSig{}
      -> noHints
    TcRnCannotBindTyVarsInPatBind{}
      -> noHints
    TcRnTooManyTyArgsInConPattern{}
      -> noHints
    TcRnMultipleInlinePragmas{}
      -> noHints
    TcRnUnexpectedPragmas{}
      -> noHints
    TcRnNonOverloadedSpecialisePragma{}
      -> noHints
    TcRnSpecialiseNotVisible name
      -> [SuggestSpecialiseVisibilityHints name]
    TcRnPragmaWarning{}
      -> noHints
    TcRnIllegalHsigDefaultMethods{}
      -> noHints
    TcRnHsigFixityMismatch{}
      -> noHints
    TcRnHsigShapeMismatch{}
      -> noHints
    TcRnHsigMissingModuleExport{}
      -> noHints
    TcRnBadGenericMethod{}
      -> noHints
    TcRnWarningMinimalDefIncomplete{}
      -> noHints
    TcRnDefaultMethodForPragmaLacksBinding{}
      -> noHints
    TcRnIgnoreSpecialisePragmaOnDefMethod{}
      -> noHints
    TcRnBadMethodErr{}
      -> noHints
    TcRnNoExplicitAssocTypeOrDefaultDeclaration{}
      -> noHints
    TcRnIllegalTypeData
      -> [suggestExtension LangExt.TypeData]
    TcRnTypeDataForbids{}
      -> noHints
    TcRnIllegalNewtype{}
      -> noHints
    TcRnTypedTHWithPolyType{}
      -> noHints
    TcRnSpliceThrewException{}
      -> noHints
    TcRnInvalidTopDecl{}
      -> noHints
    TcRnNonExactName{}
      -> noHints
    TcRnAddInvalidCorePlugin{}
      -> noHints
    TcRnAddDocToNonLocalDefn{}
      -> noHints
    TcRnFailedToLookupThInstName{}
      -> noHints
    TcRnCannotReifyInstance{}
      -> noHints
    TcRnCannotReifyOutOfScopeThing{}
      -> noHints
    TcRnCannotReifyThingNotInTypeEnv{}
      -> noHints
    TcRnNoRolesAssociatedWithThing{}
      -> noHints
    TcRnCannotRepresentType{}
      -> noHints
    TcRnRunSpliceFailure{}
      -> noHints
    TcRnReportCustomQuasiError{}
      -> noHints
    TcRnUnsatisfiedMinimalDef{}
      -> noHints
    TcRnMisplacedInstSig{}
      -> [suggestExtension LangExt.InstanceSigs]
    TcRnBadBootFamInstDecl{}
      -> noHints
    TcRnIllegalFamilyInstance{}
      -> noHints
    TcRnMissingClassAssoc{}
      -> noHints
    TcRnNotOpenFamily{}
      -> noHints
    TcRnNoRebindableSyntaxRecordDot{}
      -> noHints
    TcRnNoFieldPunsRecordDot{}
      -> noHints
    TcRnIllegalStaticExpression{}
      -> [suggestExtension LangExt.StaticPointers]
    TcRnIllegalStaticFormInSplice{}
      -> noHints
    TcRnListComprehensionDuplicateBinding{}
      -> noHints
    TcRnEmptyStmtsGroup EmptyStmtsGroupInDoNotation{}
      -> [suggestExtension LangExt.NondecreasingIndentation]
    TcRnEmptyStmtsGroup{}
      -> noHints
    TcRnLastStmtNotExpr{}
      -> noHints
    TcRnUnexpectedStatementInContext _ _ mExt
      | Nothing <- mExt -> noHints
      | Just ext <- mExt -> [suggestExtension ext]
    TcRnSectionWithoutParentheses{}
      -> noHints
    TcRnIllegalImplicitParameterBindings{}
      -> noHints
    TcRnIllegalTupleSection{}
      -> [suggestExtension LangExt.TupleSections]
    TcRnCapturedTermName{}
      -> [SuggestRenameTypeVariable]
    TcRnBindingOfExistingName{}
      -> noHints
    TcRnMultipleFixityDecls{}
      -> noHints
    TcRnIllegalPatternSynonymDecl{}
      -> [suggestExtension LangExt.PatternSynonyms]
    TcRnIllegalClassBinding{}
      -> noHints
    TcRnOrphanCompletePragma{}
      -> noHints
    TcRnEmptyCase ctxt -> case ctxt of
      LamCaseAlt LamCases -> noHints -- cases syntax doesn't support empty case.
      ArrowMatchCtxt (ArrowLamCaseAlt LamCases) -> noHints
      _ -> [suggestExtension LangExt.EmptyCase]
    TcRnNonStdGuards{}
      -> [suggestExtension LangExt.PatternGuards]
    TcRnDuplicateSigDecl{}
      -> noHints
    TcRnMisplacedSigDecl{}
      -> noHints
    TcRnUnexpectedDefaultSig{}
      -> [suggestExtension LangExt.DefaultSignatures]
    TcRnBindInBootFile{}
      -> noHints
    TcRnDuplicateMinimalSig{}
      -> noHints
    TcRnLoopySuperclassSolve wtd_loc wtd_pty
      -> [LoopySuperclassSolveHint wtd_pty cls_or_qc]
      where
        cls_or_qc :: ClsInstOrQC
        cls_or_qc = case ctLocOrigin wtd_loc of
          ScOrigin c_or_q _ -> c_or_q
          _                 -> IsClsInst -- shouldn't happen
    TcRnIllegalInstanceHeadDecl{}
      -> noHints
    TcRnUnexpectedStandaloneDerivingDecl{}
      -> [suggestExtension LangExt.StandaloneDeriving]
    TcRnUnusedVariableInRuleDecl{}
      -> noHints
    TcRnUnexpectedStandaloneKindSig{}
      -> [suggestExtension LangExt.StandaloneKindSignatures]
    TcRnIllegalRuleLhs{}
      -> noHints
    TcRnBadAssocRhs{}
      -> noHints
    TcRnDuplicateRoleAnnot{}
      -> noHints
    TcRnDuplicateKindSig{}
      -> noHints
    TcRnIllegalDerivStrategy ds -> case ds of
      ViaStrategy{} -> [suggestExtension LangExt.DerivingVia]
      _ -> [suggestExtension LangExt.DerivingStrategies]
    TcRnIllegalMultipleDerivClauses{}
      -> [suggestExtension LangExt.DerivingStrategies]
    TcRnNoDerivStratSpecified isDSEnabled -> if isDSEnabled
      then noHints
      else [suggestExtension LangExt.DerivingStrategies]
    TcRnStupidThetaInGadt{}
      -> noHints
    TcRnBadImplicitSplice{}
      -> noHints
    TcRnShadowedTyVarNameInFamResult{}
      -> noHints
    TcRnIncorrectTyVarOnLhsOfInjCond{}
      -> noHints
    TcRnUnknownTyVarsOnRhsOfInjCond{}
      -> noHints
    TcRnBadlyStaged{}
      -> noHints
    TcRnStageRestriction{}
      -> noHints
    TcRnTyThingUsedWrong{}
      -> noHints
    TcRnCannotDefaultKindVar{}
      -> noHints
    TcRnUninferrableTyVar{}
      -> noHints
    TcRnSkolemEscape{}
      -> noHints
    TcRnPatSynEscapedCoercion{}
      -> noHints
    TcRnPatSynExistentialInResult{}
      -> noHints
    TcRnPatSynArityMismatch{}
      -> noHints
    TcRnPatSynInvalidRhs name pat args (PatSynNotInvertible _)
      -> [SuggestExplicitBidiPatSyn name pat args]
    TcRnPatSynInvalidRhs{}
      -> noHints
    TcRnMultiAssocTyFamDefaults{}
      -> noHints
    TcRnTyFamDepsDisabled{}
      -> [suggestExtension LangExt.TypeFamilyDependencies]
    TcRnAbstractClosedTyFamDecl{}
      -> noHints
    TcRnPartialFieldSelector{}
      -> noHints
    TcRnBadFieldAnnotation _ _ LazyFieldsDisabled
      -> [suggestExtension LangExt.StrictData]
    TcRnBadFieldAnnotation{}
      -> noHints
    TcRnSuperclassCycle{}
      -> [suggestExtension LangExt.UndecidableSuperClasses]
    TcRnDefaultSigMismatch{}
      -> noHints
    TcRnTyFamsDisabled{}
      -> [suggestExtension LangExt.TypeFamilies]
    TcRnTyFamResultDisabled{}
      -> [suggestExtension LangExt.TypeFamilyDependencies]
    TcRnRoleValidationFailed{}
      -> noHints
    TcRnCommonFieldResultTypeMismatch{}
      -> noHints
    TcRnCommonFieldTypeMismatch{}
      -> noHints
    TcRnClassExtensionDisabled _ MultiParamDisabled{}
      -> [suggestExtension LangExt.MultiParamTypeClasses]
    TcRnClassExtensionDisabled _ FunDepsDisabled{}
      -> [suggestExtension LangExt.FunctionalDependencies]
    TcRnClassExtensionDisabled _ ConstrainedClassMethodsDisabled{}
      -> [suggestExtension LangExt.ConstrainedClassMethods]
    TcRnAssocNoClassTyVar{}
      -> noHints
    TcRnDataConParentTypeMismatch{}
      -> noHints
    TcRnGADTsDisabled{}
      -> [suggestExtension LangExt.GADTs]
    TcRnExistentialQuantificationDisabled{}
      -> [suggestExtension LangExt.ExistentialQuantification,
          suggestExtension LangExt.GADTs]
    TcRnGADTDataContext{}
      -> noHints
    TcRnMultipleConForNewtype{}
      -> noHints
    TcRnKindSignaturesDisabled{}
      -> [suggestExtension LangExt.KindSignatures]
    TcRnEmptyDataDeclsDisabled{}
      -> [suggestExtension LangExt.EmptyDataDecls]
    TcRnFamilyCategoryMismatch{}
      -> noHints
    TcRnFamilyArityMismatch{}
      -> noHints
    TcRnRoleMismatch{}
      -> noHints
    TcRnRoleCountMismatch{}
      -> noHints
    TcRnIllegalRoleAnnotation{}
      -> noHints
    TcRnRoleAnnotationsDisabled{}
      -> [suggestExtension LangExt.RoleAnnotations]
    TcRnIncoherentRoles{}
      -> [suggestExtension LangExt.IncoherentInstances]
    TcRnTyFamNameMismatch{}
      -> noHints
    TcRnBindVarAlreadyInScope{}
      -> noHints
    TcRnBindMultipleVariables{}
      -> noHints
    TcRnUnexpectedKindVar{}
      -> [suggestExtension LangExt.PolyKinds]
    TcRnNegativeNumTypeLiteral{}
      -> noHints
    TcRnIllegalKind _ suggest_polyKinds
      -> if suggest_polyKinds
         then [suggestExtension LangExt.PolyKinds]
         else noHints
    TcRnPrecedenceParsingError{}
      -> noHints
    TcRnSectionPrecedenceError{}
      -> noHints
    TcRnUnexpectedPatSigType{}
      -> [suggestExtension LangExt.ScopedTypeVariables]
    TcRnIllegalKindSignature{}
      -> [suggestExtension LangExt.KindSignatures]
    TcRnUnusedQuantifiedTypeVar{}
      -> noHints
    TcRnDataKindsError{}
      -> [suggestExtension LangExt.DataKinds]
    TcRnTypeSynonymCycle{}
      -> noHints
    TcRnCannotDefaultConcrete{}
      -> [SuggestAddTypeSignatures UnnamedBinding]
    TcRnInterfaceError reason
      -> interfaceErrorHints reason
    TcRnSelfImport{}
      -> noHints
    TcRnNoExplicitImportList{}
      -> noHints
    TcRnSafeImportsDisabled{}
      -> [SuggestSafeHaskell]
    TcRnDeprecatedModule{}
      -> noHints
    TcRnCompatUnqualifiedImport{}
      -> noHints
    TcRnRedundantSourceImport{}
      -> noHints
    TcRnImportLookup (ImportLookupBad k _ is ie patsyns_enabled) ->
      let mod = is_mod is
          occ = rdrNameOcc $ ieName ie
      in case k of
        BadImportAvailVar         -> [ImportSuggestion occ $ CouldRemoveTypeKeyword mod]
        BadImportNotExported      -> noHints
        BadImportAvailTyCon       -> [ImportSuggestion occ $ CouldAddTypeKeyword (is_mod is)]
        BadImportAvailDataCon par -> [ImportSuggestion occ $ ImportDataCon (Just (is_mod is, patsyns_enabled)) par]
        BadImportNotExportedSubordinates{} -> noHints
    TcRnImportLookup{}
      -> noHints
    TcRnUnusedImport{}
      -> noHints
    TcRnDuplicateDecls{}
      -> noHints
    TcRnPackageImportsDisabled
      -> [suggestExtension LangExt.PackageImports]
    TcRnIllegalDataCon{}
      -> noHints

  diagnosticCode :: TcRnMessage -> Maybe DiagnosticCode
  diagnosticCode = constructorCode

-- | Change [x] to "x", [x, y] to "x and y", [x, y, z] to "x, y, and z",
-- and so on.  The `and` stands for any `conjunction`, which is passed in.
commafyWith :: SDoc -> [SDoc] -> [SDoc]
commafyWith _ [] = []
commafyWith _ [x] = [x]
commafyWith conjunction [x, y] = [x <+> conjunction <+> y]
commafyWith conjunction xs = addConjunction $ punctuate comma xs
    where addConjunction [x, y] = [x, conjunction, y]
          addConjunction (x : xs) = x : addConjunction xs
          addConjunction _ = panic "commafyWith expected 2 or more elements"

deriveInstanceErrReasonHints :: Class
                             -> UsingGeneralizedNewtypeDeriving
                             -> DeriveInstanceErrReason
                             -> [GhcHint]
deriveInstanceErrReasonHints cls newtype_deriving = \case
  DerivErrNotWellKinded _ _ n_args_to_keep
    | cls `hasKey` gen1ClassKey && n_args_to_keep >= 0
    -> [suggestExtension LangExt.PolyKinds]
    | otherwise
    -> noHints
  DerivErrSafeHaskellGenericInst  -> noHints
  DerivErrDerivingViaWrongKind{}  -> noHints
  DerivErrNoEtaReduce{}           -> noHints
  DerivErrBootFileFound           -> noHints
  DerivErrDataConsNotAllInScope{} -> noHints
  DerivErrGNDUsedOnData           -> noHints
  DerivErrNullaryClasses          -> noHints
  DerivErrLastArgMustBeApp        -> noHints
  DerivErrNoFamilyInstance{}      -> noHints
  DerivErrNotStockDeriveable deriveAnyClassEnabled
    | deriveAnyClassEnabled == NoDeriveAnyClassEnabled
    -> [suggestExtension LangExt.DeriveAnyClass]
    | otherwise
    -> noHints
  DerivErrHasAssociatedDatatypes{}
    -> noHints
  DerivErrNewtypeNonDeriveableClass
    | newtype_deriving == NoGeneralizedNewtypeDeriving
    -> [useGND]
    | otherwise
    -> noHints
  DerivErrCannotEtaReduceEnough{}
    | newtype_deriving == NoGeneralizedNewtypeDeriving
    -> [useGND]
    | otherwise
    -> noHints
  DerivErrOnlyAnyClassDeriveable _ deriveAnyClassEnabled
    | deriveAnyClassEnabled == NoDeriveAnyClassEnabled
    -> [suggestExtension LangExt.DeriveAnyClass]
    | otherwise
    -> noHints
  DerivErrNotDeriveable deriveAnyClassEnabled
    | deriveAnyClassEnabled == NoDeriveAnyClassEnabled
    -> [suggestExtension LangExt.DeriveAnyClass]
    | otherwise
    -> noHints
  DerivErrNotAClass{}
    -> noHints
  DerivErrNoConstructors{}
    -> let info = text "to enable deriving for empty data types"
       in [useExtensionInOrderTo info LangExt.EmptyDataDeriving]
  DerivErrLangExtRequired{}
    -- This is a slightly weird corner case of GHC: we are failing
    -- to derive a typeclass instance because a particular 'Extension'
    -- is not enabled (and so we report in the main error), but here
    -- we don't want to /repeat/ to enable the extension in the hint.
    -> noHints
  DerivErrDunnoHowToDeriveForType{}
    -> noHints
  DerivErrMustBeEnumType rep_tc
    -- We want to suggest GND only if this /is/ a newtype.
    | newtype_deriving == NoGeneralizedNewtypeDeriving && isNewTyCon rep_tc
    -> [useGND]
    | otherwise
    -> noHints
  DerivErrMustHaveExactlyOneConstructor{}
    -> noHints
  DerivErrMustHaveSomeParameters{}
    -> noHints
  DerivErrMustNotHaveClassContext{}
    -> noHints
  DerivErrBadConstructor wcard _
    -> case wcard of
         Nothing        -> noHints
         Just YesHasWildcard -> [SuggestFillInWildcardConstraint]
         Just NoHasWildcard  -> [SuggestAddStandaloneDerivation]
  DerivErrGenerics{}
    -> noHints
  DerivErrEnumOrProduct{}
    -> noHints

messageWithInfoDiagnosticMessage :: UnitState
                                 -> ErrInfo
                                 -> Bool
                                 -> DecoratedSDoc
                                 -> DecoratedSDoc
messageWithInfoDiagnosticMessage unit_state ErrInfo{..} show_ctxt important =
  let err_info' = map (pprWithUnitState unit_state) ([errInfoContext | show_ctxt] ++ [errInfoSupplementary])
      in (mapDecoratedSDoc (pprWithUnitState unit_state) important) `unionDecoratedSDoc`
         mkDecorated err_info'

dodgy_msg :: Outputable ie => SDoc -> GlobalRdrElt -> ie -> SDoc
dodgy_msg kind tc ie
  = vcat [ text "The" <+> kind <+> text "item" <+> quotes (ppr ie) <+> text "suggests that"
         , quotes (ppr $ greName tc) <+> text "has" <+> sep rest ]
  where
    rest :: [SDoc]
    rest =
      case gre_info tc of
        IAmTyCon ClassFlavour
          -> [ text "(in-scope) class methods or associated types" <> comma
             , text "but it has none" ]
        IAmTyCon _
          -> [ text "(in-scope) constructors or record fields" <> comma
             , text "but it has none" ]
        _ -> [ text "children" <> comma
             , text "but it is not a type constructor or a class" ]

dodgy_msg_insert :: GlobalRdrElt -> IE GhcRn
dodgy_msg_insert tc_gre = IEThingAll noAnn ii
  where
    ii = noLocA (IEName noExtField $ noLocA $ greName tc_gre)

pprTypeDoesNotHaveFixedRuntimeRep :: Type -> FixedRuntimeRepProvenance -> SDoc
pprTypeDoesNotHaveFixedRuntimeRep ty prov =
  let what = pprFixedRuntimeRepProvenance prov
  in text "The" <+> what <+> text "does not have a fixed runtime representation:"
  $$ format_frr_err ty

format_frr_err :: Type  -- ^ the type which doesn't have a fixed runtime representation
                -> SDoc
format_frr_err ty
  = (bullet <+> ppr tidy_ty <+> dcolon <+> ppr tidy_ki)
  where
    (tidy_env, tidy_ty) = tidyOpenType emptyTidyEnv ty
    tidy_ki             = tidyType tidy_env (typeKind ty)

pprField :: (FieldLabelString, TcType) -> SDoc
pprField (f,ty) = ppr f <+> dcolon <+> ppr ty

pprRecordFieldPart :: RecordFieldPart -> SDoc
pprRecordFieldPart = \case
  RecordFieldDecl {}       -> text "declaration"
  RecordFieldConstructor{} -> text "construction"
  RecordFieldPattern{}     -> text "pattern"
  RecordFieldUpdate        -> text "update"

ppr_opfix :: (OpName, Fixity) -> SDoc
ppr_opfix (op, fixity) = pp_op <+> brackets (ppr fixity)
   where
     pp_op | NegateOp <- op = text "prefix `-'"
           | otherwise      = quotes (ppr op)

pprBindings :: [Name] -> SDoc
pprBindings = pprWithCommas (quotes . ppr)

injectivityErrorHerald :: SDoc
injectivityErrorHerald =
  text "Type family equation violates the family's injectivity annotation."

formatExportItemError :: SDoc -> String -> SDoc
formatExportItemError exportedThing reason =
  hsep [ text "The export item"
       , quotes exportedThing
       , text reason ]

-- | What warning flag is associated with the given missing signature?
missingSignatureWarningFlag :: MissingSignature -> Exported -> Bool -> WarningFlag
missingSignatureWarningFlag (MissingTopLevelBindingSig {}) exported overridden
  | IsExported <- exported
  , not overridden
  = Opt_WarnMissingExportedSignatures
  | otherwise
  = Opt_WarnMissingSignatures
missingSignatureWarningFlag (MissingPatSynSig {}) exported overridden
  | IsExported <- exported
  , not overridden
  = Opt_WarnMissingExportedPatternSynonymSignatures
  | otherwise
  = Opt_WarnMissingPatternSynonymSignatures
missingSignatureWarningFlag (MissingTyConKindSig {}) _ _
  = Opt_WarnMissingKindSignatures

useDerivingStrategies :: GhcHint
useDerivingStrategies =
  useExtensionInOrderTo (text "to pick a different strategy") LangExt.DerivingStrategies

useGND :: GhcHint
useGND = let info = text "for GHC's" <+> text "newtype-deriving extension"
         in suggestExtensionWithInfo info LangExt.GeneralizedNewtypeDeriving

cannotMakeDerivedInstanceHerald :: Class
                                -> [Type]
                                -> Maybe (DerivStrategy GhcTc)
                                -> UsingGeneralizedNewtypeDeriving
                                -> Bool -- ^ If False, only prints the why.
                                -> SDoc
                                -> SDoc
cannotMakeDerivedInstanceHerald cls cls_args mb_strat newtype_deriving pprHerald why =
  if pprHerald
     then sep [(hang (text "Can't make a derived instance of")
                   2 (quotes (ppr pred) <+> via_mechanism)
                $$ nest 2 extra) <> colon,
               nest 2 why]
      else why
  where
    strat_used = isJust mb_strat
    extra | not strat_used, (newtype_deriving == YesGeneralizedNewtypeDeriving)
          = text "(even with cunning GeneralizedNewtypeDeriving)"
          | otherwise = empty
    pred = mkClassPred cls cls_args
    via_mechanism | strat_used
                  , Just strat <- mb_strat
                  = text "with the" <+> (derivStrategyName strat) <+> text "strategy"
                  | otherwise
                  = empty

badCon :: DataCon -> SDoc -> SDoc
badCon con msg = text "Constructor" <+> quotes (ppr con) <+> msg

derivErrDiagnosticMessage :: Class
                          -> [Type]
                          -> Maybe (DerivStrategy GhcTc)
                          -> UsingGeneralizedNewtypeDeriving
                          -> Bool -- If True, includes the herald \"can't make a derived..\"
                          -> DeriveInstanceErrReason
                          -> SDoc
derivErrDiagnosticMessage cls cls_tys mb_strat newtype_deriving pprHerald = \case
  DerivErrNotWellKinded tc cls_kind _
    -> sep [ hang (text "Cannot derive well-kinded instance of form"
                         <+> quotes (pprClassPred cls cls_tys
                                       <+> parens (ppr tc <+> text "...")))
                  2 empty
           , nest 2 (text "Class" <+> quotes (ppr cls)
                         <+> text "expects an argument of kind"
                         <+> quotes (pprKind cls_kind))
           ]
  DerivErrSafeHaskellGenericInst
    ->     text "Generic instances can only be derived in"
       <+> text "Safe Haskell using the stock strategy."
  DerivErrDerivingViaWrongKind cls_kind via_ty via_kind
    -> hang (text "Cannot derive instance via" <+> quotes (pprType via_ty))
          2 (text "Class" <+> quotes (ppr cls)
                  <+> text "expects an argument of kind"
                  <+> quotes (pprKind cls_kind) <> char ','
         $+$ text "but" <+> quotes (pprType via_ty)
                  <+> text "has kind" <+> quotes (pprKind via_kind))
  DerivErrNoEtaReduce inst_ty
    -> sep [text "Cannot eta-reduce to an instance of form",
            nest 2 (text "instance (...) =>"
                   <+> pprClassPred cls (cls_tys ++ [inst_ty]))]
  DerivErrBootFileFound
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "Cannot derive instances in hs-boot files"
          $+$ text "Write an instance declaration instead")
  DerivErrDataConsNotAllInScope tc
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (hang (text "The data constructors of" <+> quotes (ppr tc) <+> text "are not all in scope")
            2 (text "so you cannot derive an instance for it"))
  DerivErrGNDUsedOnData
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "GeneralizedNewtypeDeriving cannot be used on non-newtypes")
  DerivErrNullaryClasses
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "Cannot derive instances for nullary classes")
  DerivErrLastArgMustBeApp
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         ( text "The last argument of the instance must be a"
         <+> text "data or newtype application")
  DerivErrNoFamilyInstance tc tc_args
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "No family instance for" <+> quotes (pprTypeApp tc tc_args))
  DerivErrNotStockDeriveable _
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (quotes (ppr cls) <+> text "is not a stock derivable class (Eq, Show, etc.)")
  DerivErrHasAssociatedDatatypes hasAdfs at_last_cls_tv_in_kinds at_without_last_cls_tv
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         $ vcat [ ppWhen (hasAdfs == YesHasAdfs) adfs_msg
               , case at_without_last_cls_tv of
                    YesAssociatedTyNotParamOverLastTyVar tc -> at_without_last_cls_tv_msg tc
                    NoAssociatedTyNotParamOverLastTyVar     -> empty
               , case at_last_cls_tv_in_kinds of
                   YesAssocTyLastVarInKind tc -> at_last_cls_tv_in_kinds_msg tc
                   NoAssocTyLastVarInKind     -> empty
               ]
       where

         adfs_msg  = text "the class has associated data types"

         at_without_last_cls_tv_msg at_tc = hang
           (text "the associated type" <+> quotes (ppr at_tc)
            <+> text "is not parameterized over the last type variable")
           2 (text "of the class" <+> quotes (ppr cls))

         at_last_cls_tv_in_kinds_msg at_tc = hang
           (text "the associated type" <+> quotes (ppr at_tc)
            <+> text "contains the last type variable")
          2 (text "of the class" <+> quotes (ppr cls)
            <+> text "in a kind, which is not (yet) allowed")
  DerivErrNewtypeNonDeriveableClass
    -> derivErrDiagnosticMessage cls cls_tys mb_strat newtype_deriving pprHerald (DerivErrNotStockDeriveable NoDeriveAnyClassEnabled)
  DerivErrCannotEtaReduceEnough eta_ok
    -> let cant_derive_err = ppUnless eta_ok eta_msg
           eta_msg = text "cannot eta-reduce the representation type enough"
       in cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
          cant_derive_err
  DerivErrOnlyAnyClassDeriveable tc _
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (quotes (ppr tc) <+> text "is a type class,"
                          <+> text "and can only have a derived instance"
                          $+$ text "if DeriveAnyClass is enabled")
  DerivErrNotDeriveable _
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald empty
  DerivErrNotAClass predType
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (quotes (ppr predType) <+> text "is not a class")
  DerivErrNoConstructors rep_tc
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (quotes (pprSourceTyCon rep_tc) <+> text "must have at least one data constructor")
  DerivErrLangExtRequired ext
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "You need " <> ppr ext
            <+> text "to derive an instance for this class")
  DerivErrDunnoHowToDeriveForType ty
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
        (hang (text "Don't know how to derive" <+> quotes (ppr cls))
              2 (text "for type" <+> quotes (ppr ty)))
  DerivErrMustBeEnumType rep_tc
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (sep [ quotes (pprSourceTyCon rep_tc) <+>
                text "must be an enumeration type"
              , text "(an enumeration consists of one or more nullary, non-GADT constructors)" ])

  DerivErrMustHaveExactlyOneConstructor rep_tc
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (quotes (pprSourceTyCon rep_tc) <+> text "must have precisely one constructor")
  DerivErrMustHaveSomeParameters rep_tc
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "Data type" <+> quotes (ppr rep_tc) <+> text "must have some type parameters")
  DerivErrMustNotHaveClassContext rep_tc bad_stupid_theta
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "Data type" <+> quotes (ppr rep_tc)
           <+> text "must not have a class context:" <+> pprTheta bad_stupid_theta)
  DerivErrBadConstructor _ reasons
    -> let why = vcat $ map renderReason reasons
       in cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald why
         where
           renderReason = \case
                 DerivErrBadConExistential con
                   -> badCon con $ text "must be truly polymorphic in the last argument of the data type"
                 DerivErrBadConCovariant con
                   -> badCon con $ text "must not use the type variable in a function argument"
                 DerivErrBadConFunTypes con
                   -> badCon con $ text "must not contain function types"
                 DerivErrBadConWrongArg con
                   -> badCon con $ text "must use the type variable only as the last argument of a data type"
                 DerivErrBadConIsGADT con
                   -> badCon con $ text "is a GADT"
                 DerivErrBadConHasExistentials con
                   -> badCon con $ text "has existential type variables in its type"
                 DerivErrBadConHasConstraints con
                   -> badCon con $ text "has constraints in its type"
                 DerivErrBadConHasHigherRankType con
                   -> badCon con $ text "has a higher-rank type"
  DerivErrGenerics reasons
    -> let why = vcat $ map renderReason reasons
       in cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald why
         where
           renderReason = \case
             DerivErrGenericsMustNotHaveDatatypeContext tc_name
                -> ppr tc_name <+> text "must not have a datatype context"
             DerivErrGenericsMustNotHaveExoticArgs dc
                -> ppr dc <+> text "must not have exotic unlifted or polymorphic arguments"
             DerivErrGenericsMustBeVanillaDataCon dc
                -> ppr dc <+> text "must be a vanilla data constructor"
             DerivErrGenericsMustHaveSomeTypeParams rep_tc
                ->     text "Data type" <+> quotes (ppr rep_tc)
                   <+> text "must have some type parameters"
             DerivErrGenericsMustNotHaveExistentials con
               -> badCon con $ text "must not have existential arguments"
             DerivErrGenericsWrongArgKind con
               -> badCon con $
                    text "applies a type to an argument involving the last parameter"
                 $$ text "but the applied type is not of kind * -> *"
  DerivErrEnumOrProduct this that
    -> let ppr1 = derivErrDiagnosticMessage cls cls_tys mb_strat newtype_deriving False this
           ppr2 = derivErrDiagnosticMessage cls cls_tys mb_strat newtype_deriving False that
       in cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
          (ppr1 $$ text "  or" $$ ppr2)

lookupInstanceErrDiagnosticMessage :: Class
                                   -> [Type]
                                   -> LookupInstanceErrReason
                                   -> SDoc
lookupInstanceErrDiagnosticMessage cls tys = \case
  LookupInstErrNotExact
    -> text "Not an exact match (i.e., some variables get instantiated)"
  LookupInstErrFlexiVar
    -> text "flexible type variable:" <+> (ppr $ mkTyConApp (classTyCon cls) tys)
  LookupInstErrNotFound
    -> text "instance not found" <+> (ppr $ mkTyConApp (classTyCon cls) tys)

{- *********************************************************************
*                                                                      *
              Outputable SolverReportErrCtxt (for debugging)
*                                                                      *
**********************************************************************-}

instance Outputable SolverReportErrCtxt where
  ppr (CEC { cec_binds              = bvar
           , cec_defer_type_errors  = dte
           , cec_expr_holes         = eh
           , cec_type_holes         = th
           , cec_out_of_scope_holes = osh
           , cec_warn_redundant     = wr
           , cec_expand_syns        = es
           , cec_suppress           = sup })
    = text "CEC" <+> braces (vcat
         [ text "cec_binds"              <+> equals <+> ppr bvar
         , text "cec_defer_type_errors"  <+> equals <+> ppr dte
         , text "cec_expr_holes"         <+> equals <+> ppr eh
         , text "cec_type_holes"         <+> equals <+> ppr th
         , text "cec_out_of_scope_holes" <+> equals <+> ppr osh
         , text "cec_warn_redundant"     <+> equals <+> ppr wr
         , text "cec_expand_syns"        <+> equals <+> ppr es
         , text "cec_suppress"           <+> equals <+> ppr sup ])

{- *********************************************************************
*                                                                      *
                    Outputting TcSolverReportMsg errors
*                                                                      *
**********************************************************************-}

-- | Pretty-print a 'SolverReportWithCtxt', containing a 'TcSolverReportMsg'
-- with its enclosing 'SolverReportErrCtxt'.
pprSolverReportWithCtxt :: SolverReportWithCtxt -> SDoc
pprSolverReportWithCtxt (SolverReportWithCtxt { reportContext = ctxt, reportContent = msg })
   = pprTcSolverReportMsg ctxt msg

-- | Pretty-print a 'TcSolverReportMsg', with its enclosing 'SolverReportErrCtxt'.
pprTcSolverReportMsg :: SolverReportErrCtxt -> TcSolverReportMsg -> SDoc
pprTcSolverReportMsg _ (BadTelescope telescope skols) =
  hang (text "These kind and type variables:" <+> ppr telescope $$
       text "are out of dependency order. Perhaps try this ordering:")
    2 (pprTyVars sorted_tvs)
  where
    sorted_tvs = scopedSort skols
pprTcSolverReportMsg _ (UserTypeError ty) =
  pprUserTypeErrorTy ty
pprTcSolverReportMsg _ (UnsatisfiableError ty) =
  pprUserTypeErrorTy ty
pprTcSolverReportMsg ctxt (ReportHoleError hole err) =
  pprHoleError ctxt hole err
pprTcSolverReportMsg ctxt
  (CannotUnifyVariable
    { mismatchMsg         = msg
    , cannotUnifyReason   = reason })
  =  pprMismatchMsg ctxt msg
  $$ pprCannotUnifyVariableReason ctxt reason
pprTcSolverReportMsg ctxt
  (Mismatch
     { mismatchMsg           = mismatch_msg
     , mismatchTyVarInfo     = tv_info
     , mismatchAmbiguityInfo = ambig_infos
     , mismatchCoercibleInfo = coercible_info })
  = vcat ([ pprMismatchMsg ctxt mismatch_msg
          , maybe empty (pprTyVarInfo ctxt) tv_info
          , maybe empty pprCoercibleMsg coercible_info ]
          ++ (map pprAmbiguityInfo ambig_infos))
pprTcSolverReportMsg _ (FixedRuntimeRepError frr_origs) =
  vcat (map make_msg frr_origs)
  where
    -- Assemble the error message: pair up each origin with the corresponding type, e.g.
    --   • FixedRuntimeRep origin msg 1 ...
    --       a :: TYPE r1
    --   • FixedRuntimeRep origin msg 2 ...
    --       b :: TYPE r2
    make_msg :: FixedRuntimeRepErrorInfo -> SDoc
    make_msg (FRR_Info { frr_info_origin =
                           FixedRuntimeRepOrigin
                             { frr_type    = ty
                             , frr_context = frr_ctxt }
                       , frr_info_not_concrete =
                         mb_not_conc }) =
      -- Add bullet points if there is more than one error.
      (if length frr_origs > 1 then (bullet <+>) else id) $
        vcat [ sep [ pprFixedRuntimeRepContext frr_ctxt
                   , text "does not have a fixed runtime representation." ]
             , type_printout ty
             , case mb_not_conc of
                Nothing -> empty
                Just (conc_tv, not_conc) ->
                  unsolved_concrete_eq_explanation conc_tv not_conc ]

    -- Don't print out the type (only the kind), if the type includes
    -- a confusing cast, unless the user passed -fprint-explicit-coercions.
    --
    -- Example:
    --
    --   In T20363, we have a representation-polymorphism error with a type
    --   of the form
    --
    --     ( (# #) |> co ) :: TYPE NilRep
    --
    --   where NilRep is a nullary type family application which reduces to TupleRep '[].
    --   We prefer avoiding showing the cast to the user, but we also don't want to
    --   print the confusing:
    --
    --     (# #) :: TYPE NilRep
    --
    --  So in this case we simply don't print the type, only the kind.
    confusing_cast :: Type -> Bool
    confusing_cast ty =
      case ty of
        CastTy inner_ty _
          -- A confusing cast is one that is responsible
          -- for a representation-polymorphism error.
          -> isConcreteType (typeKind inner_ty)
        _ -> False

    type_printout :: Type -> SDoc
    type_printout ty =
      sdocOption sdocPrintExplicitCoercions $ \ show_coercions ->
        if  confusing_cast ty && not show_coercions
        then vcat [ text "Its kind is:"
                  , nest 2 $ pprWithTYPE (typeKind ty)
                  , text "(Use -fprint-explicit-coercions to see the full type.)" ]
        else vcat [ text "Its type is:"
                  , nest 2 $ ppr ty <+> dcolon <+> pprWithTYPE (typeKind ty) ]

    unsolved_concrete_eq_explanation :: TcTyVar -> Type -> SDoc
    unsolved_concrete_eq_explanation tv not_conc =
          text "Cannot unify" <+> quotes (ppr not_conc)
      <+> text "with the type variable" <+> quotes (ppr tv)
      $$  text "because it is not a concrete" <+> what <> dot
      where
        ki = tyVarKind tv
        what :: SDoc
        what
          | isRuntimeRepTy ki
          = quotes (text "RuntimeRep")
          | isLevityTy ki
          = quotes (text "Levity")
          | otherwise
          = text "type"
pprTcSolverReportMsg _ (UntouchableVariable tv implic)
  | Implic { ic_given = given, ic_info = skol_info } <- implic
  = sep [ quotes (ppr tv) <+> text "is untouchable"
        , nest 2 $ text "inside the constraints:" <+> pprEvVarTheta given
        , nest 2 $ text "bound by" <+> ppr skol_info
        , nest 2 $ text "at" <+>
          ppr (getLclEnvLoc (ic_env implic)) ]
pprTcSolverReportMsg _ (BlockedEquality item) =
  vcat [ hang (text "Cannot use equality for substitution:")
           2 (ppr (errorItemPred item))
       , text "Doing so would be ill-kinded." ]
pprTcSolverReportMsg _ (ExpectingMoreArguments n thing) =
  text "Expecting" <+> speakN (abs n) <+>
    more <+> quotes (ppr thing)
  where
    more
     | n == 1    = text "more argument to"
     | otherwise = text "more arguments to" -- n > 1
pprTcSolverReportMsg ctxt (UnboundImplicitParams (item :| items)) =
  let givens = getUserGivens ctxt
  in if null givens
     then addArising (errorItemCtLoc item) $
            sep [ text "Unbound implicit parameter" <> plural preds
                , nest 2 (pprParendTheta preds) ]
     else pprMismatchMsg ctxt (CouldNotDeduce givens (item :| items) Nothing)
  where
    preds = map errorItemPred (item : items)
pprTcSolverReportMsg _ (AmbiguityPreventsSolvingCt item ambigs) =
  pprAmbiguityInfo (Ambiguity True ambigs) <+>
  pprArising (errorItemCtLoc item) $$
  text "prevents the constraint" <+> quotes (pprParendType $ errorItemPred item)
  <+> text "from being solved."
pprTcSolverReportMsg ctxt@(CEC {cec_encl = implics})
  (CannotResolveInstance item unifiers candidates imp_errs suggs binds)
  =
    vcat
      [ no_inst_msg
      , nest 2 extra_note
      , mb_patsyn_prov `orElse` empty
      , ppWhen (has_ambigs && not (null unifiers && null useful_givens))
        (vcat [ ppUnless lead_with_ambig $
                  pprAmbiguityInfo (Ambiguity False (ambig_kvs, ambig_tvs))
              , pprRelevantBindings binds
              , potential_msg ])
      , ppWhen (isNothing mb_patsyn_prov) $
            -- Don't suggest fixes for the provided context of a pattern
            -- synonym; the right fix is to bind more in the pattern
        show_fixes (ctxtFixes has_ambigs pred implics
                    ++ drv_fixes ++ naked_sc_fixes)
      , ppWhen (not (null candidates))
        (hang (text "There are instances for similar types:")
            2 (vcat (map ppr candidates)))
            -- See Note [Report candidate instances]
      , vcat $ map ppr imp_errs
      , vcat $ map ppr suggs ]
  where
    orig          = errorItemOrigin item
    pred          = errorItemPred item
    (clas, tys)   = getClassPredTys pred
    -- See Note [Highlighting ambiguous type variables] in GHC.Tc.Errors
    (ambig_kvs, ambig_tvs) = ambigTkvsOfTy pred
    ambigs = ambig_kvs ++ ambig_tvs
    has_ambigs = not (null ambigs)
    useful_givens = discardProvCtxtGivens orig (getUserGivensFromImplics implics)
         -- useful_givens are the enclosing implications with non-empty givens,
         -- modulo the horrid discardProvCtxtGivens
    lead_with_ambig = not (null ambigs)
                   && not (any isRuntimeUnkSkol ambigs)
                   && not (null unifiers)
                   && null useful_givens

    no_inst_msg :: SDoc
    no_inst_msg
      | lead_with_ambig
      = pprTcSolverReportMsg ctxt $ AmbiguityPreventsSolvingCt item (ambig_kvs, ambig_tvs)
      | otherwise
      = pprMismatchMsg ctxt $ CouldNotDeduce useful_givens (item :| []) Nothing

    -- Report "potential instances" only when the constraint arises
    -- directly from the user's use of an overloaded function
    want_potential (TypeEqOrigin {}) = False
    want_potential _                 = True

    potential_msg
      = ppWhen (not (null unifiers) && want_potential orig) $
          potential_hdr $$
          potentialInstancesErrMsg (PotentialInstances { matches = [], unifiers })

    potential_hdr
      = ppWhen lead_with_ambig $
        text "Probable fix: use a type annotation to specify what"
        <+> pprQuotedList ambig_tvs <+> text "should be."

    mb_patsyn_prov :: Maybe SDoc
    mb_patsyn_prov
      | not lead_with_ambig
      , ProvCtxtOrigin PSB{ psb_def = L _ pat } <- orig
      = Just (vcat [ text "In other words, a successful match on the pattern"
                   , nest 2 $ ppr pat
                   , text "does not provide the constraint" <+> pprParendType pred ])
      | otherwise = Nothing

    extra_note | any isFunTy (filterOutInvisibleTypes (classTyCon clas) tys)
               = text "(maybe you haven't applied a function to enough arguments?)"
               | className clas == typeableClassName  -- Avoid mysterious "No instance for (Typeable T)
               , [_,ty] <- tys                        -- Look for (Typeable (k->*) (T k))
               , Just (tc,_) <- tcSplitTyConApp_maybe ty
               , not (isTypeFamilyTyCon tc)
               = hang (text "GHC can't yet do polykinded")
                    2 (text "Typeable" <+>
                       parens (ppr ty <+> dcolon <+> ppr (typeKind ty)))
               | otherwise
               = empty

    drv_fixes = case orig of
                   DerivClauseOrigin                  -> [drv_fix False]
                   StandAloneDerivOrigin              -> [drv_fix True]
                   DerivOriginDC _ _       standalone -> [drv_fix standalone]
                   DerivOriginCoerce _ _ _ standalone -> [drv_fix standalone]
                   _                                  -> []

    drv_fix standalone_wildcard
      | standalone_wildcard
      = text "fill in the wildcard constraint yourself"
      | otherwise
      = hang (text "use a standalone 'deriving instance' declaration,")
           2 (text "so you can specify the instance context yourself")

    -- naked_sc_fix: try to produce a helpful error message for
    -- superclass constraints caught by the subtleties described by
    -- Note [Recursive superclasses] in GHC.TyCl.Instance
    naked_sc_fixes
      | ScOrigin _ NakedSc <- orig  -- A superclass wanted with no instance decls used yet
      , any non_tyvar_preds useful_givens  -- Some non-tyvar givens
      = [vcat [ text "If the constraint looks soluble from a superclass of the instance context,"
              , text "read 'Undecidable instances and loopy superclasses' in the user manual" ]]
      | otherwise = []

    non_tyvar_preds :: UserGiven -> Bool
    non_tyvar_preds = any non_tyvar_pred . ic_given

    non_tyvar_pred :: EvVar -> Bool
    -- Tells if the Given is of form (C ty1 .. tyn), where the tys are not all tyvars
    non_tyvar_pred given = case getClassPredTys_maybe (idType given) of
                             Just (_, tys) -> not (all isTyVarTy tys)
                             Nothing       -> False

pprTcSolverReportMsg (CEC {cec_encl = implics}) (OverlappingInstances item matches unifiers) =
  vcat
    [ addArising ct_loc $
        (text "Overlapping instances for"
        <+> pprType (mkClassPred clas tys))
    , ppUnless (null matching_givens) $
                  sep [text "Matching givens (or their superclasses):"
                      , nest 2 (vcat matching_givens)]
    ,  potentialInstancesErrMsg
        (PotentialInstances { matches = NE.toList matches, unifiers })
    ,  ppWhen (null matching_givens && null (NE.tail matches) && null unifiers) $
       -- Intuitively, some given matched the wanted in their
       -- flattened or rewritten (from given equalities) form
       -- but the matcher can't figure that out because the
       -- constraints are non-flat and non-rewritten so we
       -- simply report back the whole given
       -- context. Accelerate Smart.hs showed this problem.
         sep [ text "There exists a (perhaps superclass) match:"
             , nest 2 (vcat (pp_givens useful_givens))]

    ,  ppWhen (null $ NE.tail matches) $
       parens (vcat [ ppUnless (null tyCoVars) $
                        text "The choice depends on the instantiation of" <+>
                          quotes (pprWithCommas ppr tyCoVars)
                    , ppUnless (null famTyCons) $
                        if (null tyCoVars)
                          then
                            text "The choice depends on the result of evaluating" <+>
                              quotes (pprWithCommas ppr famTyCons)
                          else
                            text "and the result of evaluating" <+>
                              quotes (pprWithCommas ppr famTyCons)
                    , ppWhen (null (matching_givens)) $
                      vcat [ text "To pick the first instance above, use IncoherentInstances"
                           , text "when compiling the other instance declarations"]
               ])]
  where
    ct_loc          = errorItemCtLoc item
    orig            = ctLocOrigin ct_loc
    pred            = errorItemPred item
    (clas, tys)     = getClassPredTys pred
    tyCoVars        = tyCoVarsOfTypesList tys
    famTyCons       = filter isFamilyTyCon $ concatMap (nonDetEltsUniqSet . tyConsOfType) tys
    useful_givens   = discardProvCtxtGivens orig (getUserGivensFromImplics implics)
    matching_givens = mapMaybe matchable useful_givens
    matchable implic@(Implic { ic_given = evvars, ic_info = skol_info })
      = case ev_vars_matching of
             [] -> Nothing
             _  -> Just $ hang (pprTheta ev_vars_matching)
                            2 (sep [ text "bound by" <+> ppr skol_info
                                   , text "at" <+>
                                     ppr (getLclEnvLoc (ic_env implic)) ])
        where ev_vars_matching = [ pred
                                 | ev_var <- evvars
                                 , let pred = evVarPred ev_var
                                 , any can_match (pred : transSuperClasses pred) ]
              can_match pred
                 = case getClassPredTys_maybe pred of
                     Just (clas', tys') -> clas' == clas
                                          && isJust (tcMatchTys tys tys')
                     Nothing -> False
pprTcSolverReportMsg _ (UnsafeOverlap item match unsafe_overlapped) =
  vcat [ addArising ct_loc (text "Unsafe overlapping instances for"
                  <+> pprType (mkClassPred clas tys))
       , sep [text "The matching instance is:",
              nest 2 (pprInstance match)]
       , vcat [ text "It is compiled in a Safe module and as such can only"
              , text "overlap instances from the same module, however it"
              , text "overlaps the following instances from different" <+>
                text "modules:"
              , nest 2 (vcat [pprInstances $ NE.toList unsafe_overlapped])
              ]
       ]
  where
    ct_loc      = errorItemCtLoc item
    pred        = errorItemPred item
    (clas, tys) = getClassPredTys pred

pprCannotUnifyVariableReason :: SolverReportErrCtxt -> CannotUnifyVariableReason -> SDoc
pprCannotUnifyVariableReason ctxt (CannotUnifyWithPolytype item tv1 ty2 mb_tv_info) =
  vcat [ (if isSkolemTyVar tv1
          then text "Cannot equate type variable"
          else text "Cannot instantiate unification variable")
         <+> quotes (ppr tv1)
       , hang (text "with a" <+> what <+> text "involving polytypes:") 2 (ppr ty2)
       , maybe empty (pprTyVarInfo ctxt) mb_tv_info ]
  where
    what = text $ levelString $
           ctLocTypeOrKind_maybe (errorItemCtLoc item) `orElse` TypeLevel

pprCannotUnifyVariableReason _ (SkolemEscape item implic esc_skols) =
  let
    esc_doc = sep [ text "because" <+> what <+> text "variable" <> plural esc_skols
                <+> pprQuotedList esc_skols
              , text "would escape" <+>
                if isSingleton esc_skols then text "its scope"
                                         else text "their scope" ]
  in
  vcat [ nest 2 $ esc_doc
       , sep [ (if isSingleton esc_skols
                then text "This (rigid, skolem)" <+>
                     what <+> text "variable is"
                else text "These (rigid, skolem)" <+>
                     what <+> text "variables are")
         <+> text "bound by"
       , nest 2 $ ppr (ic_info implic)
       , nest 2 $ text "at" <+>
         ppr (getLclEnvLoc (ic_env implic)) ] ]
  where
    what = text $ levelString $
           ctLocTypeOrKind_maybe (errorItemCtLoc item) `orElse` TypeLevel

pprCannotUnifyVariableReason ctxt
  (OccursCheck
    { occursCheckInterestingTyVars = interesting_tvs
    , occursCheckAmbiguityInfos    = ambig_infos })
  = ppr_interesting_tyVars interesting_tvs
  $$ vcat (map pprAmbiguityInfo ambig_infos)
  where
    ppr_interesting_tyVars [] = empty
    ppr_interesting_tyVars (tv:tvs) =
      hang (text "Type variable kinds:") 2 $
      vcat (map (tyvar_binding . tidyTyCoVarOcc (cec_tidy ctxt))
                (tv:tvs))
    tyvar_binding tyvar = ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)
pprCannotUnifyVariableReason ctxt (DifferentTyVars tv_info)
  = pprTyVarInfo ctxt tv_info
pprCannotUnifyVariableReason ctxt (RepresentationalEq tv_info mb_coercible_msg)
  = pprTyVarInfo ctxt tv_info
  $$ maybe empty pprCoercibleMsg mb_coercible_msg

pprMismatchMsg :: SolverReportErrCtxt -> MismatchMsg -> SDoc
pprMismatchMsg ctxt
  (BasicMismatch { mismatch_ea   = ea
                 , mismatch_item = item
                 , mismatch_ty1  = ty1  -- Expected
                 , mismatch_ty2  = ty2  -- Actual
                 , mismatch_whenMatching = mb_match_txt
                 , mismatch_mb_same_occ  = same_occ_info })
  =  vcat [ addArising (errorItemCtLoc item) msg
          , ea_extra
          , maybe empty (pprWhenMatching ctxt) mb_match_txt
          , maybe empty pprSameOccInfo same_occ_info ]
  where
    msg
      | (isLiftedRuntimeRep ty1 && isUnliftedRuntimeRep ty2) ||
        (isLiftedRuntimeRep ty2 && isUnliftedRuntimeRep ty1) ||
        (isLiftedLevity ty1 && isUnliftedLevity ty2) ||
        (isLiftedLevity ty2 && isUnliftedLevity ty1)
      = text "Couldn't match a lifted type with an unlifted type"

      | isAtomicTy ty1 || isAtomicTy ty2
      = -- Print with quotes
        sep [ text herald1 <+> quotes (ppr ty1)
            , nest padding $
              text herald2 <+> quotes (ppr ty2) ]

      | otherwise
      = -- Print with vertical layout
        vcat [ text herald1 <> colon <+> ppr ty1
             , nest padding $
               text herald2 <> colon <+> ppr ty2 ]

    herald1 = conc [ "Couldn't match"
                   , if is_repr then "representation of" else ""
                   , if want_ea then "expected"          else ""
                   , what ]
    herald2 = conc [ "with"
                   , if is_repr then "that of"           else ""
                   , if want_ea then ("actual " ++ what) else "" ]

    padding = length herald1 - length herald2

    (want_ea, ea_extra)
      = case ea of
         NoEA        -> (False, empty)
         EA mb_extra -> (True , maybe empty (pprExpectedActualInfo ctxt) mb_extra)
    is_repr = case errorItemEqRel item of { ReprEq -> True; NomEq -> False }

    what = levelString (ctLocTypeOrKind_maybe (errorItemCtLoc item) `orElse` TypeLevel)

    conc :: [String] -> String
    conc = foldr1 add_space

    add_space :: String -> String -> String
    add_space s1 s2 | null s1   = s2
                    | null s2   = s1
                    | otherwise = s1 ++ (' ' : s2)
pprMismatchMsg _
  (KindMismatch { kmismatch_what     = thing
                , kmismatch_expected = exp
                , kmismatch_actual   = act })
  = hang (text "Expected" <+> kind_desc <> comma)
      2 (text "but" <+> quotes (ppr thing) <+> text "has kind" <+>
        quotes (ppr act))
  where
    kind_desc | isConstraintLikeKind exp = text "a constraint"
              | Just arg <- kindRep_maybe exp  -- TYPE t0
              , tcIsTyVarTy arg = sdocOption sdocPrintExplicitRuntimeReps $ \case
                                   True  -> text "kind" <+> quotes (ppr exp)
                                   False -> text "a type"
              | otherwise       = text "kind" <+> quotes (ppr exp)

pprMismatchMsg ctxt
  (TypeEqMismatch { teq_mismatch_ppr_explicit_kinds = ppr_explicit_kinds
                  , teq_mismatch_item     = item
                  , teq_mismatch_ty1      = ty1   -- These types are the actual types
                  , teq_mismatch_ty2      = ty2   --   that don't match; may be swapped
                  , teq_mismatch_expected = exp   -- These are the context of
                  , teq_mismatch_actual   = act   --   the mis-match
                  , teq_mismatch_what     = mb_thing
                  , teq_mb_same_occ       = mb_same_occ })
  = addArising ct_loc $ pprWithExplicitKindsWhen ppr_explicit_kinds msg
  $$ maybe empty pprSameOccInfo mb_same_occ
  where
    msg | Just (torc, rep) <- sORTKind_maybe exp
        = msg_for_exp_sort torc rep

        | Just nargs_msg <- num_args_msg
        , Right ea_msg <- mk_ea_msg ctxt (Just item) level orig
        = nargs_msg $$ pprMismatchMsg ctxt ea_msg

        | ea_looks_same ty1 ty2 exp act
        , Right ea_msg <- mk_ea_msg ctxt (Just item) level orig
        = pprMismatchMsg ctxt ea_msg

        | otherwise
        = bale_out_msg

      -- bale_out_msg: the mismatched types are /inside/ exp and act
    bale_out_msg = vcat errs
      where
        errs = case mk_ea_msg ctxt Nothing level orig of
                  Left ea_info -> pprMismatchMsg ctxt mismatch_err
                                : map (pprExpectedActualInfo ctxt) ea_info
                  Right ea_err -> [ pprMismatchMsg ctxt mismatch_err
                                  , pprMismatchMsg ctxt ea_err ]
        mismatch_err = mkBasicMismatchMsg NoEA item ty1 ty2

      -- 'expected' is (TYPE rep) or (CONSTRAINT rep)
    msg_for_exp_sort exp_torc exp_rep
      | Just (act_torc, act_rep) <- sORTKind_maybe act
      = -- (TYPE exp_rep) ~ (CONSTRAINT act_rep) etc
        msg_torc_torc act_torc act_rep
      | otherwise
      = -- (TYPE _) ~ Bool, etc
        maybe_num_args_msg $$
        sep [ text "Expected a" <+> ppr_torc exp_torc <> comma
            , text "but" <+> case mb_thing of
                Nothing    -> text "found something with kind"
                Just thing -> quotes (ppr thing) <+> text "has kind"
            , quotes (pprWithTYPE act) ]

      where
        msg_torc_torc act_torc act_rep
          | exp_torc == act_torc
          = msg_same_torc act_torc act_rep
          | otherwise
          = sep [ text "Expected a" <+> ppr_torc exp_torc <> comma
                , text "but" <+> case mb_thing of
                     Nothing    -> text "found a"
                     Just thing -> quotes (ppr thing) <+> text "is a"
                  <+> ppr_torc act_torc ]

        msg_same_torc act_torc act_rep
          | Just exp_doc <- describe_rep exp_rep
          , Just act_doc <- describe_rep act_rep
          = sep [ text "Expected" <+> exp_doc <+> ppr_torc exp_torc <> comma
                , text "but" <+> case mb_thing of
                     Just thing -> quotes (ppr thing) <+> text "is"
                     Nothing    -> text "got"
                  <+> act_doc <+> ppr_torc act_torc ]
        msg_same_torc _ _ = bale_out_msg

    ct_loc = errorItemCtLoc item
    orig   = errorItemOrigin item
    level  = ctLocTypeOrKind_maybe ct_loc `orElse` TypeLevel

    num_args_msg = case level of
      KindLevel
        | not (isMetaTyVarTy exp) && not (isMetaTyVarTy act)
           -- if one is a meta-tyvar, then it's possible that the user
           -- has asked for something impredicative, and we couldn't unify.
           -- Don't bother with counting arguments.
        -> let n_act = count_args act
               n_exp = count_args exp in
           case n_act - n_exp of
             n | n > 0   -- we don't know how many args there are, so don't
                         -- recommend removing args that aren't
               , Just thing <- mb_thing
               -> Just $ pprTcSolverReportMsg ctxt (ExpectingMoreArguments n thing)
             _ -> Nothing

      _ -> Nothing

    maybe_num_args_msg = num_args_msg `orElse` empty

    count_args ty = count isVisiblePiTyBinder $ fst $ splitPiTys ty

    ppr_torc TypeLike       = text "type";
    ppr_torc ConstraintLike = text "constraint"

    describe_rep :: RuntimeRepType -> Maybe SDoc
    -- describe_rep IntRep            = Just "an IntRep"
    -- describe_rep (BoxedRep Lifted) = Just "a lifted"
    --   etc
    describe_rep rep
      | Just (rr_tc, rr_args) <- splitRuntimeRep_maybe rep
      = case rr_args of
          [lev_ty] | rr_tc `hasKey` boxedRepDataConKey
                   , Just lev <- levityType_maybe lev_ty
                -> case lev of
                      Lifted   -> Just (text "a lifted")
                      Unlifted -> Just (text "a boxed unlifted")
          [] | rr_tc `hasKey` tupleRepDataConTyConKey -> Just (text "a zero-bit")
             | starts_with_vowel rr_occ -> Just (text "an" <+> text rr_occ)
             | otherwise                -> Just (text "a"  <+> text rr_occ)
             where
               rr_occ = occNameString (getOccName rr_tc)

          _ -> Nothing -- Must be TupleRep [r1..rn]
      | otherwise = Nothing

    starts_with_vowel (c:_) = c `elem` "AEIOU"
    starts_with_vowel []    = False

pprMismatchMsg ctxt (CouldNotDeduce useful_givens (item :| others) mb_extra)
  = main_msg $$
     case supplementary of
      Left infos
        -> vcat (map (pprExpectedActualInfo ctxt) infos)
      Right other_msg
        -> pprMismatchMsg ctxt other_msg
  where
    main_msg
      | null useful_givens
      = addArising ct_loc (no_instance_msg <+> missing)
      | otherwise
      = vcat (addArising ct_loc (no_deduce_msg <+> missing)
              : pp_givens useful_givens)

    supplementary = case mb_extra of
      Nothing
        -> Left []
      Just (CND_Extra level ty1 ty2)
        -> mk_supplementary_ea_msg ctxt level ty1 ty2 orig
    ct_loc = errorItemCtLoc item
    orig   = ctLocOrigin ct_loc
    wanteds = map errorItemPred (item:others)

    no_instance_msg =
      case wanteds of
        [wanted] | Just (tc, _) <- splitTyConApp_maybe wanted
                 -- Don't say "no instance" for a constraint such as "c" for a type variable c.
                 , isClassTyCon tc -> text "No instance for"
        _ -> text "Could not solve:"

    no_deduce_msg =
      case wanteds of
        [_wanted] -> text "Could not deduce"
        _         -> text "Could not deduce:"

    missing =
      case wanteds of
        [wanted] -> quotes (ppr wanted)
        _        -> pprTheta wanteds



{- *********************************************************************
*                                                                      *
                 Displaying potential instances
*                                                                      *
**********************************************************************-}

-- | Directly display the given matching and unifying instances,
-- with a header for each: `Matching instances`/`Potentially matching instances`.
pprPotentialInstances :: (ClsInst -> SDoc) -> PotentialInstances -> SDoc
pprPotentialInstances ppr_inst (PotentialInstances { matches, unifiers }) =
  vcat
    [ ppWhen (not $ null matches) $
       text "Matching instance" <> plural matches <> colon $$
         nest 2 (vcat (map ppr_inst matches))
    , ppWhen (not $ null unifiers) $
        (text "Potentially matching instance" <> plural unifiers <> colon) $$
         nest 2 (vcat (map ppr_inst unifiers))
    ]

-- | Display a summary of available instances, omitting those involving
-- out-of-scope types, in order to explain why we couldn't solve a particular
-- constraint, e.g. due to instance overlap or out-of-scope types.
--
-- To directly display a collection of matching/unifying instances,
-- use 'pprPotentialInstances'.
potentialInstancesErrMsg :: PotentialInstances -> SDoc
-- See Note [Displaying potential instances]
potentialInstancesErrMsg potentials =
  sdocOption sdocPrintPotentialInstances $ \print_insts ->
  getPprStyle $ \sty ->
    potentials_msg_with_options potentials print_insts sty

-- | Display a summary of available instances, omitting out-of-scope ones.
--
-- Use 'potentialInstancesErrMsg' to automatically set the pretty-printing
-- options.
potentials_msg_with_options :: PotentialInstances
                            -> Bool -- ^ Whether to print /all/ potential instances
                            -> PprStyle
                            -> SDoc
potentials_msg_with_options
  (PotentialInstances { matches, unifiers })
  show_all_potentials sty
  | null matches && null unifiers
  = empty

  | null show_these_matches && null show_these_unifiers
  = vcat [ not_in_scope_msg empty
         , flag_hint ]

  | otherwise
  = vcat [ pprPotentialInstances
            pprInstance -- print instance + location info
            (PotentialInstances
              { matches  = show_these_matches
              , unifiers = show_these_unifiers })
         , overlapping_but_not_more_specific_msg sorted_matches
         , nest 2 $ vcat
           [ ppWhen (n_in_scope_hidden > 0) $
             text "...plus"
               <+> speakNOf n_in_scope_hidden (text "other")
           , ppWhen (not_in_scopes > 0) $
              not_in_scope_msg (text "...plus")
           , flag_hint ] ]
  where
    n_show_matches, n_show_unifiers :: Int
    n_show_matches  = 3
    n_show_unifiers = 2

    (in_scope_matches, not_in_scope_matches) = partition inst_in_scope matches
    (in_scope_unifiers, not_in_scope_unifiers) = partition inst_in_scope unifiers
    sorted_matches = sortBy fuzzyClsInstCmp in_scope_matches
    sorted_unifiers = sortBy fuzzyClsInstCmp in_scope_unifiers
    (show_these_matches, show_these_unifiers)
       | show_all_potentials = (sorted_matches, sorted_unifiers)
       | otherwise           = (take n_show_matches  sorted_matches
                               ,take n_show_unifiers sorted_unifiers)
    n_in_scope_hidden
      = length sorted_matches + length sorted_unifiers
      - length show_these_matches - length show_these_unifiers

       -- "in scope" means that all the type constructors
       -- are lexically in scope; these instances are likely
       -- to be more useful
    inst_in_scope :: ClsInst -> Bool
    inst_in_scope cls_inst = nameSetAll name_in_scope $
                             orphNamesOfTypes (is_tys cls_inst)

    name_in_scope name
      | pretendNameIsInScope name
      = True -- E.g. (->); see Note [pretendNameIsInScope] in GHC.Builtin.Names
      | Just mod <- nameModule_maybe name
      = qual_in_scope (qualName sty mod (nameOccName name))
      | otherwise
      = True

    qual_in_scope :: QualifyName -> Bool
    qual_in_scope NameUnqual    = True
    qual_in_scope (NameQual {}) = True
    qual_in_scope _             = False

    not_in_scopes :: Int
    not_in_scopes = length not_in_scope_matches + length not_in_scope_unifiers

    not_in_scope_msg herald =
      hang (herald <+> speakNOf not_in_scopes (text "instance")
                     <+> text "involving out-of-scope types")
           2 (ppWhen show_all_potentials $
               pprPotentialInstances
               pprInstanceHdr -- only print the header, not the instance location info
                 (PotentialInstances
                   { matches = not_in_scope_matches
                   , unifiers = not_in_scope_unifiers
                   }))

    flag_hint = ppUnless (show_all_potentials
                         || (equalLength show_these_matches matches
                             && equalLength show_these_unifiers unifiers)) $
                text "(use -fprint-potential-instances to see them all)"

-- | Compute a message informing the user of any instances that are overlapped
-- but were not discarded because the instance overlapping them wasn't
-- strictly more specific.
overlapping_but_not_more_specific_msg :: [ClsInst] -> SDoc
overlapping_but_not_more_specific_msg insts
  -- Only print one example of "overlapping but not strictly more specific",
  -- to avoid information overload.
  | overlap : _ <- overlapping_but_not_more_specific
  = overlap_header $$ ppr_overlapping overlap
  | otherwise
  = empty
    where
      overlap_header :: SDoc
      overlap_header
        | [_] <- overlapping_but_not_more_specific
        = text "An overlapping instance can only be chosen when it is strictly more specific."
        | otherwise
        = text "Overlapping instances can only be chosen when they are strictly more specific."
      overlapping_but_not_more_specific :: [(ClsInst, ClsInst)]
      overlapping_but_not_more_specific
        = nubOrdBy (comparing (is_dfun . fst))
          [ (overlapper, overlappee)
          | these <- groupBy ((==) `on` is_cls_nm) insts
          -- Take all pairs of distinct instances...
          , one:others <- tails these -- if `these = [inst_1, inst_2, ...]`
          , other <- others           -- then we get pairs `(one, other) = (inst_i, inst_j)` with `i < j`
          -- ... such that one instance in the pair overlaps the other...
          , let mb_overlapping
                  | hasOverlappingFlag (overlapMode $ is_flag one)
                  || hasOverlappableFlag (overlapMode $ is_flag other)
                  = [(one, other)]
                  | hasOverlappingFlag (overlapMode $ is_flag other)
                  || hasOverlappableFlag (overlapMode $ is_flag one)
                  = [(other, one)]
                  | otherwise
                  = []
          , (overlapper, overlappee) <- mb_overlapping
          -- ... but the overlapper is not more specific than the overlappee.
          , not (overlapper `more_specific_than` overlappee)
          ]
      more_specific_than :: ClsInst -> ClsInst -> Bool
      is1 `more_specific_than` is2
        = isJust (tcMatchTys (is_tys is1) (is_tys is2))
      ppr_overlapping :: (ClsInst, ClsInst) -> SDoc
      ppr_overlapping (overlapper, overlappee)
        = text "The first instance that follows overlaps the second, but is not more specific than it:"
        $$ nest 2 (vcat $ map pprInstanceHdr [overlapper, overlappee])

{- Note [Displaying potential instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When showing a list of instances for
  - overlapping instances (show ones that match)
  - no such instance (show ones that could match)
we want to give it a bit of structure.  Here's the plan

* Say that an instance is "in scope" if all of the
  type constructors it mentions are lexically in scope.
  These are the ones most likely to be useful to the programmer.

* Show at most n_show in-scope instances,
  and summarise the rest ("plus N others")

* Summarise the not-in-scope instances ("plus 4 not in scope")

* Add the flag -fshow-potential-instances which replaces the
  summary with the full list
-}

{- *********************************************************************
*                                                                      *
             Outputting additional solver report information
*                                                                      *
**********************************************************************-}

-- | Pretty-print an informational message, to accompany a 'TcSolverReportMsg'.
pprExpectedActualInfo :: SolverReportErrCtxt -> ExpectedActualInfo -> SDoc
pprExpectedActualInfo _ (ExpectedActual { ea_expected = exp, ea_actual = act }) =
  vcat
    [ text "Expected:" <+> ppr exp
    , text "  Actual:" <+> ppr act ]
pprExpectedActualInfo _
  (ExpectedActualAfterTySynExpansion
    { ea_expanded_expected = exp
    , ea_expanded_actual   = act } )
  = vcat
      [ text "Type synonyms expanded:"
      , text "Expected type:" <+> ppr exp
      , text "  Actual type:" <+> ppr act ]

pprCoercibleMsg :: CoercibleMsg -> SDoc
pprCoercibleMsg (UnknownRoles ty) =
  hang (text "NB: We cannot know what roles the parameters to" <+>
          quotes (ppr ty) <+> text "have;")
       2 (text "we must assume that the role is nominal")
pprCoercibleMsg (TyConIsAbstract tc) =
  hsep [ text "NB: The type constructor"
       , quotes (pprSourceTyCon tc)
       , text "is abstract" ]
pprCoercibleMsg (OutOfScopeNewtypeConstructor tc dc) =
  hang (text "The data constructor" <+> quotes (ppr $ dataConName dc))
    2 (sep [ text "of newtype" <+> quotes (pprSourceTyCon tc)
           , text "is not in scope" ])

pprWhenMatching :: SolverReportErrCtxt -> WhenMatching -> SDoc
pprWhenMatching ctxt (WhenMatching cty1 cty2 sub_o mb_sub_t_or_k) =
  sdocOption sdocPrintExplicitCoercions $ \printExplicitCoercions ->
    if printExplicitCoercions
       || not (cty1 `pickyEqType` cty2)
      then vcat [ hang (text "When matching" <+> sub_whats)
                      2 (vcat [ ppr cty1 <+> dcolon <+>
                               ppr (typeKind cty1)
                             , ppr cty2 <+> dcolon <+>
                               ppr (typeKind cty2) ])
                , supplementary ]
      else text "When matching the kind of" <+> quotes (ppr cty1)
  where
    sub_t_or_k = mb_sub_t_or_k `orElse` TypeLevel
    sub_whats  = text (levelString sub_t_or_k) <> char 's'
    supplementary =
      case mk_supplementary_ea_msg ctxt sub_t_or_k cty1 cty2 sub_o of
        Left infos -> vcat $ map (pprExpectedActualInfo ctxt) infos
        Right msg  -> pprMismatchMsg ctxt msg

pprTyVarInfo :: SolverReportErrCtxt -> TyVarInfo -> SDoc
pprTyVarInfo ctxt (TyVarInfo { thisTyVar = tv1, otherTy = mb_tv2 }) =
  mk_msg tv1 $$ case mb_tv2 of { Nothing -> empty; Just tv2 -> mk_msg tv2 }
  where
    mk_msg tv = case tcTyVarDetails tv of
      SkolemTv sk_info _ _ -> pprSkols ctxt [(getSkolemInfo sk_info, [tv])]
      RuntimeUnk {} -> quotes (ppr tv) <+> text "is an interactive-debugger skolem"
      MetaTv {}     -> empty

pprAmbiguityInfo :: AmbiguityInfo -> SDoc
pprAmbiguityInfo (Ambiguity prepend_msg (ambig_kvs, ambig_tvs)) = msg
  where

    msg |  any isRuntimeUnkSkol ambig_kvs  -- See Note [Runtime skolems]
        || any isRuntimeUnkSkol ambig_tvs
        = vcat [ text "Cannot resolve unknown runtime type"
                 <> plural ambig_tvs <+> pprQuotedList ambig_tvs
               , text "Use :print or :force to determine these types"]

        | not (null ambig_tvs)
        = pp_ambig (text "type") ambig_tvs

        | otherwise
        = pp_ambig (text "kind") ambig_kvs

    pp_ambig what tkvs
      | prepend_msg -- "Ambiguous type variable 't0'"
      = text "Ambiguous" <+> what <+> text "variable"
        <> plural tkvs <+> pprQuotedList tkvs

      | otherwise -- "The type variable 't0' is ambiguous"
      = text "The" <+> what <+> text "variable" <> plural tkvs
        <+> pprQuotedList tkvs <+> isOrAre tkvs <+> text "ambiguous"
pprAmbiguityInfo (NonInjectiveTyFam tc) =
  text "NB:" <+> quotes (ppr tc)
  <+> text "is a non-injective type family"

pprSameOccInfo :: SameOccInfo -> SDoc
pprSameOccInfo (SameOcc same_pkg n1 n2) =
  text "NB:" <+> (ppr_from same_pkg n1 $$ ppr_from same_pkg n2)
  where
    ppr_from same_pkg nm
      | isGoodSrcSpan loc
      = hang (quotes (ppr nm) <+> text "is defined at")
           2 (ppr loc)
      | otherwise  -- Imported things have an UnhelpfulSrcSpan
      = hang (quotes (ppr nm))
           2 (sep [ text "is defined in" <+> quotes (ppr (moduleName mod))
                  , ppUnless (same_pkg || pkg == mainUnit) $
                    nest 4 $ text "in package" <+> quotes (ppr pkg) ])
      where
        pkg = moduleUnit mod
        mod = nameModule nm
        loc = nameSrcSpan nm

{- *********************************************************************
*                                                                      *
                  Outputting HoleError messages
*                                                                      *
**********************************************************************-}

pprHoleError :: SolverReportErrCtxt -> Hole -> HoleError -> SDoc
pprHoleError _ (Hole { hole_ty, hole_occ = rdr }) (OutOfScopeHole imp_errs)
  = out_of_scope_msg $$ vcat (map ppr imp_errs)
  where
    herald | isDataOcc (rdrNameOcc rdr) = text "Data constructor not in scope:"
           | otherwise     = text "Variable not in scope:"
    out_of_scope_msg -- Print v :: ty only if the type has structure
      | boring_type = hang herald 2 (ppr rdr)
      | otherwise   = hang herald 2 (pp_rdr_with_type rdr hole_ty)
    boring_type = isTyVarTy hole_ty
pprHoleError ctxt (Hole { hole_ty, hole_occ}) (HoleError sort other_tvs hole_skol_info) =
  vcat [ hole_msg
       , tyvars_msg
       , case sort of { ExprHole {} -> expr_hole_hint; _ -> type_hole_hint } ]

  where

    hole_msg = case sort of
      ExprHole {} ->
        hang (text "Found hole:")
          2 (pp_rdr_with_type hole_occ hole_ty)
      TypeHole ->
        hang (text "Found type wildcard" <+> quotes (ppr hole_occ))
          2 (text "standing for" <+> quotes pp_hole_type_with_kind)
      ConstraintHole ->
        hang (text "Found extra-constraints wildcard standing for")
          2 (quotes $ pprType hole_ty)  -- always kind Constraint

    hole_kind = typeKind hole_ty

    pp_hole_type_with_kind
      | isLiftedTypeKind hole_kind
        || isCoVarType hole_ty -- Don't print the kind of unlifted
                               -- equalities (#15039)
      = pprType hole_ty
      | otherwise
      = pprType hole_ty <+> dcolon <+> pprKind hole_kind

    tyvars = tyCoVarsOfTypeList hole_ty
    tyvars_msg = ppUnless (null tyvars) $
                 text "Where:" <+> (vcat (map loc_msg other_tvs)
                                    $$ pprSkols ctxt hole_skol_info)
                      -- Coercion variables can be free in the
                      -- hole, via kind casts
    expr_hole_hint                       -- Give hint for, say,   f x = _x
         | lengthFS (occNameFS (rdrNameOcc hole_occ)) > 1  -- Don't give this hint for plain "_"
         = text "Or perhaps" <+> quotes (ppr hole_occ)
           <+> text "is mis-spelled, or not in scope"
         | otherwise
         = empty

    type_hole_hint
         | ErrorWithoutFlag <- cec_type_holes ctxt
         = text "To use the inferred type, enable PartialTypeSignatures"
         | otherwise
         = empty

    loc_msg tv
       | isTyVar tv
       = case tcTyVarDetails tv of
           MetaTv {} -> quotes (ppr tv) <+> text "is an ambiguous type variable"
           _         -> empty  -- Skolems dealt with already
       | otherwise  -- A coercion variable can be free in the hole type
       = ppWhenOption sdocPrintExplicitCoercions $
           quotes (ppr tv) <+> text "is a coercion variable"

pp_rdr_with_type :: RdrName -> Type -> SDoc
pp_rdr_with_type occ hole_ty = hang (pprPrefixOcc occ) 2 (dcolon <+> pprType hole_ty)

{- *********************************************************************
*                                                                      *
                  Outputting ScopeError messages
*                                                                      *
**********************************************************************-}

pprScopeError :: RdrName -> NotInScopeError -> SDoc
pprScopeError rdr_name scope_err =
  case scope_err of
    NotInScope {} ->
      hang (text "Not in scope:")
        2 (what <+> quotes (ppr rdr_name))
    NotARecordField {} ->
      hang (text "Not in scope:")
        2 (text "record field" <+> quotes (ppr rdr_name))
    NoExactName name ->
      text "The Name" <+> quotes (ppr name) <+> text "is not in scope."
    SameName gres ->
      assertPpr (length gres >= 2) (text "pprScopeError SameName: fewer than 2 elements" $$ nest 2 (ppr gres))
      $ hang (text "Same Name in multiple name-spaces:")
           2 (vcat (map pp_one sorted_names))
      where
        sorted_names = sortBy (leftmost_smallest `on` nameSrcSpan)
                     $ map greName gres
        pp_one name
          = hang (pprNameSpace (occNameSpace (getOccName name))
                  <+> quotes (ppr name) <> comma)
               2 (text "declared at:" <+> ppr (nameSrcLoc name))
    MissingBinding thing _ ->
      sep [ text "The" <+> thing
               <+> text "for" <+> quotes (ppr rdr_name)
          , nest 2 $ text "lacks an accompanying binding" ]
    NoTopLevelBinding ->
      hang (text "No top-level binding for")
        2 (what <+> quotes (ppr rdr_name) <+> text "in this module")
    UnknownSubordinate doc ->
      quotes (ppr rdr_name) <+> text "is not a (visible)" <+> doc
    NotInScopeTc env ->
      vcat[text "GHC internal error:" <+> quotes (ppr rdr_name) <+>
      text "is not in scope during type checking, but it passed the renamer",
      text "tcl_env of environment:" <+> ppr env]
  where
    what = pprNonVarNameSpace (occNameSpace (rdrNameOcc rdr_name))

scopeErrorHints :: NotInScopeError -> [GhcHint]
scopeErrorHints scope_err =
  case scope_err of
    NotInScope             -> noHints
    NotARecordField        -> noHints
    NoExactName {}         -> [SuggestDumpSlices]
    SameName {}            -> [SuggestDumpSlices]
    MissingBinding _ hints -> hints
    NoTopLevelBinding      -> noHints
    UnknownSubordinate {}  -> noHints
    NotInScopeTc _         -> noHints

{- *********************************************************************
*                                                                      *
                  Outputting ImportError messages
*                                                                      *
**********************************************************************-}

instance Outputable ImportError where
  ppr (MissingModule mod_name) =
    hsep
      [ text "NB: no module named"
      , quotes (ppr mod_name)
      , text "is imported."
      ]
  ppr  (ModulesDoNotExport mods occ_name)
    | mod NE.:| [] <- mods
    = hsep
        [ text "NB: the module"
        , quotes (ppr mod)
        , text "does not export"
        , quotes (ppr occ_name) <> dot ]
    | otherwise
    = hsep
        [ text "NB: neither"
        , quotedListWithNor (map ppr $ NE.toList mods)
        , text "export"
        , quotes (ppr occ_name) <> dot ]

{- *********************************************************************
*                                                                      *
             Suggested fixes for implication constraints
*                                                                      *
**********************************************************************-}

-- TODO: these functions should use GhcHint instead.

show_fixes :: [SDoc] -> SDoc
show_fixes []     = empty
show_fixes (f:fs) = sep [ text "Possible fix:"
                        , nest 2 (vcat (f : map (text "or" <+>) fs))]

ctxtFixes :: Bool -> PredType -> [Implication] -> [SDoc]
ctxtFixes has_ambig_tvs pred implics
  | not has_ambig_tvs
  , isTyVarClassPred pred   -- Don't suggest adding (Eq T) to the context, say
  , (skol:skols) <- usefulContext implics pred
  , let what | null skols
             , SigSkol (PatSynCtxt {}) _ _ <- skol
             = text "\"required\""
             | otherwise
             = empty
  = [sep [ text "add" <+> pprParendType pred
           <+> text "to the" <+> what <+> text "context of"
         , nest 2 $ ppr_skol skol $$
                    vcat [ text "or" <+> ppr_skol skol
                         | skol <- skols ] ] ]
  | otherwise = []
  where
    ppr_skol (PatSkol (RealDataCon dc) _) = text "the data constructor" <+> quotes (ppr dc)
    ppr_skol (PatSkol (PatSynCon ps)   _) = text "the pattern synonym"  <+> quotes (ppr ps)
    ppr_skol skol_info = ppr skol_info

usefulContext :: [Implication] -> PredType -> [SkolemInfoAnon]
-- usefulContext picks out the implications whose context
-- the programmer might plausibly augment to solve 'pred'
usefulContext implics pred
  = go implics
  where
    pred_tvs = tyCoVarsOfType pred
    go [] = []
    go (ic : ics)
       | implausible ic = rest
       | otherwise      = ic_info ic : rest
       where
          -- Stop when the context binds a variable free in the predicate
          rest | any (`elemVarSet` pred_tvs) (ic_skols ic) = []
               | otherwise                                 = go ics

    implausible ic
      | null (ic_skols ic)            = True
      | implausible_info (ic_info ic) = True
      | otherwise                     = False

    implausible_info (SigSkol (InfSigCtxt {}) _ _) = True
    implausible_info _                             = False
    -- Do not suggest adding constraints to an *inferred* type signature

pp_givens :: [Implication] -> [SDoc]
pp_givens givens
   = case givens of
         []     -> []
         (g:gs) ->      ppr_given (text "from the context:") g
                 : map (ppr_given (text "or from:")) gs
    where
       ppr_given herald implic@(Implic { ic_given = gs, ic_info = skol_info })
           = hang (herald <+> pprEvVarTheta (mkMinimalBySCs evVarPred gs))
             -- See Note [Suppress redundant givens during error reporting]
             -- for why we use mkMinimalBySCs above.
                2 (sep [ text "bound by" <+> ppr skol_info
                       , text "at" <+> ppr (getLclEnvLoc (ic_env implic)) ])

{- *********************************************************************
*                                                                      *
                       CtOrigin information
*                                                                      *
**********************************************************************-}

levelString :: TypeOrKind -> String
levelString TypeLevel = "type"
levelString KindLevel = "kind"

pprArising :: CtLoc -> SDoc
-- Used for the main, top-level error message
-- We've done special processing for TypeEq, KindEq, givens
pprArising ct_loc
  | in_generated_code = empty  -- See Note ["Arising from" messages in generated code]
  | suppress_origin   = empty
  | otherwise         = pprCtOrigin orig
  where
    orig = ctLocOrigin ct_loc
    in_generated_code = lclEnvInGeneratedCode (ctLocEnv ct_loc)
    suppress_origin
      | isGivenOrigin orig = True
      | otherwise          = case orig of
          TypeEqOrigin {}         -> True -- We've done special processing
          KindEqOrigin {}         -> True -- for TypeEq, KindEq, givens
          AmbiguityCheckOrigin {} -> True -- The "In the ambiguity check" context
                                          -- is sufficient; more would be repetitive
          _ -> False

-- Add the "arising from..." part to a message
addArising :: CtLoc -> SDoc -> SDoc
addArising ct_loc msg = hang msg 2 (pprArising ct_loc)

pprWithArising :: [Ct] -> SDoc
-- Print something like
--    (Eq a) arising from a use of x at y
--    (Show a) arising from a use of p at q
-- Also return a location for the error message
-- Works for Wanted/Derived only
pprWithArising []
  = panic "pprWithArising"
pprWithArising (ct:cts)
  | null cts
  = addArising loc (pprTheta [ctPred ct])
  | otherwise
  = vcat (map ppr_one (ct:cts))
  where
    loc = ctLoc ct
    ppr_one ct' = hang (parens (pprType (ctPred ct')))
                     2 (pprCtLoc (ctLoc ct'))

{- Note ["Arising from" messages in generated code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider code generated when we desugar code before typechecking;
see Note [Rebindable syntax and HsExpansion].

In this code, constraints may be generated, but we don't want to
say "arising from a call of foo" if 'foo' doesn't appear in the
users code.  We leave the actual CtOrigin untouched (partly because
it is generated in many, many places), but suppress the "Arising from"
message for constraints that originate in generated code.
-}


{- *********************************************************************
*                                                                      *
                           SkolemInfo
*                                                                      *
**********************************************************************-}


tidySkolemInfo :: TidyEnv -> SkolemInfo -> SkolemInfo
tidySkolemInfo env (SkolemInfo u sk_anon) = SkolemInfo u (tidySkolemInfoAnon env sk_anon)

----------------
tidySkolemInfoAnon :: TidyEnv -> SkolemInfoAnon -> SkolemInfoAnon
tidySkolemInfoAnon env (DerivSkol ty)         = DerivSkol (tidyType env ty)
tidySkolemInfoAnon env (SigSkol cx ty tv_prs) = tidySigSkol env cx ty tv_prs
tidySkolemInfoAnon env (InferSkol ids)        = InferSkol (mapSnd (tidyType env) ids)
tidySkolemInfoAnon env (UnifyForAllSkol ty)   = UnifyForAllSkol (tidyType env ty)
tidySkolemInfoAnon _   info                   = info

tidySigSkol :: TidyEnv -> UserTypeCtxt
            -> TcType -> [(Name,TcTyVar)] -> SkolemInfoAnon
-- We need to take special care when tidying SigSkol
-- See Note [SigSkol SkolemInfo] in "GHC.Tc.Types.Origin"
tidySigSkol env cx ty tv_prs
  = SigSkol cx (tidy_ty env ty) tv_prs'
  where
    tv_prs' = mapSnd (tidyTyCoVarOcc env) tv_prs
    inst_env = mkNameEnv tv_prs'

    tidy_ty env (ForAllTy (Bndr tv vis) ty)
      = ForAllTy (Bndr tv' vis) (tidy_ty env' ty)
      where
        (env', tv') = tidy_tv_bndr env tv

    tidy_ty env ty@(FunTy af w arg res) -- Look under  c => t
      | isInvisibleFunArg af
      = ty { ft_mult = tidy_ty env w
           , ft_arg  = tidyType env arg
           , ft_res  = tidy_ty env res }

    tidy_ty env ty = tidyType env ty

    tidy_tv_bndr :: TidyEnv -> TyCoVar -> (TidyEnv, TyCoVar)
    tidy_tv_bndr env@(occ_env, subst) tv
      | Just tv' <- lookupNameEnv inst_env (tyVarName tv)
      = ((occ_env, extendVarEnv subst tv tv'), tv')

      | otherwise
      = tidyVarBndr env tv

pprSkols :: SolverReportErrCtxt -> [(SkolemInfoAnon, [TcTyVar])] -> SDoc
pprSkols ctxt zonked_ty_vars
  =
      let tidy_ty_vars = map (bimap (tidySkolemInfoAnon (cec_tidy ctxt)) id) zonked_ty_vars
      in vcat (map pp_one tidy_ty_vars)
  where

    no_msg = text "No skolem info - we could not find the origin of the following variables" <+> ppr zonked_ty_vars
       $$ text "This should not happen, please report it as a bug following the instructions at:"
       $$ text "https://gitlab.haskell.org/ghc/ghc/wikis/report-a-bug"


    pp_one (UnkSkol cs, tvs)
      = vcat [ hang (pprQuotedList tvs)
                 2 (is_or_are tvs "a" "(rigid, skolem)")
             , nest 2 (text "of unknown origin")
             , nest 2 (text "bound at" <+> ppr (skolsSpan tvs))
             , no_msg
             , prettyCallStackDoc cs
             ]
    pp_one (RuntimeUnkSkol, tvs)
      = hang (pprQuotedList tvs)
           2 (is_or_are tvs "an" "unknown runtime")
    pp_one (skol_info, tvs)
      = vcat [ hang (pprQuotedList tvs)
                  2 (is_or_are tvs "a"  "rigid" <+> text "bound by")
             , nest 2 (pprSkolInfo skol_info)
             , nest 2 (text "at" <+> ppr (skolsSpan tvs)) ]

    is_or_are [_] article adjective = text "is" <+> text article <+> text adjective
                                      <+> text "type variable"
    is_or_are _   _       adjective = text "are" <+> text adjective
                                      <+> text "type variables"

skolsSpan :: [TcTyVar] -> SrcSpan
skolsSpan skol_tvs = foldr1 combineSrcSpans (map getSrcSpan skol_tvs)

{- *********************************************************************
*                                                                      *
                Utilities for expected/actual messages
*                                                                      *
**********************************************************************-}

mk_supplementary_ea_msg :: SolverReportErrCtxt -> TypeOrKind
                        -> Type -> Type -> CtOrigin -> Either [ExpectedActualInfo] MismatchMsg
mk_supplementary_ea_msg ctxt level ty1 ty2 orig
  | TypeEqOrigin { uo_expected = exp, uo_actual = act } <- orig
  , not (ea_looks_same ty1 ty2 exp act)
  = mk_ea_msg ctxt Nothing level orig
  | otherwise
  = Left []

ea_looks_same :: Type -> Type -> Type -> Type -> Bool
-- True if the faulting types (ty1, ty2) look the same as
-- the expected/actual types (exp, act).
-- If so, we don't want to redundantly report the latter
ea_looks_same ty1 ty2 exp act
  = (act `looks_same` ty1 && exp `looks_same` ty2) ||
    (exp `looks_same` ty1 && act `looks_same` ty2)
  where
    looks_same t1 t2 = t1 `pickyEqType` t2
                    || t1 `eqType` liftedTypeKind && t2 `eqType` liftedTypeKind
      -- pickyEqType is sensitive to synonyms, so only replies True
      -- when the types really look the same.  However,
      -- (TYPE 'LiftedRep) and Type both print the same way.

mk_ea_msg :: SolverReportErrCtxt -> Maybe ErrorItem -> TypeOrKind
          -> CtOrigin -> Either [ExpectedActualInfo] MismatchMsg
-- Constructs a "Couldn't match" message
-- The (Maybe ErrorItem) says whether this is the main top-level message (Just)
--     or a supplementary message (Nothing)
mk_ea_msg ctxt at_top level
  (TypeEqOrigin { uo_actual = act, uo_expected = exp, uo_thing = mb_thing })
  | Just thing <- mb_thing
  , KindLevel <- level
  = Right $ KindMismatch { kmismatch_what     = thing
                         , kmismatch_expected = exp
                         , kmismatch_actual   = act }
  | Just item <- at_top
  , let  ea = EA $ if expanded_syns then Just ea_expanded else Nothing
         mismatch = mkBasicMismatchMsg ea item exp act
  = Right mismatch
  | otherwise
  = Left $
    if expanded_syns
    then [ea,ea_expanded]
    else [ea]

  where
    ea = ExpectedActual { ea_expected = exp, ea_actual = act }
    ea_expanded =
      ExpectedActualAfterTySynExpansion
        { ea_expanded_expected = expTy1
        , ea_expanded_actual   = expTy2 }

    expanded_syns = cec_expand_syns ctxt
                 && not (expTy1 `pickyEqType` exp && expTy2 `pickyEqType` act)
    (expTy1, expTy2) = expandSynonymsToMatch exp act
mk_ea_msg _ _ _ _ = Left []

{- Note [Expanding type synonyms to make types similar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In type error messages, if -fprint-expanded-types is used, we want to expand
type synonyms to make expected and found types as similar as possible, but we
shouldn't expand types too much to make type messages even more verbose and
harder to understand. The whole point here is to make the difference in expected
and found types clearer.

`expandSynonymsToMatch` does this, it takes two types, and expands type synonyms
only as much as necessary. Given two types t1 and t2:

  * If they're already same, it just returns the types.

  * If they're in form `C1 t1_1 .. t1_n` and `C2 t2_1 .. t2_m` (C1 and C2 are
    type constructors), it expands C1 and C2 if they're different type synonyms.
    Then it recursively does the same thing on expanded types. If C1 and C2 are
    same, then it applies the same procedure to arguments of C1 and arguments of
    C2 to make them as similar as possible.

    Most important thing here is to keep number of synonym expansions at
    minimum. For example, if t1 is `T (T3, T5, Int)` and t2 is `T (T5, T3,
    Bool)` where T5 = T4, T4 = T3, ..., T1 = X, it returns `T (T3, T3, Int)` and
    `T (T3, T3, Bool)`.

  * Otherwise types don't have same shapes and so the difference is clearly
    visible. It doesn't do any expansions and show these types.

Note that we only expand top-layer type synonyms. Only when top-layer
constructors are the same we start expanding inner type synonyms.

Suppose top-layer type synonyms of t1 and t2 can expand N and M times,
respectively. If their type-synonym-expanded forms will meet at some point (i.e.
will have same shapes according to `sameShapes` function), it's possible to find
where they meet in O(N+M) top-layer type synonym expansions and O(min(N,M))
comparisons. We first collect all the top-layer expansions of t1 and t2 in two
lists, then drop the prefix of the longer list so that they have same lengths.
Then we search through both lists in parallel, and return the first pair of
types that have same shapes. Inner types of these two types with same shapes
are then expanded using the same algorithm.

In case they don't meet, we return the last pair of types in the lists, which
has top-layer type synonyms completely expanded. (in this case the inner types
are not expanded at all, as the current form already shows the type error)
-}

-- | Expand type synonyms in given types only enough to make them as similar as
-- possible. Returned types are the same in terms of used type synonyms.
--
-- To expand all synonyms, see 'Type.expandTypeSynonyms'.
--
-- See `ExpandSynsFail` tests in tests testsuite/tests/typecheck/should_fail for
-- some examples of how this should work.
expandSynonymsToMatch :: Type -> Type -> (Type, Type)
expandSynonymsToMatch ty1 ty2 = (ty1_ret, ty2_ret)
  where
    (ty1_ret, ty2_ret) = go ty1 ty2

    -- Returns (type synonym expanded version of first type,
    --          type synonym expanded version of second type)
    go :: Type -> Type -> (Type, Type)
    go t1 t2
      | t1 `pickyEqType` t2 =
        -- Types are same, nothing to do
        (t1, t2)

    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2
      , tys1 `equalLength` tys2 =
        -- Type constructors are same. They may be synonyms, but we don't
        -- expand further. The lengths of tys1 and tys2 must be equal;
        -- for example, with type S a = a, we don't want
        -- to zip (S Monad Int) and (S Bool).
        let (tys1', tys2') =
              unzip (zipWithEqual "expandSynonymsToMatch" go tys1 tys2)
         in (TyConApp tc1 tys1', TyConApp tc2 tys2')

    go (AppTy t1_1 t1_2) (AppTy t2_1 t2_2) =
      let (t1_1', t2_1') = go t1_1 t2_1
          (t1_2', t2_2') = go t1_2 t2_2
       in (mkAppTy t1_1' t1_2', mkAppTy t2_1' t2_2')

    go ty1@(FunTy _ w1 t1_1 t1_2) ty2@(FunTy _ w2 t2_1 t2_2) | w1 `eqType` w2 =
      let (t1_1', t2_1') = go t1_1 t2_1
          (t1_2', t2_2') = go t1_2 t2_2
       in ( ty1 { ft_arg = t1_1', ft_res = t1_2' }
          , ty2 { ft_arg = t2_1', ft_res = t2_2' })

    go (ForAllTy b1 t1) (ForAllTy b2 t2) =
      -- NOTE: We may have a bug here, but we just can't reproduce it easily.
      -- See D1016 comments for details and our attempts at producing a test
      -- case. Short version: We probably need RnEnv2 to really get this right.
      let (t1', t2') = go t1 t2
       in (ForAllTy b1 t1', ForAllTy b2 t2')

    go (CastTy ty1 _) ty2 = go ty1 ty2
    go ty1 (CastTy ty2 _) = go ty1 ty2

    go t1 t2 =
      -- See Note [Expanding type synonyms to make types similar] for how this
      -- works
      let
        t1_exp_tys = t1 : tyExpansions t1
        t2_exp_tys = t2 : tyExpansions t2
        t1_exps    = length t1_exp_tys
        t2_exps    = length t2_exp_tys
        dif        = abs (t1_exps - t2_exps)
      in
        followExpansions $
          zipEqual "expandSynonymsToMatch.go"
            (if t1_exps > t2_exps then drop dif t1_exp_tys else t1_exp_tys)
            (if t2_exps > t1_exps then drop dif t2_exp_tys else t2_exp_tys)

    -- Expand the top layer type synonyms repeatedly, collect expansions in a
    -- list. The list does not include the original type.
    --
    -- Example, if you have:
    --
    --   type T10 = T9
    --   type T9  = T8
    --   ...
    --   type T0  = Int
    --
    -- `tyExpansions T10` returns [T9, T8, T7, ..., Int]
    --
    -- This only expands the top layer, so if you have:
    --
    --   type M a = Maybe a
    --
    -- `tyExpansions (M T10)` returns [Maybe T10] (T10 is not expanded)
    tyExpansions :: Type -> [Type]
    tyExpansions = unfoldr (\t -> (\x -> (x, x)) `fmap` coreView t)

    -- Drop the type pairs until types in a pair look alike (i.e. the outer
    -- constructors are the same).
    followExpansions :: [(Type, Type)] -> (Type, Type)
    followExpansions [] = pprPanic "followExpansions" empty
    followExpansions [(t1, t2)]
      | sameShapes t1 t2 = go t1 t2 -- expand subtrees
      | otherwise        = (t1, t2) -- the difference is already visible
    followExpansions ((t1, t2) : tss)
      -- Traverse subtrees when the outer shapes are the same
      | sameShapes t1 t2 = go t1 t2
      -- Otherwise follow the expansions until they look alike
      | otherwise = followExpansions tss

    sameShapes :: Type -> Type -> Bool
    sameShapes AppTy{}          AppTy{}          = True
    sameShapes (TyConApp tc1 _) (TyConApp tc2 _) = tc1 == tc2
    sameShapes (FunTy {})       (FunTy {})       = True
    sameShapes (ForAllTy {})    (ForAllTy {})    = True
    sameShapes (CastTy ty1 _)   ty2              = sameShapes ty1 ty2
    sameShapes ty1              (CastTy ty2 _)   = sameShapes ty1 ty2
    sameShapes _                _                = False

{-
************************************************************************
*                                                                      *
\subsection{Contexts for renaming errors}
*                                                                      *
************************************************************************
-}

inHsDocContext :: HsDocContext -> SDoc
inHsDocContext ctxt = text "In" <+> pprHsDocContext ctxt

pprHsDocContext :: HsDocContext -> SDoc
pprHsDocContext (GenericCtx doc)      = doc
pprHsDocContext (TypeSigCtx doc)      = text "the type signature for" <+> doc
pprHsDocContext (StandaloneKindSigCtx doc) = text "the standalone kind signature for" <+> doc
pprHsDocContext PatCtx                = text "a pattern type-signature"
pprHsDocContext SpecInstSigCtx        = text "a SPECIALISE instance pragma"
pprHsDocContext DefaultDeclCtx        = text "a `default' declaration"
pprHsDocContext DerivDeclCtx          = text "a deriving declaration"
pprHsDocContext (RuleCtx name)        = text "the rewrite rule" <+> doubleQuotes (ftext name)
pprHsDocContext (TyDataCtx tycon)     = text "the data type declaration for" <+> quotes (ppr tycon)
pprHsDocContext (FamPatCtx tycon)     = text "a type pattern of family instance for" <+> quotes (ppr tycon)
pprHsDocContext (TySynCtx name)       = text "the declaration for type synonym" <+> quotes (ppr name)
pprHsDocContext (TyFamilyCtx name)    = text "the declaration for type family" <+> quotes (ppr name)
pprHsDocContext (ClassDeclCtx name)   = text "the declaration for class" <+> quotes (ppr name)
pprHsDocContext ExprWithTySigCtx      = text "an expression type signature"
pprHsDocContext TypBrCtx              = text "a Template-Haskell quoted type"
pprHsDocContext HsTypeCtx             = text "a type argument"
pprHsDocContext HsTypePatCtx          = text "a type argument in a pattern"
pprHsDocContext GHCiCtx               = text "GHCi input"
pprHsDocContext (SpliceTypeCtx hs_ty) = text "the spliced type" <+> quotes (ppr hs_ty)
pprHsDocContext ClassInstanceCtx      = text "GHC.Tc.Gen.Splice.reifyInstances"

pprHsDocContext (ForeignDeclCtx name)
   = text "the foreign declaration for" <+> quotes (ppr name)
pprHsDocContext (ConDeclCtx [name])
   = text "the definition of data constructor" <+> quotes (ppr name)
pprHsDocContext (ConDeclCtx names)
   = text "the definition of data constructors" <+> interpp'SP names

pprConversionFailReason :: ConversionFailReason -> SDoc
pprConversionFailReason = \case
  IllegalOccName ctxt_ns occ ->
    text "Illegal" <+> pprNameSpace ctxt_ns
    <+> text "name:" <+> quotes (text occ)
  SumAltArityExceeded alt arity ->
    text "Sum alternative" <+> int alt
    <+> text "exceeds its arity," <+> int arity
  IllegalSumAlt alt ->
    vcat [ text "Illegal sum alternative:" <+> int alt
         , nest 2 $ text "Sum alternatives must start from 1" ]
  IllegalSumArity arity ->
    vcat [ text "Illegal sum arity:" <+> int arity
         , nest 2 $ text "Sums must have an arity of at least 2" ]
  MalformedType typeOrKind ty ->
    text "Malformed " <> text ty_str <+> text (show ty)
    where ty_str = case typeOrKind of
                     TypeLevel -> "type"
                     KindLevel -> "kind"
  IllegalLastStatement do_or_lc stmt ->
    vcat [ text "Illegal last statement of" <+> pprAHsDoFlavour do_or_lc <> colon
         , nest 2 $ ppr stmt
         , text "(It should be an expression.)" ]
  KindSigsOnlyAllowedOnGADTs ->
    text "Kind signatures are only allowed on GADTs"
  IllegalDeclaration declDescr bad_decls ->
    sep [ text "Illegal" <+> what <+> text "in" <+> descrDoc <> colon
        , nest 2 bads ]
    where
      (what, bads) = case bad_decls of
        IllegalDecls (NE.toList -> decls) ->
            (text "declaration" <> plural decls, vcat $ map ppr decls)
        IllegalFamDecls (NE.toList -> decls) ->
            ( text "family declaration" <> plural decls, vcat $ map ppr decls)
      descrDoc = text $ case declDescr of
                   InstanceDecl -> "an instance declaration"
                   WhereClause -> "a where clause"
                   LetBinding -> "a let expression"
                   LetExpression -> "a let expression"
                   ClssDecl -> "a class declaration"
  CannotMixGADTConsWith98Cons ->
    text "Cannot mix GADT constructors with Haskell 98"
    <+> text "constructors"
  EmptyStmtListInDoBlock ->
    text "Empty stmt list in do-block"
  NonVarInInfixExpr ->
    text "Non-variable expression is not allowed in an infix expression"
  MultiWayIfWithoutAlts ->
    text "Multi-way if-expression with no alternatives"
  CasesExprWithoutAlts ->
    text "\\cases expression with no alternatives"
  ImplicitParamsWithOtherBinds ->
    text "Implicit parameters mixed with other bindings"
  InvalidCCallImpent from ->
    text (show from) <+> text "is not a valid ccall impent"
  RecGadtNoCons ->
    quotes (text "RecGadtC") <+> text "must have at least one constructor name"
  GadtNoCons ->
    quotes (text "GadtC") <+> text "must have at least one constructor name"
  InvalidTypeInstanceHeader tys ->
    text "Invalid type instance header:"
    <+> text (show tys)
  InvalidTyFamInstLHS lhs ->
    text "Invalid type family instance LHS:"
    <+> text (show lhs)
  InvalidImplicitParamBinding ->
    text "Implicit parameter binding only allowed in let or where"
  DefaultDataInstDecl adts ->
    (text "Default data instance declarations"
    <+> text "are not allowed:")
      $$ ppr adts
  FunBindLacksEquations nm ->
    text "Function binding for"
    <+> quotes (text (TH.pprint nm))
    <+> text "has no equations"

pprTyThingUsedWrong :: WrongThingSort -> TcTyThing -> Name -> SDoc
pprTyThingUsedWrong sort thing name =
  pprTcTyThingCategory thing <+> quotes (ppr name) <+>
  text "used as a" <+> pprWrongThingSort sort

pprWrongThingSort :: WrongThingSort -> SDoc
pprWrongThingSort =
  text . \case
    WrongThingType -> "type"
    WrongThingDataCon -> "data constructor"
    WrongThingPatSyn -> "pattern synonym"
    WrongThingConLike -> "constructor-like thing"
    WrongThingClass -> "class"
    WrongThingTyCon -> "type constructor"
    WrongThingAxiom -> "axiom"

pprStageCheckReason :: StageCheckReason -> SDoc
pprStageCheckReason = \case
  StageCheckInstance _ t ->
    text "instance for" <+> quotes (ppr t)
  StageCheckSplice t ->
    quotes (ppr t)

pprUninferrableTyVarCtx :: UninferrableTyVarCtx -> SDoc
pprUninferrableTyVarCtx = \case
  UninfTyCtx_ClassContext theta ->
    sep [ text "the class context:", pprTheta theta ]
  UninfTyCtx_DataContext theta ->
    sep [ text "the datatype context:", pprTheta theta ]
  UninfTyCtx_ProvidedContext theta ->
    sep [ text "the provided context:" , pprTheta theta ]
  UninfTyCtx_TyFamRhs rhs_ty ->
    sep [ text "the type family equation right-hand side:" , ppr rhs_ty ]
  UninfTyCtx_TySynRhs rhs_ty ->
    sep [ text "the type synonym right-hand side:" , ppr rhs_ty ]
  UninfTyCtx_Sig exp_kind full_hs_ty ->
    hang (text "the kind" <+> ppr exp_kind) 2
         (text "of the type signature:" <+> ppr full_hs_ty)

pprPatSynInvalidRhsReason :: PatSynInvalidRhsReason -> SDoc
pprPatSynInvalidRhsReason = \case
  PatSynNotInvertible p ->
    text "Pattern" <+> quotes (ppr p) <+> text "is not invertible"
  PatSynUnboundVar var ->
    quotes (ppr var) <+> text "is not bound by the LHS of the pattern synonym"

pprBadFieldAnnotationReason :: BadFieldAnnotationReason -> SDoc
pprBadFieldAnnotationReason = \case
  LazyFieldsDisabled ->
    text "Lazy field annotations (~) are disabled"
  UnpackWithoutStrictness ->
    text "UNPACK pragma lacks '!'"
  BackpackUnpackAbstractType ->
    text "Ignoring unusable UNPACK pragma"

pprSuperclassCycleDetail :: SuperclassCycleDetail -> SDoc
pprSuperclassCycleDetail = \case
  SCD_HeadTyVar pred ->
    hang (text "one of whose superclass constraints is headed by a type variable:")
       2 (quotes (ppr pred))
  SCD_HeadTyFam pred ->
    hang (text "one of whose superclass constraints is headed by a type family:")
       2 (quotes (ppr pred))
  SCD_Superclass cls ->
    text "one of whose superclasses is" <+> quotes (ppr cls)

pprRoleValidationFailedReason :: Role -> RoleValidationFailedReason -> SDoc
pprRoleValidationFailedReason role = \case
  TyVarRoleMismatch tv role' ->
    text "type variable" <+> quotes (ppr tv) <+>
    text "cannot have role" <+> ppr role <+>
    text "because it was assigned role" <+> ppr role'
  TyVarMissingInEnv tv ->
    text "type variable" <+> quotes (ppr tv) <+> text "missing in environment"
  BadCoercionRole co ->
    text "coercion" <+> ppr co <+> text "has bad role" <+> ppr role

pprDisabledClassExtension :: Class -> DisabledClassExtension -> SDoc
pprDisabledClassExtension cls = \case
  MultiParamDisabled n ->
    text howMany <+> text "parameters for class" <+> quotes (ppr cls)
    where
      howMany | n == 0 = "No"
              | otherwise = "Too many"
  FunDepsDisabled ->
    text "Fundeps in class" <+> quotes (ppr cls)
  ConstrainedClassMethodsDisabled sel_id pred ->
    vcat [ hang (text "Constraint" <+> quotes (ppr pred)
                 <+> text "in the type of" <+> quotes (ppr sel_id))
              2 (text "constrains only the class type variables")]

pprImportLookup :: ImportLookupReason -> SDoc
pprImportLookup = \case
  ImportLookupBad k iface decl_spec ie _ps ->
    let
      pprImpDeclSpec :: ModIface -> ImpDeclSpec -> SDoc
      pprImpDeclSpec iface decl_spec =
        quotes (ppr (is_mod decl_spec)) <+> case mi_boot iface of
            IsBoot  -> text "(hi-boot interface)"
            NotBoot -> empty
      withContext msgs =
        hang (text "In the import of" <+> pprImpDeclSpec iface decl_spec <> colon)
          2 (vcat msgs)
    in case k of
      BadImportNotExported ->
        vcat
          [ text "Module" <+> pprImpDeclSpec iface decl_spec <+>
            text "does not export" <+> quotes (ppr ie) <> dot
          ]
      BadImportAvailVar ->
        withContext
          [ text "an item called"
              <+> quotes val <+> text "is exported, but it is not a type."
          ]
        where
          val_occ = rdrNameOcc $ ieName ie
          val = parenSymOcc val_occ (ppr val_occ)
      BadImportAvailTyCon {} ->
        withContext
          [ text "an item called"
            <+> quotes tycon <+> text "is exported, but it is a type."
          ]
        where
          tycon_occ = rdrNameOcc $ ieName ie
          tycon = parenSymOcc tycon_occ (ppr tycon_occ)
      BadImportNotExportedSubordinates ns ->
        withContext
          [ text "an item called" <+> quotes sub <+> text "is exported, but it does not export any children"
          , text "(constructors, class methods or field names) called"
          <+> pprWithCommas (quotes . ppr) ns <> dot
          ]
          where
            sub_occ = rdrNameOcc $ ieName ie
            sub = parenSymOcc sub_occ (ppr sub_occ)
      BadImportAvailDataCon dataType_occ ->
        withContext
          [ text "an item called" <+> quotes datacon
          , text "is exported, but it is a data constructor of"
          , quotes dataType <> dot
          ]
          where
            datacon_occ = rdrNameOcc $ ieName ie
            datacon = parenSymOcc datacon_occ (ppr datacon_occ)
            dataType = parenSymOcc dataType_occ (ppr dataType_occ)
  ImportLookupQualified rdr ->
    hang (text "Illegal qualified name in import item:")
       2 (ppr rdr)
  ImportLookupIllegal ->
    text "Illegal import item"
  ImportLookupAmbiguous rdr gres ->
    hang (text "Ambiguous name" <+> quotes (ppr rdr) <+> text "in import item. It could refer to:")
       2 (vcat (map (ppr . greOccName) gres))

pprUnusedImport :: ImportDecl GhcRn -> UnusedImportReason -> SDoc
pprUnusedImport decl = \case
  UnusedImportNone ->
    vcat [ pp_herald <+> quotes pp_mod <+> text "is redundant"
         , nest 2 (text "except perhaps to import instances from"
                   <+> quotes pp_mod)
         , text "To import instances alone, use:"
           <+> text "import" <+> pp_mod <> parens empty ]
  UnusedImportSome sort_unused ->
    sep [ pp_herald <+> quotes (pprWithCommas pp_unused sort_unused)
        , text "from module" <+> quotes pp_mod <+> text "is redundant"]
  where
    pp_mod = ppr (unLoc (ideclName decl))
    pp_herald = text "The" <+> pp_qual <+> text "import of"
    pp_qual
      | isImportDeclQualified (ideclQualified decl) = text "qualified"
      | otherwise                                   = empty
    pp_unused = \case
      UnusedImportNameRegular n ->
        pprNameUnqualified n
      UnusedImportNameRecField par fld_occ ->
        case par of
          ParentIs p -> pprNameUnqualified p <> parens (ppr fld_occ)
          NoParent   -> ppr fld_occ
