{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic TcRnMessage

module GHC.Tc.Errors.Ppr
  ( pprTypeDoesNotHaveFixedRuntimeRep
  , pprScopeError
  --
  , tidySkolemInfo
  , tidySkolemInfoAnon
  --
  , withHsDocContext
  , pprHsDocContext
  , inHsDocContext
  )
  where

import GHC.Prelude

import GHC.Builtin.Names

import GHC.Core.Coercion
import GHC.Core.Unify     ( tcMatchTys )
import GHC.Core.TyCon
import GHC.Core.Class
import GHC.Core.DataCon
import GHC.Core.Coercion.Axiom (coAxiomTyCon, coAxiomSingleBranch)
import GHC.Core.ConLike
import GHC.Core.FamInstEnv (famInstAxiom)
import GHC.Core.InstEnv
import GHC.Core.TyCo.Rep (Type(..))
import GHC.Core.TyCo.Ppr (pprWithExplicitKindsWhen,
                          pprSourceTyCon, pprTyVars, pprWithTYPE)
import GHC.Core.PatSyn ( patSynName, pprPatSynType )
import GHC.Core.Predicate
import GHC.Core.Type

import GHC.Driver.Flags

import GHC.Hs

import GHC.Tc.Errors.Types
import GHC.Tc.Types.Constraint
import {-# SOURCE #-} GHC.Tc.Types (getLclEnvLoc)
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Rank (Rank(..))
import GHC.Tc.Utils.TcType
import GHC.Types.Error
import GHC.Types.FieldLabel (flIsOverloaded)
import GHC.Types.Hint (UntickedPromotedThing(..), pprUntickedConstructor, isBareSymbol)
import GHC.Types.Hint.Ppr () -- Outputable GhcHint
import GHC.Types.Basic
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Reader ( GreName(..), pprNameProvenance
                             , RdrName, rdrNameOcc, greMangledName )
import GHC.Types.Name.Set
import GHC.Types.SrcLoc
import GHC.Types.TyThing
import GHC.Types.Unique.Set ( nonDetEltsUniqSet )
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env

import GHC.Unit.State (pprWithUnitState, UnitState)
import GHC.Unit.Module

import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.List.SetOps ( nubOrdBy )
import GHC.Data.Maybe
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import qualified GHC.LanguageExtensions as LangExt

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Function (on)
import Data.List ( groupBy, sortBy, tails
                 , partition, unfoldr )
import Data.Ord ( comparing )
import Data.Bifunctor
import GHC.Types.Name.Env


instance Diagnostic TcRnMessage where
  diagnosticMessage = \case
    TcRnUnknownMessage m
      -> diagnosticMessage m
    TcRnMessageWithInfo unit_state msg_with_info
      -> case msg_with_info of
           TcRnMessageDetailed err_info msg
             -> messageWithInfoDiagnosticMessage unit_state err_info (diagnosticMessage msg)
    TcRnSolverReport msgs _ _
      -> mkDecorated $
           map pprSolverReportWithCtxt msgs
    TcRnRedundantConstraints redundants (info, show_info)
      -> mkSimpleDecorated $
         text "Redundant constraint" <> plural redundants <> colon
           <+> pprEvVarTheta redundants
         $$ if show_info then text "In" <+> ppr info else empty
    TcRnInaccessibleCode implic contras
      -> mkSimpleDecorated $
         hang (text "Inaccessible code in")
           2 (ppr (ic_info implic))
         $$ vcat (map pprSolverReportWithCtxt (NE.toList contras))
    TcRnTypeDoesNotHaveFixedRuntimeRep ty prov (ErrInfo extra supplementary)
      -> mkDecorated [pprTypeDoesNotHaveFixedRuntimeRep ty prov, extra, supplementary]
    TcRnImplicitLift id_or_name ErrInfo{..}
      -> mkDecorated $
           ( text "The variable" <+> quotes (ppr id_or_name) <+>
             text "is implicitly lifted in the TH quotation"
           ) : [errInfoContext, errInfoSupplementary]
    TcRnUnusedPatternBinds bind
      -> mkDecorated [hang (text "This pattern-binding binds no variables:") 2 (ppr bind)]
    TcRnDodgyImports name
      -> mkDecorated [dodgy_msg (text "import") name (dodgy_msg_insert name :: IE GhcPs)]
    TcRnDodgyExports name
      -> mkDecorated [dodgy_msg (text "export") name (dodgy_msg_insert name :: IE GhcRn)]
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
    TcRnIllegalWildcardInType mb_name bad mb_ctxt
      -> mkSimpleDecorated $ vcat [ main_msg, context_msg ]
      where
        main_msg :: SDoc
        main_msg = case bad of
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
        context_msg :: SDoc
        context_msg = case mb_ctxt of
          Just ctxt -> nest 2 (text "in" <+> pprHsDocContext ctxt)
          _         -> empty
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
           hsep [text "duplicate field name",
                 quotes (ppr (NE.head dups)),
                 text "in record", pprRecordFieldPart fld_part]
    TcRnIllegalViewPattern pat
      -> mkSimpleDecorated $ vcat [text "Illegal view pattern: " <+> ppr pat]
    TcRnCharLiteralOutOfRange c
      -> mkSimpleDecorated $ text "character literal out of range: '\\" <> char c  <> char '\''
    TcRnIllegalWildcardsInConstructor con
      -> mkSimpleDecorated $
           vcat [ text "Illegal `..' notation for constructor" <+> quotes (ppr con)
                , nest 2 (text "The constructor has no labelled fields") ]
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
    TcRnVDQInTermType ty
      -> mkSimpleDecorated $ vcat
           [ hang (text "Illegal visible, dependent quantification" <+>
                   text "in the type of a term:")
                2 (pprType ty)
           , text "(GHC does not yet support this)" ]
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
    TcRnOrphanInstance inst
      -> mkSimpleDecorated $
           hsep [ text "Orphan instance:"
                , pprInstanceHdr inst
                ]
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
    TcRnDuplicateExport child ie1 ie2
      -> mkSimpleDecorated $
           hsep [ quotes (ppr child)
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
        thing = ppr child
        parents = map ppr parent_names
    TcRnConflictingExports occ child1 gre1 ie1 child2 gre2 ie2
      -> mkSimpleDecorated $
           vcat [ text "Conflicting exports for" <+> quotes (ppr occ) <> colon
                , ppr_export child1 gre1 ie1
                , ppr_export child2 gre2 ie2
                ]
      where
        ppr_export child gre ie = nest 3 (hang (quotes (ppr ie) <+> text "exports" <+>
                                                quotes (ppr_name child))
                                            2 (pprNameProvenance gre))

        -- DuplicateRecordFields means that nameOccName might be a
        -- mangled $sel-prefixed thing, in which case show the correct OccName
        -- alone (but otherwise show the Name so it will have a module
        -- qualifier)
        ppr_name (FieldGreName fl) | flIsOverloaded fl = ppr fl
                                   | otherwise         = ppr (flSelector fl)
        ppr_name (NormalGreName name) = ppr name
    TcRnAmbiguousField rupd parent_type
      -> mkSimpleDecorated $
          vcat [ text "The record update" <+> ppr rupd
                   <+> text "with type" <+> ppr parent_type
                   <+> text "is ambiguous."
               , text "This will not be supported by -XDuplicateRecordFields in future releases of GHC."
               ]
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
    TcRnNoConstructorHasAllFields conflictingFields
      -> mkSimpleDecorated $
           hang (text "No constructor has all these fields:")
              2 (pprQuotedList conflictingFields)
    TcRnMixedSelectors data_name data_sels pat_name pat_syn_sels
      -> mkSimpleDecorated $
           text "Cannot use a mixture of pattern synonym and record selectors" $$
           text "Record selectors defined by"
             <+> quotes (ppr data_name)
             <> colon
             <+> pprWithCommas ppr data_sels $$
           text "Pattern synonym selectors defined by"
             <+> quotes (ppr pat_name)
             <> colon
             <+> pprWithCommas ppr pat_syn_sels
    TcRnMissingStrictFields con fields
      -> mkSimpleDecorated $ vcat [header, nest 2 rest]
         where
           rest | null fields = empty  -- Happens for non-record constructors
                                       -- with strict fields
                | otherwise   = vcat (fmap pprField fields)

           header = text "Constructor" <+> quotes (ppr con) <+>
                    text "does not have the required strict field(s)" <>
                    if null fields then empty else colon
    TcRnNoPossibleParentForFields rbinds
      -> mkSimpleDecorated $
           hang (text "No type has all these fields:")
              2 (pprQuotedList fields)
         where fields = map (hfbLHS . unLoc) rbinds
    TcRnBadOverloadedRecordUpdate _rbinds
      -> mkSimpleDecorated $
           text "Record update is ambiguous, and requires a type signature"
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
      -> mkSimpleDecorated $ text "Illegal foreign declaration:" <+>
           case expectedBknds of
             COrAsmOrLlvm ->
               text "requires unregisterised, llvm (-fllvm) or native code generation (-fasm)"
             COrAsmOrLlvmOrInterp ->
               text "requires interpreted, unregisterised, llvm or native code generation"

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
                NewtypeDataConNotInScope Nothing ->
                  hang innerMsg 2 $ text "because its data constructor is not in scope"
                NewtypeDataConNotInScope (Just tc) ->
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

  diagnosticReason = \case
    TcRnUnknownMessage m
      -> diagnosticReason m
    TcRnMessageWithInfo _ msg_with_info
      -> case msg_with_info of
           TcRnMessageDetailed _ m -> diagnosticReason m
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
    TcRnAmbiguousField{}
      -> WarningWithFlag Opt_WarnAmbiguousFields
    TcRnMissingFields{}
      -> WarningWithFlag Opt_WarnMissingFields
    TcRnFieldUpdateInvalidType{}
      -> ErrorWithoutFlag
    TcRnNoConstructorHasAllFields{}
      -> ErrorWithoutFlag
    TcRnMixedSelectors{}
      -> ErrorWithoutFlag
    TcRnMissingStrictFields{}
      -> ErrorWithoutFlag
    TcRnNoPossibleParentForFields{}
      -> ErrorWithoutFlag
    TcRnBadOverloadedRecordUpdate{}
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
    TcRnGADTMonoLocalBinds {}
      -> WarningWithFlag Opt_WarnGADTMonoLocalBinds
    TcRnIncorrectNameSpace {}
      -> ErrorWithoutFlag
    TcRnNotInScope {}
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

  diagnosticHints = \case
    TcRnUnknownMessage m
      -> diagnosticHints m
    TcRnMessageWithInfo _ msg_with_info
      -> case msg_with_info of
           TcRnMessageDetailed _ m -> diagnosticHints m
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
    TcRnOrphanInstance{}
      -> [SuggestFixOrphanInstance]
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
    TcRnAmbiguousField{}
      -> noHints
    TcRnMissingFields{}
      -> noHints
    TcRnFieldUpdateInvalidType{}
      -> noHints
    TcRnNoConstructorHasAllFields{}
      -> noHints
    TcRnMixedSelectors{}
      -> noHints
    TcRnMissingStrictFields{}
      -> noHints
    TcRnNoPossibleParentForFields{}
      -> noHints
    TcRnBadOverloadedRecordUpdate{}
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
    TcRnGADTMonoLocalBinds {}
      -> [suggestAnyExtension [LangExt.GADTs, LangExt.TypeFamilies]]
    TcRnIncorrectNameSpace nm is_th_use
      | is_th_use
      -> [SuggestAppropriateTHTick $ nameNameSpace nm]
      | otherwise
      -> noHints
    TcRnNotInScope err _ _ hints
      -> scopeErrorHints err ++ hints
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
             | NewtypeDataConNotInScope{} <- why -> [SuggestImportingDataCon]
             | UnliftedFFITypesNeeded <- why -> [suggestExtension LangExt.UnliftedFFITypes]
           _ -> noHints
    TcRnInvalidCIdentifier{}
      -> noHints

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
                                 -> DecoratedSDoc
                                 -> DecoratedSDoc
messageWithInfoDiagnosticMessage unit_state ErrInfo{..} important =
  let err_info' = map (pprWithUnitState unit_state) [errInfoContext, errInfoSupplementary]
      in (mapDecoratedSDoc (pprWithUnitState unit_state) important) `unionDecoratedSDoc`
         mkDecorated err_info'

dodgy_msg :: (Outputable a, Outputable b) => SDoc -> a -> b -> SDoc
dodgy_msg kind tc ie
  = sep [ text "The" <+> kind <+> text "item"
                     <+> quotes (ppr ie)
                <+> text "suggests that",
          quotes (ppr tc) <+> text "has (in-scope) constructors or class methods,",
          text "but it has none" ]

dodgy_msg_insert :: forall p . IdP (GhcPass p) -> IE (GhcPass p)
dodgy_msg_insert tc = IEThingAll noAnn ii
  where
    ii :: LIEWrappedName (IdP (GhcPass p))
    ii = noLocA (IEName $ noLocA tc)

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
    tidy_ki             = tidyType tidy_env (tcTypeKind ty)

pprField :: (FieldLabelString, TcType) -> SDoc
pprField (f,ty) = ppr f <+> dcolon <+> ppr ty

pprRecordFieldPart :: RecordFieldPart -> SDoc
pprRecordFieldPart = \case
  RecordFieldConstructor{} -> text "construction"
  RecordFieldPattern{}     -> text "pattern"
  RecordFieldUpdate        -> text "update"

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
pprTcSolverReportMsg ctxt (TcReportWithInfo msg (info :| infos)) =
  vcat
    ( pprTcSolverReportMsg ctxt msg
    : pprTcSolverReportInfo ctxt info
    : map (pprTcSolverReportInfo ctxt) infos )
pprTcSolverReportMsg _ (BadTelescope telescope skols) =
  hang (text "These kind and type variables:" <+> ppr telescope $$
       text "are out of dependency order. Perhaps try this ordering:")
    2 (pprTyVars sorted_tvs)
  where
    sorted_tvs = scopedSort skols
pprTcSolverReportMsg _ (UserTypeError ty) =
  pprUserTypeErrorTy ty
pprTcSolverReportMsg ctxt (ReportHoleError hole err) =
  pprHoleError ctxt hole err
pprTcSolverReportMsg _ (CannotUnifyWithPolytype item tv1 ty2) =
  vcat [ (if isSkolemTyVar tv1
          then text "Cannot equate type variable"
          else text "Cannot instantiate unification variable")
         <+> quotes (ppr tv1)
       , hang (text "with a" <+> what <+> text "involving polytypes:") 2 (ppr ty2) ]
  where
    what = text $ levelString $
           ctLocTypeOrKind_maybe (errorItemCtLoc item) `orElse` TypeLevel
pprTcSolverReportMsg _
  (Mismatch { mismatch_ea   = add_ea
            , mismatch_item = item
            , mismatch_ty1  = ty1
            , mismatch_ty2  = ty2 })
  = addArising (errorItemOrigin item) msg
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
                   , if add_ea then "expected"          else ""
                   , what ]
    herald2 = conc [ "with"
                   , if is_repr then "that of"          else ""
                   , if add_ea then ("actual " ++ what) else "" ]

    padding = length herald1 - length herald2

    is_repr = case errorItemEqRel item of { ReprEq -> True; NomEq -> False }

    what = levelString (ctLocTypeOrKind_maybe (errorItemCtLoc item) `orElse` TypeLevel)

    conc :: [String] -> String
    conc = foldr1 add_space

    add_space :: String -> String -> String
    add_space s1 s2 | null s1   = s2
                    | null s2   = s1
                    | otherwise = s1 ++ (' ' : s2)
pprTcSolverReportMsg _
  (KindMismatch { kmismatch_what     = thing
                , kmismatch_expected = exp
                , kmismatch_actual   = act })
  = hang (text "Expected" <+> kind_desc <> comma)
      2 (text "but" <+> quotes (ppr thing) <+> text "has kind" <+>
        quotes (ppr act))
  where
    kind_desc | tcIsConstraintKind exp = text "a constraint"
              | Just arg <- kindRep_maybe exp  -- TYPE t0
              , tcIsTyVarTy arg = sdocOption sdocPrintExplicitRuntimeReps $ \case
                                   True  -> text "kind" <+> quotes (ppr exp)
                                   False -> text "a type"
              | otherwise       = text "kind" <+> quotes (ppr exp)


pprTcSolverReportMsg ctxt
  (TypeEqMismatch { teq_mismatch_ppr_explicit_kinds = ppr_explicit_kinds
                  , teq_mismatch_item     = item
                  , teq_mismatch_ty1      = ty1
                  , teq_mismatch_ty2      = ty2
                  , teq_mismatch_expected = exp
                  , teq_mismatch_actual   = act
                  , teq_mismatch_what     = mb_thing })
  = addArising orig $ pprWithExplicitKindsWhen ppr_explicit_kinds msg
  where
    msg
      | isUnliftedTypeKind act, isLiftedTypeKind exp
      = sep [ text "Expecting a lifted type, but"
            , thing_msg mb_thing (text "an") (text "unlifted") ]
      | isLiftedTypeKind act, isUnliftedTypeKind exp
      = sep [ text "Expecting an unlifted type, but"
            , thing_msg mb_thing (text "a") (text "lifted") ]
      | tcIsLiftedTypeKind exp
      = maybe_num_args_msg $$
        sep [ text "Expected a type, but"
            , case mb_thing of
                Nothing    -> text "found something with kind"
                Just thing -> quotes (ppr thing) <+> text "has kind"
            , quotes (pprWithTYPE act) ]
      | Just nargs_msg <- num_args_msg
      , Right ea_msg <- mk_ea_msg ctxt (Just item) level orig
      = nargs_msg $$ pprTcSolverReportMsg ctxt ea_msg
      | -- pprTrace "check" (ppr ea_looks_same $$ ppr exp $$ ppr act $$ ppr ty1 $$ ppr ty2) $
        ea_looks_same ty1 ty2 exp act
      , Right ea_msg <- mk_ea_msg ctxt (Just item) level orig
      = pprTcSolverReportMsg ctxt ea_msg
      -- The mismatched types are /inside/ exp and act
      | let mismatch_err = Mismatch False item ty1 ty2
            errs = case mk_ea_msg ctxt Nothing level orig of
              Left ea_info -> [ mkTcReportWithInfo mismatch_err ea_info ]
              Right ea_err -> [ mismatch_err, ea_err ]
      = vcat $ map (pprTcSolverReportMsg ctxt) errs

    ct_loc = errorItemCtLoc item
    orig   = errorItemOrigin item
    level  = ctLocTypeOrKind_maybe ct_loc `orElse` TypeLevel

    thing_msg (Just thing) _  levity = quotes (ppr thing) <+> text "is" <+> levity
    thing_msg Nothing      an levity = text "got" <+> an <+> levity <+> text "type"

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

    count_args ty = count isVisibleBinder $ fst $ splitPiTys ty
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
          -> isConcrete (typeKind inner_ty)
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

pprTcSolverReportMsg _ (SkolemEscape item implic esc_skols) =
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
     then addArising (errorItemOrigin item) $
            sep [ text "Unbound implicit parameter" <> plural preds
                , nest 2 (pprParendTheta preds) ]
     else pprTcSolverReportMsg ctxt (CouldNotDeduce givens (item :| items) Nothing)
  where
    preds = map errorItemPred (item : items)
pprTcSolverReportMsg ctxt (CouldNotDeduce useful_givens (item :| others) mb_extra)
  = main_msg $$
     case supplementary of
      Left infos
        -> vcat (map (pprTcSolverReportInfo ctxt) infos)
      Right other_msg
        -> pprTcSolverReportMsg ctxt other_msg
  where
    main_msg
      | null useful_givens
      = addArising orig (no_instance_msg <+> missing)
      | otherwise
      = vcat (addArising orig (no_deduce_msg <+> missing)
              : pp_givens useful_givens)

    supplementary = case mb_extra of
      Nothing
        -> Left []
      Just (CND_Extra level ty1 ty2)
        -> mk_supplementary_ea_msg ctxt level ty1 ty2 orig
    orig = errorItemOrigin item
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
        [wanted] -> pprParendType wanted
        _        -> pprTheta wanteds

pprTcSolverReportMsg ctxt (AmbiguityPreventsSolvingCt item ambigs) =
  pprTcSolverReportInfo ctxt (Ambiguity True ambigs) <+>
  pprArising (errorItemOrigin item) $$
  text "prevents the constraint" <+> quotes (pprParendType $ errorItemPred item)
  <+> text "from being solved."
pprTcSolverReportMsg ctxt@(CEC {cec_encl = implics})
  (CannotResolveInstance item unifiers candidates imp_errs suggs binds)
  =
    vcat
      [ pprTcSolverReportMsg ctxt no_inst_msg
      , nest 2 extra_note
      , mb_patsyn_prov `orElse` empty
      , ppWhen (has_ambigs && not (null unifiers && null useful_givens))
        (vcat [ ppUnless lead_with_ambig $
                  pprTcSolverReportInfo ctxt (Ambiguity False (ambig_kvs, ambig_tvs))
              , pprRelevantBindings binds
              , potential_msg ])
      , ppWhen (isNothing mb_patsyn_prov) $
            -- Don't suggest fixes for the provided context of a pattern
            -- synonym; the right fix is to bind more in the pattern
        show_fixes (ctxtFixes has_ambigs pred implics
                    ++ drv_fixes)
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
    -- See Note [Highlighting ambiguous type variables]
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

    no_inst_msg :: TcSolverReportMsg
    no_inst_msg
      | lead_with_ambig
      = AmbiguityPreventsSolvingCt item (ambig_kvs, ambig_tvs)
      | otherwise
      = CouldNotDeduce useful_givens (item :| []) Nothing

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
                       parens (ppr ty <+> dcolon <+> ppr (tcTypeKind ty)))
               | otherwise
               = empty

    drv_fixes = case orig of
                   DerivClauseOrigin                  -> [drv_fix False]
                   StandAloneDerivOrigin              -> [drv_fix True]
                   DerivOriginDC _ _       standalone -> [drv_fix standalone]
                   DerivOriginCoerce _ _ _ standalone -> [drv_fix standalone]
                   _                -> []

    drv_fix standalone_wildcard
      | standalone_wildcard
      = text "fill in the wildcard constraint yourself"
      | otherwise
      = hang (text "use a standalone 'deriving instance' declaration,")
           2 (text "so you can specify the instance context yourself")

pprTcSolverReportMsg (CEC {cec_encl = implics}) (OverlappingInstances item matches unifiers) =
  vcat
    [ addArising orig $
        (text "Overlapping instances for"
        <+> pprType (mkClassPred clas tys))
    , ppUnless (null matching_givens) $
                  sep [text "Matching givens (or their superclasses):"
                      , nest 2 (vcat matching_givens)]
    ,  potentialInstancesErrMsg
        (PotentialInstances { matches, unifiers })
    ,  ppWhen (null matching_givens && isSingleton matches && null unifiers) $
       -- Intuitively, some given matched the wanted in their
       -- flattened or rewritten (from given equalities) form
       -- but the matcher can't figure that out because the
       -- constraints are non-flat and non-rewritten so we
       -- simply report back the whole given
       -- context. Accelerate Smart.hs showed this problem.
         sep [ text "There exists a (perhaps superclass) match:"
             , nest 2 (vcat (pp_givens useful_givens))]

    ,  ppWhen (isSingleton matches) $
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
    orig            = errorItemOrigin item
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
pprTcSolverReportMsg _ (UnsafeOverlap item matches unsafe_overlapped) =
  vcat [ addArising orig (text "Unsafe overlapping instances for"
                  <+> pprType (mkClassPred clas tys))
       , sep [text "The matching instance is:",
              nest 2 (pprInstance $ head matches)]
       , vcat [ text "It is compiled in a Safe module and as such can only"
              , text "overlap instances from the same module, however it"
              , text "overlaps the following instances from different" <+>
                text "modules:"
              , nest 2 (vcat [pprInstances $ unsafe_overlapped])
              ]
       ]
  where
    orig        = errorItemOrigin item
    pred        = errorItemPred item
    (clas, tys) = getClassPredTys pred

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
                    Outputting TcSolverReportInfo
*                                                                      *
**********************************************************************-}

-- | Pretty-print an informational message, to accompany a 'TcSolverReportMsg'.
pprTcSolverReportInfo :: SolverReportErrCtxt -> TcSolverReportInfo -> SDoc
pprTcSolverReportInfo _ (Ambiguity prepend_msg (ambig_kvs, ambig_tvs)) = msg
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
pprTcSolverReportInfo ctxt (TyVarInfo tv ) =
  case tcTyVarDetails tv of
    SkolemTv sk_info _ _   -> pprSkols ctxt [(getSkolemInfo sk_info, [tv])]
    RuntimeUnk {} -> quotes (ppr tv) <+> text "is an interactive-debugger skolem"
    MetaTv {}     -> empty
pprTcSolverReportInfo _ (NonInjectiveTyFam tc) =
  text "NB:" <+> quotes (ppr tc)
  <+> text "is a non-injective type family"
pprTcSolverReportInfo _ (ReportCoercibleMsg msg) =
  pprCoercibleMsg msg
pprTcSolverReportInfo _ (ExpectedActual { ea_expected = exp, ea_actual = act }) =
  vcat
    [ text "Expected:" <+> ppr exp
    , text "  Actual:" <+> ppr act ]
pprTcSolverReportInfo _
  (ExpectedActualAfterTySynExpansion
    { ea_expanded_expected = exp
    , ea_expanded_actual   = act } )
  = vcat
      [ text "Type synonyms expanded:"
      , text "Expected type:" <+> ppr exp
      , text "  Actual type:" <+> ppr act ]
pprTcSolverReportInfo ctxt (WhenMatching cty1 cty2 sub_o mb_sub_t_or_k) =
  sdocOption sdocPrintExplicitCoercions $ \printExplicitCoercions ->
    if printExplicitCoercions
       || not (cty1 `pickyEqType` cty2)
      then vcat [ hang (text "When matching" <+> sub_whats)
                      2 (vcat [ ppr cty1 <+> dcolon <+>
                               ppr (tcTypeKind cty1)
                             , ppr cty2 <+> dcolon <+>
                               ppr (tcTypeKind cty2) ])
                , supplementary ]
      else text "When matching the kind of" <+> quotes (ppr cty1)
  where
    sub_t_or_k = mb_sub_t_or_k `orElse` TypeLevel
    sub_whats  = text (levelString sub_t_or_k) <> char 's'
    supplementary =
      case mk_supplementary_ea_msg ctxt sub_t_or_k cty1 cty2 sub_o of
        Left infos -> vcat $ map (pprTcSolverReportInfo ctxt) infos
        Right msg  -> pprTcSolverReportMsg ctxt msg
pprTcSolverReportInfo _ (SameOcc same_pkg n1 n2) =
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
pprTcSolverReportInfo ctxt (OccursCheckInterestingTyVars (tv :| tvs)) =
  hang (text "Type variable kinds:") 2 $
    vcat (map (tyvar_binding . tidyTyCoVarOcc (cec_tidy ctxt))
              (tv:tvs))
  where
    tyvar_binding tyvar = ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)

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

{- *********************************************************************
*                                                                      *
                  Outputting HoleError messages
*                                                                      *
**********************************************************************-}

pprHoleError :: SolverReportErrCtxt -> Hole -> HoleError -> SDoc
pprHoleError _ (Hole { hole_ty, hole_occ = occ }) (OutOfScopeHole imp_errs)
  = out_of_scope_msg $$ vcat (map ppr imp_errs)
  where
    herald | isDataOcc occ = text "Data constructor not in scope:"
           | otherwise     = text "Variable not in scope:"
    out_of_scope_msg -- Print v :: ty only if the type has structure
      | boring_type = hang herald 2 (ppr occ)
      | otherwise   = hang herald 2 (pp_occ_with_type occ hole_ty)
    boring_type = isTyVarTy hole_ty
pprHoleError ctxt (Hole { hole_ty, hole_occ}) (HoleError sort other_tvs hole_skol_info) =
  vcat [ hole_msg
       , tyvars_msg
       , case sort of { ExprHole {} -> expr_hole_hint; _ -> type_hole_hint } ]

  where

    hole_msg = case sort of
      ExprHole {} ->
        hang (text "Found hole:")
          2 (pp_occ_with_type hole_occ hole_ty)
      TypeHole ->
        hang (text "Found type wildcard" <+> quotes (ppr hole_occ))
          2 (text "standing for" <+> quotes pp_hole_type_with_kind)
      ConstraintHole ->
        hang (text "Found extra-constraints wildcard standing for")
          2 (quotes $ pprType hole_ty)  -- always kind constraint

    hole_kind = tcTypeKind hole_ty

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
         | lengthFS (occNameFS hole_occ) > 1  -- Don't give this hint for plain "_"
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

pp_occ_with_type :: OccName -> Type -> SDoc
pp_occ_with_type occ hole_ty = hang (pprPrefixOcc occ) 2 (dcolon <+> pprType hole_ty)

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
    NoExactName name ->
      text "The Name" <+> quotes (ppr name) <+> text "is not in scope."
    SameName gres ->
      assertPpr (length gres >= 2) (text "pprScopeError SameName: fewer than 2 elements" $$ nest 2 (ppr gres))
      $ hang (text "Same Name in multiple name-spaces:")
           2 (vcat (map pp_one sorted_names))
      where
        sorted_names = sortBy (leftmost_smallest `on` nameSrcSpan) (map greMangledName gres)
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
  where
    what = pprNonVarNameSpace (occNameSpace (rdrNameOcc rdr_name))

scopeErrorHints :: NotInScopeError -> [GhcHint]
scopeErrorHints scope_err =
  case scope_err of
    NotInScope             -> noHints
    NoExactName {}         -> [SuggestDumpSlices]
    SameName {}            -> [SuggestDumpSlices]
    MissingBinding _ hints -> hints
    NoTopLevelBinding      -> noHints
    UnknownSubordinate {}  -> noHints

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
  , isTyVarClassPred pred
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

pprArising :: CtOrigin -> SDoc
-- Used for the main, top-level error message
-- We've done special processing for TypeEq, KindEq, givens
pprArising (TypeEqOrigin {})         = empty
pprArising (KindEqOrigin {})         = empty
pprArising (AmbiguityCheckOrigin {}) = empty  -- the "In the ambiguity check" context
                                              -- is sufficient; this would just be
                                              -- repetitive
pprArising orig | isGivenOrigin orig = empty
                | otherwise          = pprCtOrigin orig

-- Add the "arising from..." part to a message
addArising :: CtOrigin -> SDoc -> SDoc
addArising orig msg = hang msg 2 (pprArising orig)

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
  = addArising (ctLocOrigin loc) (pprTheta [ctPred ct])
  | otherwise
  = vcat (map ppr_one (ct:cts))
  where
    loc = ctLoc ct
    ppr_one ct' = hang (parens (pprType (ctPred ct')))
                     2 (pprCtLoc (ctLoc ct'))

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

    tidy_ty env ty@(FunTy InvisArg w arg res) -- Look under  c => t
      = ty { ft_mult = tidy_ty env w,
             ft_arg = tidyType env arg,
             ft_res = tidy_ty env res }

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
                        -> Type -> Type -> CtOrigin -> Either [TcSolverReportInfo] TcSolverReportMsg
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
          -> CtOrigin -> Either [TcSolverReportInfo] TcSolverReportMsg
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
  , let mismatch =
          Mismatch
            { mismatch_ea   = True
            , mismatch_item = item
            , mismatch_ty1  = exp
            , mismatch_ty2  = act }
  = Right $
    if expanded_syns
    then mkTcReportWithInfo mismatch [ea_expanded]
    else mismatch
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
    -- `tyExpansions T10` returns [T9, T8, T7, ... Int]
    --
    -- This only expands the top layer, so if you have:
    --
    --   type M a = Maybe a
    --
    -- `tyExpansions (M T10)` returns [Maybe T10] (T10 is not expanded)
    tyExpansions :: Type -> [Type]
    tyExpansions = unfoldr (\t -> (\x -> (x, x)) `fmap` tcView t)

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

withHsDocContext :: HsDocContext -> SDoc -> SDoc
withHsDocContext ctxt doc = doc $$ inHsDocContext ctxt

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
