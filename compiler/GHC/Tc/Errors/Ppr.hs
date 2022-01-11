{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic TcRnMessage
{-# LANGUAGE RecordWildCards #-}

module GHC.Tc.Errors.Ppr ( pprTypeDoesNotHaveFixedRuntimeRep )
  where

import GHC.Prelude

import Data.Maybe (isJust)

import GHC.Builtin.Names
import GHC.Core.Class (Class(..))
import GHC.Core.Coercion (pprCoAxBranchUser)
import GHC.Core.Coercion.Axiom (coAxiomTyCon, coAxiomSingleBranch)
import GHC.Core.DataCon (DataCon)
import GHC.Core.FamInstEnv (famInstAxiom)
import GHC.Core.InstEnv
import GHC.Core.TyCon (isNewTyCon)
import GHC.Core.TyCo.Ppr (pprKind, pprParendType, pprType,
                          pprWithExplicitKindsWhen, pprTheta, pprClassPred, pprTypeApp,
                          pprSourceTyCon)
import GHC.Core.Type
import GHC.Data.Bag
import GHC.Tc.Errors.Types
import GHC.Tc.Types.Rank (Rank(..))
import GHC.Tc.Utils.TcType (TcType, tcSplitForAllTyVars, mkClassPred)
import GHC.Types.Basic (UnboxedTupleOrSum(..), unboxedTupleOrSumExtension)
import GHC.Types.Error
import GHC.Types.FieldLabel (FieldLabelString, flIsOverloaded, flSelector)
import GHC.Types.Id (isRecordSelector)
import GHC.Types.Name
import GHC.Types.Name.Reader (GreName(..), pprNameProvenance)
import GHC.Types.SrcLoc (GenLocated(..), unLoc)
import GHC.Types.TyThing
import GHC.Types.Var.Env (emptyTidyEnv)
import GHC.Types.Var.Set (pprVarSet, pluralVarSet)
import GHC.Driver.Flags
import GHC.Hs
import GHC.Utils.Misc (capitalise)
import GHC.Utils.Outputable
import GHC.Unit.State (pprWithUnitState, UnitState)
import qualified GHC.LanguageExtensions as LangExt
import qualified Data.List.NonEmpty as NE


instance Diagnostic TcRnMessage where
  diagnosticMessage = \case
    TcRnUnknownMessage m
      -> diagnosticMessage m
    TcRnTypeDoesNotHaveFixedRuntimeRep ty prov (ErrInfo extra supplementary)
      -> mkDecorated [pprTypeDoesNotHaveFixedRuntimeRep ty prov, extra, supplementary]
    TcRnMessageWithInfo unit_state msg_with_info
      -> case msg_with_info of
           TcRnMessageDetailed err_info msg
             -> messageWithInfoDiagnosticMessage unit_state err_info (diagnosticMessage msg)
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
    TcRnArrowCommandExpected cmd
      -> mkSimpleDecorated $
           vcat [text "The expression", nest 2 (ppr cmd),
                 text "was found where an arrow command was expected"]
    TcRnIllegalHsBootFileDecl
      -> mkSimpleDecorated $
           text "Illegal declarations in an hs-boot file"
    TcRnRecursivePatternSynonym binds
      -> mkSimpleDecorated $
            hang (text "Recursive pattern synonym definition with following bindings:")
               2 (vcat $ map pprLBind . bagToList $ binds)
          where
            pprLoc loc = parens (text "defined at" <+> ppr loc)
            pprLBind :: GenLocated (SrcSpanAnn' a) (HsBindLR GhcRn idR) -> SDoc
            pprLBind (L loc bind) = pprWithCommas ppr (collectHsBindBinders CollNoDictBinders bind)
                                        <+> pprLoc (locA loc)
    TcRnPartialTypeSigTyVarMismatch n1 n2 fn_name hs_ty
      -> mkSimpleDecorated $
           hang (text "Couldn't match" <+> quotes (ppr n1)
                   <+> text "with" <+> quotes (ppr n2))
                2 (hang (text "both bound by the partial type signature:")
                        2 (ppr fn_name <+> dcolon <+> ppr hs_ty))
    TcRnPartialTypeSigBadQuantifier n fn_name hs_ty
      -> mkSimpleDecorated $
           hang (text "Can't quantify over" <+> quotes (ppr n))
                2 (hang (text "bound by the partial type signature:")
                        2 (ppr fn_name <+> dcolon <+> ppr hs_ty))
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
    TcRnIllegalEqualConstraints ty
      -> mkSimpleDecorated $
           text "Illegal equational constraint" <+> pprType ty
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

        -- DuplicateRecordFields means that nameOccName might be a mangled
        -- $sel-prefixed thing, in which case show the correct OccName alone
        -- (but otherwise show the Name so it will have a module qualifier)
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

  diagnosticReason = \case
    TcRnUnknownMessage m
      -> diagnosticReason m
    TcRnTypeDoesNotHaveFixedRuntimeRep{}
      -> ErrorWithoutFlag
    TcRnMessageWithInfo _ msg_with_info
      -> case msg_with_info of
           TcRnMessageDetailed _ m -> diagnosticReason m
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
    TcRnArrowCommandExpected{}
      -> ErrorWithoutFlag
    TcRnIllegalHsBootFileDecl
      -> ErrorWithoutFlag
    TcRnRecursivePatternSynonym{}
      -> ErrorWithoutFlag
    TcRnPartialTypeSigTyVarMismatch{}
      -> ErrorWithoutFlag
    TcRnPartialTypeSigBadQuantifier{}
      -> ErrorWithoutFlag
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
    TcRnIllegalEqualConstraints{}
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
    TcRnGADTMonoLocalBinds {}
      -> WarningWithFlag Opt_WarnGADTMonoLocalBinds
    TcRnIncorrectNameSpace {}
      -> ErrorWithoutFlag

  diagnosticHints = \case
    TcRnUnknownMessage m
      -> diagnosticHints m
    TcRnTypeDoesNotHaveFixedRuntimeRep{}
      -> noHints
    TcRnMessageWithInfo _ msg_with_info
      -> case msg_with_info of
           TcRnMessageDetailed _ m -> diagnosticHints m
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
    TcRnArrowCommandExpected{}
      -> noHints
    TcRnIllegalHsBootFileDecl
      -> noHints
    TcRnRecursivePatternSynonym{}
      -> noHints
    TcRnPartialTypeSigTyVarMismatch{}
      -> noHints
    TcRnPartialTypeSigBadQuantifier{}
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
    TcRnIllegalEqualConstraints{}
      -> [suggestAnyExtension [LangExt.GADTs, LangExt.TypeFamilies]]
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
    TcRnGADTMonoLocalBinds {}
      -> [suggestAnyExtension [LangExt.GADTs, LangExt.TypeFamilies]]
    TcRnIncorrectNameSpace nm is_th_use
      | is_th_use
      -> [SuggestAppropriateTHTick $ nameNameSpace nm]
      | otherwise
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
