{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic TcRnMessage
{-# LANGUAGE RecordWildCards #-}

module GHC.Tc.Errors.Ppr (
    formatLevPolyErr
  , pprLevityPolyInType
  ) where

import GHC.Prelude

import GHC.Core.TyCo.Ppr (pprWithTYPE)
import GHC.Core.Type
import GHC.Data.Bag
import GHC.Tc.Errors.Types
import GHC.Types.Error
import GHC.Types.Name (pprPrefixName)
import GHC.Types.Name.Reader (pprNameProvenance)
import GHC.Types.SrcLoc (GenLocated(..))
import GHC.Types.Name.Occurrence (occName)
import GHC.Types.Var.Env (emptyTidyEnv)
import GHC.Driver.Flags
import GHC.Hs
import GHC.Utils.Outputable
import GHC.Unit.State (pprWithUnitState, UnitState)
import qualified GHC.LanguageExtensions as LangExt
import qualified Data.List.NonEmpty as NE


instance Diagnostic TcRnMessage where
  diagnosticMessage = \case
    TcRnUnknownMessage m
      -> diagnosticMessage m
    TcLevityPolyInType ty prov (ErrInfo extra supplementary)
      -> mkDecorated [pprLevityPolyInType ty prov, extra, supplementary]
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
    TcRnSimplifierTooManyIterations limit wc
      -> mkSimpleDecorated $
           hang (text "solveWanteds: too many iterations"
                   <+> parens (text "limit =" <+> ppr limit))
                2 (text "Unsolved:" <+> ppr wc)
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

  diagnosticReason = \case
    TcRnUnknownMessage m
      -> diagnosticReason m
    TcLevityPolyInType{}
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

  diagnosticHints = \case
    TcRnUnknownMessage m
      -> diagnosticHints m
    TcLevityPolyInType{}
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

formatLevPolyErr :: Type  -- representation-polymorphic type
                 -> SDoc
formatLevPolyErr ty
  = hang (text "A representation-polymorphic type is not allowed here:")
       2 (vcat [ text "Type:" <+> pprWithTYPE tidy_ty
               , text "Kind:" <+> pprWithTYPE tidy_ki ])
  where
    (tidy_env, tidy_ty) = tidyOpenType emptyTidyEnv ty
    tidy_ki             = tidyType tidy_env (tcTypeKind ty)

pprLevityPolyInType :: Type -> LevityCheckProvenance -> SDoc
pprLevityPolyInType ty prov =
  let extra = case prov of
        LevityCheckInBinder v
          -> text "In the type of binder" <+> quotes (ppr v)
        LevityCheckInVarType
          -> text "When trying to create a variable of type:" <+> ppr ty
        LevityCheckInWildcardPattern
          -> text "In a wildcard pattern"
        LevityCheckInUnboxedTuplePattern p
          -> text "In the type of an element of an unboxed tuple pattern:" $$ ppr p
        LevityCheckPatSynSig
          -> empty
        LevityCheckCmdStmt
          -> empty -- I (Richard E, Dec '16) have no idea what to say here
        LevityCheckMkCmdEnv id_var
          -> text "In the result of the function" <+> quotes (ppr id_var)
        LevityCheckDoCmd do_block
          -> text "In the do-command:" <+> ppr do_block
        LevityCheckDesugaringCmd cmd
          -> text "When desugaring the command:" <+> ppr cmd
        LevityCheckInCmd body
          -> text "In the command:" <+> ppr body
        LevityCheckInFunUse using
          -> text "In the result of a" <+> quotes (text "using") <+> text "function:" <+> ppr using
        LevityCheckInValidDataCon
          -> empty
        LevityCheckInValidClass
          -> empty
  in formatLevPolyErr ty $$ extra


pprRecordFieldPart :: RecordFieldPart -> SDoc
pprRecordFieldPart = \case
  RecordFieldConstructor{} -> text "construction"
  RecordFieldPattern{}     -> text "pattern"
  RecordFieldUpdate        -> text "update"
