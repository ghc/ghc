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
import GHC.Tc.Errors.Types
import GHC.Types.Error
import GHC.Types.Name.Reader (pprNameProvenance)
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
    TcRnShadowedName{}
      -> WarningWithFlag Opt_WarnNameShadowing
    TcRnDuplicateWarningDecls{}
      -> ErrorWithoutFlag
    TcRnSimplifierTooManyIterations{}
      -> ErrorWithoutFlag
    TcRnIllegalPatSynDecl{}
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
    TcRnShadowedName{}
      -> noHints
    TcRnDuplicateWarningDecls{}
      -> noHints
    TcRnSimplifierTooManyIterations{}
      -> [SuggestIncreaseSimplifierIterations]
    TcRnIllegalPatSynDecl{}
      -> noHints
    TcRnEmptyRecordUpdate{}
      -> noHints
    TcRnIllegalFieldPunning{}
      -> [SuggestExtension LangExt.RecordPuns]
    TcRnIllegalWildcardsInRecord{}
      -> [SuggestExtension LangExt.RecordWildCards]
    TcRnDuplicateFieldName{}
      -> noHints
    TcRnIllegalViewPattern{}
      -> [SuggestExtension LangExt.ViewPatterns]

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
