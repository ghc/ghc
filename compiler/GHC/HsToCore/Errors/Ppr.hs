{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic DsMessage

module GHC.HsToCore.Errors.Ppr where

import GHC.Core.Utils (exprType)
import GHC.Driver.Flags
import GHC.Hs
import GHC.HsToCore.Errors.Types
import GHC.Prelude
import GHC.Tc.Errors.Ppr (formatLevPolyErr)
import GHC.Types.Error
import GHC.Types.Id (idType)
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Utils.Outputable
import qualified GHC.LanguageExtensions as LangExt
import {-# SOURCE #-} GHC.HsToCore.Pmc.Ppr (pprUncovered)


instance Diagnostic DsMessage where
  diagnosticMessage = \case
    DsUnknownMessage m
      -> diagnosticMessage m
    DsEmptyEnumeration
      -> mkSimpleDecorated $ text "Enumeration is empty"
    DsIdentitiesFound conv_fn type_of_conv
      -> mkSimpleDecorated $
           vcat [ text "Call of" <+> ppr conv_fn <+> dcolon <+> ppr type_of_conv
                , nest 2 $ text "can probably be omitted"
                ]
    DsOverflowedLiterals i tc bounds _possiblyUsingNegativeLiterals
      -> let msg = case bounds of
               Nothing
                 -> vcat [ text "Literal" <+> integer i
                       <+> text "is negative but" <+> ppr tc
                       <+> text "only supports positive numbers"
                         ]
               Just (MinBound minB, MaxBound maxB)
                 -> vcat [ text "Literal" <+> integer i
                                 <+> text "is out of the" <+> ppr tc <+> text "range"
                                 <+> integer minB <> text ".." <> integer maxB
                         ]
         in mkSimpleDecorated msg
    DsRedundantBangPatterns ctx q
      -> mkSimpleDecorated $ pprEqn ctx q "has redundant bang"
    DsOverlappingPatterns ctx q
      -> mkSimpleDecorated $ pprEqn ctx q "is redundant"
    DsInaccessibleRhs ctx q
      -> mkSimpleDecorated $ pprEqn ctx q "has inaccessible right hand side"
    DsMaxPmCheckModelsReached limit
      -> mkSimpleDecorated $ vcat
           [ hang
               (text "Pattern match checker ran into -fmax-pmcheck-models="
                 <> int limit
                 <> text " limit, so")
               2
               (  bullet <+> text "Redundant clauses might not be reported at all"
               $$ bullet <+> text "Redundant clauses might be reported as inaccessible"
               $$ bullet <+> text "Patterns reported as unmatched might actually be matched")
           ]
    DsNonExhaustivePatterns kind _flag maxPatterns vars nablas
      -> mkSimpleDecorated $
           pprContext False kind (text "are non-exhaustive") $ \_ ->
             case vars of -- See #11245
                  [] -> text "Guards do not cover entire pattern space"
                  _  -> let us = map (\nabla -> pprUncovered nabla vars) nablas
                            pp_tys = pprQuotedList $ map idType vars
                        in  hang
                              (text "Patterns of type" <+> pp_tys <+> text "not matched:")
                              4
                              (vcat (take maxPatterns us) $$ dots maxPatterns us)
    DsTopLevelBindsNotAllowed bindsType bind
      -> let desc = case bindsType of
               UnliftedTypeBinds -> "bindings for unlifted types"
               StrictBinds       -> "strict bindings"
         in mkSimpleDecorated $
              hang (text "Top-level" <+> text desc <+> text "aren't allowed:") 2 (ppr bind)
    DsUselessSpecialiseForClassMethodSelector poly_id
      -> mkSimpleDecorated $
           text "Ignoring useless SPECIALISE pragma for NOINLINE function:" <+> quotes (ppr poly_id)
    DsUselessSpecialiseForNoInlineFunction poly_id
      -> mkSimpleDecorated $
          text "Ignoring useless SPECIALISE pragma for NOINLINE function:" <+> quotes (ppr poly_id)
    DsMultiplicityCoercionsNotSupported
      -> mkSimpleDecorated $ text "Multiplicity coercions are currently not supported"
    DsLevityPolyInExpr e prov
      -> let extra = case prov of
               LevityCheckHsExpr hsExpr -> ppr hsExpr
               LevityCheckWpFun doc     -> doc

         in mkSimpleDecorated $
               formatLevPolyErr (exprType e) $$ (text "In the type of expression:" <+> extra)
    DsLevityPolyInType ty prov
      -> let extra = case prov of
               LevityCheckInBinder v
                 -> text "In the type of binder" <+> quotes (ppr v)
               LevityCheckInVarType
                 -> text "When trying to create a variable of type:" <+> ppr ty
               LevityCheckInWildcardPattern
                 -> text "In a wildcard pattern"
               LevityCheckInUnboxedTuplePattern p
                 -> text "In the type of an element of an unboxed tuple pattern:" $$ ppr p
               LevityCheckGenSig
                 -> empty
        in mkSimpleDecorated $ formatLevPolyErr ty $$ extra

  diagnosticReason = \case
    DsUnknownMessage m          -> diagnosticReason m
    DsEmptyEnumeration          -> WarningWithFlag Opt_WarnEmptyEnumerations
    DsIdentitiesFound{}         -> WarningWithFlag Opt_WarnOverflowedLiterals
    DsOverflowedLiterals{}      -> WarningWithFlag Opt_WarnOverflowedLiterals
    DsRedundantBangPatterns{}   -> WarningWithFlag Opt_WarnRedundantBangPatterns
    DsOverlappingPatterns{}     -> WarningWithFlag Opt_WarnOverlappingPatterns
    DsInaccessibleRhs{}         -> WarningWithFlag Opt_WarnOverlappingPatterns
    DsMaxPmCheckModelsReached{} -> WarningWithoutFlag
    DsNonExhaustivePatterns _ (ExhaustivityCheckType mb_flag) _ _ _
      -> maybe WarningWithoutFlag WarningWithFlag mb_flag
    DsTopLevelBindsNotAllowed{} -> ErrorWithoutFlag
    DsUselessSpecialiseForClassMethodSelector{} -> WarningWithoutFlag
    DsUselessSpecialiseForNoInlineFunction{}    -> WarningWithoutFlag
    DsMultiplicityCoercionsNotSupported{}       -> ErrorWithoutFlag
    DsLevityPolyInExpr{}        -> ErrorWithoutFlag
    DsLevityPolyInType{}        -> ErrorWithoutFlag

  diagnosticHints  = \case
    DsUnknownMessage m          -> diagnosticHints m
    DsEmptyEnumeration          -> noHints
    DsIdentitiesFound{}         -> noHints
    DsOverflowedLiterals i _tc bounds usingNegLiterals
      -> case (bounds, usingNegLiterals) of
          (Just (MinBound minB, MaxBound _), NotUsingNegLiterals)
            | minB == -i -- Note [Suggest NegativeLiterals]
            , i > 0 -> [SuggestExtension LangExt.NegativeLiterals]
          _ -> noHints
    DsRedundantBangPatterns{}   -> noHints
    DsOverlappingPatterns{}     -> noHints
    DsInaccessibleRhs{}         -> noHints
    DsMaxPmCheckModelsReached{} -> [SuggestIncreaseMaxPmCheckModels]
    DsNonExhaustivePatterns{}   -> noHints
    DsTopLevelBindsNotAllowed{} -> noHints
    DsUselessSpecialiseForClassMethodSelector{} -> noHints
    DsUselessSpecialiseForNoInlineFunction{}    -> noHints
    DsMultiplicityCoercionsNotSupported         -> noHints
    DsLevityPolyInExpr{}        -> noHints
    DsLevityPolyInType{}        -> noHints

--
-- Helper functions
--

-- Print a single clause (for redundant/with-inaccessible-rhs)
pprEqn :: HsMatchContext GhcRn -> SDoc -> String -> SDoc
pprEqn ctx q txt = pprContext True ctx (text txt) $ \f ->
  f (q <+> matchSeparator ctx <+> text "...")

pprContext :: Bool -> HsMatchContext GhcRn -> SDoc -> ((SDoc -> SDoc) -> SDoc) -> SDoc
pprContext singular kind msg rest_of_msg_fun
  = vcat [text txt <+> msg,
          sep [ text "In" <+> ppr_match <> char ':'
              , nest 4 (rest_of_msg_fun pref)]]
  where
    txt | singular  = "Pattern match"
        | otherwise = "Pattern match(es)"

    (ppr_match, pref)
        = case kind of
             FunRhs { mc_fun = L _ fun }
                  -> (pprMatchContext kind, \ pp -> ppr fun <+> pp)
             _    -> (pprMatchContext kind, \ pp -> pp)

dots :: Int -> [a] -> SDoc
dots maxPatterns qs
    | qs `lengthExceeds` maxPatterns = text "..."
    | otherwise                      = empty
