{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic DsMessage

module GHC.HsToCore.Errors.Ppr where

import GHC.Core.Predicate (isEvVar)
import GHC.Core.Type
import GHC.Driver.Flags
import GHC.Hs
import GHC.HsToCore.Errors.Types
import GHC.Prelude
import GHC.Types.Basic (pprRuleName)
import GHC.Types.Error
import GHC.Types.Error.Codes
import GHC.Types.Id (idType)
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Utils.Outputable
import qualified GHC.LanguageExtensions as LangExt
import GHC.HsToCore.Pmc.Ppr


instance Diagnostic DsMessage where
  type DiagnosticOpts DsMessage = NoDiagnosticOpts
  diagnosticMessage opts = \case
    DsUnknownMessage (UnknownDiagnostic f _ m)
      -> diagnosticMessage (f opts) m
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
    DsUselessSpecialisePragma poly_nm is_dfun rea ->
      mkSimpleDecorated $
        vcat [ what <+> pragma <+> text "pragma" <> why
             , additional ]
      where
        quoted_nm = quotes (ppr poly_nm)
        what
          | uselessSpecialisePragmaKeepAnyway rea
          = text "Dubious"
          | otherwise
          = text "Ignoring useless"
        pragma = if is_dfun
                 then text "SPECIALISE instance"
                 else text "SPECIALISE"
        why = case rea of
          UselessSpecialiseForClassMethodSelector ->
            text " for class selector:" <+> quoted_nm
          UselessSpecialiseForNoInlineFunction ->
            text " for NOINLINE function:" <+> quoted_nm
          UselessSpecialiseNoSpecialisation ->
            -- Omit the Name for a DFunId, as it will be internal and not
            -- very illuminating to users who don't know what a DFunId is.
            (if is_dfun then empty else text " for" <+> quoted_nm) <> dot

        additional
          | uselessSpecialisePragmaKeepAnyway rea
          = -- No specialisation happening, but the pragma may still be useful.
            -- For example (#25389):
            --
            --   data G a where { G1 :: G Int, G2 :: G Bool }
            --   f :: G a -> a
            --   f G1 = <branch1>; f G2 = <branch2>
            --   {-# SPECIALISE f :: G Int -> Int #-}
            --     -- In $sf, we get rid of dead code in <branch2>
            vcat
              [ text "The pragma does not specialise away any class dictionaries,"
              , text "and neither is there any value specialisation."
              ]
          | otherwise
          = empty
    DsOrphanRule rule
      -> mkSimpleDecorated $ text "Orphan rule:" <+> ppr rule
    DsRuleLhsTooComplicated orig_lhs lhs2
      -> mkSimpleDecorated $
           hang (text "RULE left-hand side too complicated to desugar")
                      2 (vcat [ text "Optimised lhs:" <+> ppr lhs2
                              , text "Orig lhs:" <+> ppr orig_lhs])
    DsRuleIgnoredDueToConstructor con
      -> mkSimpleDecorated $ vcat
           [ text "A constructor," <+> ppr con <>
               text ", appears as outermost match in RULE lhs."
           , text "This rule will be ignored." ]
    DsRuleBindersNotBound unbound orig_bndrs orig_lhs lhs2
      -> mkSimpleDecorated $ vcat (map pp_dead unbound)
         where
           pp_dead bndr =
             hang (sep [ text "Forall'd" <+> pp_bndr bndr
                       , text "is not bound in RULE lhs"])
                2 (vcat [ text "Orig bndrs:" <+> ppr orig_bndrs
                        , text "Orig lhs:" <+> ppr orig_lhs
                        , text "Optimised lhs:" <+> ppr lhs2 ])

           pp_bndr b
            | isTyVar b = text "type variable" <+> quotes (ppr b)
            | isEvVar b = text "constraint"    <+> quotes (ppr (varType b))
            | otherwise = text "variable"      <+> quotes (ppr b)
    DsLazyPatCantBindVarsOfUnliftedType unlifted_bndrs
      -> mkSimpleDecorated $
          hang (text "A lazy (~) pattern cannot bind variables of unlifted type." $$
                text "Unlifted variables:")
             2 (vcat (map (\id -> ppr id <+> dcolon <+> ppr (idType id)) unlifted_bndrs))
    DsNotYetHandledByTH reason
      -> case reason of
             ThAmbiguousRecordUpdates fld
               -> mkMsg "Ambiguous record updates" (ppr fld)
             ThAbstractClosedTypeFamily decl
               -> mkMsg "abstract closed type family" (ppr decl)
             ThForeignLabel cls
               -> mkMsg "Foreign label" (doubleQuotes (ppr cls))
             ThForeignExport decl
               -> mkMsg "Foreign export" (ppr decl)
             ThMinimalPragmas
               -> mkMsg "MINIMAL pragmas" empty
             ThSCCPragmas
               -> mkMsg "SCC pragmas" empty
             ThNoUserInline
               -> mkMsg "NOUSERINLINE" empty
             ThExoticFormOfType ty
               -> mkMsg "Exotic form of type" (ppr ty)
             ThAmbiguousRecordSelectors e
               -> mkMsg "Ambiguous record selectors" (ppr e)
             ThMonadComprehensionSyntax e
               -> mkMsg "monad comprehension and [: :]" (ppr e)
             ThCostCentres e
               -> mkMsg "Cost centres" (ppr e)
             ThExpressionForm e
               -> mkMsg "Expression form" (ppr e)
             ThExoticStatement other
               -> mkMsg "Exotic statement" (ppr other)
             ThExoticLiteral lit
               -> mkMsg "Exotic literal" (ppr lit)
             ThExoticPattern pat
               -> mkMsg "Exotic pattern" (ppr pat)
             ThGuardedLambdas m
               -> mkMsg "Guarded lambdas" (pprMatch m)
             ThNegativeOverloadedPatterns pat
               -> mkMsg "Negative overloaded patterns" (ppr pat)
             ThHaddockDocumentation
               -> mkMsg "Haddock documentation" empty
             ThWarningAndDeprecationPragmas decl
               -> mkMsg "WARNING and DEPRECATION pragmas" $
                    text "Pragma for declaration of" <+> ppr decl
             ThSplicesWithinDeclBrackets
               -> mkMsg "Splices within declaration brackets" empty
             ThNonLinearDataCon
               -> mkMsg "Non-linear fields in data constructors" empty
         where
           mkMsg what doc =
             mkSimpleDecorated $
               hang (text what <+> text "not (yet) handled by Template Haskell") 2 doc
    DsAggregatedViewExpressions views
      -> mkSimpleDecorated (vcat msgs)
         where
           msgs = map (\g -> text "Putting these view expressions into the same case:" <+> (ppr g)) views
    DsUnbangedStrictPatterns bind
      -> mkSimpleDecorated $
           hang (text "Pattern bindings containing unlifted types should use" $$
                 text "an outermost bang pattern:")
              2 (ppr bind)
    DsCannotMixPolyAndUnliftedBindings bind
      -> mkSimpleDecorated $
           hang (text "You can't mix polymorphic and unlifted bindings:")
              2 (ppr bind)
    DsWrongDoBind _rhs elt_ty
      -> mkSimpleDecorated $ badMonadBind elt_ty
    DsUnusedDoBind _rhs elt_ty
      -> mkSimpleDecorated $ badMonadBind elt_ty
    DsRecBindsNotAllowedForUnliftedTys binds
      -> mkSimpleDecorated $
           hang (text "Recursive bindings for unlifted types aren't allowed:")
              2 (vcat (map ppr binds))
    DsRuleMightInlineFirst rule_name lhs_id _
      -> mkSimpleDecorated $
           vcat [ hang (text "Rule" <+> pprRuleName rule_name
                          <+> text "may never fire")
                       2 (text "because" <+> quotes (ppr lhs_id)
                          <+> text "might inline first")
                ]
    DsAnotherRuleMightFireFirst rule_name bad_rule lhs_id
      -> mkSimpleDecorated $
           vcat [ hang (text "Rule" <+> pprRuleName rule_name
                          <+> text "may never fire")
                       2 (text "because rule" <+> pprRuleName bad_rule
                          <+> text "for"<+> quotes (ppr lhs_id)
                          <+> text "might fire first")
                ]
    DsIncompleteRecordSelector name cons maxCons -> mkSimpleDecorated $
      hang (text "Selecting the record field" <+> quotes (ppr name)
              <+> text "may fail for the following constructors:")
           2
           (hsep $ punctuate comma $
            map ppr (take maxCons cons) ++ [ text "..." | lengthExceeds cons maxCons ])

  diagnosticReason = \case
    DsUnknownMessage m          -> diagnosticReason m
    DsEmptyEnumeration          -> WarningWithFlag Opt_WarnEmptyEnumerations
    DsIdentitiesFound{}         -> WarningWithFlag Opt_WarnIdentities
    DsOverflowedLiterals{}      -> WarningWithFlag Opt_WarnOverflowedLiterals
    DsRedundantBangPatterns{}   -> WarningWithFlag Opt_WarnRedundantBangPatterns
    DsOverlappingPatterns{}     -> WarningWithFlag Opt_WarnOverlappingPatterns
    DsInaccessibleRhs{}         -> WarningWithFlag Opt_WarnOverlappingPatterns
    DsMaxPmCheckModelsReached{} -> WarningWithoutFlag
    DsNonExhaustivePatterns _ (ExhaustivityCheckType mb_flag) _ _ _
      -> maybe WarningWithoutFlag WarningWithFlag mb_flag
    DsTopLevelBindsNotAllowed{}                 -> ErrorWithoutFlag
    DsUselessSpecialisePragma{}                 -> WarningWithFlag Opt_WarnUselessSpecialisations
    DsOrphanRule{}                              -> WarningWithFlag Opt_WarnOrphans
    DsRuleLhsTooComplicated{}                   -> WarningWithoutFlag
    DsRuleIgnoredDueToConstructor{}             -> WarningWithoutFlag
    DsRuleBindersNotBound{}                     -> WarningWithoutFlag
    DsLazyPatCantBindVarsOfUnliftedType{}       -> ErrorWithoutFlag
    DsNotYetHandledByTH{}                       -> ErrorWithoutFlag
    DsAggregatedViewExpressions{}               -> WarningWithoutFlag
    DsUnbangedStrictPatterns{}                  -> WarningWithFlag Opt_WarnUnbangedStrictPatterns
    DsCannotMixPolyAndUnliftedBindings{}        -> ErrorWithoutFlag
    DsWrongDoBind{}                             -> WarningWithFlag Opt_WarnWrongDoBind
    DsUnusedDoBind{}                            -> WarningWithFlag Opt_WarnUnusedDoBind
    DsRecBindsNotAllowedForUnliftedTys{}        -> ErrorWithoutFlag
    DsRuleMightInlineFirst{}                    -> WarningWithFlag Opt_WarnInlineRuleShadowing
    DsAnotherRuleMightFireFirst{}               -> WarningWithFlag Opt_WarnInlineRuleShadowing
    DsIncompleteRecordSelector{}                -> WarningWithFlag Opt_WarnIncompleteRecordSelectors

  diagnosticHints = \case
    DsUnknownMessage m          -> diagnosticHints m
    DsEmptyEnumeration          -> noHints
    DsIdentitiesFound{}         -> noHints
    DsOverflowedLiterals i _tc bounds usingNegLiterals
      -> case (bounds, usingNegLiterals) of
          (Just (MinBound minB, MaxBound _), NotUsingNegLiterals)
            | minB == -i -- Note [Suggest NegativeLiterals]
            , i > 0
            -> [ suggestExtensionWithInfo (text "If you are trying to write a large negative literal")
                                          LangExt.NegativeLiterals ]
          _ -> noHints
    DsRedundantBangPatterns{}                   -> noHints
    DsOverlappingPatterns{}                     -> noHints
    DsInaccessibleRhs{}                         -> noHints
    DsMaxPmCheckModelsReached{}                 -> [SuggestIncreaseMaxPmCheckModels]
    DsNonExhaustivePatterns{}                   -> noHints
    DsTopLevelBindsNotAllowed{}                 -> noHints
    DsUselessSpecialisePragma{}                 -> noHints
    DsOrphanRule{}                              -> noHints
    DsRuleLhsTooComplicated{}                   -> noHints
    DsRuleIgnoredDueToConstructor{}             -> noHints
    DsRuleBindersNotBound{}                     -> noHints
    DsLazyPatCantBindVarsOfUnliftedType{}       -> noHints
    DsNotYetHandledByTH{}                       -> noHints
    DsAggregatedViewExpressions{}               -> noHints
    DsUnbangedStrictPatterns{}                  -> noHints
    DsCannotMixPolyAndUnliftedBindings{}        -> [SuggestAddTypeSignatures UnnamedBinding]
    DsWrongDoBind rhs _                         -> [SuggestBindToWildcard rhs]
    DsUnusedDoBind rhs _                        -> [SuggestBindToWildcard rhs]
    DsRecBindsNotAllowedForUnliftedTys{}        -> noHints
    DsRuleMightInlineFirst _ lhs_id rule_act    -> [SuggestAddInlineOrNoInlinePragma lhs_id rule_act]
    DsAnotherRuleMightFireFirst _ bad_rule _    -> [SuggestAddPhaseToCompetingRule bad_rule]
    DsIncompleteRecordSelector{}                -> noHints

  diagnosticCode = constructorCode @GHC

{-
Note [Suggest NegativeLiterals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If you write
  x :: Int8
  x = -128
it'll parse as (negate 128), and overflow.  In this case, suggest NegativeLiterals.
We get an erroneous suggestion for
  x = 128
but perhaps that does not matter too much.
-}

--
-- Helper functions
--

badMonadBind :: Type -> SDoc
badMonadBind elt_ty
  = hang (text "A do-notation statement discarded a result of type")
       2 (quotes (ppr elt_ty))

-- Print a single clause (for redundant/with-inaccessible-rhs)
pprEqn :: HsMatchContextRn -> SDoc -> String -> SDoc
pprEqn ctx q txt = pprContext True ctx (text txt) $ \f ->
  f (q <+> matchSeparator ctx <+> text "...")

pprContext :: Bool -> HsMatchContextRn -> SDoc -> ((SDoc -> SDoc) -> SDoc) -> SDoc
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
