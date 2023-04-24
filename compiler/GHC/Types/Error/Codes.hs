{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines diagnostic codes for the diagnostics emitted by GHC.
--
-- A diagnostic code is a numeric unique identifier for a diagnostic.
-- See Note [Diagnostic codes].
module GHC.Types.Error.Codes
  ( constructorCode )
  where

import GHC.Prelude
import GHC.Types.Error  ( DiagnosticCode(..), UnknownDiagnostic (..), diagnosticCode )

import GHC.Hs.Extension ( GhcRn )

import GHC.Driver.Errors.Types   ( DriverMessage )
import GHC.Parser.Errors.Types   ( PsMessage, PsHeaderMessage )
import GHC.HsToCore.Errors.Types ( DsMessage )
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.TcType      ( IllegalForeignTypeReason, TypeCannotBeMarshaledReason )
import GHC.Unit.Module.Warnings ( WarningTxt )
import GHC.Utils.Panic.Plain

import Data.Kind    ( Type, Constraint )
import GHC.Exts     ( proxy# )
import GHC.Generics
import GHC.TypeLits ( Symbol, TypeError, ErrorMessage(..) )
import GHC.TypeNats ( Nat, KnownNat, natVal' )
import GHC.Core.InstEnv (LookupInstanceErrReason)
import GHC.Iface.Errors.Types

{- Note [Diagnostic codes]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Every time a new diagnostic (error or warning) is introduced to GHC,
it is assigned a new numeric code, which has never been used before.

To ensure uniqueness across GHC versions, we proceed as follows:

  - all diagnostic codes are defined in a single module, GHC.Types.Error.Codes.
  - uniqueness of diagnostic codes is ensured by the use of an injective type family,
    GhcDiagnosticCode,
  - a diagnostic code never gets deleted from the GhcDiagnosticCode type family
    in GHC.Types.Error.Codes, even if it is no longer used.
    Older versions of GHC might still display the code, and we don't want that
    old code to get confused with the error code of a different, new, error message.

[Instructions for adding a new diagnostic code]

  After adding a constructor to a diagnostic datatype, such as PsMessage,
  TcRnMessage, DsMessage or DriverMessage, you can add corresponding
  diagnostic codes as follows:

    a. To give a single diagnostic code to the constructor, simply add a
       type family equation to GHC.Error.Codes.GhcDiagnosticCode, e.g.:

         GhcDiagnosticCode "MyNewErrorConstructor" = 12345

       You can obtain new randomly-generated error codes by using
       https://www.random.org/integers/?num=10&min=1&max=99999&col=1&base=10&format=plain.

       You will get a type error if you try to use an error code that is already
       used by another constructor.

    b. If you instead require more granular diagnostic codes, add a type family
       equation to GHC.Error.Codes.ConRecursInto, specifying which argument
       to recur into to obtain an diagnostic code.

       For example, the 'TcRnCannotDeriveInstance' constructor is associated
       with several diagnostic codes, depending on the value of the argument of
       type 'DeriveInstanceErrReason'. This is achieved as follows:

         - The equation
              ConRecursInto "TcRnCannotDeriveInstance" = 'Just DeriveInstanceErrReason
           says to recur into the argument of type 'DeriveInstanceErrReason'
           to get a diagnostic code.

        - The equations
              GhcDiagnosticCode "DerivErrNotWellKinded"          = 62016
              GhcDiagnosticCode "DerivErrSafeHaskellGenericInst" = 07214
              GhcDiagnosticCode "DerivErrDerivingViaWrongKind"   = 63174
              ...
          give the diagnostic codes for the various constructors of DeriveInstanceErrReason.
          These are added following the procedure in (a).

  Never remove a return value from the 'GhcDiagnosticCode' type family!
  Outdated error messages must still be tracked to ensure uniqueness
  of diagnostic codes across GHC versions.
-}

{- *********************************************************************
*                                                                      *
                 The GhcDiagnosticCode type family
*                                                                      *
********************************************************************* -}

-- | This function obtain a diagnostic code by looking up the constructor
-- name using generics, and using the 'GhcDiagnosticCode' type family.
constructorCode :: (Generic diag, GDiagnosticCode (Rep diag))
                => diag -> Maybe DiagnosticCode
constructorCode diag = gdiagnosticCode (from diag)

-- | Type family computing the numeric diagnostic code for a given error message constructor.
--
-- Its injectivity annotation ensures uniqueness of error codes.
--
-- Never remove a return value from this type family! Outdated error messages must still
-- be tracked here to ensure uniqueness of diagnostic codes across GHC versions.
--
-- See Note [Diagnostic codes] in GHC.Types.Error.
type GhcDiagnosticCode :: Symbol -> Nat
type family GhcDiagnosticCode c = n | n -> c where

  -- Desugarer diagnostic codes
  GhcDiagnosticCode "DsEmptyEnumeration"                            = 10190
  GhcDiagnosticCode "DsIdentitiesFound"                             = 04214
  GhcDiagnosticCode "DsOverflowedLiterals"                          = 97441
  GhcDiagnosticCode "DsRedundantBangPatterns"                       = 38520
  GhcDiagnosticCode "DsOverlappingPatterns"                         = 53633
  GhcDiagnosticCode "DsInaccessibleRhs"                             = 94210
  GhcDiagnosticCode "DsMaxPmCheckModelsReached"                     = 61505
  GhcDiagnosticCode "DsNonExhaustivePatterns"                       = 62161
  GhcDiagnosticCode "DsTopLevelBindsNotAllowed"                     = 48099
  GhcDiagnosticCode "DsUselessSpecialiseForClassMethodSelector"     = 93315
  GhcDiagnosticCode "DsUselessSpecialiseForNoInlineFunction"        = 38524
  GhcDiagnosticCode "DsMultiplicityCoercionsNotSupported"           = 59840
  GhcDiagnosticCode "DsOrphanRule"                                  = 58181
  GhcDiagnosticCode "DsRuleLhsTooComplicated"                       = 69441
  GhcDiagnosticCode "DsRuleIgnoredDueToConstructor"                 = 00828
  GhcDiagnosticCode "DsRuleBindersNotBound"                         = 40548
  GhcDiagnosticCode "DsLazyPatCantBindVarsOfUnliftedType"           = 17879
  GhcDiagnosticCode "DsNotYetHandledByTH"                           = 65904
  GhcDiagnosticCode "DsAggregatedViewExpressions"                   = 19551
  GhcDiagnosticCode "DsUnbangedStrictPatterns"                      = 21030
  GhcDiagnosticCode "DsCannotMixPolyAndUnliftedBindings"            = 20036
  GhcDiagnosticCode "DsWrongDoBind"                                 = 08838
  GhcDiagnosticCode "DsUnusedDoBind"                                = 81995
  GhcDiagnosticCode "DsRecBindsNotAllowedForUnliftedTys"            = 20185
  GhcDiagnosticCode "DsRuleMightInlineFirst"                        = 95396
  GhcDiagnosticCode "DsAnotherRuleMightFireFirst"                   = 87502


  -- Parser diagnostic codes
  GhcDiagnosticCode "PsErrParseLanguagePragma"                      = 68686
  GhcDiagnosticCode "PsErrUnsupportedExt"                           = 46537
  GhcDiagnosticCode "PsErrParseOptionsPragma"                       = 24342
  GhcDiagnosticCode "PsErrUnknownOptionsPragma"                     = 04924
  GhcDiagnosticCode "PsWarnBidirectionalFormatChars"                = 03272
  GhcDiagnosticCode "PsWarnTab"                                     = 94817
  GhcDiagnosticCode "PsWarnTransitionalLayout"                      = 93617
  GhcDiagnosticCode "PsWarnOperatorWhitespaceExtConflict"           = 47082
  GhcDiagnosticCode "PsWarnOperatorWhitespace"                      = 40798
  GhcDiagnosticCode "PsWarnHaddockInvalidPos"                       = 94458
  GhcDiagnosticCode "PsWarnHaddockIgnoreMulti"                      = 05641
  GhcDiagnosticCode "PsWarnStarBinder"                              = 21887
  GhcDiagnosticCode "PsWarnStarIsType"                              = 39567
  GhcDiagnosticCode "PsWarnUnrecognisedPragma"                      = 42044
  GhcDiagnosticCode "PsWarnMisplacedPragma"                         = 28007
  GhcDiagnosticCode "PsWarnImportPreQualified"                      = 07924
  GhcDiagnosticCode "PsErrLexer"                                    = 21231
  GhcDiagnosticCode "PsErrCmmLexer"                                 = 75725
  GhcDiagnosticCode "PsErrCmmParser"                                = 09848
  GhcDiagnosticCode "PsErrParse"                                    = 58481
  GhcDiagnosticCode "PsErrTypeAppWithoutSpace"                      = 84077
  GhcDiagnosticCode "PsErrLazyPatWithoutSpace"                      = 27207
  GhcDiagnosticCode "PsErrBangPatWithoutSpace"                      = 95644
  GhcDiagnosticCode "PsErrInvalidInfixHole"                         = 45106
  GhcDiagnosticCode "PsErrExpectedHyphen"                           = 44524
  GhcDiagnosticCode "PsErrSpaceInSCC"                               = 76176
  GhcDiagnosticCode "PsErrEmptyDoubleQuotes"                        = 11861
  GhcDiagnosticCode "PsErrLambdaCase"                               = 51179
  GhcDiagnosticCode "PsErrEmptyLambda"                              = 71614
  GhcDiagnosticCode "PsErrLinearFunction"                           = 31574
  GhcDiagnosticCode "PsErrMultiWayIf"                               = 28985
  GhcDiagnosticCode "PsErrOverloadedRecordUpdateNotEnabled"         = 82135
  GhcDiagnosticCode "PsErrNumUnderscores"                           = 62330
  GhcDiagnosticCode "PsErrIllegalBangPattern"                       = 79767
  GhcDiagnosticCode "PsErrOverloadedRecordDotInvalid"               = 26832
  GhcDiagnosticCode "PsErrIllegalPatSynExport"                      = 89515
  GhcDiagnosticCode "PsErrOverloadedRecordUpdateNoQualifiedFields"  = 94863
  GhcDiagnosticCode "PsErrExplicitForall"                           = 25955
  GhcDiagnosticCode "PsErrIllegalQualifiedDo"                       = 40280
  GhcDiagnosticCode "PsErrQualifiedDoInCmd"                         = 54089
  GhcDiagnosticCode "PsErrRecordSyntaxInPatSynDecl"                 = 28021
  GhcDiagnosticCode "PsErrEmptyWhereInPatSynDecl"                   = 13248
  GhcDiagnosticCode "PsErrInvalidWhereBindInPatSynDecl"             = 24737
  GhcDiagnosticCode "PsErrNoSingleWhereBindInPatSynDecl"            = 65536
  GhcDiagnosticCode "PsErrDeclSpliceNotAtTopLevel"                  = 08451
  GhcDiagnosticCode "PsErrMultipleNamesInStandaloneKindSignature"   = 42569
  GhcDiagnosticCode "PsErrIllegalExplicitNamespace"                 = 47007
  GhcDiagnosticCode "PsErrUnallowedPragma"                          = 85314
  GhcDiagnosticCode "PsErrImportPostQualified"                      = 87491
  GhcDiagnosticCode "PsErrImportQualifiedTwice"                     = 05661
  GhcDiagnosticCode "PsErrIllegalImportBundleForm"                  = 81284
  GhcDiagnosticCode "PsErrInvalidRuleActivationMarker"              = 50396
  GhcDiagnosticCode "PsErrMissingBlock"                             = 16849
  GhcDiagnosticCode "PsErrUnsupportedBoxedSumExpr"                  = 09550
  GhcDiagnosticCode "PsErrUnsupportedBoxedSumPat"                   = 16863
  GhcDiagnosticCode "PsErrUnexpectedQualifiedConstructor"           = 73413
  GhcDiagnosticCode "PsErrTupleSectionInPat"                        = 09646
  GhcDiagnosticCode "PsErrOpFewArgs"                                = 24180
  GhcDiagnosticCode "PsErrVarForTyCon"                              = 18208
  GhcDiagnosticCode "PsErrMalformedEntityString"                    = 26204
  GhcDiagnosticCode "PsErrDotsInRecordUpdate"                       = 70712
  GhcDiagnosticCode "PsErrInvalidDataCon"                           = 46574
  GhcDiagnosticCode "PsErrInvalidInfixDataCon"                      = 30670
  GhcDiagnosticCode "PsErrIllegalPromotionQuoteDataCon"             = 80236
  GhcDiagnosticCode "PsErrUnpackDataCon"                            = 40845
  GhcDiagnosticCode "PsErrUnexpectedKindAppInDataCon"               = 83653
  GhcDiagnosticCode "PsErrInvalidRecordCon"                         = 08195
  GhcDiagnosticCode "PsErrIllegalUnboxedStringInPat"                = 69925
  GhcDiagnosticCode "PsErrIllegalUnboxedFloatingLitInPat"           = 76595
  GhcDiagnosticCode "PsErrDoNotationInPat"                          = 06446
  GhcDiagnosticCode "PsErrIfThenElseInPat"                          = 45696
  GhcDiagnosticCode "PsErrLambdaCaseInPat"                          = 07636
  GhcDiagnosticCode "PsErrCaseInPat"                                = 53786
  GhcDiagnosticCode "PsErrLetInPat"                                 = 78892
  GhcDiagnosticCode "PsErrLambdaInPat"                              = 00482
  GhcDiagnosticCode "PsErrArrowExprInPat"                           = 04584
  GhcDiagnosticCode "PsErrArrowCmdInPat"                            = 98980
  GhcDiagnosticCode "PsErrArrowCmdInExpr"                           = 66043
  GhcDiagnosticCode "PsErrViewPatInExpr"                            = 66228
  GhcDiagnosticCode "PsErrLambdaCmdInFunAppCmd"                     = 12178
  GhcDiagnosticCode "PsErrCaseCmdInFunAppCmd"                       = 92971
  GhcDiagnosticCode "PsErrLambdaCaseCmdInFunAppCmd"                 = 47171
  GhcDiagnosticCode "PsErrIfCmdInFunAppCmd"                         = 97005
  GhcDiagnosticCode "PsErrLetCmdInFunAppCmd"                        = 70526
  GhcDiagnosticCode "PsErrDoCmdInFunAppCmd"                         = 77808
  GhcDiagnosticCode "PsErrDoInFunAppExpr"                           = 52095
  GhcDiagnosticCode "PsErrMDoInFunAppExpr"                          = 67630
  GhcDiagnosticCode "PsErrLambdaInFunAppExpr"                       = 06074
  GhcDiagnosticCode "PsErrCaseInFunAppExpr"                         = 25037
  GhcDiagnosticCode "PsErrLambdaCaseInFunAppExpr"                   = 77182
  GhcDiagnosticCode "PsErrLetInFunAppExpr"                          = 90355
  GhcDiagnosticCode "PsErrIfInFunAppExpr"                           = 01239
  GhcDiagnosticCode "PsErrProcInFunAppExpr"                         = 04807
  GhcDiagnosticCode "PsErrMalformedTyOrClDecl"                      = 47568
  GhcDiagnosticCode "PsErrIllegalWhereInDataDecl"                   = 36952
  GhcDiagnosticCode "PsErrIllegalDataTypeContext"                   = 87429
  GhcDiagnosticCode "PsErrPrimStringInvalidChar"                    = 43080
  GhcDiagnosticCode "PsErrSuffixAT"                                 = 33856
  GhcDiagnosticCode "PsErrPrecedenceOutOfRange"                     = 25078
  GhcDiagnosticCode "PsErrSemiColonsInCondExpr"                     = 75254
  GhcDiagnosticCode "PsErrSemiColonsInCondCmd"                      = 18910
  GhcDiagnosticCode "PsErrAtInPatPos"                               = 08382
  GhcDiagnosticCode "PsErrParseErrorOnInput"                        = 66418
  GhcDiagnosticCode "PsErrMalformedDecl"                            = 85316
  GhcDiagnosticCode "PsErrUnexpectedTypeAppInDecl"                  = 45054
  GhcDiagnosticCode "PsErrNotADataCon"                              = 25742
  GhcDiagnosticCode "PsErrInferredTypeVarNotAllowed"                = 57342
  GhcDiagnosticCode "PsErrIllegalTraditionalRecordSyntax"           = 65719
  GhcDiagnosticCode "PsErrParseErrorInCmd"                          = 03790
  GhcDiagnosticCode "PsErrInPat"                                    = 07626
  GhcDiagnosticCode "PsErrIllegalRoleName"                          = 09009
  GhcDiagnosticCode "PsErrInvalidTypeSignature"                     = 94426
  GhcDiagnosticCode "PsErrUnexpectedTypeInDecl"                     = 77878
  GhcDiagnosticCode "PsErrInvalidPackageName"                       = 21926
  GhcDiagnosticCode "PsErrParseRightOpSectionInPat"                 = 72516
  GhcDiagnosticCode "PsErrIllegalGadtRecordMultiplicity"            = 37475
  GhcDiagnosticCode "PsErrInvalidCApiImport"                        = 72744
  GhcDiagnosticCode "PsErrMultipleConForNewtype"                    = 05380
  GhcDiagnosticCode "PsErrUnicodeCharLooksLike"                     = 31623

  -- Driver diagnostic codes
  GhcDiagnosticCode "DriverMissingHomeModules"                      = 32850
  GhcDiagnosticCode "DriverUnknownHiddenModules"                    = 38189
  GhcDiagnosticCode "DriverUnknownReexportedModules"                = 68286
  GhcDiagnosticCode "DriverUnusedPackages"                          = 42258
  GhcDiagnosticCode "DriverUnnecessarySourceImports"                = 88907
  GhcDiagnosticCode "DriverDuplicatedModuleDeclaration"             = 29235
  GhcDiagnosticCode "DriverModuleNotFound"                          = 82272
  GhcDiagnosticCode "DriverFileModuleNameMismatch"                  = 28623
  GhcDiagnosticCode "DriverUnexpectedSignature"                     = 66004
  GhcDiagnosticCode "DriverFileNotFound"                            = 49196
  GhcDiagnosticCode "DriverStaticPointersNotSupported"              = 77799
  GhcDiagnosticCode "DriverBackpackModuleNotFound"                  = 19971
  GhcDiagnosticCode "DriverUserDefinedRuleIgnored"                  = 56147
  GhcDiagnosticCode "DriverMixedSafetyImport"                       = 70172
  GhcDiagnosticCode "DriverCannotLoadInterfaceFile"                 = 37141
  GhcDiagnosticCode "DriverInferredSafeModule"                      = 58656
  GhcDiagnosticCode "DriverMarkedTrustworthyButInferredSafe"        = 19244
  GhcDiagnosticCode "DriverInferredSafeImport"                      = 82658
  GhcDiagnosticCode "DriverCannotImportUnsafeModule"                = 44360
  GhcDiagnosticCode "DriverMissingSafeHaskellMode"                  = 29747
  GhcDiagnosticCode "DriverPackageNotTrusted"                       = 08674
  GhcDiagnosticCode "DriverCannotImportFromUntrustedPackage"        = 75165
  GhcDiagnosticCode "DriverRedirectedNoMain"                        = 95379
  GhcDiagnosticCode "DriverHomePackagesNotClosed"                   = 03271

  -- Constraint solver diagnostic codes
  GhcDiagnosticCode "BadTelescope"                                  = 97739
  GhcDiagnosticCode "UserTypeError"                                 = 64725
  GhcDiagnosticCode "UnsatisfiableError"                            = 22250
  GhcDiagnosticCode "ReportHoleError"                               = 88464
  GhcDiagnosticCode "UntouchableVariable"                           = 34699
  GhcDiagnosticCode "FixedRuntimeRepError"                          = 55287
  GhcDiagnosticCode "BlockedEquality"                               = 06200
  GhcDiagnosticCode "ExpectingMoreArguments"                        = 81325
  GhcDiagnosticCode "UnboundImplicitParams"                         = 91416
  GhcDiagnosticCode "AmbiguityPreventsSolvingCt"                    = 78125
  GhcDiagnosticCode "CannotResolveInstance"                         = 39999
  GhcDiagnosticCode "OverlappingInstances"                          = 43085
  GhcDiagnosticCode "UnsafeOverlap"                                 = 36705

  -- Type mismatch errors
  GhcDiagnosticCode "BasicMismatch"                                 = 18872
  GhcDiagnosticCode "KindMismatch"                                  = 89223
  GhcDiagnosticCode "TypeEqMismatch"                                = 83865
  GhcDiagnosticCode "CouldNotDeduce"                                = 05617

  -- Variable unification errors
  GhcDiagnosticCode "CannotUnifyWithPolytype"                       = 91028
  GhcDiagnosticCode "OccursCheck"                                   = 27958
  GhcDiagnosticCode "SkolemEscape"                                  = 46956
  GhcDiagnosticCode "DifferentTyVars"                               = 25897
  GhcDiagnosticCode "RepresentationalEq"                            = 10283

  -- Typechecker/renamer diagnostic codes
  GhcDiagnosticCode "TcRnRedundantConstraints"                      = 30606
  GhcDiagnosticCode "TcRnInaccessibleCode"                          = 40564
  GhcDiagnosticCode "TcRnTypeDoesNotHaveFixedRuntimeRep"            = 18478
  GhcDiagnosticCode "TcRnImplicitLift"                              = 00846
  GhcDiagnosticCode "TcRnUnusedPatternBinds"                        = 61367
  GhcDiagnosticCode "TcRnDodgyExports"                              = 75356
  GhcDiagnosticCode "TcRnMissingImportList"                         = 77037
  GhcDiagnosticCode "TcRnUnsafeDueToPlugin"                         = 01687
  GhcDiagnosticCode "TcRnModMissingRealSrcSpan"                     = 84170
  GhcDiagnosticCode "TcRnIdNotExportedFromModuleSig"                = 44188
  GhcDiagnosticCode "TcRnIdNotExportedFromLocalSig"                 = 50058
  GhcDiagnosticCode "TcRnShadowedName"                              = 63397
  GhcDiagnosticCode "TcRnInvalidWarningCategory"                    = 53573
  GhcDiagnosticCode "TcRnDuplicateWarningDecls"                     = 00711
  GhcDiagnosticCode "TcRnSimplifierTooManyIterations"               = 95822
  GhcDiagnosticCode "TcRnIllegalPatSynDecl"                         = 82077
  GhcDiagnosticCode "TcRnLinearPatSyn"                              = 15172
  GhcDiagnosticCode "TcRnEmptyRecordUpdate"                         = 20825
  GhcDiagnosticCode "TcRnIllegalFieldPunning"                       = 44287
  GhcDiagnosticCode "TcRnIllegalWildcardsInRecord"                  = 37132
  GhcDiagnosticCode "TcRnIllegalWildcardInType"                     = 65507
  GhcDiagnosticCode "TcRnDuplicateFieldName"                        = 85524
  GhcDiagnosticCode "TcRnIllegalViewPattern"                        = 22406
  GhcDiagnosticCode "TcRnCharLiteralOutOfRange"                     = 17268
  GhcDiagnosticCode "TcRnIllegalWildcardsInConstructor"             = 47217
  GhcDiagnosticCode "TcRnIgnoringAnnotations"                       = 66649
  GhcDiagnosticCode "TcRnAnnotationInSafeHaskell"                   = 68934
  GhcDiagnosticCode "TcRnInvalidTypeApplication"                    = 95781
  GhcDiagnosticCode "TcRnTagToEnumMissingValArg"                    = 36495
  GhcDiagnosticCode "TcRnTagToEnumUnspecifiedResTy"                 = 08522
  GhcDiagnosticCode "TcRnTagToEnumResTyNotAnEnum"                   = 49356
  GhcDiagnosticCode "TcRnTagToEnumResTyTypeData"                    = 96189
  GhcDiagnosticCode "TcRnArrowIfThenElsePredDependsOnResultTy"      = 55868
  GhcDiagnosticCode "TcRnIllegalHsBootFileDecl"                     = 58195
  GhcDiagnosticCode "TcRnRecursivePatternSynonym"                   = 72489
  GhcDiagnosticCode "TcRnPartialTypeSigTyVarMismatch"               = 88793
  GhcDiagnosticCode "TcRnPartialTypeSigBadQuantifier"               = 94185
  GhcDiagnosticCode "TcRnMissingSignature"                          = 38417
  GhcDiagnosticCode "TcRnPolymorphicBinderMissingSig"               = 64414
  GhcDiagnosticCode "TcRnOverloadedSig"                             = 16675
  GhcDiagnosticCode "TcRnTupleConstraintInst"                       = 69012
  GhcDiagnosticCode "TcRnAbstractClassInst"                         = 51758
  GhcDiagnosticCode "TcRnNoClassInstHead"                           = 56538
  GhcDiagnosticCode "TcRnUserTypeError"                             = 47403
  GhcDiagnosticCode "TcRnConstraintInKind"                          = 01259
  GhcDiagnosticCode "TcRnUnboxedTupleOrSumTypeFuncArg"              = 19590
  GhcDiagnosticCode "TcRnLinearFuncInKind"                          = 13218
  GhcDiagnosticCode "TcRnForAllEscapeError"                         = 31147
  GhcDiagnosticCode "TcRnVDQInTermType"                             = 51580
  GhcDiagnosticCode "TcRnBadQuantPredHead"                          = 02550
  GhcDiagnosticCode "TcRnIllegalTupleConstraint"                    = 77539
  GhcDiagnosticCode "TcRnNonTypeVarArgInConstraint"                 = 80003
  GhcDiagnosticCode "TcRnIllegalImplicitParam"                      = 75863
  GhcDiagnosticCode "TcRnIllegalConstraintSynonymOfKind"            = 75844
  GhcDiagnosticCode "TcRnIllegalClassInst"                          = 53946
  GhcDiagnosticCode "TcRnOversaturatedVisibleKindArg"               = 45474
  GhcDiagnosticCode "TcRnBadAssociatedType"                         = 38351
  GhcDiagnosticCode "TcRnForAllRankErr"                             = 91510
  GhcDiagnosticCode "TcRnMonomorphicBindings"                       = 55524
  GhcDiagnosticCode "TcRnOrphanInstance"                            = 90177
  GhcDiagnosticCode "TcRnFunDepConflict"                            = 46208
  GhcDiagnosticCode "TcRnDupInstanceDecls"                          = 59692
  GhcDiagnosticCode "TcRnConflictingFamInstDecls"                   = 34447
  GhcDiagnosticCode "TcRnFamInstNotInjective"                       = 05175
  GhcDiagnosticCode "TcRnBangOnUnliftedType"                        = 55666
  GhcDiagnosticCode "TcRnLazyBangOnUnliftedType"                    = 71444
  GhcDiagnosticCode "TcRnMultipleDefaultDeclarations"               = 99565
  GhcDiagnosticCode "TcRnBadDefaultType"                            = 88933
  GhcDiagnosticCode "TcRnPatSynBundledWithNonDataCon"               = 66775
  GhcDiagnosticCode "TcRnPatSynBundledWithWrongType"                = 66025
  GhcDiagnosticCode "TcRnDupeModuleExport"                          = 51876
  GhcDiagnosticCode "TcRnExportedModNotImported"                    = 90973
  GhcDiagnosticCode "TcRnNullExportedModule"                        = 64649
  GhcDiagnosticCode "TcRnMissingExportList"                         = 85401
  GhcDiagnosticCode "TcRnExportHiddenComponents"                    = 94558
  GhcDiagnosticCode "TcRnDuplicateExport"                           = 47854
  GhcDiagnosticCode "TcRnExportedParentChildMismatch"               = 88993
  GhcDiagnosticCode "TcRnConflictingExports"                        = 69158
  GhcDiagnosticCode "TcRnDuplicateFieldExport"                      = 97219
  GhcDiagnosticCode "TcRnAmbiguousFieldInUpdate"                    = 56428
  GhcDiagnosticCode "TcRnAmbiguousRecordUpdate"                     = 02256
  GhcDiagnosticCode "TcRnMissingFields"                             = 20125
  GhcDiagnosticCode "TcRnFieldUpdateInvalidType"                    = 63055
  GhcDiagnosticCode "TcRnMissingStrictFields"                       = 95909
  GhcDiagnosticCode "TcRnStaticFormNotClosed"                       = 88431
  GhcDiagnosticCode "TcRnUselessTypeable"                           = 90584
  GhcDiagnosticCode "TcRnDerivingDefaults"                          = 20042
  GhcDiagnosticCode "TcRnNonUnaryTypeclassConstraint"               = 73993
  GhcDiagnosticCode "TcRnPartialTypeSignatures"                     = 60661
  GhcDiagnosticCode "TcRnLazyGADTPattern"                           = 87005
  GhcDiagnosticCode "TcRnArrowProcGADTPattern"                      = 64525
  GhcDiagnosticCode "TcRnSpecialClassInst"                          = 97044
  GhcDiagnosticCode "TcRnForallIdentifier"                          = 64088
  GhcDiagnosticCode "TcRnTypeEqualityOutOfScope"                    = 12003
  GhcDiagnosticCode "TcRnTypeEqualityRequiresOperators"             = 58520
  GhcDiagnosticCode "TcRnIllegalTypeOperator"                       = 62547
  GhcDiagnosticCode "TcRnGADTMonoLocalBinds"                        = 58008
  GhcDiagnosticCode "TcRnIncorrectNameSpace"                        = 31891
  GhcDiagnosticCode "TcRnNoRebindableSyntaxRecordDot"               = 65945
  GhcDiagnosticCode "TcRnNoFieldPunsRecordDot"                      = 57365
  GhcDiagnosticCode "TcRnIllegalStaticExpression"                   = 23800
  GhcDiagnosticCode "TcRnIllegalStaticFormInSplice"                 = 12219
  GhcDiagnosticCode "TcRnListComprehensionDuplicateBinding"         = 81232
  GhcDiagnosticCode "TcRnLastStmtNotExpr"                           = 55814
  GhcDiagnosticCode "TcRnUnexpectedStatementInContext"              = 42026
  GhcDiagnosticCode "TcRnSectionWithoutParentheses"                 = 95880
  GhcDiagnosticCode "TcRnIllegalImplicitParameterBindings"          = 50730
  GhcDiagnosticCode "TcRnIllegalTupleSection"                       = 59155
  GhcDiagnosticCode "TcRnTermNameInType"                            = 37479
  GhcDiagnosticCode "TcRnUnexpectedKindVar"                         = 12875
  GhcDiagnosticCode "TcRnNegativeNumTypeLiteral"                    = 93632
  GhcDiagnosticCode "TcRnUnusedQuantifiedTypeVar"                   = 54180

  GhcDiagnosticCode "TcRnUntickedPromotedThing"                     = 49957
  GhcDiagnosticCode "TcRnIllegalBuiltinSyntax"                      = 39716
  GhcDiagnosticCode "TcRnWarnDefaulting"                            = 18042
  GhcDiagnosticCode "TcRnForeignImportPrimExtNotSet"                = 49692
  GhcDiagnosticCode "TcRnForeignImportPrimSafeAnn"                  = 26133
  GhcDiagnosticCode "TcRnForeignFunctionImportAsValue"              = 76251
  GhcDiagnosticCode "TcRnFunPtrImportWithoutAmpersand"              = 57989
  GhcDiagnosticCode "TcRnIllegalForeignDeclBackend"                 = 03355
  GhcDiagnosticCode "TcRnUnsupportedCallConv"                       = 01245
  GhcDiagnosticCode "TcRnInvalidCIdentifier"                        = 95774
  GhcDiagnosticCode "TcRnExpectedValueId"                           = 01570
  GhcDiagnosticCode "TcRnRecSelectorEscapedTyVar"                   = 55876
  GhcDiagnosticCode "TcRnPatSynNotBidirectional"                    = 16444
  GhcDiagnosticCode "TcRnSplicePolymorphicLocalVar"                 = 06568
  GhcDiagnosticCode "TcRnIllegalDerivingItem"                       = 11913
  GhcDiagnosticCode "TcRnUnexpectedAnnotation"                      = 18932
  GhcDiagnosticCode "TcRnIllegalRecordSyntax"                       = 89246
  GhcDiagnosticCode "TcRnUnexpectedTypeSplice"                      = 39180
  GhcDiagnosticCode "TcRnInvalidVisibleKindArgument"                = 20967
  GhcDiagnosticCode "TcRnTooManyBinders"                            = 05989
  GhcDiagnosticCode "TcRnDifferentNamesForTyVar"                    = 17370
  GhcDiagnosticCode "TcRnInvalidReturnKind"                         = 55233
  GhcDiagnosticCode "TcRnClassKindNotConstraint"                    = 80768
  GhcDiagnosticCode "TcRnUnpromotableThing"                         = 88634
  GhcDiagnosticCode "TcRnMatchesHaveDiffNumArgs"                    = 91938
  GhcDiagnosticCode "TcRnCannotBindScopedTyVarInPatSig"             = 46131
  GhcDiagnosticCode "TcRnCannotBindTyVarsInPatBind"                 = 48361
  GhcDiagnosticCode "TcRnTooManyTyArgsInConPattern"                 = 01629
  GhcDiagnosticCode "TcRnMultipleInlinePragmas"                     = 96665
  GhcDiagnosticCode "TcRnUnexpectedPragmas"                         = 88293
  GhcDiagnosticCode "TcRnNonOverloadedSpecialisePragma"             = 35827
  GhcDiagnosticCode "TcRnSpecialiseNotVisible"                      = 85337
  GhcDiagnosticCode "TcRnIllegalTypeOperatorDecl"                   = 50649
  GhcDiagnosticCode "TcRnBindVarAlreadyInScope"                     = 69710
  GhcDiagnosticCode "TcRnBindMultipleVariables"                     = 92957
  GhcDiagnosticCode "TcRnIllegalKind"                               = 64861
  GhcDiagnosticCode "TcRnUnexpectedPatSigType"                      = 74097
  GhcDiagnosticCode "TcRnIllegalKindSignature"                      = 91382
  GhcDiagnosticCode "TcRnDataKindsError"                            = 68567

  GhcDiagnosticCode "TcRnIllegalHsigDefaultMethods"                 = 93006
  GhcDiagnosticCode "TcRnHsigFixityMismatch"                        = 93007
  GhcDiagnosticCode "TcRnHsigNoIface"                               = 93010
  GhcDiagnosticCode "TcRnHsigMissingModuleExport"                   = 93011
  GhcDiagnosticCode "TcRnBadGenericMethod"                          = 59794
  GhcDiagnosticCode "TcRnWarningMinimalDefIncomplete"               = 13511
  GhcDiagnosticCode "TcRnDefaultMethodForPragmaLacksBinding"        = 28587
  GhcDiagnosticCode "TcRnIgnoreSpecialisePragmaOnDefMethod"         = 72520
  GhcDiagnosticCode "TcRnBadMethodErr"                              = 46284
  GhcDiagnosticCode "TcRnNoExplicitAssocTypeOrDefaultDeclaration"   = 08585
  GhcDiagnosticCode "TcRnIllegalTypeData"                           = 15013
  GhcDiagnosticCode "TcRnTypeDataForbids"                           = 67297
  GhcDiagnosticCode "TcRnTypedTHWithPolyType"                       = 94642
  GhcDiagnosticCode "TcRnSpliceThrewException"                      = 87897
  GhcDiagnosticCode "TcRnInvalidTopDecl"                            = 52886
  GhcDiagnosticCode "TcRnNonExactName"                              = 77923
  GhcDiagnosticCode "TcRnAddInvalidCorePlugin"                      = 86463
  GhcDiagnosticCode "TcRnAddDocToNonLocalDefn"                      = 67760
  GhcDiagnosticCode "TcRnFailedToLookupThInstName"                  = 49530
  GhcDiagnosticCode "TcRnCannotReifyInstance"                       = 30384
  GhcDiagnosticCode "TcRnCannotReifyOutOfScopeThing"                = 24922
  GhcDiagnosticCode "TcRnCannotReifyThingNotInTypeEnv"              = 79890
  GhcDiagnosticCode "TcRnNoRolesAssociatedWithThing"                = 65923
  GhcDiagnosticCode "TcRnCannotRepresentType"                       = 75721
  GhcDiagnosticCode "TcRnReportCustomQuasiError"                    = 39584
  GhcDiagnosticCode "TcRnInterfaceLookupError"                      = 52243
  GhcDiagnosticCode "TcRnUnsatisfiedMinimalDef"                     = 06201
  GhcDiagnosticCode "TcRnMisplacedInstSig"                          = 06202
  GhcDiagnosticCode "TcRnBadBootFamInstDecl"                        = 06203
  GhcDiagnosticCode "TcRnIllegalFamilyInstance"                     = 06204
  GhcDiagnosticCode "TcRnMissingClassAssoc"                         = 06205
  GhcDiagnosticCode "TcRnNotOpenFamily"                             = 06207
  GhcDiagnosticCode "TcRnCapturedTermName"                          = 54201
  GhcDiagnosticCode "TcRnBindingOfExistingName"                     = 58805
  GhcDiagnosticCode "TcRnMultipleFixityDecls"                       = 50419
  GhcDiagnosticCode "TcRnIllegalPatternSynonymDecl"                 = 41507
  GhcDiagnosticCode "TcRnIllegalClassBinding"                       = 69248
  GhcDiagnosticCode "TcRnOrphanCompletePragma"                      = 93961
  GhcDiagnosticCode "TcRnEmptyCase"                                 = 48010
  GhcDiagnosticCode "TcRnNonStdGuards"                              = 59119
  GhcDiagnosticCode "TcRnDuplicateSigDecl"                          = 31744
  GhcDiagnosticCode "TcRnMisplacedSigDecl"                          = 87866
  GhcDiagnosticCode "TcRnUnexpectedDefaultSig"                      = 40700
  GhcDiagnosticCode "TcRnBindInBootFile"                            = 11247
  GhcDiagnosticCode "TcRnDuplicateMinimalSig"                       = 85346
  GhcDiagnosticCode "TcRnLoopySuperclassSolve"                      = 36038
  GhcDiagnosticCode "TcRnIllegalInstanceHeadDecl"                   = 12222
  GhcDiagnosticCode "TcRnUnexpectedStandaloneDerivingDecl"          = 95159
  GhcDiagnosticCode "TcRnUnusedVariableInRuleDecl"                  = 65669
  GhcDiagnosticCode "TcRnUnexpectedStandaloneKindSig"               = 45906
  GhcDiagnosticCode "TcRnIllegalRuleLhs"                            = 63294
  GhcDiagnosticCode "TcRnBadAssocRhs"                               = 53634
  GhcDiagnosticCode "TcRnDuplicateRoleAnnot"                        = 97170
  GhcDiagnosticCode "TcRnDuplicateKindSig"                          = 43371
  GhcDiagnosticCode "TcRnIllegalDerivStrategy"                      = 87139
  GhcDiagnosticCode "TcRnIllegalMultipleDerivClauses"               = 30281
  GhcDiagnosticCode "TcRnNoDerivStratSpecified"                     = 55631
  GhcDiagnosticCode "TcRnStupidThetaInGadt"                         = 18403
  GhcDiagnosticCode "TcRnBadImplicitSplice"                         = 25277
  GhcDiagnosticCode "TcRnShadowedTyVarNameInFamResult"              = 99412
  GhcDiagnosticCode "TcRnIncorrectTyVarOnLhsOfInjCond"              = 88333
  GhcDiagnosticCode "TcRnUnknownTyVarsOnRhsOfInjCond"               = 48254
  GhcDiagnosticCode "TcRnBadlyStaged"                               = 28914
  GhcDiagnosticCode "TcRnStageRestriction"                          = 18157
  GhcDiagnosticCode "TcRnTyThingUsedWrong"                          = 10969
  GhcDiagnosticCode "TcRnCannotDefaultKindVar"                      = 79924
  GhcDiagnosticCode "TcRnUninferrableTyVar"                         = 16220
  GhcDiagnosticCode "TcRnSkolemEscape"                              = 71451
  GhcDiagnosticCode "TcRnPatSynEscapedCoercion"                     = 88986
  GhcDiagnosticCode "TcRnPatSynExistentialInResult"                 = 33973
  GhcDiagnosticCode "TcRnPatSynArityMismatch"                       = 18365
  GhcDiagnosticCode "TcRnCannotDefaultConcrete"                     = 52083
  GhcDiagnosticCode "TcRnMultiAssocTyFamDefaults"                   = 59128
  GhcDiagnosticCode "TcRnTyFamDepsDisabled"                         = 43991
  GhcDiagnosticCode "TcRnAbstractClosedTyFamDecl"                   = 60012
  GhcDiagnosticCode "TcRnPartialFieldSelector"                      = 82712
  GhcDiagnosticCode "TcRnSuperclassCycle"                           = 29210
  GhcDiagnosticCode "TcRnDefaultSigMismatch"                        = 72771
  GhcDiagnosticCode "TcRnTyFamResultDisabled"                       = 44012
  GhcDiagnosticCode "TcRnCommonFieldResultTypeMismatch"             = 31004
  GhcDiagnosticCode "TcRnCommonFieldTypeMismatch"                   = 91827
  GhcDiagnosticCode "TcRnAssocNoClassTyVar"                         = 55912
  GhcDiagnosticCode "TcRnDataConParentTypeMismatch"                 = 45219
  GhcDiagnosticCode "TcRnGADTsDisabled"                             = 23894
  GhcDiagnosticCode "TcRnExistentialQuantificationDisabled"         = 25709
  GhcDiagnosticCode "TcRnGADTDataContext"                           = 61072
  GhcDiagnosticCode "TcRnMultipleConForNewtype"                     = 16409
  GhcDiagnosticCode "TcRnKindSignaturesDisabled"                    = 49378
  GhcDiagnosticCode "TcRnEmptyDataDeclsDisabled"                    = 32478
  GhcDiagnosticCode "TcRnFamilyCategoryMismatch"                    = 52347
  GhcDiagnosticCode "TcRnFamilyArityMismatch"                       = 12985
  GhcDiagnosticCode "TcRnRoleMismatch"                              = 29178
  GhcDiagnosticCode "TcRnRoleCountMismatch"                         = 54298
  GhcDiagnosticCode "TcRnIllegalRoleAnnotation"                     = 77192
  GhcDiagnosticCode "TcRnRoleAnnotationsDisabled"                   = 17779
  GhcDiagnosticCode "TcRnIncoherentRoles"                           = 18273
  GhcDiagnosticCode "TcRnTyFamNameMismatch"                         = 88221
  GhcDiagnosticCode "TcRnTypeSynonymCycle"                          = 97522
  GhcDiagnosticCode "TcRnSelfImport"                                = 43281
  GhcDiagnosticCode "TcRnNoExplicitImportList"                      = 16029
  GhcDiagnosticCode "TcRnSafeImportsDisabled"                       = 26971
  GhcDiagnosticCode "TcRnDeprecatedModule"                          = 15328
  GhcDiagnosticCode "TcRnCompatUnqualifiedImport"                   = 82347
  GhcDiagnosticCode "TcRnRedundantSourceImport"                     = 54478
  GhcDiagnosticCode "TcRnDuplicateDecls"                            = 29916
  GhcDiagnosticCode "TcRnPackageImportsDisabled"                    = 10032
  GhcDiagnosticCode "TcRnIllegalDataCon"                            = 78448

  -- PatSynInvalidRhsReason
  GhcDiagnosticCode "PatSynNotInvertible"                           = 69317
  GhcDiagnosticCode "PatSynUnboundVar"                              = 28572

  -- TcRnBadFieldAnnotation/BadFieldAnnotationReason
  GhcDiagnosticCode "LazyFieldsDisabled"                            = 81601
  GhcDiagnosticCode "UnpackWithoutStrictness"                       = 10107
  GhcDiagnosticCode "BackpackUnpackAbstractType"                    = 40091

  -- TcRnRoleValidationFailed/RoleInferenceFailedReason
  GhcDiagnosticCode "TyVarRoleMismatch"                             = 22221
  GhcDiagnosticCode "TyVarMissingInEnv"                             = 99991
  GhcDiagnosticCode "BadCoercionRole"                               = 92834

  -- TcRnClassExtensionDisabled/DisabledClassExtension
  GhcDiagnosticCode "MultiParamDisabled"                            = 28349
  GhcDiagnosticCode "FunDepsDisabled"                               = 15708
  GhcDiagnosticCode "ConstrainedClassMethodsDisabled"               = 25079

  -- TcRnTyFamsDisabled/TyFamsDisabledReason
  GhcDiagnosticCode "TyFamsDisabledFamily"                          = 39191
  GhcDiagnosticCode "TyFamsDisabledInstance"                        = 06206
  GhcDiagnosticCode "TcRnPrecedenceParsingError"                    = 88747
  GhcDiagnosticCode "TcRnSectionPrecedenceError"                    = 46878

  -- HsigShapeMismatchReason
  GhcDiagnosticCode "HsigShapeSortMismatch"                         = 93008
  GhcDiagnosticCode "HsigShapeNotUnifiable"                         = 93009

  -- IllegalNewtypeReason
  GhcDiagnosticCode "DoesNotHaveSingleField"                        = 23517
  GhcDiagnosticCode "IsNonLinear"                                   = 38291
  GhcDiagnosticCode "IsGADT"                                        = 89498
  GhcDiagnosticCode "HasConstructorContext"                         = 17440
  GhcDiagnosticCode "HasExistentialTyVar"                           = 07525
  GhcDiagnosticCode "HasStrictnessAnnotation"                       = 04049

  -- TcRnBadRecordUpdate
  GhcDiagnosticCode "NoConstructorHasAllFields"                     = 14392
  GhcDiagnosticCode "MultiplePossibleParents"                       = 99339
  GhcDiagnosticCode "InvalidTyConParent"                            = 33238

  -- BadImport
  GhcDiagnosticCode "BadImportNotExported"                          = 61689
  GhcDiagnosticCode "BadImportAvailDataCon"                         = 35373
  GhcDiagnosticCode "BadImportNotExportedSubordinates"              = 10237
  GhcDiagnosticCode "BadImportAvailTyCon"                           = 56449
  GhcDiagnosticCode "BadImportAvailVar"                             = 12112

  -- TcRnPragmaWarning
  GhcDiagnosticCode "WarningTxt"                                    = 63394
  GhcDiagnosticCode "DeprecatedTxt"                                 = 68441

  -- TcRnRunSliceFailure/ConversionFail
  GhcDiagnosticCode "IllegalOccName"                                = 55017
  GhcDiagnosticCode "SumAltArityExceeded"                           = 68444
  GhcDiagnosticCode "IllegalSumAlt"                                 = 63966
  GhcDiagnosticCode "IllegalSumArity"                               = 97721
  GhcDiagnosticCode "MalformedType"                                 = 28709
  GhcDiagnosticCode "IllegalLastStatement"                          = 47373
  GhcDiagnosticCode "KindSigsOnlyAllowedOnGADTs"                    = 40746
  GhcDiagnosticCode "IllegalDeclaration"                            = 23882
  GhcDiagnosticCode "CannotMixGADTConsWith98Cons"                   = 24104
  GhcDiagnosticCode "EmptyStmtListInDoBlock"                        = 34949
  GhcDiagnosticCode "NonVarInInfixExpr"                             = 99831
  GhcDiagnosticCode "MultiWayIfWithoutAlts"                         = 63930
  GhcDiagnosticCode "CasesExprWithoutAlts"                          = 91745
  GhcDiagnosticCode "ImplicitParamsWithOtherBinds"                  = 42974
  GhcDiagnosticCode "InvalidCCallImpent"                            = 60220
  GhcDiagnosticCode "RecGadtNoCons"                                 = 18816
  GhcDiagnosticCode "GadtNoCons"                                    = 38140
  GhcDiagnosticCode "InvalidTypeInstanceHeader"                     = 37056
  GhcDiagnosticCode "InvalidTyFamInstLHS"                           = 78486
  GhcDiagnosticCode "InvalidImplicitParamBinding"                   = 51603
  GhcDiagnosticCode "DefaultDataInstDecl"                           = 39639
  GhcDiagnosticCode "FunBindLacksEquations"                         = 52078

  -- TcRnDodgyImports/DodgyImportsReason
  GhcDiagnosticCode "DodgyImportsEmptyParent"                       = 99623

  -- TcRnImportLookup/ImportLookupReason
  GhcDiagnosticCode "ImportLookupQualified"                         = 48795
  GhcDiagnosticCode "ImportLookupIllegal"                           = 14752
  GhcDiagnosticCode "ImportLookupAmbiguous"                         = 92057

  -- TcRnUnusedImport/UnusedImportReason
  GhcDiagnosticCode "UnusedImportNone"                              = 66111
  GhcDiagnosticCode "UnusedImportSome"                              = 38856

  -- Diagnostic codes for the foreign function interface
  GhcDiagnosticCode "NotADataType"                                  = 31136
  GhcDiagnosticCode "NewtypeDataConNotInScope"                      = 72317
  GhcDiagnosticCode "UnliftedFFITypesNeeded"                        = 10964
  GhcDiagnosticCode "NotABoxedMarshalableTyCon"                     = 89401
  GhcDiagnosticCode "ForeignLabelNotAPtr"                           = 26070
  GhcDiagnosticCode "NotSimpleUnliftedType"                         = 43510
  GhcDiagnosticCode "NotBoxedKindAny"                               = 64097
  GhcDiagnosticCode "ForeignDynNotPtr"                              = 27555
  GhcDiagnosticCode "SafeHaskellMustBeInIO"                         = 57638
  GhcDiagnosticCode "IOResultExpected"                              = 41843
  GhcDiagnosticCode "UnexpectedNestedForall"                        = 92994
  GhcDiagnosticCode "LinearTypesNotAllowed"                         = 57396
  GhcDiagnosticCode "OneArgExpected"                                = 91490
  GhcDiagnosticCode "AtLeastOneArgExpected"                         = 07641

  -- Interface errors
  GhcDiagnosticCode "BadSourceImport"                               = 64852
  GhcDiagnosticCode "HomeModError"                                  = 58427
  GhcDiagnosticCode "DynamicHashMismatchError"                      = 54709
  GhcDiagnosticCode "CouldntFindInFiles"                            = 94559
  GhcDiagnosticCode "GenericMissing"                                = 87110
  GhcDiagnosticCode "MissingPackageFiles"                           = 22211
  GhcDiagnosticCode "MissingPackageWayFiles"                        = 88719
  GhcDiagnosticCode "ModuleSuggestion"                              = 61948
  GhcDiagnosticCode "MultiplePackages"                              = 45102
  GhcDiagnosticCode "NoUnitIdMatching"                              = 51294
  GhcDiagnosticCode "NotAModule"                                    = 35235
  GhcDiagnosticCode "Can'tFindNameInInterface"                      = 83249
  GhcDiagnosticCode "HiModuleNameMismatchWarn"                      = 53693
  GhcDiagnosticCode "ExceptionOccurred"                             = 47808

  -- Out of scope errors
  GhcDiagnosticCode "NotInScope"                                    = 76037
  GhcDiagnosticCode "NotARecordField"                               = 22385
  GhcDiagnosticCode "NoExactName"                                   = 97784
  GhcDiagnosticCode "SameName"                                      = 81573
  GhcDiagnosticCode "MissingBinding"                                = 44432
  GhcDiagnosticCode "NoTopLevelBinding"                             = 10173
  GhcDiagnosticCode "UnknownSubordinate"                            = 54721
  GhcDiagnosticCode "NotInScopeTc"                                  = 76329

  -- Diagnostic codes for deriving
  GhcDiagnosticCode "DerivErrNotWellKinded"                         = 62016
  GhcDiagnosticCode "DerivErrSafeHaskellGenericInst"                = 07214
  GhcDiagnosticCode "DerivErrDerivingViaWrongKind"                  = 63174
  GhcDiagnosticCode "DerivErrNoEtaReduce"                           = 38996
  GhcDiagnosticCode "DerivErrBootFileFound"                         = 30903
  GhcDiagnosticCode "DerivErrDataConsNotAllInScope"                 = 54540
  GhcDiagnosticCode "DerivErrGNDUsedOnData"                         = 10333
  GhcDiagnosticCode "DerivErrNullaryClasses"                        = 04956
  GhcDiagnosticCode "DerivErrLastArgMustBeApp"                      = 28323
  GhcDiagnosticCode "DerivErrNoFamilyInstance"                      = 82614
  GhcDiagnosticCode "DerivErrNotStockDeriveable"                    = 00158
  GhcDiagnosticCode "DerivErrHasAssociatedDatatypes"                = 34611
  GhcDiagnosticCode "DerivErrNewtypeNonDeriveableClass"             = 82023
  GhcDiagnosticCode "DerivErrCannotEtaReduceEnough"                 = 26557
  GhcDiagnosticCode "DerivErrOnlyAnyClassDeriveable"                = 23244
  GhcDiagnosticCode "DerivErrNotDeriveable"                         = 38178
  GhcDiagnosticCode "DerivErrNotAClass"                             = 63388
  GhcDiagnosticCode "DerivErrNoConstructors"                        = 64560
  GhcDiagnosticCode "DerivErrLangExtRequired"                       = 86639
  GhcDiagnosticCode "DerivErrDunnoHowToDeriveForType"               = 48959
  GhcDiagnosticCode "DerivErrMustBeEnumType"                        = 30750
  GhcDiagnosticCode "DerivErrMustHaveExactlyOneConstructor"         = 37542
  GhcDiagnosticCode "DerivErrMustHaveSomeParameters"                = 45539
  GhcDiagnosticCode "DerivErrMustNotHaveClassContext"               = 16588
  GhcDiagnosticCode "DerivErrBadConstructor"                        = 16437
  GhcDiagnosticCode "DerivErrGenerics"                              = 30367
  GhcDiagnosticCode "DerivErrEnumOrProduct"                         = 58291

  -- Diagnostic codes for instance lookup
  GhcDiagnosticCode "LookupInstErrNotExact"                         = 10372
  GhcDiagnosticCode "LookupInstErrFlexiVar"                         = 10373
  GhcDiagnosticCode "LookupInstErrNotFound"                         = 10374

  -- TcRnEmptyStmtsGroupError/EmptyStatementGroupErrReason
  GhcDiagnosticCode "EmptyStmtsGroupInParallelComp"                 = 41242
  GhcDiagnosticCode "EmptyStmtsGroupInTransformListComp"            = 92693
  GhcDiagnosticCode "EmptyStmtsGroupInDoNotation"                   = 82311
  GhcDiagnosticCode "EmptyStmtsGroupInArrowNotation"                = 19442

  -- To generate new random numbers:
  --  https://www.random.org/integers/?num=10&min=1&max=99999&col=1&base=10&format=plain
  --
  -- NB: never remove a return value from this type family!
  -- We need to ensure uniquess of diagnostic codes across GHC versions,
  -- and this includes outdated diagnostic codes for errors that GHC
  -- no longer reports. These are collected below.

  GhcDiagnosticCode "TcRnNameByTemplateHaskellQuote"                = 40027
  GhcDiagnosticCode "TcRnIllegalBindingOfBuiltIn"                   = 69639
  GhcDiagnosticCode "TcRnMixedSelectors"                            = 40887

{- *********************************************************************
*                                                                      *
                 Recurring into an argument
*                                                                      *
********************************************************************* -}

-- | Some constructors of diagnostic datatypes don't have
-- corresponding error codes, because we recur inside them.
--
-- For example, we don't have an error code for the
-- 'TcRnCannotDeriveInstance' constructor of 'TcRnMessage',
-- because we recur into the 'DeriveInstanceErrReason' to obtain
-- an error code.
--
-- This type family keeps track of such constructors.
type ConRecursInto :: Symbol -> Maybe Type
type family ConRecursInto con where

  ----------------------------------
  -- Constructors of GhcMessage

  ConRecursInto "GhcDriverMessage"         = 'Just DriverMessage
  ConRecursInto "GhcPsMessage"             = 'Just PsMessage
  ConRecursInto "GhcTcRnMessage"           = 'Just TcRnMessage
  ConRecursInto "GhcDsMessage"             = 'Just DsMessage
  ConRecursInto "GhcUnknownMessage"        = 'Just UnknownDiagnostic

  ----------------------------------
  -- Constructors of DriverMessage

  ConRecursInto "DriverUnknownMessage"     = 'Just UnknownDiagnostic
  ConRecursInto "DriverPsHeaderMessage"    = 'Just PsMessage
  ConRecursInto "DriverInterfaceError"     = 'Just IfaceMessage

  ConRecursInto "CantFindErr"              = 'Just CantFindInstalled
  ConRecursInto "CantFindInstalledErr"     = 'Just CantFindInstalled

  ConRecursInto "CantFindInstalled"        = 'Just CantFindInstalledReason

  ConRecursInto "BadIfaceFile"                 = 'Just ReadInterfaceError
  ConRecursInto "FailedToLoadDynamicInterface" = 'Just ReadInterfaceError

  ----------------------------------
  -- Constructors of PsMessage

  ConRecursInto "PsUnknownMessage"         = 'Just UnknownDiagnostic
  ConRecursInto "PsHeaderMessage"          = 'Just PsHeaderMessage

  ----------------------------------
  -- Constructors of TcRnMessage

  ConRecursInto "TcRnUnknownMessage"       = 'Just UnknownDiagnostic

    -- Recur into TcRnMessageWithInfo to get the underlying TcRnMessage
  ConRecursInto "TcRnMessageWithInfo"      = 'Just TcRnMessageDetailed
  ConRecursInto "TcRnMessageDetailed"      = 'Just TcRnMessage
  ConRecursInto "TcRnWithHsDocContext"     = 'Just TcRnMessage

  ConRecursInto "TcRnCannotDeriveInstance" = 'Just DeriveInstanceErrReason
  ConRecursInto "TcRnLookupInstance"       = 'Just LookupInstanceErrReason
  ConRecursInto "TcRnPragmaWarning"        = 'Just (WarningTxt GhcRn)
  ConRecursInto "TcRnNotInScope"           = 'Just NotInScopeError
  ConRecursInto "TcRnIllegalNewtype"       = 'Just IllegalNewtypeReason
  ConRecursInto "TcRnHsigShapeMismatch"    = 'Just HsigShapeMismatchReason
  ConRecursInto "TcRnPatSynInvalidRhs"     = 'Just PatSynInvalidRhsReason
  ConRecursInto "TcRnBadRecordUpdate"      = 'Just BadRecordUpdateReason
  ConRecursInto "TcRnBadFieldAnnotation"   = 'Just BadFieldAnnotationReason
  ConRecursInto "TcRnRoleValidationFailed" = 'Just RoleValidationFailedReason
  ConRecursInto "TcRnClassExtensionDisabled" = 'Just DisabledClassExtension
  ConRecursInto "TcRnTyFamsDisabled"       = 'Just TyFamsDisabledReason
  ConRecursInto "TcRnDodgyImports"         = 'Just DodgyImportsReason
  ConRecursInto "DodgyImportsHiding"       = 'Just ImportLookupReason
  ConRecursInto "TcRnImportLookup"         = 'Just ImportLookupReason
  ConRecursInto "TcRnUnusedImport"         = 'Just UnusedImportReason

    --
    -- TH errors

  ConRecursInto "TcRnRunSpliceFailure"     = 'Just RunSpliceFailReason
  ConRecursInto "ConversionFail"           = 'Just ConversionFailReason

    -- Interface file errors

  ConRecursInto "TcRnInterfaceError"       = 'Just IfaceMessage
  ConRecursInto "Can'tFindInterface"       = 'Just MissingInterfaceError

    ------------------
    -- FFI errors

  ConRecursInto "TcRnIllegalForeignType"   = 'Just IllegalForeignTypeReason
    -- IllegalForeignTypeReason: recur into TypeCannotBeMarshaled for the reason
  ConRecursInto "TypeCannotBeMarshaled"    = 'Just TypeCannotBeMarshaledReason

    ------------------
    -- Solver reports

    -- Recur inside TcRnSolverReport to get the underlying TcSolverReportMsg
  ConRecursInto "TcRnSolverReport"         = 'Just SolverReportWithCtxt
  ConRecursInto "SolverReportWithCtxt"     = 'Just TcSolverReportMsg
  ConRecursInto "TcReportWithInfo"         = 'Just TcSolverReportMsg

    -- Recur inside CannotUnifyVariable to get the underlying reason
  ConRecursInto "CannotUnifyVariable"      = 'Just CannotUnifyVariableReason

    -- Recur inside Mismatch to get the underlying reason
  ConRecursInto "Mismatch"                 = 'Just MismatchMsg

    -- Recur inside empty statements groups to get the underlying statements block
  ConRecursInto "TcRnEmptyStmtsGroup"      = 'Just EmptyStatementGroupErrReason
  ----------------------------------
  -- Constructors of DsMessage

  ConRecursInto "DsUnknownMessage"         = 'Just UnknownDiagnostic

  ----------------------------------
  -- Constructors of ImportLookupBad
  ConRecursInto "ImportLookupBad"          = 'Just BadImportKind

  ----------------------------------
  -- Any other constructors: don't recur, instead directly
  -- use the constructor name for the error code.

  ConRecursInto _                          = 'Nothing

{- *********************************************************************
*                                                                      *
                         Generics machinery
*                                                                      *
********************************************************************* -}

{- Note [Diagnostic codes using generics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Diagnostic codes are specified at the type-level using the injective
type family 'GhcDiagnosticCode'. This ensures uniqueness of diagnostic
codes, giving quick feedback (in the form of a type error).

Using this type family, we need to obtain corresponding value-level
functions, e.g.

  diagnosticCode :: TcRnMessage -> DiagnosticCode
  diagnosticCode diag = case diag of
    TcRnInaccessibleCode               {} -> ghcDiagnosticCode 40564
    TcRnTypeDoesNotHaveFixedRuntimeRep {} -> ghcDiagnosticCode 18478
    TcRnCannotDeriveInstance _ _ _ _ reason ->
      case reason of
        DerivErrNotWellKinded          {} -> ghcDiagnosticCode 62016
        DerivErrNotAClass              {} -> ghcDiagnosticCode 63388
        ...
    ...

For some constructors, such as 'TcRnInaccessibleCode', we directly get a
diagnostic code, using the 'GhcDiagnosticCode' type family. For other
constructors, such as 'TcRnCannotDeriveInstance', we instead recur into an
argument (in this case 'DeriveInstanceErrReason') to obtain a diagnostic code.

To achieve this, we use a variant of the 'typed' lens from 'generic-lens'
(we only need a getter, not a setter):

  - Using GHC.Generics, we obtain the type-level structure
    of diagnostic types, as sums of products, with extra metadata.
  - The 'ConRecursInto' type family declares when we should
    recur into an argument of the constructor instead of using
    the constructor name itself for the diagnostic code.
  - To decide whether to recur, in the generic representation,
    we must look at all factors of a product to see if there is
    a type we should recur into. We look at the left branch
    first, and decide whether to recur into it using the
    HasTypeQ type family.
  - The two different behaviours are controlled by two main instances (*) and (**).
    - (*) recurs into a subtype, when we have a type family equation such as:

        ConRecursInto "TcRnCannotDeriveInstance" = 'Just DeriveInstanceErrReason

      In this case, for the constructor 'TcRnCannotDeriveInstance', we recur into the
      type 'DeriveInstanceErrReason'.
      The overlapping instance (ERR1) provides an error message in case a constructor
      does not have the type specified by the 'ConRecursInto' type family.
    - (**) directly uses the constructor name, by using the 'GhcDiagnosticCode'
      type family. The 'KnownConstructor' context (ERR2) on the instance provides
      a custom error message in case of a missing diagnostic code, which points
      GHC contributors to the documentation explaining how to add diagnostic codes
      for their diagnostics.
-}

-- | Use the generic representation of a type to retrieve the
-- diagnostic code, using the 'GhcDiagnosticCode' type family.
--
-- See Note [Diagnostic codes using generics] in GHC.Types.Error.Codes.
type GDiagnosticCode :: (Type -> Type) -> Constraint
class GDiagnosticCode f where
  gdiagnosticCode :: f a -> Maybe DiagnosticCode

type ConstructorCode :: Symbol -> (Type -> Type) -> Maybe Type -> Constraint
class ConstructorCode con f recur where
  gconstructorCode :: f a -> Maybe DiagnosticCode
instance KnownConstructor con => ConstructorCode con f 'Nothing where
  gconstructorCode _ = Just $ DiagnosticCode "GHC" $ natVal' @(GhcDiagnosticCode con) proxy#

-- If we recur into the 'UnknownDiagnostic' existential datatype,
-- unwrap the existential and obtain the error code.
instance {-# OVERLAPPING #-}
         ( ConRecursInto con ~ 'Just UnknownDiagnostic
         , HasType UnknownDiagnostic con f )
      => ConstructorCode con f ('Just UnknownDiagnostic) where
  gconstructorCode diag = case getType @UnknownDiagnostic @con @f diag of
    UnknownDiagnostic diag -> diagnosticCode diag

-- (*) Recursive instance: Recur into the given type.
instance ( ConRecursInto con ~ 'Just ty, HasType ty con f
         , Generic ty, GDiagnosticCode (Rep ty) )
      => ConstructorCode con f ('Just ty) where
  gconstructorCode diag = constructorCode (getType @ty @con @f diag)

-- (**) Constructor instance: handle constructors directly.
--
-- Obtain the code from the 'GhcDiagnosticCode'
-- type family, applied to the name of the constructor.
instance (ConstructorCode con f recur, recur ~ ConRecursInto con)
      => GDiagnosticCode (M1 i ('MetaCons con x y) f) where
  gdiagnosticCode (M1 x) = gconstructorCode @con @f @recur x

-- Handle sum types (the diagnostic types are sums of constructors).
instance (GDiagnosticCode f, GDiagnosticCode g) => GDiagnosticCode (f :+: g) where
  gdiagnosticCode (L1 x) = gdiagnosticCode @f x
  gdiagnosticCode (R1 y) = gdiagnosticCode @g y

-- Discard metadata we don't need.
instance GDiagnosticCode f
      => GDiagnosticCode (M1 i ('MetaData nm mod pkg nt) f) where
  gdiagnosticCode (M1 x) = gdiagnosticCode @f x

-- | Decide whether to pick the left or right branch
-- when deciding how to recurse into a product.
type family HasTypeQ (ty :: Type) f :: Maybe Type where
  HasTypeQ typ (M1 _ _ (K1 _ typ))
    = 'Just typ
  HasTypeQ typ (M1 _ _ x)
    = HasTypeQ typ x
  HasTypeQ typ (l :*: r)
    = Alt (HasTypeQ typ l) (HasTypeQ typ r)
  HasTypeQ typ (l :+: r)
    = Both (HasTypeQ typ l) (HasTypeQ typ r)
  HasTypeQ typ (K1 _ _)
    = 'Nothing
  HasTypeQ typ U1
    = 'Nothing
  HasTypeQ typ V1
    = 'Nothing

type family Both (m1 :: Maybe a) (m2 :: Maybe a) :: Maybe a where
  Both ('Just a) ('Just a) = 'Just a

type family Alt (m1 :: Maybe a) (m2 :: Maybe a) :: Maybe a where
  Alt ('Just a) _ = 'Just a
  Alt _ b = b

type HasType :: Type -> Symbol -> (Type -> Type) -> Constraint
class HasType ty orig f where
  getType :: f a -> ty

instance HasType ty orig (M1 i s (K1 x ty)) where
  getType (M1 (K1 x)) = x
instance HasTypeProd ty (HasTypeQ ty f) orig f g => HasType ty orig (f :*: g) where
  getType = getTypeProd @ty @(HasTypeQ ty f) @orig

-- The lr parameter tells us whether to pick the left or right
-- branch in a product, and is computed using 'HasTypeQ'.
--
-- If it's @Just l@, then we have found the type in the left branch,
-- so use that. Otherwise, look in the right branch.
class HasTypeProd ty lr orig f g where
  getTypeProd :: (f :*: g) a -> ty

-- Pick the left branch.
instance HasType ty orig  f => HasTypeProd ty ('Just l) orig f g where
  getTypeProd (x :*: _) = getType @ty @orig @f x

-- Pick the right branch.
instance HasType ty orig g => HasTypeProd ty 'Nothing orig f g where
  getTypeProd (_ :*: y) = getType @ty @orig @g y

{- *********************************************************************
*                                                                      *
               Custom type errors for diagnostic codes
*                                                                      *
********************************************************************* -}

-- (ERR1) Improve error messages for recurring into an argument.
instance {-# OVERLAPPABLE #-}
  TypeError
    (     'Text "The constructor '" ':<>: 'Text orig ':<>: 'Text "'"
    ':$$: 'Text "does not have any argument of type '" ':<>: 'ShowType ty ':<>: 'Text "'."
    ':$$: 'Text ""
    ':$$: 'Text "This is likely due to an incorrect type family equation:"
    ':$$: 'Text "  ConRecursInto \"" ':<>: 'Text orig ':<>: 'Text "\" = " ':<>: 'ShowType ty )
  => HasType ty orig f where
  getType = panic "getType: unreachable"

-- (ERR2) Improve error messages for missing 'GhcDiagnosticCode' equations.
type KnownConstructor :: Symbol -> Constraint
type family KnownConstructor con where
  KnownConstructor con =
    KnownNatOrErr
      ( TypeError
        (     'Text "Missing diagnostic code for constructor "
        ':<>: 'Text "'" ':<>: 'Text con ':<>: 'Text "'."
        ':$$: 'Text ""
        ':$$: 'Text "Note [Diagnostic codes] in GHC.Types.Error.Codes"
        ':$$: 'Text "contains instructions for adding a new diagnostic code."
        )
      )
      (GhcDiagnosticCode con)

type KnownNatOrErr :: Constraint -> Nat -> Constraint
type KnownNatOrErr err n = (Assert err n, KnownNat n)

-- Detecting a stuck type family using a data family.
-- See https://blog.csongor.co.uk/report-stuck-families/.
type Assert :: Constraint -> k -> Constraint
type family Assert err n where
  Assert _ Dummy = Dummy
  Assert _ n     = ()
data family Dummy :: k
