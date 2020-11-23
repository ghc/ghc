{-# LANGUAGE LambdaCase #-}

module GHC.Parser.Errors
   ( -- * Types
     module GHC.Parser.Errors.Types
   -- * Constructing errors and messages
   , mkParserErr
   , mkParserErrNoHints
   , mkParserWarn
   )
where

import GHC.Parser.Errors.Types
import GHC.Parser.Errors.Ppr ()
import GHC.Driver.Flags (WarningFlag(..), WarnReason(..))
import GHC.Types.Error (ErrMsg, mkErr, makeIntoWarning)
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (alwaysQualify)

--
-- Constructing errors and messages
--

-- | Builds a new 'ErrMsg' out of a parser 'Error'.
mkParserErr :: SrcSpan -> ErrorDesc -> [Hint] -> ErrMsg Error
mkParserErr span desc hints = mkErr span alwaysQualify (Error desc hints)

mkParserErrNoHints :: SrcSpan -> ErrorDesc -> ErrMsg Error
mkParserErrNoHints span desc = mkParserErr span desc noHints

-- | Builds a new 'ErrMsg' out of a parser 'Warning'.
mkParserWarn :: SrcSpan -> Warning -> ErrMsg Warning
mkParserWarn span warn = makeIntoWarning (Reason (toWarningFlag warn)) (mkErr span alwaysQualify warn)

-- | Static mapping function between a 'Warning' and the associated 'WarningFlag'.
toWarningFlag :: Warning -> WarningFlag
toWarningFlag = \case
  WarnTab{} -> Opt_WarnTabs
  WarnTransitionalLayout{}            -> Opt_WarnAlternativeLayoutRuleTransitional
  WarnUnrecognisedPragma{}            -> Opt_WarnUnrecognisedPragmas
  WarnHaddockInvalidPos{}             -> Opt_WarnInvalidHaddock
  WarnHaddockIgnoreMulti{}            -> Opt_WarnInvalidHaddock
  WarnStarBinder{}                    -> Opt_WarnStarBinder
  WarnStarIsType{}                    -> Opt_WarnStarIsType
  WarnImportPreQualified{}            -> Opt_WarnPrepositiveQualifiedModule
  WarnOperatorWhitespaceExtConflict{} -> Opt_WarnOperatorWhitespaceExtConflict
  WarnOperatorWhitespace{}            -> Opt_WarnOperatorWhitespace
