module GHC.Parser.Error where

import Prelude hiding ((<>))
import GHC.Utils.Error
import GHC.Utils.Outputable

-- | Dedicated error type for the parser
data PsError
  = PsErrorDoc ErrDoc

    -- errors thrown in Header.hs
  | PsUnsupportedExtension String [String]
  | PsUnknownOptionsFlag String
  | PsLanguagePragmaParseError
  | PsOptionsGhcParseError String

psErrorDoc :: PsError -> ErrDoc
psErrorDoc (PsErrorDoc d) = d
psErrorDoc (PsUnsupportedExtension ext sug) =
  errDoc [m] [] []

  where m = text "Unsupported extension: " <> text ext $$
            suggestions
        suggestions
          | null sug  = GHC.Utils.Outputable.empty
          | otherwise = text "Perhaps you meant" <+>
              quotedListWithOr (map text sug)
psErrorDoc (PsUnknownOptionsFlag flag) =
  errDoc [m] [] []

  where m = text "unknown flag in {-# OPTIONS_GHC #-} pragma:" <+> text flag
psErrorDoc PsLanguagePragmaParseError =
  errDoc [m] [] []

  where m = vcat [ text "Cannot parse LANGUAGE pragma"
                 , text "Expecting comma-separated list of language options,"
                 , text "each starting with a capital letter"
                 , nest 2 (text "E.g. {-# LANGUAGE TemplateHaskell, GADTs #-}") ]
psErrorDoc (PsOptionsGhcParseError inp) =
  errDoc [m] [] []

  where m = vcat [ text "Error while parsing OPTIONS_GHC pragma."
                 , text "Expecting whitespace-separated list of GHC options."
                 , text "  E.g. {-# OPTIONS_GHC -Wall -O2 #-}"
                 , text ("Input was: " ++ show inp) ]

instance RenderableError PsError where
  renderError = psErrorDoc
