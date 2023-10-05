{-# LANGUAGE LambdaCase #-}
module GHC.Parser.Errors.Basic where

import GHC.Utils.Outputable ( SDoc, text )

-- | The operator symbol in the 'PsOperatorWhitespaceExtConflictMessage' diagnostic.
data OperatorWhitespaceSymbol
   = OperatorWhitespaceSymbol_PrefixPercent
   | OperatorWhitespaceSymbol_PrefixDollar
   | OperatorWhitespaceSymbol_PrefixDollarDollar

pprOperatorWhitespaceSymbol :: OperatorWhitespaceSymbol -> SDoc
pprOperatorWhitespaceSymbol = \case
  OperatorWhitespaceSymbol_PrefixPercent      -> text "%"
  OperatorWhitespaceSymbol_PrefixDollar       -> text "$"
  OperatorWhitespaceSymbol_PrefixDollarDollar -> text "$$"

-- | The operator occurrence type in the 'PsOperatorWhitespaceMessage' diagnostic.
data OperatorWhitespaceOccurrence
   = OperatorWhitespaceOccurrence_Prefix
   | OperatorWhitespaceOccurrence_Suffix
   | OperatorWhitespaceOccurrence_TightInfix
