{-# LANGUAGE TemplateHaskell #-}

-- Duplicate declarations brought into being by a single splice all share
-- the splice's source span. The diagnostic must not repeat that span as a
-- related location: only one caret, and no echo of the primary span in the
-- JSON relatedSpans field.

module MultiCaretDupSpan where

import Language.Haskell.TH

$(do let d = ValD (VarP (mkName "x")) (NormalB (LitE (IntegerL 1))) []
     return [d, d])
