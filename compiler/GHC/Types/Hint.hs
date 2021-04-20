{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance IsHint Hint, instance RenderableMessage Hint
module GHC.Types.Hint where

import GHC.Prelude
import GHC.Types.Error
import GHC.Utils.Outputable
import GHC.Types.Name.Reader

instance Outputable Hint where
  ppr = \case
    UnknownHint m
      -> ppr m
    SuggestExtension ext
      -> text "Perhaps you intended to use" <+> ppr ext
    SuggestDo
      -> text "Perhaps this statement should be within a 'do' block?"
    SuggestMissingDo
      -> text "Possibly caused by a missing 'do'?"
    SuggestLetInDo
      -> text "Perhaps you need a 'let' in a 'do' block?"
           $$ text "e.g. 'let x = 5' instead of 'x = 5'"
    SuggestInfixBindMaybeAtPat fun
      -> text "In a function binding for the"
              <+> quotes (ppr fun)
              <+> text "operator."
           $$ if opIsAt fun
                 then perhapsAsPat
                 else empty
    TypeApplicationsInPatternsOnlyDataCons
      -> text "Type applications in patterns are only allowed on data constructors."

instance IsHint Hint where
  hintRefactoring = \case
    UnknownHint m
      -> hintRefactoring m
    SuggestExtension _
      -> Nothing
    SuggestDo
      -> Nothing
    SuggestMissingDo
      -> Nothing
    SuggestLetInDo
      -> Nothing
    SuggestInfixBindMaybeAtPat _
      -> Nothing
    TypeApplicationsInPatternsOnlyDataCons
      -> Nothing

perhapsAsPat :: SDoc
perhapsAsPat = text "Perhaps you meant an as-pattern, which must not be surrounded by whitespace"
