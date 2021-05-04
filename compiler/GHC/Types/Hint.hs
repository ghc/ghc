{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance IsHint Hint, instance RenderableMessage Hint
module GHC.Types.Hint where

import GHC.Prelude
import GHC.Types.Error
import GHC.Utils.Outputable
import GHC.Types.Name.Reader

instance RenderableMessage Hint where
  diagnosticMessage = \case
    UnknownHint m
      -> diagnosticMessage m
    SuggestExtension ext
      -> mkSimpleDecorated $ text "Perhaps you intended to use" <+> ppr ext
    SuggestDo
      -> mkSimpleDecorated $ text "Perhaps this statement should be within a 'do' block?"
    SuggestMissingDo
      -> mkSimpleDecorated $ text "Possibly caused by a missing 'do'?"
    SuggestLetInDo
      -> mkSimpleDecorated $
           text "Perhaps you need a 'let' in a 'do' block?"
             $$ text "e.g. 'let x = 5' instead of 'x = 5'"
    SuggestInfixBindMaybeAtPat fun
      -> mkSimpleDecorated $
           text "In a function binding for the"
              <+> quotes (ppr fun)
              <+> text "operator."
           $$ if opIsAt fun
                 then perhapsAsPat
                 else empty
         where
    TypeApplicationsInPatternsOnlyDataCons
      -> mkSimpleDecorated $ text "Type applications in patterns are only allowed on data constructors."

instance IsHint Hint where
  hintRefactoring = \case
    UnknownHint m
      -> hintRefactoring m
    SuggestExtension _
      -> NoRefactoring
    SuggestDo
      -> NoRefactoring
    SuggestMissingDo
      -> NoRefactoring
    SuggestLetInDo
      -> NoRefactoring
    SuggestInfixBindMaybeAtPat _
      -> NoRefactoring
    TypeApplicationsInPatternsOnlyDataCons
      -> NoRefactoring

perhapsAsPat :: SDoc
perhapsAsPat = text "Perhaps you meant an as-pattern, which must not be surrounded by whitespace"
