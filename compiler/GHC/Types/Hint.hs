{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module GHC.Types.Hint where

import Data.Typeable

import GHC.Hs.Extension
import GHC.LanguageExtensions
import GHC.Prelude
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Utils.Outputable

import Language.Haskell.Syntax.Expr
import Language.Haskell.Syntax.Type


--
-- Types for hints and refactorings
--

-- | An /action/ which can be performed on an Haskell expression, typically
-- to transform it according to an 'Hint'.
data Refactoring
  = MkRefactoring SrcSpan      -- ^ where the current code to be replaced lives
                  Replacement  -- ^ replacement for that code

-- | A mechanical transformation returned as part of a 'Refactoring',
-- which can be applied to the AST.
data Replacement
  = ReplaceExpr (HsExpr GhcPs)
  | ReplaceType (HsType GhcPs)
  | ReplaceMatch (Match GhcPs (HsExpr GhcPs))

-- | A typeclass for /hints/, emitted by GHC together with diagnostics. A /hint/
-- is a program transformation which suggests a possible way to deal with a particular
-- warning or error.
class Outputable a => IsHint a where
  hintRefactoring :: a -> Maybe Refactoring

-- | A type for hints emitted by GHC.
data Hint where
  -- | An \"unknown\" hint. This type constructor allows arbitrary
  -- hints to be embedded. The typical use case would be GHC plugins
  -- willing to emit hints alonside their custom diagnostics.
  UnknownHint :: (IsHint a, Typeable a) => a -> Hint
  -- | Suggests adding a particular language extension.
  SuggestExtension :: !Extension -> Hint
  -- | Suggests that a particular statement should be written within a \"do\"
  -- block.
  SuggestDo :: Hint
  -- | Suggests that a missing \"do\" block.
  SuggestMissingDo :: Hint
  -- | Suggests that a \"let\" expression is needed in a \"do\" block.
  SuggestLetInDo :: Hint
  SuggestInfixBindMaybeAtPat :: !RdrName -> Hint
  -- | Type applications in patterns are only allowed on data constructors
  TypeApplicationsInPatternsOnlyDataCons :: Hint

--
-- Instances
--

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
