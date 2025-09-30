{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Warnings for a module
module Language.Haskell.Textual.Warning
   ( WarningCategory(..)
   , mkWarningCategory
   , defaultWarningCategory
   , validWarningCategory
   , InWarningCategory(..)
   , fromWarningCategory

   , WarningCategorySet
   , emptyWarningCategorySet
   , completeWarningCategorySet
   , nullWarningCategorySet
   , elemWarningCategorySet
   , insertWarningCategorySet
   , deleteWarningCategorySet

   , WarningTxt (..)
   , warningTxtCategory
   , warningTxtMessage
   , warningTxtSame
   )
where

import Language.Haskell.Syntax.Extension
import Language.Haskell.Textual.Documentation
import Language.Haskell.Textual.ExactPrint
import Language.Haskell.Textual.Location
import qualified Language.Haskell.Textual.Source as Source
import Language.Haskell.Textual.UTF8

import Control.DeepSeq
import Data.Char (isAlphaNum)
import Data.Data
import Data.List (isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics ( Generic )

import Prelude


{-
Note [Warning categories]
~~~~~~~~~~~~~~~~~~~~~~~~~
See GHC Proposal 541 for the design of the warning categories feature:
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0541-warning-pragmas-with-categories.rst

A WARNING pragma may be annotated with a category such as "x-partial" written
after the 'in' keyword, like this:

    {-# WARNING in "x-partial" head "This function is partial..." #-}

This is represented by the 'Maybe (Located WarningCategory)' field in
'WarningTxt'.  The parser will accept an arbitrary string as the category name,
then the renamer (in 'rnWarningTxt') will check it contains only valid
characters, so we can generate a nicer error message than a parse error.

The corresponding warnings can then be controlled with the -Wx-partial,
-Wno-x-partial, -Werror=x-partial and -Wwarn=x-partial flags.  Such a flag is
distinguished from an 'unrecognisedWarning' by the flag parser testing
'validWarningCategory'.  The 'x-' prefix means we can still usually report an
unrecognised warning where the user has made a mistake.

A DEPRECATED pragma may not have a user-defined category, and is always treated
as belonging to the special category 'deprecations'.  Similarly, a WARNING
pragma without a category belongs to the 'deprecations' category.
Thus the '-Wdeprecations' flag will enable all of the following:

    {-# WARNING in "deprecations" foo "This function is deprecated..." #-}
    {-# WARNING foo "This function is deprecated..." #-}
    {-# DEPRECATED foo "This function is deprecated..." #-}

The '-Wwarnings-deprecations' flag is supported for backwards compatibility
purposes as being equivalent to '-Wdeprecations'.

The '-Wextended-warnings' warning group collects together all warnings with
user-defined categories, so they can be enabled or disabled
collectively. Moreover they are treated as being part of other warning groups
such as '-Wdefault' (see 'warningGroupIncludesExtendedWarnings').

'DynFlags' and 'DiagOpts' each contain a set of enabled and a set of fatal
warning categories, just as they do for the finite enumeration of 'WarningFlag's
built in to GHC.  These are represented as 'WarningCategorySet's to allow for
the possibility of them being infinite.

-}

data InWarningCategory
  = InWarningCategory
    { iwc_in :: !(EpToken "in"),
      iwc_st :: !Source.CodeSnippet,
      iwc_wc :: (LocatedE WarningCategory)
    } deriving Data

fromWarningCategory :: WarningCategory -> InWarningCategory
fromWarningCategory wc = InWarningCategory NoEpTok Source.CodeSnippetAbsent $
    L (EpaSpan noSrcSpan) wc


-- See Note [Warning categories]
newtype WarningCategory = WarningCategory TextUTF8
  deriving stock Data
  deriving newtype (Eq, Ord, Show, NFData)
--  deriving newtype (Binary, Eq, Outputable, Show, Uniquable, NFData)

mkWarningCategory :: TextUTF8 -> WarningCategory
mkWarningCategory = WarningCategory

-- | The @deprecations@ category is used for all DEPRECATED pragmas and for
-- WARNING pragmas that do not specify a category.
defaultWarningCategory :: WarningCategory
defaultWarningCategory = mkWarningCategory (encodeUTF8 "deprecations")

-- | Is this warning category allowed to appear in user-defined WARNING pragmas?
-- It must either be the known category @deprecations@, or be a custom category
-- that begins with @x-@ and contains only valid characters (letters, numbers,
-- apostrophes and dashes).
validWarningCategory :: WarningCategory -> Bool
validWarningCategory cat@(WarningCategory c) =
    cat == defaultWarningCategory || ("x-" `isPrefixOf` s && all is_allowed s)
  where
    s = decodeUTF8 c
    is_allowed c = isAlphaNum c || c == '\'' || c == '-'


-- | A finite or infinite set of warning categories.
--
-- Unlike 'WarningFlag', there are (in principle) infinitely many warning
-- categories, so we cannot necessarily enumerate all of them. However the set
-- is constructed by adding or removing categories one at a time, so we can
-- represent it as either a finite set of categories, or a cofinite set (where
-- we store the complement).
data WarningCategorySet =
    FiniteWarningCategorySet   (Set WarningCategory)
      -- ^ The set of warning categories is the given finite set.
  | CofiniteWarningCategorySet (Set WarningCategory)
      -- ^ The set of warning categories is infinite, so the constructor stores
      -- its (finite) complement.

-- | The empty set of warning categories.
emptyWarningCategorySet :: WarningCategorySet
emptyWarningCategorySet = FiniteWarningCategorySet Set.empty

-- | The set consisting of all possible warning categories.
completeWarningCategorySet :: WarningCategorySet
completeWarningCategorySet = CofiniteWarningCategorySet Set.empty

-- | Is this set empty?
nullWarningCategorySet :: WarningCategorySet -> Bool
nullWarningCategorySet (FiniteWarningCategorySet s) = Set.null s
nullWarningCategorySet CofiniteWarningCategorySet{} = False

-- | Does this warning category belong to the set?
elemWarningCategorySet :: WarningCategory -> WarningCategorySet -> Bool
elemWarningCategorySet c (FiniteWarningCategorySet   s) =      c `Set.member` s
elemWarningCategorySet c (CofiniteWarningCategorySet s) = not (c `Set.member` s)

-- | Insert an element into a warning category set.
insertWarningCategorySet :: WarningCategory -> WarningCategorySet -> WarningCategorySet
insertWarningCategorySet c (FiniteWarningCategorySet   s) = FiniteWarningCategorySet   (Set.insert c s)
insertWarningCategorySet c (CofiniteWarningCategorySet s) = CofiniteWarningCategorySet (Set.delete c s)

-- | Delete an element from a warning category set.
deleteWarningCategorySet :: WarningCategory -> WarningCategorySet -> WarningCategorySet
deleteWarningCategorySet c (FiniteWarningCategorySet   s) = FiniteWarningCategorySet   (Set.delete c s)
deleteWarningCategorySet c (CofiniteWarningCategorySet s) = CofiniteWarningCategorySet (Set.insert c s)

type LWarningTxt pass = XRec pass (WarningTxt pass)

-- | Warning Text
--
-- reason/explanation from a WARNING or DEPRECATED pragma
data WarningTxt pass
   = WarningTxt
      (Maybe (LocatedE InWarningCategory))
        -- ^ Warning category attached to this WARNING pragma, if any;
        -- see Note [Warning categories]
      Source.CodeSnippet
      [LocatedE (WithHsDocIdentifiers Source.StringLiteral pass)]
   | DeprecatedTxt
      Source.CodeSnippet
      [LocatedE (WithHsDocIdentifiers Source.StringLiteral pass)]
  deriving Generic

-- | To which warning category does this WARNING or DEPRECATED pragma belong?
-- See Note [Warning categories].
warningTxtCategory :: WarningTxt pass -> WarningCategory
warningTxtCategory (WarningTxt (Just (L _ (InWarningCategory _  _ (L _ cat)))) _ _) = cat
warningTxtCategory _ = defaultWarningCategory

-- | The message that the WarningTxt was specified to output
warningTxtMessage :: WarningTxt p -> [LocatedE (WithHsDocIdentifiers Source.StringLiteral p)]
warningTxtMessage (WarningTxt _ _ m) = m
warningTxtMessage (DeprecatedTxt _ m) = m

-- | True if the 2 WarningTxts have the same category and messages
warningTxtSame :: WarningTxt p1 -> WarningTxt p2 -> Bool
warningTxtSame w1 w2
  = warningTxtCategory w1 == warningTxtCategory w2
  && literal_message w1 == literal_message w2
  && same_type
  where
    literal_message :: WarningTxt p -> [Source.StringLiteral]
    literal_message = map (hsDocString . unLoc) . warningTxtMessage
    same_type | DeprecatedTxt {} <- w1, DeprecatedTxt {} <- w2 = True
              | WarningTxt {} <- w1, WarningTxt {} <- w2       = True
              | otherwise                                      = False

deriving instance Eq InWarningCategory
deriving instance (Eq (IdP pass)) => Eq (WarningTxt pass)
deriving instance (Data pass, Data (IdP pass)) => Data (WarningTxt pass)
