{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- (c) The University of Glasgow, 1992-2006

-- | This module contains types that relate to the positions of things
-- in source files, and allow tagging of those things with locations
module Language.Haskell.Textual.ExactPrint (
        -- * Exact print locations
        LocatedE,
        EpaLocation,
        EpaLocation'(..),
        EpToken(..),
        EpaComment(..),
        EpaCommentTok(..),
        NoCommentsLocation,
        NoComments(..),
        DeltaPos(..), deltaPos, getDeltaLine,
    ) where

import Data.Data

import GHC.TypeLits (KnownSymbol, Symbol) -- From @base@, not @ghc@ package

import Language.Haskell.Textual.Documentation.String
import Language.Haskell.Textual.Location

import Prelude

-- ---------------------------------------------------------------------
-- The following section contains basic types related to exact printing.
-- See https://gitlab.haskell.org/ghc/ghc/wikis/api-annotations for
-- details.
-- This is only s subset, to prevent import loops. The balance are in
-- GHC.Parser.Annotation
-- ---------------------------------------------------------------------


-- | The anchor for an exact print annotation. The Parser inserts the
-- @'EpaSpan'@ variant, giving the exact location of the original item
-- in the parsed source.  This can be replaced by the @'EpaDelta'@
-- version, to provide a position for the item relative to the end of
-- the previous item in the source.  This is useful when editing an
-- AST prior to exact printing the changed one.
-- The EpaDelta also contains the original @'SrcSpan'@ for use by
-- tools wanting to manipulate the AST after converting it using
-- ghc-exactprint' @'makeDeltaAst'@.

data EpaLocation' a = EpaSpan !SrcSpan
                    | EpaDelta !SrcSpan !DeltaPos !a
                    deriving (Data,Eq,Show)

type NoCommentsLocation = EpaLocation' NoComments

data NoComments = NoComments
  deriving (Data,Eq,Ord,Show)

type EpaLocation = EpaLocation' [LEpaComment]

instance Semigroup EpaLocation where
  EpaSpan s1       <> EpaSpan s2        = EpaSpan (combineSrcSpans s1 s2)
  EpaSpan s1       <> _                 = EpaSpan s1
  _                <> EpaSpan s2        = EpaSpan s2
  EpaDelta s1 dp1 cs1 <> EpaDelta s2 _dp2 cs2 = EpaDelta (combineSrcSpans s1 s2) dp1 (cs1<>cs2)

type LEpaComment = GenLocated NoCommentsLocation EpaComment

type LocatedE = GenLocated EpaLocation

data EpaComment =
  EpaComment
    { ac_tok :: EpaCommentTok
    , ac_prior_tok :: RealSrcSpan
    -- ^ The location of the prior token, used in exact printing.  The
    -- 'EpaComment' appears as an 'LEpaComment' containing its
    -- location.  The difference between the end of the prior token
    -- and the start of this location is used for the spacing when
    -- exact printing the comment.
    }
    deriving (Eq, Data, Show)

data EpaCommentTok =
  -- Documentation annotations
    EpaDocComment   HsDocString -- ^ a docstring that can be pretty printed using pprHsDocString
  | EpaDocOptions   String     -- ^ doc options (prune, ignore-exports, etc)
  | EpaLineComment  String     -- ^ comment starting by "--"
  | EpaBlockComment String     -- ^ comment in {- -}
    deriving (Eq, Data, Show)
-- Note: these are based on the Token versions, but the Token type is
-- defined in GHC.Parser.Lexer and bringing it in here would create a loop

-- | A token stored in the syntax tree. For example, when parsing a
-- let-expression, we store @EpToken "let"@ and @EpToken "in"@.
-- The locations of those tokens can be used to faithfully reproduce
-- (exactprint) the original program text.
data EpToken (tok :: Symbol)
  = NoEpTok
  | EpTok !EpaLocation

deriving instance Eq (EpToken tok)
deriving instance KnownSymbol tok => Data (EpToken tok)

-- | Spacing between output items when exact printing.  It captures
-- the spacing from the current print position on the page to the
-- position required for the thing about to be printed.  This is
-- either on the same line in which case is is simply the number of
-- spaces to emit, or it is some number of lines down, with a given
-- column offset.  The exact printing algorithm keeps track of the
-- column offset pertaining to the current anchor position, so the
-- `deltaColumn` is the additional spaces to add in this case.  See
-- https://gitlab.haskell.org/ghc/ghc/wikis/api-annotations for
-- details.
data DeltaPos
  = SameLine { deltaColumn :: !Int }
  | DifferentLine
      { deltaLine   :: !Int, -- ^ deltaLine should always be > 0
        deltaColumn :: !Int
      } deriving (Show,Eq,Ord,Data)

-- | Smart constructor for a 'DeltaPos'. It preserves the invariant
-- that for the 'DifferentLine' constructor 'deltaLine' is always > 0.
deltaPos :: Int -> Int -> DeltaPos
deltaPos l c = case l of
  0 -> SameLine c
  _ -> DifferentLine l c

getDeltaLine :: DeltaPos -> Int
getDeltaLine (SameLine _) = 0
getDeltaLine (DifferentLine r _) = r
