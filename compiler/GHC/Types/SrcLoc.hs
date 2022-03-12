{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- (c) The University of Glasgow, 1992-2006

-- | This module contains types that relate to the positions of things
-- in source files, and allow tagging of those things with locations
module GHC.Types.SrcLoc (
        -- * SrcLoc
        RealSrcLoc,             -- Abstract
        SrcLoc(..),

        -- ** Constructing SrcLoc
        mkSrcLoc, mkRealSrcLoc, mkGeneralSrcLoc,

        noSrcLoc,               -- "I'm sorry, I haven't a clue"
        generatedSrcLoc,        -- Code generated within the compiler
        interactiveSrcLoc,      -- Code from an interactive session

        advanceSrcLoc,
        advanceBufPos,

        -- ** Unsafely deconstructing SrcLoc
        -- These are dubious exports, because they crash on some inputs
        srcLocFile,             -- return the file name part
        srcLocLine,             -- return the line part
        srcLocCol,              -- return the column part

        -- * SrcSpan
        RealSrcSpan,            -- Abstract
        SrcSpan(..),
        UnhelpfulSpanReason(..),

        -- ** Constructing SrcSpan
        mkGeneralSrcSpan, mkSrcSpan, mkRealSrcSpan,
        noSrcSpan, generatedSrcSpan, isGeneratedSrcSpan,
        wiredInSrcSpan,         -- Something wired into the compiler
        interactiveSrcSpan,
        srcLocSpan, realSrcLocSpan,
        combineSrcSpans,
        srcSpanFirstCharacter,

        -- ** Deconstructing SrcSpan
        srcSpanStart, srcSpanEnd,
        realSrcSpanStart, realSrcSpanEnd,
        srcSpanFileName_maybe,
        pprUserRealSpan, pprUnhelpfulSpanReason,
        pprUserSpan,
        unhelpfulSpanFS,
        srcSpanToRealSrcSpan,

        -- ** Unsafely deconstructing SrcSpan
        -- These are dubious exports, because they crash on some inputs
        srcSpanFile,
        srcSpanStartLine, srcSpanEndLine,
        srcSpanStartCol, srcSpanEndCol,

        -- ** Predicates on SrcSpan
        isGoodSrcSpan, isOneLineSpan, isZeroWidthSpan,
        containsSpan,

        -- * StringBuffer locations
        BufPos(..),
        getBufPos,
        BufSpan(..),
        getBufSpan,

        -- * Located
        Located,
        RealLocated,
        GenLocated(..),

        -- ** Constructing Located
        noLoc,
        mkGeneralLocated,

        -- ** Deconstructing Located
        getLoc, unLoc,
        unRealSrcSpan, getRealSrcSpan,
        pprLocated,
        pprLocatedAlways,

        -- ** Modifying Located
        mapLoc,

        -- ** Combining and comparing Located values
        eqLocated, cmpLocated, cmpBufSpan,
        combineLocs, addCLoc,
        leftmost_smallest, leftmost_largest, rightmost_smallest,
        spans, isSubspanOf, isRealSubspanOf,
        sortLocated, sortRealLocated,
        lookupSrcLoc, lookupSrcSpan,

        liftL,

        -- * Parser locations
        PsLoc(..),
        PsSpan(..),
        PsLocated,
        advancePsLoc,
        mkPsSpan,
        psSpanStart,
        psSpanEnd,
        mkSrcSpanPs,
        combineRealSrcSpans,
        psLocatedToLocated,

        -- * Layout information
        LayoutInfo(..),
        leftmostColumn

    ) where

import GHC.Prelude

import GHC.Utils.Misc
import GHC.Utils.Json
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.FastString
import qualified GHC.Data.Strict as Strict

import Control.DeepSeq
import Control.Applicative (liftA2)
import Data.Data
import Data.List (sortBy, intercalate)
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.Semigroup

{-
************************************************************************
*                                                                      *
\subsection[SrcLoc-SrcLocations]{Source-location information}
*                                                                      *
************************************************************************

We keep information about the {\em definition} point for each entity;
this is the obvious stuff:
-}

-- | Real Source Location
--
-- Represents a single point within a file
data RealSrcLoc
  = SrcLoc      LexicalFastString       -- A precise location (file name)
                {-# UNPACK #-} !Int     -- line number, begins at 1
                {-# UNPACK #-} !Int     -- column number, begins at 1
  deriving (Eq, Ord)

-- | 0-based offset identifying the raw location in the 'StringBuffer'.
--
-- The lexer increments the 'BufPos' every time a character (UTF-8 code point)
-- is read from the input buffer. As UTF-8 is a variable-length encoding and
-- 'StringBuffer' needs a byte offset for indexing, a 'BufPos' cannot be used
-- for indexing.
--
-- The parser guarantees that 'BufPos' are monotonic. See #17632. This means
-- that syntactic constructs that appear later in the 'StringBuffer' are guaranteed to
-- have a higher 'BufPos'. Constrast that with 'RealSrcLoc', which does *not* make the
-- analogous guarantee about higher line/column numbers.
--
-- This is due to #line and {-# LINE ... #-} pragmas that can arbitrarily
-- modify 'RealSrcLoc'. Notice how 'setSrcLoc' and 'resetAlrLastLoc' in
-- "GHC.Parser.Lexer" update 'PsLoc', modifying 'RealSrcLoc' but preserving
-- 'BufPos'.
--
-- Monotonicity makes 'BufPos' useful to determine the order in which syntactic
-- elements appear in the source. Consider this example (haddockA041 in the test suite):
--
--  haddockA041.hs
--      {-# LANGUAGE CPP #-}
--      -- | Module header documentation
--      module Comments_and_CPP_include where
--      #include "IncludeMe.hs"
--
--  IncludeMe.hs:
--      -- | Comment on T
--      data T = MkT -- ^ Comment on MkT
--
-- After the C preprocessor runs, the 'StringBuffer' will contain a program that
-- looks like this (unimportant lines at the beginning removed):
--
--    # 1 "haddockA041.hs"
--    {-# LANGUAGE CPP #-}
--    -- | Module header documentation
--    module Comments_and_CPP_include where
--    # 1 "IncludeMe.hs" 1
--    -- | Comment on T
--    data T = MkT -- ^ Comment on MkT
--    # 7 "haddockA041.hs" 2
--
-- The line pragmas inserted by CPP make the error messages more informative.
-- The downside is that we can't use RealSrcLoc to determine the ordering of
-- syntactic elements.
--
-- With RealSrcLoc, we have the following location information recorded in the AST:
--   * The module name is located at haddockA041.hs:3:8-31
--   * The Haddock comment "Comment on T" is located at IncludeMe:1:1-17
--   * The data declaration is located at IncludeMe.hs:2:1-32
--
-- Is the Haddock comment located between the module name and the data
-- declaration? This is impossible to tell because the locations are not
-- comparable; they even refer to different files.
--
-- On the other hand, with 'BufPos', we have the following location information:
--   * The module name is located at 846-870
--   * The Haddock comment "Comment on T" is located at 898-915
--   * The data declaration is located at 916-928
--
-- Aside:  if you're wondering why the numbers are so high, try running
--           @ghc -E haddockA041.hs@
--         and see the extra fluff that CPP inserts at the start of the file.
--
-- For error messages, 'BufPos' is not useful at all. On the other hand, this is
-- exactly what we need to determine the order of syntactic elements:
--    870 < 898, therefore the Haddock comment appears *after* the module name.
--    915 < 916, therefore the Haddock comment appears *before* the data declaration.
--
-- We use 'BufPos' in in GHC.Parser.PostProcess.Haddock to associate Haddock
-- comments with parts of the AST using location information (#17544).
newtype BufPos = BufPos { bufPos :: Int }
  deriving (Eq, Ord, Show, Data)

-- | Source Location
data SrcLoc
  = RealSrcLoc !RealSrcLoc !(Strict.Maybe BufPos)  -- See Note [Why Maybe BufPos]
  | UnhelpfulLoc !FastString     -- Just a general indication
  deriving (Eq, Show)

{-
************************************************************************
*                                                                      *
\subsection[SrcLoc-access-fns]{Access functions}
*                                                                      *
************************************************************************
-}

mkSrcLoc :: FastString -> Int -> Int -> SrcLoc
mkSrcLoc x line col = RealSrcLoc (mkRealSrcLoc x line col) Strict.Nothing

mkRealSrcLoc :: FastString -> Int -> Int -> RealSrcLoc
mkRealSrcLoc x line col = SrcLoc (LexicalFastString x) line col

getBufPos :: SrcLoc -> Strict.Maybe BufPos
getBufPos (RealSrcLoc _ mbpos) = mbpos
getBufPos (UnhelpfulLoc _) = Strict.Nothing

-- | Built-in "bad" 'SrcLoc' values for particular locations
noSrcLoc, generatedSrcLoc, interactiveSrcLoc :: SrcLoc
noSrcLoc          = UnhelpfulLoc (fsLit "<no location info>")
generatedSrcLoc   = UnhelpfulLoc (fsLit "<compiler-generated code>")
interactiveSrcLoc = UnhelpfulLoc (fsLit "<interactive>")

-- | Creates a "bad" 'SrcLoc' that has no detailed information about its location
mkGeneralSrcLoc :: FastString -> SrcLoc
mkGeneralSrcLoc = UnhelpfulLoc

-- | Gives the filename of the 'RealSrcLoc'
srcLocFile :: RealSrcLoc -> FastString
srcLocFile (SrcLoc (LexicalFastString fname) _ _) = fname

-- | Raises an error when used on a "bad" 'SrcLoc'
srcLocLine :: RealSrcLoc -> Int
srcLocLine (SrcLoc _ l _) = l

-- | Raises an error when used on a "bad" 'SrcLoc'
srcLocCol :: RealSrcLoc -> Int
srcLocCol (SrcLoc _ _ c) = c

-- | Move the 'SrcLoc' down by one line if the character is a newline,
-- to the next 8-char tabstop if it is a tab, and across by one
-- character in any other case
advanceSrcLoc :: RealSrcLoc -> Char -> RealSrcLoc
advanceSrcLoc (SrcLoc f l _) '\n' = SrcLoc f  (l + 1) 1
advanceSrcLoc (SrcLoc f l c) '\t' = SrcLoc f  l (advance_tabstop c)
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f  l (c + 1)

advance_tabstop :: Int -> Int
advance_tabstop c = ((((c - 1) `shiftR` 3) + 1) `shiftL` 3) + 1

advanceBufPos :: BufPos -> BufPos
advanceBufPos (BufPos i) = BufPos (i+1)

{-
************************************************************************
*                                                                      *
\subsection[SrcLoc-instances]{Instance declarations for various names}
*                                                                      *
************************************************************************
-}

sortLocated :: [Located a] -> [Located a]
sortLocated = sortBy (leftmost_smallest `on` getLoc)

sortRealLocated :: [RealLocated a] -> [RealLocated a]
sortRealLocated = sortBy (compare `on` getLoc)

lookupSrcLoc :: SrcLoc -> Map.Map RealSrcLoc a -> Maybe a
lookupSrcLoc (RealSrcLoc l _) = Map.lookup l
lookupSrcLoc (UnhelpfulLoc _) = const Nothing

lookupSrcSpan :: SrcSpan -> Map.Map RealSrcSpan a -> Maybe a
lookupSrcSpan (RealSrcSpan l _) = Map.lookup l
lookupSrcSpan (UnhelpfulSpan _) = const Nothing

instance Outputable RealSrcLoc where
    ppr (SrcLoc (LexicalFastString src_path) src_line src_col)
      = hcat [ pprFastFilePath src_path <> colon
             , int src_line <> colon
             , int src_col ]

-- I don't know why there is this style-based difference
--        if userStyle sty || debugStyle sty then
--            hcat [ pprFastFilePath src_path, char ':',
--                   int src_line,
--                   char ':', int src_col
--                 ]
--        else
--            hcat [text "{-# LINE ", int src_line, space,
--                  char '\"', pprFastFilePath src_path, text " #-}"]

instance Outputable SrcLoc where
    ppr (RealSrcLoc l _) = ppr l
    ppr (UnhelpfulLoc s)  = ftext s

instance Data RealSrcSpan where
  -- don't traverse?
  toConstr _   = abstractConstr "RealSrcSpan"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "RealSrcSpan"

instance Data SrcSpan where
  -- don't traverse?
  toConstr _   = abstractConstr "SrcSpan"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "SrcSpan"

{-
************************************************************************
*                                                                      *
\subsection[SrcSpan]{Source Spans}
*                                                                      *
************************************************************************
-}

{- |
A 'RealSrcSpan' delimits a portion of a text file.  It could be represented
by a pair of (line,column) coordinates, but in fact we optimise
slightly by using more compact representations for single-line and
zero-length spans, both of which are quite common.

The end position is defined to be the column /after/ the end of the
span.  That is, a span of (1,1)-(1,2) is one character long, and a
span of (1,1)-(1,1) is zero characters long.
-}

-- | Real Source Span
data RealSrcSpan
  = RealSrcSpan'
        { srcSpanFile     :: !FastString,
          srcSpanSLine    :: {-# UNPACK #-} !Int,
          srcSpanSCol     :: {-# UNPACK #-} !Int,
          srcSpanELine    :: {-# UNPACK #-} !Int,
          srcSpanECol     :: {-# UNPACK #-} !Int
        }
  deriving Eq

-- | StringBuffer Source Span
data BufSpan =
  BufSpan { bufSpanStart, bufSpanEnd :: {-# UNPACK #-} !BufPos }
  deriving (Eq, Ord, Show, Data)

instance Semigroup BufSpan where
  BufSpan start1 end1 <> BufSpan start2 end2 =
    BufSpan (min start1 start2) (max end1 end2)

-- | Source Span
--
-- A 'SrcSpan' identifies either a specific portion of a text file
-- or a human-readable description of a location.
data SrcSpan =
    RealSrcSpan !RealSrcSpan !(Strict.Maybe BufSpan)  -- See Note [Why Maybe BufPos]
  | UnhelpfulSpan !UnhelpfulSpanReason

  deriving (Eq, Show) -- Show is used by GHC.Parser.Lexer, because we
                      -- derive Show for Token

data UnhelpfulSpanReason
  = UnhelpfulNoLocationInfo
  | UnhelpfulWiredIn
  | UnhelpfulInteractive
  | UnhelpfulGenerated
  | UnhelpfulOther !FastString
  deriving (Eq, Show)

{- Note [Why Maybe BufPos]
~~~~~~~~~~~~~~~~~~~~~~~~~~
In SrcLoc we store (Maybe BufPos); in SrcSpan we store (Maybe BufSpan).
Why the Maybe?

Surely, the lexer can always fill in the buffer position, and it guarantees to do so.
However, sometimes the SrcLoc/SrcSpan is constructed in a different context
where the buffer location is not available, and then we use Nothing instead of
a fake value like BufPos (-1).

Perhaps the compiler could be re-engineered to pass around BufPos more
carefully and never discard it, and this 'Maybe' could be removed. If you're
interested in doing so, you may find this ripgrep query useful:

  rg "RealSrc(Loc|Span).*?Nothing"

For example, it is not uncommon to whip up source locations for e.g. error
messages, constructing a SrcSpan without a BufSpan.
-}

instance ToJson SrcSpan where
  json (UnhelpfulSpan {} ) = JSNull --JSObject [( "type", "unhelpful")]
  json (RealSrcSpan rss _) = json rss

instance ToJson RealSrcSpan where
  json (RealSrcSpan'{..}) = JSObject [ ("file", JSString (unpackFS srcSpanFile))
                                     , ("startLine", JSInt srcSpanSLine)
                                     , ("startCol", JSInt srcSpanSCol)
                                     , ("endLine", JSInt srcSpanELine)
                                     , ("endCol", JSInt srcSpanECol)
                                     ]

instance NFData SrcSpan where
  rnf x = x `seq` ()

getBufSpan :: SrcSpan -> Strict.Maybe BufSpan
getBufSpan (RealSrcSpan _ mbspan) = mbspan
getBufSpan (UnhelpfulSpan _) = Strict.Nothing

-- | Built-in "bad" 'SrcSpan's for common sources of location uncertainty
noSrcSpan, generatedSrcSpan, wiredInSrcSpan, interactiveSrcSpan :: SrcSpan
noSrcSpan          = UnhelpfulSpan UnhelpfulNoLocationInfo
wiredInSrcSpan     = UnhelpfulSpan UnhelpfulWiredIn
interactiveSrcSpan = UnhelpfulSpan UnhelpfulInteractive
generatedSrcSpan   = UnhelpfulSpan UnhelpfulGenerated

isGeneratedSrcSpan :: SrcSpan -> Bool
isGeneratedSrcSpan (UnhelpfulSpan UnhelpfulGenerated) = True
isGeneratedSrcSpan _                                  = False

-- | Create a "bad" 'SrcSpan' that has not location information
mkGeneralSrcSpan :: FastString -> SrcSpan
mkGeneralSrcSpan = UnhelpfulSpan . UnhelpfulOther

-- | Create a 'SrcSpan' corresponding to a single point
srcLocSpan :: SrcLoc -> SrcSpan
srcLocSpan (UnhelpfulLoc str) = UnhelpfulSpan (UnhelpfulOther str)
srcLocSpan (RealSrcLoc l mb) = RealSrcSpan (realSrcLocSpan l) (fmap (\b -> BufSpan b b) mb)

realSrcLocSpan :: RealSrcLoc -> RealSrcSpan
realSrcLocSpan (SrcLoc (LexicalFastString file) line col) = RealSrcSpan' file line col line col

-- | Create a 'SrcSpan' between two points in a file
mkRealSrcSpan :: RealSrcLoc -> RealSrcLoc -> RealSrcSpan
mkRealSrcSpan loc1 loc2 = RealSrcSpan' file line1 col1 line2 col2
  where
        line1 = srcLocLine loc1
        line2 = srcLocLine loc2
        col1 = srcLocCol loc1
        col2 = srcLocCol loc2
        file = srcLocFile loc1

-- | 'True' if the span is known to straddle only one line.
isOneLineRealSpan :: RealSrcSpan -> Bool
isOneLineRealSpan (RealSrcSpan' _ line1 _ line2 _)
  = line1 == line2

-- | 'True' if the span is a single point
isPointRealSpan :: RealSrcSpan -> Bool
isPointRealSpan (RealSrcSpan' _ line1 col1 line2 col2)
  = line1 == line2 && col1 == col2

-- | Create a 'SrcSpan' between two points in a file
mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan (UnhelpfulLoc str) _ = UnhelpfulSpan (UnhelpfulOther str)
mkSrcSpan _ (UnhelpfulLoc str) = UnhelpfulSpan (UnhelpfulOther str)
mkSrcSpan (RealSrcLoc loc1 mbpos1) (RealSrcLoc loc2 mbpos2)
    = RealSrcSpan (mkRealSrcSpan loc1 loc2) (liftA2 BufSpan mbpos1 mbpos2)

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Returns UnhelpfulSpan if the files differ.
combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans (UnhelpfulSpan _) r = r -- this seems more useful
combineSrcSpans l (UnhelpfulSpan _) = l
combineSrcSpans (RealSrcSpan span1 mbspan1) (RealSrcSpan span2 mbspan2)
  | srcSpanFile span1 == srcSpanFile span2
      = RealSrcSpan (combineRealSrcSpans span1 span2) (liftA2 combineBufSpans mbspan1 mbspan2)
  | otherwise = UnhelpfulSpan $
      UnhelpfulOther (fsLit "<combineSrcSpans: files differ>")

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Assumes the "file" part is the same in both inputs
combineRealSrcSpans :: RealSrcSpan -> RealSrcSpan -> RealSrcSpan
combineRealSrcSpans span1 span2
  = RealSrcSpan' file line_start col_start line_end col_end
  where
    (line_start, col_start) = min (srcSpanStartLine span1, srcSpanStartCol span1)
                                  (srcSpanStartLine span2, srcSpanStartCol span2)
    (line_end, col_end)     = max (srcSpanEndLine span1, srcSpanEndCol span1)
                                  (srcSpanEndLine span2, srcSpanEndCol span2)
    file = srcSpanFile span1

combineBufSpans :: BufSpan -> BufSpan -> BufSpan
combineBufSpans span1 span2 = BufSpan start end
  where
    start = min (bufSpanStart span1) (bufSpanStart span2)
    end   = max (bufSpanEnd   span1) (bufSpanEnd   span2)


-- | Convert a SrcSpan into one that represents only its first character
srcSpanFirstCharacter :: SrcSpan -> SrcSpan
srcSpanFirstCharacter l@(UnhelpfulSpan {}) = l
srcSpanFirstCharacter (RealSrcSpan span mbspan) =
    RealSrcSpan (mkRealSrcSpan loc1 loc2) (fmap mkBufSpan mbspan)
  where
    loc1@(SrcLoc f l c) = realSrcSpanStart span
    loc2 = SrcLoc f l (c+1)
    mkBufSpan bspan =
      let bpos1@(BufPos i) = bufSpanStart bspan
          bpos2 = BufPos (i+1)
      in BufSpan bpos1 bpos2

{-
************************************************************************
*                                                                      *
\subsection[SrcSpan-predicates]{Predicates}
*                                                                      *
************************************************************************
-}

-- | Test if a 'SrcSpan' is "good", i.e. has precise location information
isGoodSrcSpan :: SrcSpan -> Bool
isGoodSrcSpan (RealSrcSpan _ _) = True
isGoodSrcSpan (UnhelpfulSpan _) = False

isOneLineSpan :: SrcSpan -> Bool
-- ^ True if the span is known to straddle only one line.
-- For "bad" 'SrcSpan', it returns False
isOneLineSpan (RealSrcSpan s _) = srcSpanStartLine s == srcSpanEndLine s
isOneLineSpan (UnhelpfulSpan _) = False

isZeroWidthSpan :: SrcSpan -> Bool
-- ^ True if the span has a width of zero, as returned for "virtual"
-- semicolons in the lexer.
-- For "bad" 'SrcSpan', it returns False
isZeroWidthSpan (RealSrcSpan s _) = srcSpanStartLine s == srcSpanEndLine s
                                 && srcSpanStartCol s == srcSpanEndCol s
isZeroWidthSpan (UnhelpfulSpan _) = False

-- | Tests whether the first span "contains" the other span, meaning
-- that it covers at least as much source code. True where spans are equal.
containsSpan :: RealSrcSpan -> RealSrcSpan -> Bool
containsSpan s1 s2
  = (srcSpanStartLine s1, srcSpanStartCol s1)
       <= (srcSpanStartLine s2, srcSpanStartCol s2)
    && (srcSpanEndLine s1, srcSpanEndCol s1)
       >= (srcSpanEndLine s2, srcSpanEndCol s2)
    && (srcSpanFile s1 == srcSpanFile s2)
    -- We check file equality last because it is (presumably?) least
    -- likely to fail.
{-
%************************************************************************
%*                                                                      *
\subsection[SrcSpan-unsafe-access-fns]{Unsafe access functions}
*                                                                      *
************************************************************************
-}

srcSpanStartLine :: RealSrcSpan -> Int
srcSpanEndLine :: RealSrcSpan -> Int
srcSpanStartCol :: RealSrcSpan -> Int
srcSpanEndCol :: RealSrcSpan -> Int

srcSpanStartLine RealSrcSpan'{ srcSpanSLine=l } = l
srcSpanEndLine RealSrcSpan'{ srcSpanELine=l } = l
srcSpanStartCol RealSrcSpan'{ srcSpanSCol=l } = l
srcSpanEndCol RealSrcSpan'{ srcSpanECol=c } = c

{-
************************************************************************
*                                                                      *
\subsection[SrcSpan-access-fns]{Access functions}
*                                                                      *
************************************************************************
-}

-- | Returns the location at the start of the 'SrcSpan' or a "bad" 'SrcSpan' if that is unavailable
srcSpanStart :: SrcSpan -> SrcLoc
srcSpanStart (UnhelpfulSpan r) = UnhelpfulLoc (unhelpfulSpanFS r)
srcSpanStart (RealSrcSpan s b) = RealSrcLoc (realSrcSpanStart s) (fmap bufSpanStart b)

-- | Returns the location at the end of the 'SrcSpan' or a "bad" 'SrcSpan' if that is unavailable
srcSpanEnd :: SrcSpan -> SrcLoc
srcSpanEnd (UnhelpfulSpan r) = UnhelpfulLoc (unhelpfulSpanFS r)
srcSpanEnd (RealSrcSpan s b) = RealSrcLoc (realSrcSpanEnd s) (fmap bufSpanEnd b)

realSrcSpanStart :: RealSrcSpan -> RealSrcLoc
realSrcSpanStart s = mkRealSrcLoc (srcSpanFile s)
                                  (srcSpanStartLine s)
                                  (srcSpanStartCol s)

realSrcSpanEnd :: RealSrcSpan -> RealSrcLoc
realSrcSpanEnd s = mkRealSrcLoc (srcSpanFile s)
                                (srcSpanEndLine s)
                                (srcSpanEndCol s)

-- | Obtains the filename for a 'SrcSpan' if it is "good"
srcSpanFileName_maybe :: SrcSpan -> Maybe FastString
srcSpanFileName_maybe (RealSrcSpan s _) = Just (srcSpanFile s)
srcSpanFileName_maybe (UnhelpfulSpan _) = Nothing

srcSpanToRealSrcSpan :: SrcSpan -> Maybe RealSrcSpan
srcSpanToRealSrcSpan (RealSrcSpan ss _) = Just ss
srcSpanToRealSrcSpan _ = Nothing

{-
************************************************************************
*                                                                      *
\subsection[SrcSpan-instances]{Instances}
*                                                                      *
************************************************************************
-}

-- We want to order RealSrcSpans first by the start point, then by the
-- end point.
instance Ord RealSrcSpan where
  a `compare` b =
     (realSrcSpanStart a `compare` realSrcSpanStart b) `thenCmp`
     (realSrcSpanEnd   a `compare` realSrcSpanEnd   b)

instance Show RealSrcLoc where
  show (SrcLoc filename row col)
      = "SrcLoc " ++ show filename ++ " " ++ show row ++ " " ++ show col

-- Show is used by GHC.Parser.Lexer, because we derive Show for Token
instance Show RealSrcSpan where
  show span@(RealSrcSpan' file sl sc el ec)
    | isPointRealSpan span
    = "SrcSpanPoint " ++ show file ++ " " ++ intercalate " " (map show [sl,sc])

    | isOneLineRealSpan span
    = "SrcSpanOneLine " ++ show file ++ " "
                        ++ intercalate " " (map show [sl,sc,ec])

    | otherwise
    = "SrcSpanMultiLine " ++ show file ++ " "
                          ++ intercalate " " (map show [sl,sc,el,ec])


instance Outputable RealSrcSpan where
    ppr span = pprUserRealSpan True span

-- I don't know why there is this style-based difference
--      = getPprStyle $ \ sty ->
--        if userStyle sty || debugStyle sty then
--           text (showUserRealSpan True span)
--        else
--           hcat [text "{-# LINE ", int (srcSpanStartLine span), space,
--                 char '\"', pprFastFilePath $ srcSpanFile span, text " #-}"]

instance Outputable SrcSpan where
    ppr span = pprUserSpan True span

instance Outputable UnhelpfulSpanReason where
    ppr = pprUnhelpfulSpanReason

-- I don't know why there is this style-based difference
--      = getPprStyle $ \ sty ->
--        if userStyle sty || debugStyle sty then
--           pprUserSpan True span
--        else
--           case span of
--           UnhelpfulSpan _ -> panic "Outputable UnhelpfulSpan"
--           RealSrcSpan s -> ppr s

unhelpfulSpanFS :: UnhelpfulSpanReason -> FastString
unhelpfulSpanFS r = case r of
  UnhelpfulOther s        -> s
  UnhelpfulNoLocationInfo -> fsLit "<no location info>"
  UnhelpfulWiredIn        -> fsLit "<wired into compiler>"
  UnhelpfulInteractive    -> fsLit "<interactive>"
  UnhelpfulGenerated      -> fsLit "<generated>"

pprUnhelpfulSpanReason :: UnhelpfulSpanReason -> SDoc
pprUnhelpfulSpanReason r = ftext (unhelpfulSpanFS r)

pprUserSpan :: Bool -> SrcSpan -> SDoc
pprUserSpan _         (UnhelpfulSpan r) = pprUnhelpfulSpanReason r
pprUserSpan show_path (RealSrcSpan s _) = pprUserRealSpan show_path s

pprUserRealSpan :: Bool -> RealSrcSpan -> SDoc
pprUserRealSpan show_path span@(RealSrcSpan' src_path line col _ _)
  | isPointRealSpan span
  = hcat [ ppWhen show_path (pprFastFilePath src_path <> colon)
         , int line <> colon
         , int col ]

pprUserRealSpan show_path span@(RealSrcSpan' src_path line scol _ ecol)
  | isOneLineRealSpan span
  = hcat [ ppWhen show_path (pprFastFilePath src_path <> colon)
         , int line <> colon
         , int scol
         , ppUnless (ecol - scol <= 1) (char '-' <> int (ecol - 1)) ]
            -- For single-character or point spans, we just
            -- output the starting column number

pprUserRealSpan show_path (RealSrcSpan' src_path sline scol eline ecol)
  = hcat [ ppWhen show_path (pprFastFilePath src_path <> colon)
         , parens (int sline <> comma <> int scol)
         , char '-'
         , parens (int eline <> comma <> int ecol') ]
 where
   ecol' = if ecol == 0 then ecol else ecol - 1

{-
************************************************************************
*                                                                      *
\subsection[Located]{Attaching SrcSpans to things}
*                                                                      *
************************************************************************
-}

-- | We attach SrcSpans to lots of things, so let's have a datatype for it.
data GenLocated l e = L l e
  deriving (Eq, Ord, Show, Data, Functor, Foldable, Traversable)

type Located = GenLocated SrcSpan
type RealLocated = GenLocated RealSrcSpan

mapLoc :: (a -> b) -> GenLocated l a -> GenLocated l b
mapLoc = fmap

unLoc :: GenLocated l e -> e
unLoc (L _ e) = e

getLoc :: GenLocated l e -> l
getLoc (L l _) = l

noLoc :: e -> Located e
noLoc e = L noSrcSpan e

mkGeneralLocated :: String -> e -> Located e
mkGeneralLocated s e = L (mkGeneralSrcSpan (fsLit s)) e

combineLocs :: Located a -> Located b -> SrcSpan
combineLocs a b = combineSrcSpans (getLoc a) (getLoc b)

-- | Combine locations from two 'Located' things and add them to a third thing
addCLoc :: Located a -> Located b -> c -> Located c
addCLoc a b c = L (combineSrcSpans (getLoc a) (getLoc b)) c

-- not clear whether to add a general Eq instance, but this is useful sometimes:

-- | Tests whether the two located things are equal
eqLocated :: Eq a => GenLocated l a -> GenLocated l a -> Bool
eqLocated a b = unLoc a == unLoc b

-- not clear whether to add a general Ord instance, but this is useful sometimes:

-- | Tests the ordering of the two located things
cmpLocated :: Ord a => GenLocated l a -> GenLocated l a -> Ordering
cmpLocated a b = unLoc a `compare` unLoc b

-- | Compare the 'BufSpan' of two located things.
--
-- Precondition: both operands have an associated 'BufSpan'.
cmpBufSpan :: HasDebugCallStack => Located a -> Located a -> Ordering
cmpBufSpan (L l1 _) (L l2  _)
  | Strict.Just a <- getBufSpan l1
  , Strict.Just b <- getBufSpan l2
  = compare a b

  | otherwise = panic "cmpBufSpan: no BufSpan"

instance (Outputable e) => Outputable (Located e) where
  ppr (L l e) = -- GenLocated:
                -- Print spans without the file name etc
                whenPprDebug (braces (pprUserSpan False l))
             $$ ppr e
instance (Outputable e) => Outputable (GenLocated RealSrcSpan e) where
  ppr (L l e) = -- GenLocated:
                -- Print spans without the file name etc
                whenPprDebug (braces (pprUserSpan False (RealSrcSpan l Strict.Nothing)))
             $$ ppr e


pprLocated :: (Outputable l, Outputable e) => GenLocated l e -> SDoc
pprLocated (L l e) =
                -- Print spans without the file name etc
                whenPprDebug (braces (ppr l))
             $$ ppr e

-- | Always prints the location, even without -dppr-debug
pprLocatedAlways :: (Outputable l, Outputable e) => GenLocated l e -> SDoc
pprLocatedAlways (L l e) =
     braces (ppr l)
  $$ ppr e

{-
************************************************************************
*                                                                      *
\subsection{Ordering SrcSpans for InteractiveUI}
*                                                                      *
************************************************************************
-}

-- | Strategies for ordering 'SrcSpan's
leftmost_smallest, leftmost_largest, rightmost_smallest :: SrcSpan -> SrcSpan -> Ordering
rightmost_smallest = compareSrcSpanBy (flip compare)
leftmost_smallest = compareSrcSpanBy compare
leftmost_largest = compareSrcSpanBy $ \a b ->
  (realSrcSpanStart a `compare` realSrcSpanStart b)
    `thenCmp`
  (realSrcSpanEnd b `compare` realSrcSpanEnd a)

compareSrcSpanBy :: (RealSrcSpan -> RealSrcSpan -> Ordering) -> SrcSpan -> SrcSpan -> Ordering
compareSrcSpanBy cmp (RealSrcSpan a _) (RealSrcSpan b _) = cmp a b
compareSrcSpanBy _   (RealSrcSpan _ _) (UnhelpfulSpan _) = LT
compareSrcSpanBy _   (UnhelpfulSpan _) (RealSrcSpan _ _) = GT
compareSrcSpanBy _   (UnhelpfulSpan _) (UnhelpfulSpan _) = EQ

-- | Determines whether a span encloses a given line and column index
spans :: SrcSpan -> (Int, Int) -> Bool
spans (UnhelpfulSpan _) _ = panic "spans UnhelpfulSpan"
spans (RealSrcSpan span _) (l,c) = realSrcSpanStart span <= loc && loc <= realSrcSpanEnd span
   where loc = mkRealSrcLoc (srcSpanFile span) l c

-- | Determines whether a span is enclosed by another one
isSubspanOf :: SrcSpan -- ^ The span that may be enclosed by the other
            -> SrcSpan -- ^ The span it may be enclosed by
            -> Bool
isSubspanOf (RealSrcSpan src _) (RealSrcSpan parent _) = isRealSubspanOf src parent
isSubspanOf _ _ = False

-- | Determines whether a span is enclosed by another one
isRealSubspanOf :: RealSrcSpan -- ^ The span that may be enclosed by the other
                -> RealSrcSpan -- ^ The span it may be enclosed by
                -> Bool
isRealSubspanOf src parent
    | srcSpanFile parent /= srcSpanFile src = False
    | otherwise = realSrcSpanStart parent <= realSrcSpanStart src &&
                  realSrcSpanEnd parent   >= realSrcSpanEnd src

liftL :: Monad m => (a -> m b) -> GenLocated l a -> m (GenLocated l b)
liftL f (L loc a) = do
  a' <- f a
  return $ L loc a'

getRealSrcSpan :: RealLocated a -> RealSrcSpan
getRealSrcSpan (L l _) = l

unRealSrcSpan :: RealLocated a -> a
unRealSrcSpan  (L _ e) = e


-- | A location as produced by the parser. Consists of two components:
--
-- * The location in the file, adjusted for #line and {-# LINE ... #-} pragmas (RealSrcLoc)
-- * The location in the string buffer (BufPos) with monotonicity guarantees (see #17632)
data PsLoc
  = PsLoc { psRealLoc :: !RealSrcLoc, psBufPos :: !BufPos }
  deriving (Eq, Ord, Show)

data PsSpan
  = PsSpan { psRealSpan :: !RealSrcSpan, psBufSpan :: !BufSpan }
  deriving (Eq, Ord, Show, Data)

type PsLocated = GenLocated PsSpan

psLocatedToLocated :: PsLocated a -> Located a
psLocatedToLocated (L sp a) = L (mkSrcSpanPs sp) a

advancePsLoc :: PsLoc -> Char -> PsLoc
advancePsLoc (PsLoc real_loc buf_loc) c =
  PsLoc (advanceSrcLoc real_loc c) (advanceBufPos buf_loc)

mkPsSpan :: PsLoc -> PsLoc -> PsSpan
mkPsSpan (PsLoc r1 b1) (PsLoc r2 b2) = PsSpan (mkRealSrcSpan r1 r2) (BufSpan b1 b2)

psSpanStart :: PsSpan -> PsLoc
psSpanStart (PsSpan r b) = PsLoc (realSrcSpanStart r) (bufSpanStart b)

psSpanEnd :: PsSpan -> PsLoc
psSpanEnd (PsSpan r b) = PsLoc (realSrcSpanEnd r) (bufSpanEnd b)

mkSrcSpanPs :: PsSpan -> SrcSpan
mkSrcSpanPs (PsSpan r b) = RealSrcSpan r (Strict.Just b)

-- | Layout information for declarations.
data LayoutInfo =

    -- | Explicit braces written by the user.
    --
    -- @
    -- class C a where { foo :: a; bar :: a }
    -- @
    ExplicitBraces
  |
    -- | Virtual braces inserted by the layout algorithm.
    --
    -- @
    -- class C a where
    --   foo :: a
    --   bar :: a
    -- @
    VirtualBraces
      !Int -- ^ Layout column (indentation level, begins at 1)
  |
    -- | Empty or compiler-generated blocks do not have layout information
    -- associated with them.
    NoLayoutInfo

  deriving (Eq, Ord, Show, Data)

-- | Indentation level is 1-indexed, so the leftmost column is 1.
leftmostColumn :: Int
leftmostColumn = 1
