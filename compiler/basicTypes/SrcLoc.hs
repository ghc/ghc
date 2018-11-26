-- (c) The University of Glasgow, 1992-2006

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE PatternSynonyms    #-}


-- | This module contains types that relate to the positions of things
-- in source files, and allow tagging of those things with locations
module SrcLoc (
        -- * SrcLoc
        RealSrcLoc,             -- Abstract
        SrcLoc(..),

        -- ** Constructing SrcLoc
        mkSrcLoc, mkRealSrcLoc, mkGeneralSrcLoc,

        noSrcLoc,               -- "I'm sorry, I haven't a clue"
        generatedSrcLoc,        -- Code generated within the compiler
        interactiveSrcLoc,      -- Code from an interactive session

        advanceSrcLoc,

        -- ** Unsafely deconstructing SrcLoc
        -- These are dubious exports, because they crash on some inputs
        srcLocFile,             -- return the file name part
        srcLocLine,             -- return the line part
        srcLocCol,              -- return the column part

        -- * SrcSpan
        RealSrcSpan,            -- Abstract
        SrcSpan(..),

        -- ** Constructing SrcSpan
        mkGeneralSrcSpan, mkSrcSpan, mkRealSrcSpan,
        noSrcSpan,
        wiredInSrcSpan,         -- Something wired into the compiler
        interactiveSrcSpan,
        srcLocSpan, realSrcLocSpan,
        combineSrcSpans,
        srcSpanFirstCharacter,

        -- ** Deconstructing SrcSpan
        srcSpanStart, srcSpanEnd,
        realSrcSpanStart, realSrcSpanEnd,
        srcSpanFileName_maybe,
        pprUserRealSpan,

        -- ** Unsafely deconstructing SrcSpan
        -- These are dubious exports, because they crash on some inputs
        srcSpanFile,
        srcSpanStartLine, srcSpanEndLine,
        srcSpanStartCol, srcSpanEndCol,

        -- ** Predicates on SrcSpan
        isGoodSrcSpan, isOneLineSpan,
        containsSpan,

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

        -- ** Combining and comparing Located values
        eqLocated, cmpLocated, combineLocs, addCLoc,
        leftmost_smallest, leftmost_largest, rightmost,
        spans, isSubspanOf, sortLocated,

        -- ** HasSrcSpan
        HasSrcSpan(..), SrcSpanLess, dL, cL,
        pattern LL, onHasSrcSpan, liftL
    ) where

import GhcPrelude

import Util
import Json
import Outputable
import FastString

import Control.DeepSeq
import Data.Bits
import Data.Data
import Data.List
import Data.Ord

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
  = SrcLoc      FastString              -- A precise location (file name)
                {-# UNPACK #-} !Int     -- line number, begins at 1
                {-# UNPACK #-} !Int     -- column number, begins at 1
  deriving (Eq, Ord)

-- | Source Location
data SrcLoc
  = RealSrcLoc {-# UNPACK #-}!RealSrcLoc
  | UnhelpfulLoc FastString     -- Just a general indication
  deriving (Eq, Ord, Show)

{-
************************************************************************
*                                                                      *
\subsection[SrcLoc-access-fns]{Access functions}
*                                                                      *
************************************************************************
-}

mkSrcLoc :: FastString -> Int -> Int -> SrcLoc
mkSrcLoc x line col = RealSrcLoc (mkRealSrcLoc x line col)

mkRealSrcLoc :: FastString -> Int -> Int -> RealSrcLoc
mkRealSrcLoc x line col = SrcLoc x line col

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
srcLocFile (SrcLoc fname _ _) = fname

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
advanceSrcLoc (SrcLoc f l c) '\t' = SrcLoc f  l (((((c - 1) `shiftR` 3) + 1)
                                                  `shiftL` 3) + 1)
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f  l (c + 1)

{-
************************************************************************
*                                                                      *
\subsection[SrcLoc-instances]{Instance declarations for various names}
*                                                                      *
************************************************************************
-}

sortLocated :: HasSrcSpan a => [a] -> [a]
sortLocated things = sortBy (comparing getLoc) things

instance Outputable RealSrcLoc where
    ppr (SrcLoc src_path src_line src_col)
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
    ppr (RealSrcLoc l) = ppr l
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

-- | Source Span
--
-- A 'SrcSpan' identifies either a specific portion of a text file
-- or a human-readable description of a location.
data SrcSpan =
    RealSrcSpan !RealSrcSpan
  | UnhelpfulSpan !FastString   -- Just a general indication
                                -- also used to indicate an empty span

  deriving (Eq, Ord, Show) -- Show is used by Lexer.x, because we
                           -- derive Show for Token

instance ToJson SrcSpan where
  json (UnhelpfulSpan {} ) = JSNull --JSObject [( "type", "unhelpful")]
  json (RealSrcSpan rss)  = json rss

instance ToJson RealSrcSpan where
  json (RealSrcSpan'{..}) = JSObject [ ("file", JSString (unpackFS srcSpanFile))
                                     , ("startLine", JSInt srcSpanSLine)
                                     , ("startCol", JSInt srcSpanSCol)
                                     , ("endLine", JSInt srcSpanELine)
                                     , ("endCol", JSInt srcSpanECol)
                                     ]

instance NFData SrcSpan where
  rnf x = x `seq` ()

-- | Built-in "bad" 'SrcSpan's for common sources of location uncertainty
noSrcSpan, wiredInSrcSpan, interactiveSrcSpan :: SrcSpan
noSrcSpan          = UnhelpfulSpan (fsLit "<no location info>")
wiredInSrcSpan     = UnhelpfulSpan (fsLit "<wired into compiler>")
interactiveSrcSpan = UnhelpfulSpan (fsLit "<interactive>")

-- | Create a "bad" 'SrcSpan' that has not location information
mkGeneralSrcSpan :: FastString -> SrcSpan
mkGeneralSrcSpan = UnhelpfulSpan

-- | Create a 'SrcSpan' corresponding to a single point
srcLocSpan :: SrcLoc -> SrcSpan
srcLocSpan (UnhelpfulLoc str) = UnhelpfulSpan str
srcLocSpan (RealSrcLoc l) = RealSrcSpan (realSrcLocSpan l)

realSrcLocSpan :: RealSrcLoc -> RealSrcSpan
realSrcLocSpan (SrcLoc file line col) = RealSrcSpan' file line col line col

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
mkSrcSpan (UnhelpfulLoc str) _ = UnhelpfulSpan str
mkSrcSpan _ (UnhelpfulLoc str) = UnhelpfulSpan str
mkSrcSpan (RealSrcLoc loc1) (RealSrcLoc loc2)
    = RealSrcSpan (mkRealSrcSpan loc1 loc2)

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Returns UnhelpfulSpan if the files differ.
combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans (UnhelpfulSpan _) r = r -- this seems more useful
combineSrcSpans l (UnhelpfulSpan _) = l
combineSrcSpans (RealSrcSpan span1) (RealSrcSpan span2)
  | srcSpanFile span1 == srcSpanFile span2
      = RealSrcSpan (combineRealSrcSpans span1 span2)
  | otherwise = UnhelpfulSpan (fsLit "<combineSrcSpans: files differ>")

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

-- | Convert a SrcSpan into one that represents only its first character
srcSpanFirstCharacter :: SrcSpan -> SrcSpan
srcSpanFirstCharacter l@(UnhelpfulSpan {}) = l
srcSpanFirstCharacter (RealSrcSpan span) = RealSrcSpan $ mkRealSrcSpan loc1 loc2
  where
    loc1@(SrcLoc f l c) = realSrcSpanStart span
    loc2 = SrcLoc f l (c+1)

{-
************************************************************************
*                                                                      *
\subsection[SrcSpan-predicates]{Predicates}
*                                                                      *
************************************************************************
-}

-- | Test if a 'SrcSpan' is "good", i.e. has precise location information
isGoodSrcSpan :: SrcSpan -> Bool
isGoodSrcSpan (RealSrcSpan _) = True
isGoodSrcSpan (UnhelpfulSpan _) = False

isOneLineSpan :: SrcSpan -> Bool
-- ^ True if the span is known to straddle only one line.
-- For "bad" 'SrcSpan', it returns False
isOneLineSpan (RealSrcSpan s) = srcSpanStartLine s == srcSpanEndLine s
isOneLineSpan (UnhelpfulSpan _) = False

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
srcSpanStart (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanStart (RealSrcSpan s) = RealSrcLoc (realSrcSpanStart s)

-- | Returns the location at the end of the 'SrcSpan' or a "bad" 'SrcSpan' if that is unavailable
srcSpanEnd :: SrcSpan -> SrcLoc
srcSpanEnd (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanEnd (RealSrcSpan s) = RealSrcLoc (realSrcSpanEnd s)

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
srcSpanFileName_maybe (RealSrcSpan s)   = Just (srcSpanFile s)
srcSpanFileName_maybe (UnhelpfulSpan _) = Nothing

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

-- Show is used by Lexer.x, because we derive Show for Token
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

-- I don't know why there is this style-based difference
--      = getPprStyle $ \ sty ->
--        if userStyle sty || debugStyle sty then
--           pprUserSpan True span
--        else
--           case span of
--           UnhelpfulSpan _ -> panic "Outputable UnhelpfulSpan"
--           RealSrcSpan s -> ppr s

pprUserSpan :: Bool -> SrcSpan -> SDoc
pprUserSpan _         (UnhelpfulSpan s) = ftext s
pprUserSpan show_path (RealSrcSpan s)   = pprUserRealSpan show_path s

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
  deriving (Eq, Ord, Data, Functor, Foldable, Traversable)

type Located = GenLocated SrcSpan
type RealLocated = GenLocated RealSrcSpan

unLoc :: HasSrcSpan a => a -> SrcSpanLess a
unLoc (dL->L _ e) = e

getLoc :: HasSrcSpan a => a -> SrcSpan
getLoc (dL->L l _) = l

noLoc :: HasSrcSpan a => SrcSpanLess a -> a
noLoc e = cL noSrcSpan e

mkGeneralLocated :: HasSrcSpan e => String -> SrcSpanLess e -> e
mkGeneralLocated s e = cL (mkGeneralSrcSpan (fsLit s)) e

combineLocs :: (HasSrcSpan a , HasSrcSpan b) => a -> b -> SrcSpan
combineLocs a b = combineSrcSpans (getLoc a) (getLoc b)

-- | Combine locations from two 'Located' things and add them to a third thing
addCLoc :: (HasSrcSpan a , HasSrcSpan b , HasSrcSpan c) =>
           a -> b -> SrcSpanLess c -> c
addCLoc a b c = cL (combineSrcSpans (getLoc a) (getLoc b)) c

-- not clear whether to add a general Eq instance, but this is useful sometimes:

-- | Tests whether the two located things are equal
eqLocated :: (HasSrcSpan a , Eq (SrcSpanLess a)) => a -> a -> Bool
eqLocated a b = unLoc a == unLoc b

-- not clear whether to add a general Ord instance, but this is useful sometimes:

-- | Tests the ordering of the two located things
cmpLocated :: (HasSrcSpan a , Ord (SrcSpanLess a)) => a -> a -> Ordering
cmpLocated a b = unLoc a `compare` unLoc b

instance (Outputable l, Outputable e) => Outputable (GenLocated l e) where
  ppr (L l e) = -- TODO: We can't do this since Located was refactored into
                -- GenLocated:
                -- Print spans without the file name etc
                -- ifPprDebug (braces (pprUserSpan False l))
                whenPprDebug (braces (ppr l))
             $$ ppr e

{-
************************************************************************
*                                                                      *
\subsection{Ordering SrcSpans for InteractiveUI}
*                                                                      *
************************************************************************
-}

-- | Alternative strategies for ordering 'SrcSpan's
leftmost_smallest, leftmost_largest, rightmost :: SrcSpan -> SrcSpan -> Ordering
rightmost            = flip compare
leftmost_smallest    = compare
leftmost_largest a b = (srcSpanStart a `compare` srcSpanStart b)
                                `thenCmp`
                       (srcSpanEnd b `compare` srcSpanEnd a)

-- | Determines whether a span encloses a given line and column index
spans :: SrcSpan -> (Int, Int) -> Bool
spans (UnhelpfulSpan _) _ = panic "spans UnhelpfulSpan"
spans (RealSrcSpan span) (l,c) = realSrcSpanStart span <= loc && loc <= realSrcSpanEnd span
   where loc = mkRealSrcLoc (srcSpanFile span) l c

-- | Determines whether a span is enclosed by another one
isSubspanOf :: SrcSpan -- ^ The span that may be enclosed by the other
            -> SrcSpan -- ^ The span it may be enclosed by
            -> Bool
isSubspanOf src parent
    | srcSpanFileName_maybe parent /= srcSpanFileName_maybe src = False
    | otherwise = srcSpanStart parent <= srcSpanStart src &&
                  srcSpanEnd parent   >= srcSpanEnd src


{-
************************************************************************
*                                                                      *
\subsection{HasSrcSpan Typeclass to Set/Get Source Location Spans}
*                                                                      *
************************************************************************
-}

{-
Note [HasSrcSpan Typeclass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To be able to uniformly set/get source location spans (of `SrcSpan`) in
syntactic entities (`HsSyn`), we use the typeclass `HasSrcSpan`.
More details can be found at the following wiki page
  ImplementingTreesThatGrow/HandlingSourceLocations

For most syntactic entities, the source location spans are stored in
a syntactic entity by a wapper constuctor (introduced by TTG's
new constructor extension), e.g., by `NewPat (WrapperPat sp pat)`
for a source location span `sp` and a pattern `pat`.
-}

-- | Determines the type of undecorated syntactic entities
-- For most syntactic entities `E`, where source location spans are
-- introduced by a wrapper construtor of the same syntactic entity,
-- we have `SrcSpanLess E = E`.
-- However, some syntactic entities have a different type compared to
-- a syntactic entity `e :: E` may have the type `Located E` when
-- decorated by wrapping it with `L sp e` for a source span `sp`.
type family SrcSpanLess a

-- | A typeclass to set/get SrcSpans
class HasSrcSpan a where
  -- | Composes a `SrcSpan` decoration with an undecorated syntactic
  --   entity to form its decorated variant
  composeSrcSpan   :: Located (SrcSpanLess a) -> a

  -- | Decomposes a decorated syntactic entity into its `SrcSpan`
  --   decoration and its undecorated variant
  decomposeSrcSpan :: a -> Located (SrcSpanLess a)
  {- laws:
       composeSrcSpan . decomposeSrcSpan = id
       decomposeSrcSpan . composeSrcSpan = id

     in other words, `HasSrcSpan` defines an iso relation between
     a `SrcSpan`-decorated syntactic entity and its undecorated variant
     (together with the `SrcSpan`).
  -}

type instance SrcSpanLess (GenLocated l e) = e
instance HasSrcSpan (Located a) where
  composeSrcSpan   = id
  decomposeSrcSpan = id


-- | An abbreviated form of decomposeSrcSpan,
--   mainly to be used in ViewPatterns
dL :: HasSrcSpan a => a -> Located (SrcSpanLess a)
dL = decomposeSrcSpan

-- | An abbreviated form of composeSrcSpan,
--   mainly to replace the hardcoded `L`
cL :: HasSrcSpan a => SrcSpan -> SrcSpanLess a -> a
cL sp e = composeSrcSpan (L sp e)

-- | A Pattern Synonym to Set/Get SrcSpans
pattern LL :: HasSrcSpan a => SrcSpan -> SrcSpanLess a -> a
pattern LL sp e <- (dL->L sp e)
  where
        LL sp e = cL sp e

-- | Lifts a function of undecorated entities to one of decorated ones
onHasSrcSpan :: (HasSrcSpan a , HasSrcSpan b) =>
                (SrcSpanLess a -> SrcSpanLess b) -> a -> b
onHasSrcSpan f (dL->L l e) = cL l (f e)

liftL :: (HasSrcSpan a, HasSrcSpan b, Monad m) =>
         (SrcSpanLess a -> m (SrcSpanLess b)) -> a -> m b
liftL f (dL->L loc a) = do
  a' <- f a
  return $ cL loc a'


getRealSrcSpan :: RealLocated a -> RealSrcSpan
getRealSrcSpan (L l _) = l

unRealSrcSpan :: RealLocated a -> a
unRealSrcSpan  (L _ e) = e
