%
% (c) The University of Glasgow, 1992-2006
%

\begin{code}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
   -- Workaround for Trac #5252 crashes the bootstrap compiler without -O
   -- When the earliest compiler we want to boostrap with is
   -- GHC 7.2, we can make RealSrcLoc properly abstract

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
        srcLocSpan, realSrcLocSpan,
        combineSrcSpans,

        -- ** Deconstructing SrcSpan
        srcSpanStart, srcSpanEnd,
        realSrcSpanStart, realSrcSpanEnd,
        srcSpanFileName_maybe,
        showUserSpan,

        -- ** Unsafely deconstructing SrcSpan
        -- These are dubious exports, because they crash on some inputs
        srcSpanFile,
        srcSpanStartLine, srcSpanEndLine,
        srcSpanStartCol, srcSpanEndCol,

        -- ** Predicates on SrcSpan
        isGoodSrcSpan, isOneLineSpan,

        -- * Located
        Located,
        RealLocated,
        GenLocated(..),

        -- ** Constructing Located
        noLoc,
        mkGeneralLocated,

        -- ** Deconstructing Located
        getLoc, unLoc,

        -- ** Combining and comparing Located values
        eqLocated, cmpLocated, combineLocs, addCLoc,
        leftmost_smallest, leftmost_largest, rightmost,
        spans, isSubspanOf, sortLocated
    ) where

#include "Typeable.h"

import Util
import Outputable
import FastString

import Data.Bits
import Data.Data
import Data.List
import Data.Ord
import System.FilePath
\end{code}

%************************************************************************
%*                                                                      *
\subsection[SrcLoc-SrcLocations]{Source-location information}
%*                                                                      *
%************************************************************************

We keep information about the {\em definition} point for each entity;
this is the obvious stuff:
\begin{code}
-- | Represents a single point within a file
data RealSrcLoc
  = SrcLoc      FastString              -- A precise location (file name)
                {-# UNPACK #-} !Int     -- line number, begins at 1
                {-# UNPACK #-} !Int     -- column number, begins at 1
  deriving Show

data SrcLoc
  = RealSrcLoc {-# UNPACK #-}!RealSrcLoc
  | UnhelpfulLoc FastString     -- Just a general indication
\end{code}

%************************************************************************
%*                                                                      *
\subsection[SrcLoc-access-fns]{Access functions}
%*                                                                      *
%************************************************************************

\begin{code}
mkSrcLoc :: FastString -> Int -> Int -> SrcLoc
mkSrcLoc x line col = RealSrcLoc (mkRealSrcLoc x line col)

mkRealSrcLoc :: FastString -> Int -> Int -> RealSrcLoc
mkRealSrcLoc x line col = SrcLoc x line col

-- | Built-in "bad" 'SrcLoc' values for particular locations
noSrcLoc, generatedSrcLoc, interactiveSrcLoc :: SrcLoc
noSrcLoc          = UnhelpfulLoc (fsLit "<no location info>")
generatedSrcLoc   = UnhelpfulLoc (fsLit "<compiler-generated code>")
interactiveSrcLoc = UnhelpfulLoc (fsLit "<interactive session>")

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
\end{code}

%************************************************************************
%*                                                                      *
\subsection[SrcLoc-instances]{Instance declarations for various names}
%*                                                                      *
%************************************************************************

\begin{code}
-- SrcLoc is an instance of Ord so that we can sort error messages easily
instance Eq SrcLoc where
  loc1 == loc2 = case loc1 `cmpSrcLoc` loc2 of
                 EQ     -> True
                 _other -> False

instance Eq RealSrcLoc where
  loc1 == loc2 = case loc1 `cmpRealSrcLoc` loc2 of
                 EQ     -> True
                 _other -> False

instance Ord SrcLoc where
  compare = cmpSrcLoc

instance Ord RealSrcLoc where
  compare = cmpRealSrcLoc

sortLocated :: [Located a] -> [Located a]
sortLocated things = sortBy (comparing getLoc) things

cmpSrcLoc :: SrcLoc -> SrcLoc -> Ordering
cmpSrcLoc (UnhelpfulLoc s1) (UnhelpfulLoc s2) = s1 `compare` s2
cmpSrcLoc (UnhelpfulLoc _)  (RealSrcLoc _)    = GT
cmpSrcLoc (RealSrcLoc _)    (UnhelpfulLoc _)  = LT
cmpSrcLoc (RealSrcLoc l1)   (RealSrcLoc l2)   = (l1 `compare` l2)

cmpRealSrcLoc :: RealSrcLoc -> RealSrcLoc -> Ordering
cmpRealSrcLoc (SrcLoc s1 l1 c1) (SrcLoc s2 l2 c2)
  = (s1 `compare` s2) `thenCmp` (l1 `compare` l2) `thenCmp` (c1 `compare` c2)

instance Outputable RealSrcLoc where
    ppr (SrcLoc src_path src_line src_col)
      = getPprStyle $ \ sty ->
        if userStyle sty || debugStyle sty then
            hcat [ pprFastFilePath src_path, char ':',
                   int src_line,
                   char ':', int src_col
                 ]
        else
            hcat [text "{-# LINE ", int src_line, space,
                  char '\"', pprFastFilePath src_path, text " #-}"]

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
\end{code}

%************************************************************************
%*                                                                      *
\subsection[SrcSpan]{Source Spans}
%*                                                                      *
%************************************************************************

\begin{code}
{- |
A SrcSpan delimits a portion of a text file.  It could be represented
by a pair of (line,column) coordinates, but in fact we optimise
slightly by using more compact representations for single-line and
zero-length spans, both of which are quite common.

The end position is defined to be the column /after/ the end of the
span.  That is, a span of (1,1)-(1,2) is one character long, and a
span of (1,1)-(1,1) is zero characters long.
-}
data RealSrcSpan
  = SrcSpanOneLine              -- a common case: a single line
        { srcSpanFile     :: !FastString,
          srcSpanLine     :: {-# UNPACK #-} !Int,
          srcSpanSCol     :: {-# UNPACK #-} !Int,
          srcSpanECol     :: {-# UNPACK #-} !Int
        }

  | SrcSpanMultiLine
        { srcSpanFile     :: !FastString,
          srcSpanSLine    :: {-# UNPACK #-} !Int,
          srcSpanSCol     :: {-# UNPACK #-} !Int,
          srcSpanELine    :: {-# UNPACK #-} !Int,
          srcSpanECol     :: {-# UNPACK #-} !Int
        }

  | SrcSpanPoint
        { srcSpanFile     :: !FastString,
          srcSpanLine     :: {-# UNPACK #-} !Int,
          srcSpanCol      :: {-# UNPACK #-} !Int
        }
  deriving (Eq, Typeable, Show) -- Show is used by Lexer.x, because we
                                -- derive Show for Token

data SrcSpan =
    RealSrcSpan !RealSrcSpan
  | UnhelpfulSpan !FastString   -- Just a general indication
                                -- also used to indicate an empty span

  deriving (Eq, Typeable, Show) -- Show is used by Lexer.x, because we
                                -- derive Show for Token

-- | Built-in "bad" 'SrcSpan's for common sources of location uncertainty
noSrcSpan, wiredInSrcSpan :: SrcSpan
noSrcSpan      = UnhelpfulSpan (fsLit "<no location info>")
wiredInSrcSpan = UnhelpfulSpan (fsLit "<wired into compiler>")

-- | Create a "bad" 'SrcSpan' that has not location information
mkGeneralSrcSpan :: FastString -> SrcSpan
mkGeneralSrcSpan = UnhelpfulSpan

-- | Create a 'SrcSpan' corresponding to a single point
srcLocSpan :: SrcLoc -> SrcSpan
srcLocSpan (UnhelpfulLoc str) = UnhelpfulSpan str
srcLocSpan (RealSrcLoc l) = RealSrcSpan (realSrcLocSpan l)

realSrcLocSpan :: RealSrcLoc -> RealSrcSpan
realSrcLocSpan (SrcLoc file line col) = SrcSpanPoint file line col

-- | Create a 'SrcSpan' between two points in a file
mkRealSrcSpan :: RealSrcLoc -> RealSrcLoc -> RealSrcSpan
mkRealSrcSpan loc1 loc2
  | line1 == line2 = if col1 == col2
                        then SrcSpanPoint file line1 col1
                        else SrcSpanOneLine file line1 col1 col2
  | otherwise      = SrcSpanMultiLine file line1 col1 line2 col2
  where
        line1 = srcLocLine loc1
        line2 = srcLocLine loc2
        col1 = srcLocCol loc1
        col2 = srcLocCol loc2
        file = srcLocFile loc1

-- | Create a 'SrcSpan' between two points in a file
mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan (UnhelpfulLoc str) _ = UnhelpfulSpan str
mkSrcSpan _ (UnhelpfulLoc str) = UnhelpfulSpan str
mkSrcSpan (RealSrcLoc loc1) (RealSrcLoc loc2)
    = RealSrcSpan (mkRealSrcSpan loc1 loc2)

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Assumes the "file" part is the same in both inputs
combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans (UnhelpfulSpan _) r = r -- this seems more useful
combineSrcSpans l (UnhelpfulSpan _) = l
combineSrcSpans (RealSrcSpan span1) (RealSrcSpan span2)
    = RealSrcSpan (combineRealSrcSpans span1 span2)

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Assumes the "file" part is the same in both inputs
combineRealSrcSpans :: RealSrcSpan -> RealSrcSpan -> RealSrcSpan
combineRealSrcSpans span1 span2
 = if line_start == line_end
   then if col_start == col_end
        then SrcSpanPoint     file line_start col_start
        else SrcSpanOneLine   file line_start col_start col_end
   else      SrcSpanMultiLine file line_start col_start line_end col_end
  where
    (line_start, col_start) = min (srcSpanStartLine span1, srcSpanStartCol span1)
                                  (srcSpanStartLine span2, srcSpanStartCol span2)
    (line_end, col_end)     = max (srcSpanEndLine span1, srcSpanEndCol span1)
                                  (srcSpanEndLine span2, srcSpanEndCol span2)
    file = srcSpanFile span1
\end{code}

%************************************************************************
%*                                                                      *
\subsection[SrcSpan-predicates]{Predicates}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Test if a 'SrcSpan' is "good", i.e. has precise location information
isGoodSrcSpan :: SrcSpan -> Bool
isGoodSrcSpan (RealSrcSpan _) = True
isGoodSrcSpan (UnhelpfulSpan _) = False

isOneLineSpan :: SrcSpan -> Bool
-- ^ True if the span is known to straddle only one line.
-- For "bad" 'SrcSpan', it returns False
isOneLineSpan (RealSrcSpan s) = srcSpanStartLine s == srcSpanEndLine s
isOneLineSpan (UnhelpfulSpan _) = False

\end{code}

%************************************************************************
%*                                                                      *
\subsection[SrcSpan-unsafe-access-fns]{Unsafe access functions}
%*                                                                      *
%************************************************************************

\begin{code}

srcSpanStartLine :: RealSrcSpan -> Int
srcSpanEndLine :: RealSrcSpan -> Int
srcSpanStartCol :: RealSrcSpan -> Int
srcSpanEndCol :: RealSrcSpan -> Int

srcSpanStartLine SrcSpanOneLine{ srcSpanLine=l } = l
srcSpanStartLine SrcSpanMultiLine{ srcSpanSLine=l } = l
srcSpanStartLine SrcSpanPoint{ srcSpanLine=l } = l

srcSpanEndLine SrcSpanOneLine{ srcSpanLine=l } = l
srcSpanEndLine SrcSpanMultiLine{ srcSpanELine=l } = l
srcSpanEndLine SrcSpanPoint{ srcSpanLine=l } = l

srcSpanStartCol SrcSpanOneLine{ srcSpanSCol=l } = l
srcSpanStartCol SrcSpanMultiLine{ srcSpanSCol=l } = l
srcSpanStartCol SrcSpanPoint{ srcSpanCol=l } = l

srcSpanEndCol SrcSpanOneLine{ srcSpanECol=c } = c
srcSpanEndCol SrcSpanMultiLine{ srcSpanECol=c } = c
srcSpanEndCol SrcSpanPoint{ srcSpanCol=c } = c

\end{code}

%************************************************************************
%*                                                                      *
\subsection[SrcSpan-access-fns]{Access functions}
%*                                                                      *
%************************************************************************

\begin{code}

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

\end{code}

%************************************************************************
%*                                                                      *
\subsection[SrcSpan-instances]{Instances}
%*                                                                      *
%************************************************************************

\begin{code}

-- We want to order SrcSpans first by the start point, then by the end point.
instance Ord SrcSpan where
  a `compare` b =
     (srcSpanStart a `compare` srcSpanStart b) `thenCmp`
     (srcSpanEnd   a `compare` srcSpanEnd   b)


instance Outputable RealSrcSpan where
    ppr span
      = getPprStyle $ \ sty ->
        if userStyle sty || debugStyle sty then
           text (showUserRealSpan True span)
        else
           hcat [text "{-# LINE ", int (srcSpanStartLine span), space,
                 char '\"', pprFastFilePath $ srcSpanFile span, text " #-}"]

instance Outputable SrcSpan where
    ppr span
      = getPprStyle $ \ sty ->
        if userStyle sty || debugStyle sty then
           pprUserSpan True span
        else
           case span of
           UnhelpfulSpan _ -> panic "Outputable UnhelpfulSpan"
           RealSrcSpan s -> ppr s

pprUserSpan :: Bool -> SrcSpan -> SDoc
pprUserSpan _         (UnhelpfulSpan s) = ftext s
pprUserSpan show_path (RealSrcSpan s)   = text (showUserRealSpan show_path s)

showUserSpan :: Bool -> SrcSpan -> String
showUserSpan _         (UnhelpfulSpan s) = unpackFS s
showUserSpan show_path (RealSrcSpan s)   = showUserRealSpan show_path s

showUserRealSpan :: Bool -> RealSrcSpan -> String
showUserRealSpan show_path (SrcSpanOneLine src_path line start_col end_col)
  = (if show_path then normalise (unpackFS src_path) ++ ":" else "")
 ++ show line ++ ":" ++ show start_col
 ++ (if end_col - start_col <= 1 then "" else '-' : show (end_col - 1))
            -- For single-character or point spans, we just
            -- output the starting column number

showUserRealSpan show_path (SrcSpanMultiLine src_path sline scol eline ecol)
  = (if show_path then normalise (unpackFS src_path) ++ ":" else "")
 ++ "(" ++ show sline ++ "," ++ show scol ++ ")"
 ++ "-"
 ++ "(" ++ show eline ++ "," ++ show ecol' ++ ")"
    where ecol' = if ecol == 0 then ecol else ecol - 1

showUserRealSpan show_path (SrcSpanPoint src_path line col)
  = (if show_path then normalise (unpackFS src_path) ++ ":" else "")
 ++ show line ++ ":" ++ show col
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Located]{Attaching SrcSpans to things}
%*                                                                      *
%************************************************************************

\begin{code}
-- | We attach SrcSpans to lots of things, so let's have a datatype for it.
data GenLocated l e = L l e
  deriving (Eq, Ord, Typeable, Data)

type Located e = GenLocated SrcSpan e
type RealLocated e = GenLocated RealSrcSpan e

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
eqLocated :: Eq a => Located a -> Located a -> Bool
eqLocated a b = unLoc a == unLoc b

-- not clear whether to add a general Ord instance, but this is useful sometimes:

-- | Tests the ordering of the two located things
cmpLocated :: Ord a => Located a -> Located a -> Ordering
cmpLocated a b = unLoc a `compare` unLoc b

instance Functor (GenLocated l) where
  fmap f (L l e) = L l (f e)

instance (Outputable l, Outputable e) => Outputable (GenLocated l e) where
  ppr (L l e) = -- TODO: We can't do this since Located was refactored into
                -- GenLocated:
                -- Print spans without the file name etc
                -- ifPprDebug (braces (pprUserSpan False l))
                ifPprDebug (braces (ppr l))
             $$ ppr e
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Ordering SrcSpans for InteractiveUI}
%*                                                                      *
%************************************************************************

\begin{code}
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

\end{code}
