%
% (c) The University of Glasgow, 1992-2006
%

\begin{code}
-- | This module contains types that relate to the positions of things
-- in source files, and allow tagging of those things with locations
module SrcLoc (
	-- * SrcLoc
	SrcLoc,			-- Abstract

        -- ** Constructing SrcLoc
	mkSrcLoc, mkGeneralSrcLoc,

	noSrcLoc, 		-- "I'm sorry, I haven't a clue"
	generatedSrcLoc,	-- Code generated within the compiler
	interactiveSrcLoc,	-- Code from an interactive session

        advanceSrcLoc,

	-- ** Unsafely deconstructing SrcLoc
	-- These are dubious exports, because they crash on some inputs
	srcLocFile,		-- return the file name part
	srcLocLine,		-- return the line part
	srcLocCol,		-- return the column part
	
	-- ** Misc. operations on SrcLoc
	pprDefnLoc,
	
        -- ** Predicates on SrcLoc
        isGoodSrcLoc,

        -- * SrcSpan
	SrcSpan,		-- Abstract

        -- ** Constructing SrcSpan
	mkGeneralSrcSpan, mkSrcSpan, 
	noSrcSpan, 
	wiredInSrcSpan,		-- Something wired into the compiler
	srcLocSpan,
	combineSrcSpans,
	
	-- ** Deconstructing SrcSpan
	srcSpanStart, srcSpanEnd,
	srcSpanFileName_maybe,

	-- ** Unsafely deconstructing SrcSpan
	-- These are dubious exports, because they crash on some inputs
	srcSpanFile, 
        srcSpanStartLine, srcSpanEndLine, 
        srcSpanStartCol, srcSpanEndCol,

        -- ** Predicates on SrcSpan
        isGoodSrcSpan, isOneLineSpan,

        -- * Located
	Located(..), 
	
	-- ** Constructing Located
	noLoc,
        mkGeneralLocated,
	
	-- ** Deconstructing Located
	getLoc, unLoc, 
	
	-- ** Combining and comparing Located values
	eqLocated, cmpLocated, combineLocs, addCLoc,
        leftmost_smallest, leftmost_largest, rightmost, 
        spans, isSubspanOf
    ) where

#include "Typeable.h"

import Util
import Outputable
import FastString
import {-# SOURCE #-} DynFlags (DynFlags)

import Data.Bits
import Data.Data
\end{code}

%************************************************************************
%*									*
\subsection[SrcLoc-SrcLocations]{Source-location information}
%*									*
%************************************************************************

We keep information about the {\em definition} point for each entity;
this is the obvious stuff:
\begin{code}
-- | Represents a single point within a file
data SrcLoc
  = SrcLoc	FastString	-- A precise location (file name)
		{-# UNPACK #-} !Int		-- line number, begins at 1
		{-# UNPACK #-} !Int		-- column number, begins at 1
  | UnhelpfulLoc FastString	-- Just a general indication
\end{code}

%************************************************************************
%*									*
\subsection[SrcLoc-access-fns]{Access functions}
%*									*
%************************************************************************

\begin{code}
mkSrcLoc :: FastString -> Int -> Int -> SrcLoc
mkSrcLoc x line col = SrcLoc x line col

-- | Built-in "bad" 'SrcLoc' values for particular locations
noSrcLoc, generatedSrcLoc, interactiveSrcLoc :: SrcLoc
noSrcLoc	  = UnhelpfulLoc (fsLit "<no location info>")
generatedSrcLoc   = UnhelpfulLoc (fsLit "<compiler-generated code>")
interactiveSrcLoc = UnhelpfulLoc (fsLit "<interactive session>")

-- | Creates a "bad" 'SrcLoc' that has no detailed information about its location
mkGeneralSrcLoc :: FastString -> SrcLoc
mkGeneralSrcLoc = UnhelpfulLoc 

-- | "Good" 'SrcLoc's have precise information about their location
isGoodSrcLoc :: SrcLoc -> Bool
isGoodSrcLoc (SrcLoc _ _ _) = True
isGoodSrcLoc _other         = False

-- | Gives the filename of the 'SrcLoc' if it is available, otherwise returns a dummy value
srcLocFile :: SrcLoc -> FastString
srcLocFile (SrcLoc fname _ _) = fname
srcLocFile _other	      = (fsLit "<unknown file")

-- | Raises an error when used on a "bad" 'SrcLoc'
srcLocLine :: DynFlags -> SrcLoc -> Int
srcLocLine _      (SrcLoc _ l _) = l
srcLocLine dflags (UnhelpfulLoc s) = pprPanic dflags "srcLocLine" (ftext s)

-- | Raises an error when used on a "bad" 'SrcLoc'
srcLocCol :: DynFlags -> SrcLoc -> Int
srcLocCol _      (SrcLoc _ _ c) = c
srcLocCol dflags (UnhelpfulLoc s) = pprPanic dflags "srcLocCol" (ftext s)

-- | Move the 'SrcLoc' down by one line if the character is a newline,
-- to the next 8-char tabstop if it is a tab, and across by one
-- character in any other case
advanceSrcLoc :: SrcLoc -> Char -> SrcLoc
advanceSrcLoc (SrcLoc f l _) '\n' = SrcLoc f  (l + 1) 1
advanceSrcLoc (SrcLoc f l c) '\t' = SrcLoc f  l (((((c - 1) `shiftR` 3) + 1)
                                                  `shiftL` 3) + 1)
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f  l (c + 1)
advanceSrcLoc loc            _    = loc -- Better than nothing
\end{code}

%************************************************************************
%*									*
\subsection[SrcLoc-instances]{Instance declarations for various names}
%*									*
%************************************************************************

\begin{code}
-- SrcLoc is an instance of Ord so that we can sort error messages easily
instance Eq SrcLoc where
  loc1 == loc2 = case loc1 `cmpSrcLoc` loc2 of
		   EQ     -> True
		   _other -> False

instance Ord SrcLoc where
  compare = cmpSrcLoc
   
cmpSrcLoc :: SrcLoc -> SrcLoc -> Ordering
cmpSrcLoc (UnhelpfulLoc s1) (UnhelpfulLoc s2) = s1 `compare` s2
cmpSrcLoc (UnhelpfulLoc _)  (SrcLoc _ _ _)    = GT
cmpSrcLoc (SrcLoc _ _ _)    (UnhelpfulLoc _)  = LT

cmpSrcLoc (SrcLoc s1 l1 c1) (SrcLoc s2 l2 c2)      
  = (s1 `compare` s2) `thenCmp` (l1 `compare` l2) `thenCmp` (c1 `compare` c2)

instance Outputable SrcLoc where
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

    ppr (UnhelpfulLoc s)  = ftext s

instance Data SrcSpan where
  -- don't traverse?
  toConstr _   = abstractConstr "SrcSpan"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "SrcSpan"
\end{code}

%************************************************************************
%*									*
\subsection[SrcSpan]{Source Spans}
%*									*
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
data SrcSpan
  = SrcSpanOneLine 		-- a common case: a single line
	{ srcSpanFile     :: !FastString,
	  srcSpanLine     :: {-# UNPACK #-} !Int,
	  srcSpanSCol     :: {-# UNPACK #-} !Int,
	  srcSpanECol     :: {-# UNPACK #-} !Int
	}

  | SrcSpanMultiLine
	{ srcSpanFile	  :: !FastString,
	  srcSpanSLine    :: {-# UNPACK #-} !Int,
	  srcSpanSCol	  :: {-# UNPACK #-} !Int,
	  srcSpanELine    :: {-# UNPACK #-} !Int,
	  srcSpanECol     :: {-# UNPACK #-} !Int
	}

  | SrcSpanPoint
	{ srcSpanFile	  :: !FastString,
	  srcSpanLine	  :: {-# UNPACK #-} !Int,
	  srcSpanCol      :: {-# UNPACK #-} !Int
	}

  | UnhelpfulSpan !FastString	-- Just a general indication
				-- also used to indicate an empty span

#ifdef DEBUG
  deriving (Eq, Typeable, Show) -- Show is used by Lexer.x, becuase we
                                -- derive Show for Token
#else
  deriving (Eq, Typeable)
#endif

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
srcLocSpan (SrcLoc file line col) = SrcSpanPoint file line col

-- | Create a 'SrcSpan' between two points in a file
mkSrcSpan :: DynFlags -> SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan _ (UnhelpfulLoc str) _ = UnhelpfulSpan str
mkSrcSpan _ _ (UnhelpfulLoc str) = UnhelpfulSpan str
mkSrcSpan dflags loc1 loc2
  | line1 == line2 = if col1 == col2
			then SrcSpanPoint file line1 col1
			else SrcSpanOneLine file line1 col1 col2
  | otherwise      = SrcSpanMultiLine file line1 col1 line2 col2
  where
	line1 = srcLocLine dflags loc1
	line2 = srcLocLine dflags loc2
	col1 = srcLocCol dflags loc1
	col2 = srcLocCol dflags loc2
	file = srcLocFile loc1

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Assumes the "file" part is the same in both inputs
combineSrcSpans	:: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans	(UnhelpfulSpan _) r = r -- this seems more useful
combineSrcSpans	l (UnhelpfulSpan _) = l
combineSrcSpans	span1 span2
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
%*									*
\subsection[SrcSpan-predicates]{Predicates}
%*									*
%************************************************************************

\begin{code}
-- | Test if a 'SrcSpan' is "good", i.e. has precise location information
isGoodSrcSpan :: SrcSpan -> Bool
isGoodSrcSpan SrcSpanOneLine{} = True
isGoodSrcSpan SrcSpanMultiLine{} = True
isGoodSrcSpan SrcSpanPoint{} = True
isGoodSrcSpan _ = False

isOneLineSpan :: SrcSpan -> Bool
-- ^ True if the span is known to straddle only one line.
-- For "bad" 'SrcSpan', it returns False
isOneLineSpan s
  | isGoodSrcSpan s = srcSpanStartLine s == srcSpanEndLine s
  | otherwise	    = False		

\end{code}

%************************************************************************
%*									*
\subsection[SrcSpan-unsafe-access-fns]{Unsafe access functions}
%*									*
%************************************************************************

\begin{code}

-- | Raises an error when used on a "bad" 'SrcSpan'
srcSpanStartLine :: SrcSpan -> Int
-- | Raises an error when used on a "bad" 'SrcSpan'
srcSpanEndLine :: SrcSpan -> Int
-- | Raises an error when used on a "bad" 'SrcSpan'
srcSpanStartCol :: SrcSpan -> Int
-- | Raises an error when used on a "bad" 'SrcSpan'
srcSpanEndCol :: SrcSpan -> Int

srcSpanStartLine SrcSpanOneLine{ srcSpanLine=l } = l
srcSpanStartLine SrcSpanMultiLine{ srcSpanSLine=l } = l
srcSpanStartLine SrcSpanPoint{ srcSpanLine=l } = l
srcSpanStartLine _ = panic "SrcLoc.srcSpanStartLine"

srcSpanEndLine SrcSpanOneLine{ srcSpanLine=l } = l
srcSpanEndLine SrcSpanMultiLine{ srcSpanELine=l } = l
srcSpanEndLine SrcSpanPoint{ srcSpanLine=l } = l
srcSpanEndLine _ = panic "SrcLoc.srcSpanEndLine"

srcSpanStartCol SrcSpanOneLine{ srcSpanSCol=l } = l
srcSpanStartCol SrcSpanMultiLine{ srcSpanSCol=l } = l
srcSpanStartCol SrcSpanPoint{ srcSpanCol=l } = l
srcSpanStartCol _ = panic "SrcLoc.srcSpanStartCol"

srcSpanEndCol SrcSpanOneLine{ srcSpanECol=c } = c
srcSpanEndCol SrcSpanMultiLine{ srcSpanECol=c } = c
srcSpanEndCol SrcSpanPoint{ srcSpanCol=c } = c
srcSpanEndCol _ = panic "SrcLoc.srcSpanEndCol"

\end{code}

%************************************************************************
%*									*
\subsection[SrcSpan-access-fns]{Access functions}
%*									*
%************************************************************************

\begin{code}

-- | Returns the location at the start of the 'SrcSpan' or a "bad" 'SrcSpan' if that is unavailable
srcSpanStart :: SrcSpan -> SrcLoc
-- | Returns the location at the end of the 'SrcSpan' or a "bad" 'SrcSpan' if that is unavailable
srcSpanEnd :: SrcSpan -> SrcLoc

srcSpanStart (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanStart s = mkSrcLoc (srcSpanFile s) 
			  (srcSpanStartLine s)
		 	  (srcSpanStartCol s)

srcSpanEnd (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanEnd s = 
  mkSrcLoc (srcSpanFile s) 
	   (srcSpanEndLine s)
 	   (srcSpanEndCol s)

-- | Obtains the filename for a 'SrcSpan' if it is "good"
srcSpanFileName_maybe :: SrcSpan -> Maybe FastString
srcSpanFileName_maybe (SrcSpanOneLine { srcSpanFile = nm })   = Just nm
srcSpanFileName_maybe (SrcSpanMultiLine { srcSpanFile = nm }) = Just nm
srcSpanFileName_maybe (SrcSpanPoint { srcSpanFile = nm})      = Just nm
srcSpanFileName_maybe _                                       = Nothing

\end{code}

%************************************************************************
%*									*
\subsection[SrcSpan-instances]{Instances}
%*									*
%************************************************************************

\begin{code}

-- We want to order SrcSpans first by the start point, then by the end point.
instance Ord SrcSpan where
  a `compare` b = 
     (srcSpanStart a `compare` srcSpanStart b) `thenCmp` 
     (srcSpanEnd   a `compare` srcSpanEnd   b)


instance Outputable SrcSpan where
    ppr span
      = getPprStyle $ \ sty ->
        if userStyle sty || debugStyle sty then
           pprUserSpan True span
        else
           hcat [text "{-# LINE ", int (srcSpanStartLine span), space,
                 char '\"', pprFastFilePath $ srcSpanFile span, text " #-}"]

pprUserSpan :: Bool -> SrcSpan -> SDoc
pprUserSpan show_path (SrcSpanOneLine src_path line start_col end_col)
  = hcat [ ppWhen show_path (pprFastFilePath src_path <> colon)
         , int line, char ':', int start_col
         , ppUnless (end_col - start_col <= 1)
                    (char '-' <> int (end_col-1)) 
	    -- For single-character or point spans, we just 
	    -- output the starting column number
         ]
	  

pprUserSpan show_path (SrcSpanMultiLine src_path sline scol eline ecol)
  = hcat [ ppWhen show_path (pprFastFilePath src_path <> colon)
	 , parens (int sline <> char ',' <>  int scol)
	 , char '-'
	 , parens (int eline <> char ',' <>  
	   	   if ecol == 0 then int ecol else int (ecol-1))
	 ]

pprUserSpan show_path (SrcSpanPoint src_path line col)
  = hcat [ ppWhen show_path $ (pprFastFilePath src_path <> colon)
         , int line, char ':', int col ]

pprUserSpan _ (UnhelpfulSpan s)  = ftext s

pprDefnLoc :: SrcSpan -> SDoc
-- ^ Pretty prints information about the 'SrcSpan' in the style "defined at ..."
pprDefnLoc loc
  | isGoodSrcSpan loc = ptext (sLit "Defined at") <+> ppr loc
  | otherwise	      = ppr loc
\end{code}

%************************************************************************
%*									*
\subsection[Located]{Attaching SrcSpans to things}
%*									*
%************************************************************************

\begin{code}
-- | We attach SrcSpans to lots of things, so let's have a datatype for it.
data Located e = L SrcSpan e
  deriving (Eq, Ord, Typeable, Data)

unLoc :: Located e -> e
unLoc (L _ e) = e

getLoc :: Located e -> SrcSpan
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

instance Functor Located where
  fmap f (L l e) = L l (f e)

instance Outputable e => Outputable (Located e) where
  ppr (L l e) = ifPprDebug (braces (pprUserSpan False l)) $$ ppr e
		-- Print spans without the file name etc
\end{code}

%************************************************************************
%*									*
\subsection{Ordering SrcSpans for InteractiveUI}
%*									*
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
spans span (l,c) = srcSpanStart span <= loc && loc <= srcSpanEnd span
   where loc = mkSrcLoc (srcSpanFile span) l c

-- | Determines whether a span is enclosed by another one
isSubspanOf :: SrcSpan -- ^ The span that may be enclosed by the other
            -> SrcSpan -- ^ The span it may be enclosed by
            -> Bool
isSubspanOf src parent 
    | srcSpanFileName_maybe parent /= srcSpanFileName_maybe src = False
    | otherwise = srcSpanStart parent <= srcSpanStart src &&
                  srcSpanEnd parent   >= srcSpanEnd src

\end{code}
