%
% (c) The University of Glasgow, 1992-2006
%

\begin{code}
module SrcLoc (
	SrcLoc,			-- Abstract

	mkSrcLoc, isGoodSrcLoc,	mkGeneralSrcLoc,
	noSrcLoc, 		-- "I'm sorry, I haven't a clue"
	advanceSrcLoc,

	generatedSrcLoc,	-- Code generated within the compiler
	interactiveSrcLoc,	-- Code from an interactive session

	srcLocFile,		-- return the file name part
	srcLocLine,		-- return the line part
	srcLocCol,		-- return the column part
	pprDefnLoc,

	SrcSpan,		-- Abstract
	noSrcSpan, 
	wiredInSrcSpan,		-- Something wired into the compiler
	mkGeneralSrcSpan, 
	isGoodSrcSpan, isOneLineSpan,
	mkSrcSpan, srcLocSpan,
	combineSrcSpans,
	srcSpanStart, srcSpanEnd,
	optSrcSpanFileName,

	-- These are dubious exports, because they crash on some inputs,
	-- used only in Lexer.x where we are sure what the Span looks like
	srcSpanFile, 
        srcSpanStartLine, srcSpanEndLine, 
        srcSpanStartCol, srcSpanEndCol,

	Located(..), getLoc, unLoc, noLoc, eqLocated, cmpLocated, combineLocs, addCLoc,
        leftmost_smallest, leftmost_largest, rightmost, spans, isSubspanOf
    ) where

#include "HsVersions.h"

import Util
import Outputable
import FastString
\end{code}

%************************************************************************
%*									*
\subsection[SrcLoc-SrcLocations]{Source-location information}
%*									*
%************************************************************************

We keep information about the {\em definition} point for each entity;
this is the obvious stuff:
\begin{code}
data SrcLoc
  = SrcLoc	FastString	-- A precise location (file name)
		!Int		-- line number, begins at 1
		!Int		-- column number, begins at 0
		-- Don't ask me why lines start at 1 and columns start at
		-- zero.  That's just the way it is, so there.  --SDM

  | UnhelpfulLoc FastString	-- Just a general indication
\end{code}

%************************************************************************
%*									*
\subsection[SrcLoc-access-fns]{Access functions for names}
%*									*
%************************************************************************

Things to make 'em:
\begin{code}
mkSrcLoc :: FastString -> Int -> Int -> SrcLoc
mkSrcLoc x line col = SrcLoc x line col

noSrcLoc, generatedSrcLoc, interactiveSrcLoc :: SrcLoc
noSrcLoc	  = UnhelpfulLoc FSLIT("<no location info>")
generatedSrcLoc   = UnhelpfulLoc FSLIT("<compiler-generated code>")
interactiveSrcLoc = UnhelpfulLoc FSLIT("<interactive session>")

mkGeneralSrcLoc :: FastString -> SrcLoc
mkGeneralSrcLoc = UnhelpfulLoc 

isGoodSrcLoc :: SrcLoc -> Bool
isGoodSrcLoc (SrcLoc _ _ _) = True
isGoodSrcLoc _other         = False

srcLocFile :: SrcLoc -> FastString
srcLocFile (SrcLoc fname _ _) = fname
srcLocFile _other	      = FSLIT("<unknown file")

srcLocLine :: SrcLoc -> Int
srcLocLine (SrcLoc _ l _) = l
srcLocLine _other	  = panic "srcLocLine: unknown line"

srcLocCol :: SrcLoc -> Int
srcLocCol (SrcLoc _ _ c) = c
srcLocCol _other         = panic "srcLocCol: unknown col"

advanceSrcLoc :: SrcLoc -> Char -> SrcLoc
advanceSrcLoc (SrcLoc f l _) '\n' = SrcLoc f  (l + 1) 0
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f  l (c + 1)
advanceSrcLoc loc	     _	  = loc	-- Better than nothing
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
cmpSrcLoc (UnhelpfulLoc _)  _other            = LT

cmpSrcLoc (SrcLoc s1 l1 c1) (SrcLoc s2 l2 c2)      
  = (s1 `compare` s2) `thenCmp` (l1 `compare` l2) `thenCmp` (c1 `compare` c2)
cmpSrcLoc (SrcLoc _ _ _) _other = GT

instance Outputable SrcLoc where
    ppr (SrcLoc src_path src_line src_col)
      = getPprStyle $ \ sty ->
        if userStyle sty || debugStyle sty then
	   hcat [ ftext src_path, char ':', 
		  int src_line,
		  char ':', int src_col
		]
	else
	   hcat [text "{-# LINE ", int src_line, space,
		 char '\"', ftext src_path, text " #-}"]

    ppr (UnhelpfulLoc s)  = ftext s
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

The end position is defined to be the column *after* the end of the
span.  That is, a span of (1,1)-(1,2) is one character long, and a
span of (1,1)-(1,1) is zero characters long.
-}
data SrcSpan
  = SrcSpanOneLine 		-- a common case: a single line
	{ srcSpanFile     :: FastString,
	  srcSpanLine     :: !Int,
	  srcSpanSCol     :: !Int,
	  srcSpanECol     :: !Int
	}

  | SrcSpanMultiLine
	{ srcSpanFile	  :: FastString,
	  srcSpanSLine    :: !Int,
	  srcSpanSCol	  :: !Int,
	  srcSpanELine    :: !Int,
	  srcSpanECol     :: !Int
	}

  | SrcSpanPoint
	{ srcSpanFile	  :: FastString,
	  srcSpanLine	  :: !Int,
	  srcSpanCol      :: !Int
	}

  | UnhelpfulSpan FastString	-- Just a general indication
				-- also used to indicate an empty span

  deriving Eq

-- We want to order SrcSpans first by the start point, then by the end point.
instance Ord SrcSpan where
  a `compare` b = 
     (srcSpanStart a `compare` srcSpanStart b) `thenCmp` 
     (srcSpanEnd   a `compare` srcSpanEnd   b)

noSrcSpan, wiredInSrcSpan :: SrcSpan
noSrcSpan      = UnhelpfulSpan FSLIT("<no location info>")
wiredInSrcSpan = UnhelpfulSpan FSLIT("<wired into compiler>")

mkGeneralSrcSpan :: FastString -> SrcSpan
mkGeneralSrcSpan = UnhelpfulSpan

isGoodSrcSpan :: SrcSpan -> Bool
isGoodSrcSpan SrcSpanOneLine{} = True
isGoodSrcSpan SrcSpanMultiLine{} = True
isGoodSrcSpan SrcSpanPoint{} = True
isGoodSrcSpan _ = False

optSrcSpanFileName :: SrcSpan -> Maybe FastString
optSrcSpanFileName (SrcSpanOneLine { srcSpanFile = nm })   = Just nm
optSrcSpanFileName (SrcSpanMultiLine { srcSpanFile = nm }) = Just nm
optSrcSpanFileName (SrcSpanPoint { srcSpanFile = nm})      = Just nm
optSrcSpanFileName _                                       = Nothing

isOneLineSpan :: SrcSpan -> Bool
-- True if the span is known to straddle more than one line
-- By default, it returns False
isOneLineSpan s
  | isGoodSrcSpan s = srcSpanStartLine s == srcSpanEndLine s
  | otherwise	    = False		

--------------------------------------------------------
-- Don't export these four;
-- they panic on Unhelpful.
-- They are for internal use only
-- Urk!  Some are needed for Lexer.x; see comment in export list

srcSpanStartLine, srcSpanEndLine, srcSpanStartCol, srcSpanEndCol
  :: SrcSpan -> Int

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
--------------------------------------------------------

srcSpanStart, srcSpanEnd :: SrcSpan -> SrcLoc

srcSpanStart (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanStart s = mkSrcLoc (srcSpanFile s) 
			  (srcSpanStartLine s)
		 	  (srcSpanStartCol s)

srcSpanEnd (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanEnd s = 
  mkSrcLoc (srcSpanFile s) 
	   (srcSpanEndLine s)
 	   (srcSpanEndCol s)

srcLocSpan :: SrcLoc -> SrcSpan
srcLocSpan (UnhelpfulLoc str) = UnhelpfulSpan str
srcLocSpan (SrcLoc file line col) = SrcSpanPoint file line col

mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan (UnhelpfulLoc str) _ = UnhelpfulSpan str
mkSrcSpan _ (UnhelpfulLoc str) = UnhelpfulSpan str
mkSrcSpan loc1 loc2
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

combineSrcSpans	:: SrcSpan -> SrcSpan -> SrcSpan
-- Assumes the 'file' part is the same in both
combineSrcSpans	(UnhelpfulSpan _) r = r -- this seems more useful
combineSrcSpans	l (UnhelpfulSpan _) = l
combineSrcSpans	start end 
 = case line1 `compare` line2 of
     EQ -> case col1 `compare` col2 of
		EQ -> SrcSpanPoint file line1 col1
		LT -> SrcSpanOneLine file line1 col1 col2
		GT -> SrcSpanOneLine file line1 col2 col1
     LT -> SrcSpanMultiLine file line1 col1 line2 col2
     GT -> SrcSpanMultiLine file line2 col2 line1 col1
  where
	line1 = srcSpanStartLine start
	col1  = srcSpanStartCol start
	line2 = srcSpanEndLine end
	col2  = srcSpanEndCol end
	file  = srcSpanFile start

pprDefnLoc :: SrcSpan -> SDoc
-- "defined at ..."
pprDefnLoc loc
  | isGoodSrcSpan loc = ptext SLIT("Defined at") <+> ppr loc
  | otherwise	      = ppr loc

instance Outputable SrcSpan where
    ppr span
      = getPprStyle $ \ sty ->
        if userStyle sty || debugStyle sty then
	   pprUserSpan span
	else
	   hcat [text "{-# LINE ", int (srcSpanStartLine span), space,
		 char '\"', ftext (srcSpanFile span), text " #-}"]


pprUserSpan :: SrcSpan -> SDoc
pprUserSpan (SrcSpanOneLine src_path line start_col end_col)
  = hcat [ ftext src_path, char ':', 
	   int line,
	   char ':', int start_col
	 ]
    <> if end_col - start_col <= 1 
	  then empty 
	    -- for single-character or point spans, we just output the starting
	    -- column number
	  else  char '-' <> int (end_col-1)

pprUserSpan (SrcSpanMultiLine src_path sline scol eline ecol)
  = hcat [ ftext src_path, char ':', 
		  parens (int sline <> char ',' <>  int scol),
		  char '-',
		  parens (int eline <> char ',' <>  
			   if ecol == 0 then int ecol else int (ecol-1))
		]

pprUserSpan (SrcSpanPoint src_path line col)
  = hcat [ ftext src_path, char ':', 
	   int line,
	   char ':', int col
	 ]

pprUserSpan (UnhelpfulSpan s)  = ftext s
\end{code}

%************************************************************************
%*									*
\subsection[Located]{Attaching SrcSpans to things}
%*									*
%************************************************************************

\begin{code}
-- | We attach SrcSpans to lots of things, so let's have a datatype for it.
data Located e = L SrcSpan e

unLoc :: Located e -> e
unLoc (L _ e) = e

getLoc :: Located e -> SrcSpan
getLoc (L l _) = l

noLoc :: e -> Located e
noLoc e = L noSrcSpan e

combineLocs :: Located a -> Located b -> SrcSpan
combineLocs a b = combineSrcSpans (getLoc a) (getLoc b)

addCLoc :: Located a -> Located b -> c -> Located c
addCLoc a b c = L (combineSrcSpans (getLoc a) (getLoc b)) c

-- not clear whether to add a general Eq instance, but this is useful sometimes:
eqLocated :: Eq a => Located a -> Located a -> Bool
eqLocated a b = unLoc a == unLoc b

-- not clear whether to add a general Eq instance, but this is useful sometimes:
cmpLocated :: Ord a => Located a -> Located a -> Ordering
cmpLocated a b = unLoc a `compare` unLoc b

instance Functor Located where
  fmap f (L l e) = L l (f e)

instance Outputable e => Outputable (Located e) where
  ppr (L _ e) =  ppr e
	-- do we want to dump the span in debugSty mode?    
\end{code}


%************************************************************************
%*									*
\subsection{Manipulating SrcSpans}
%*									*
%************************************************************************

\begin{code}
leftmost_smallest, leftmost_largest, rightmost :: SrcSpan -> SrcSpan -> Ordering
rightmost            = flip compare
leftmost_smallest    = compare 
leftmost_largest a b = (srcSpanStart a `compare` srcSpanStart b)
                                `thenCmp`
                       (srcSpanEnd b `compare` srcSpanEnd a)


spans :: SrcSpan -> (Int,Int) -> Bool
spans span (l,c) = srcSpanStart span <= loc && loc <= srcSpanEnd span
   where loc = mkSrcLoc (srcSpanFile span) l c

isSubspanOf :: SrcSpan -> SrcSpan -> Bool
isSubspanOf src parent 
    | optSrcSpanFileName parent /= optSrcSpanFileName src = False
    | otherwise = srcSpanStart parent <= srcSpanStart src &&
                  srcSpanEnd parent   >= srcSpanEnd src

\end{code}
