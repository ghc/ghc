%
% (c) The University of Glasgow, 1992-2006
%

\begin{code}
module SrcLoc (
	SrcLoc,			-- Abstract

	mkSrcLoc, isGoodSrcLoc,	mkGeneralSrcLoc,
	noSrcLoc, 		-- "I'm sorry, I haven't a clue"
	advanceSrcLoc,

	importedSrcLoc,		-- Unknown place in an interface
	wiredInSrcLoc,		-- Something wired into the compiler
	generatedSrcLoc,	-- Code generated within the compiler
	interactiveSrcLoc,	-- Code from an interactive session

	srcLocFile,		-- return the file name part
	srcLocLine,		-- return the line part
	srcLocCol,		-- return the column part
	pprDefnLoc,

	SrcSpan,		-- Abstract
	noSrcSpan, 
	mkGeneralSrcSpan, 
	isGoodSrcSpan, isOneLineSpan,
	mkSrcSpan, srcLocSpan,
	combineSrcSpans,
	srcSpanStart, srcSpanEnd,

	-- These are dubious exports, because they crash on some inputs,
	-- used only in Lexer.x where we are sure what the Span looks like
	srcSpanFile, srcSpanEndLine, srcSpanEndCol,

	Located(..), getLoc, unLoc, noLoc, eqLocated, cmpLocated, combineLocs, addCLoc
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

  | ImportedLoc	String		-- Module name

  | UnhelpfulLoc FastString	-- Just a general indication
\end{code}

Note that an entity might be imported via more than one route, and
there could be more than one ``definition point'' --- in two or more
\tr{.hi} files.	 We deemed it probably-unworthwhile to cater for this
rare case.

%************************************************************************
%*									*
\subsection[SrcLoc-access-fns]{Access functions for names}
%*									*
%************************************************************************

Things to make 'em:
\begin{code}
mkSrcLoc x line col = SrcLoc x line col
noSrcLoc	  = UnhelpfulLoc FSLIT("<no location info>")
generatedSrcLoc   = UnhelpfulLoc FSLIT("<compiler-generated code>")
wiredInSrcLoc     = UnhelpfulLoc FSLIT("<wired into compiler>")
interactiveSrcLoc = UnhelpfulLoc FSLIT("<interactive session>")

mkGeneralSrcLoc :: FastString -> SrcLoc
mkGeneralSrcLoc = UnhelpfulLoc 

importedSrcLoc :: String -> SrcLoc
importedSrcLoc mod_name = ImportedLoc mod_name

isGoodSrcLoc (SrcLoc _ _ _) = True
isGoodSrcLoc other          = False

srcLocFile :: SrcLoc -> FastString
srcLocFile (SrcLoc fname _ _) = fname
srcLocFile other	      = FSLIT("<unknown file")

srcLocLine :: SrcLoc -> Int
srcLocLine (SrcLoc _ l c) = l
srcLocLine other	  = panic "srcLocLine: unknown line"

srcLocCol :: SrcLoc -> Int
srcLocCol (SrcLoc _ l c) = c
srcLocCol other	  = panic "srcLocCol: unknown col"

advanceSrcLoc :: SrcLoc -> Char -> SrcLoc
advanceSrcLoc (SrcLoc f l c) '\n' = SrcLoc f  (l + 1) 0
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
		   EQ    -> True
		   other -> False

instance Ord SrcLoc where
  compare = cmpSrcLoc

cmpSrcLoc (UnhelpfulLoc s1) (UnhelpfulLoc s2) = s1 `compare` s2
cmpSrcLoc (UnhelpfulLoc _)  other      	      = LT

cmpSrcLoc (ImportedLoc _)  (UnhelpfulLoc _)  = GT
cmpSrcLoc (ImportedLoc m1) (ImportedLoc m2)  = m1 `compare` m2
cmpSrcLoc (ImportedLoc _)  other	     = LT

cmpSrcLoc (SrcLoc s1 l1 c1) (SrcLoc s2 l2 c2)      
  = (s1 `compare` s2) `thenCmp` (l1 `compare` l2) `thenCmp` (c1 `compare` c2)
cmpSrcLoc (SrcLoc _ _ _) other = GT

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

    ppr (ImportedLoc mod) = ptext SLIT("Defined in") <+> text mod
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

  | ImportedSpan String		-- Module name

  | UnhelpfulSpan FastString	-- Just a general indication
				-- also used to indicate an empty span

  deriving Eq

-- We want to order SrcSpans first by the start point, then by the end point.
instance Ord SrcSpan where
  a `compare` b = 
     (srcSpanStart a `compare` srcSpanStart b) `thenCmp` 
     (srcSpanEnd   a `compare` srcSpanEnd   b)

noSrcSpan  = UnhelpfulSpan FSLIT("<no location info>")

mkGeneralSrcSpan :: FastString -> SrcSpan
mkGeneralSrcSpan = UnhelpfulSpan

isGoodSrcSpan SrcSpanOneLine{} = True
isGoodSrcSpan SrcSpanMultiLine{} = True
isGoodSrcSpan SrcSpanPoint{} = True
isGoodSrcSpan _ = False

isOneLineSpan :: SrcSpan -> Bool
-- True if the span is known to straddle more than one line
-- By default, it returns False
isOneLineSpan s
  | isGoodSrcSpan s = srcSpanStartLine s == srcSpanEndLine s
  | otherwise	    = False		

--------------------------------------------------------
-- Don't export these four;
-- they panic on Imported, Unhelpful.
-- They are for internal use only
-- Urk!  Some are needed for Lexer.x; see comment in export list

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

srcSpanStart (ImportedSpan str) = ImportedLoc str
srcSpanStart (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanStart s = mkSrcLoc (srcSpanFile s) 
			  (srcSpanStartLine s)
		 	  (srcSpanStartCol s)

srcSpanEnd (ImportedSpan str) = ImportedLoc str
srcSpanEnd (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanEnd s = 
  mkSrcLoc (srcSpanFile s) 
	   (srcSpanEndLine s)
 	   (srcSpanEndCol s)

srcLocSpan :: SrcLoc -> SrcSpan
srcLocSpan (ImportedLoc str)  = ImportedSpan str
srcLocSpan (UnhelpfulLoc str) = UnhelpfulSpan str
srcLocSpan (SrcLoc file line col) = SrcSpanPoint file line col

mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan (ImportedLoc str) _  = ImportedSpan str
mkSrcSpan (UnhelpfulLoc str) _ = UnhelpfulSpan str
mkSrcSpan _ (ImportedLoc str)  = ImportedSpan str
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
combineSrcSpans	(ImportedSpan str) _  = ImportedSpan str
combineSrcSpans	(UnhelpfulSpan str) r = r -- this seems more useful
combineSrcSpans	_ (ImportedSpan str)  = ImportedSpan str
combineSrcSpans	l (UnhelpfulSpan str) = l
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

pprDefnLoc :: SrcLoc -> SDoc
-- "defined at ..." or "imported from ..."
pprDefnLoc loc
  | isGoodSrcLoc loc = ptext SLIT("Defined at") <+> ppr loc
  | otherwise	     = ppr loc

instance Outputable SrcSpan where
    ppr span
      = getPprStyle $ \ sty ->
        if userStyle sty || debugStyle sty then
	   pprUserSpan span
	else
	   hcat [text "{-# LINE ", int (srcSpanStartLine span), space,
		 char '\"', ftext (srcSpanFile span), text " #-}"]


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

pprUserSpan (ImportedSpan mod) = ptext SLIT("Defined in") <+> text mod
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
  ppr (L span e) =  ppr e
	-- do we want to dump the span in debugSty mode?    
\end{code}
