%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
%************************************************************************
%*									*
\section[SrcLoc]{The @SrcLoc@ type}
%*									*
%************************************************************************

\begin{code}
module SrcLoc (
	SrcLoc,			-- Abstract

	mkSrcLoc, isGoodSrcLoc,	isWiredInLoc,
	noSrcLoc, 		-- "I'm sorry, I haven't a clue"
	advanceSrcLoc,

	importedSrcLoc,		-- Unknown place in an interface
	wiredInSrcLoc,		-- Something wired into the compiler
	generatedSrcLoc,	-- Code generated within the compiler

	srcLocFile,		-- return the file name part
	srcLocLine,		-- return the line part
	srcLocCol,		-- return the column part
    ) where

#include "HsVersions.h"

import Util		( thenCmp )
import Outputable
import FastString	( unpackFS )
import FastTypes
import FastString

import GLAEXTS		( (+#), quotInt# )
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
  = WiredInLoc		-- Used exclusively for Ids and TyCons
			-- that are totally wired in to the
			-- compiler.  That supports the 
			-- occasionally-useful predicate
			-- isWiredInName

  | SrcLoc	FastString	-- A precise location (file name)
		FastInt		-- line
		FastInt		-- column

  | UnhelpfulSrcLoc FastString	-- Just a general indication

{-
data SrcSpan
  = WiredInSpan

	-- A precise source file span
  | SrcSpan	FastString 	-- file name
		FastInt		-- beginning line
		FastInt		-- beginning column
		FastInt		-- end line
		FastInt		-- end column		

  | UnhelpfulSrcSpan FastString	-- Just a general indication
-}
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
mkSrcLoc x line col = SrcLoc x (iUnbox line) (iUnbox col)
wiredInSrcLoc	  = WiredInLoc
noSrcLoc	  = UnhelpfulSrcLoc FSLIT("<No locn>")
importedSrcLoc	  = UnhelpfulSrcLoc FSLIT("<imported>")
generatedSrcLoc   = UnhelpfulSrcLoc FSLIT("<compiler-generated-code>")

isGoodSrcLoc (SrcLoc _ _ _) = True
isGoodSrcLoc other        = False

isWiredInLoc WiredInLoc = True
isWiredInLoc other	= False

srcLocFile :: SrcLoc -> FastString
srcLocFile (SrcLoc fname _ _) = fname

srcLocLine :: SrcLoc -> Int
srcLocLine (SrcLoc _ l c) = iBox l

srcLocCol :: SrcLoc -> Int
srcLocCol (SrcLoc _ l c) = iBox c

advanceSrcLoc :: SrcLoc -> Char -> SrcLoc
advanceSrcLoc (SrcLoc f l c) '\t' = SrcLoc f  l (tab c)
advanceSrcLoc (SrcLoc f l c) '\n' = SrcLoc f  (l +# 1#) 0#
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f  l (c +# 1#)

-- Advance to the next tab stop.  Tabs are at column positions 0, 8, 16, etc.
tab :: FastInt -> FastInt
tab c = (c `quotInt#` 8# +# 1#) *# 8#
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

cmpSrcLoc WiredInLoc WiredInLoc = EQ
cmpSrcLoc WiredInLoc other      = LT

cmpSrcLoc (UnhelpfulSrcLoc s1) (UnhelpfulSrcLoc s2) = s1 `compare` s2
cmpSrcLoc (UnhelpfulSrcLoc s1) other		    = GT

cmpSrcLoc (SrcLoc _ _ _) WiredInLoc	     = GT
cmpSrcLoc (SrcLoc _ _ _) (UnhelpfulSrcLoc _) = LT
cmpSrcLoc (SrcLoc s1 l1 c1) (SrcLoc s2 l2 c2)      
  = (s1 `compare` s2) `thenCmp` (l1 `cmpline` l2) `thenCmp` (c1 `cmpline` c2)
  where
	l1 `cmpline` l2 | l1 <#  l2 = LT
			| l1 ==# l2 = EQ
			| otherwise = GT 
					  
instance Outputable SrcLoc where
    ppr (SrcLoc src_path src_line src_col)
      = getPprStyle $ \ sty ->
        if userStyle sty || debugStyle sty then
	   hcat [ ftext src_path, char ':', 
		  int (iBox src_line)
		  {- TODO: char ':', int (iBox src_col) -} 
		]
	else
	   hcat [text "{-# LINE ", int (iBox src_line), space,
		 char '\"', ftext src_path, text " #-}"]
      where
	src_file = unpackFS src_path	-- Leave the directory prefix intact,
					-- so emacs can find the file

    ppr (UnhelpfulSrcLoc s) = ftext s
    ppr WiredInLoc	    = ptext SLIT("<Wired in>")
\end{code}
