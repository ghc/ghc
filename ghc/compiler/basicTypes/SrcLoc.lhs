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

	importedSrcLoc,		-- Unknown place in an interface
	wiredInSrcLoc,		-- Something wired into the compiler
	generatedSrcLoc,	-- Code generated within the compiler

	incSrcLine, replaceSrcLine,
	
	srcLocFile,		-- return the file name part.
	srcLocLine		-- return the line part.
    ) where

#include "HsVersions.h"

import Util		( thenCmp )
import Outputable
import FastString	( unpackFS )
import FastTypes
import FastString

import GLAEXTS		( (+#) )
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
		FastInt

  | UnhelpfulSrcLoc FastString	-- Just a general indication
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
mkSrcLoc x y      = SrcLoc x (iUnbox y)
wiredInSrcLoc	  = WiredInLoc
noSrcLoc	  = UnhelpfulSrcLoc FSLIT("<No locn>")
importedSrcLoc	  = UnhelpfulSrcLoc FSLIT("<imported>")
generatedSrcLoc   = UnhelpfulSrcLoc FSLIT("<compiler-generated-code>")

isGoodSrcLoc (SrcLoc _ _) = True
isGoodSrcLoc other        = False

isWiredInLoc WiredInLoc = True
isWiredInLoc other	= False

srcLocFile :: SrcLoc -> FastString
srcLocFile (SrcLoc fname _) = fname

srcLocLine :: SrcLoc -> FastInt
srcLocLine (SrcLoc _ l) = l

incSrcLine :: SrcLoc -> SrcLoc
incSrcLine (SrcLoc s l) = SrcLoc s (l +# 1#)
incSrcLine loc  	= loc

replaceSrcLine :: SrcLoc -> FastInt -> SrcLoc
replaceSrcLine (SrcLoc s _) l = SrcLoc s l
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

cmpSrcLoc (SrcLoc s1 l1) WiredInLoc	     = GT
cmpSrcLoc (SrcLoc s1 l1) (UnhelpfulSrcLoc _) = LT
cmpSrcLoc (SrcLoc s1 l1) (SrcLoc s2 l2)      = (s1 `compare` s2) `thenCmp` (l1 `cmpline` l2)
					     where
				  		l1 `cmpline` l2 | l1 <#  l2 = LT
								| l1 ==# l2 = EQ
								| otherwise = GT 
					  
instance Outputable SrcLoc where
    ppr (SrcLoc src_path src_line)
      = getPprStyle $ \ sty ->
        if userStyle sty || debugStyle sty then
	   hcat [ ftext src_path, char ':', int (iBox src_line) ]
	else
	   hcat [text "{-# LINE ", int (iBox src_line), space,
		 char '\"', ftext src_path, text " #-}"]
      where
	src_file = unpackFS src_path	-- Leave the directory prefix intact,
					-- so emacs can find the file

    ppr (UnhelpfulSrcLoc s) = ftext s
    ppr WiredInLoc	    = ptext SLIT("<Wired in>")
\end{code}
