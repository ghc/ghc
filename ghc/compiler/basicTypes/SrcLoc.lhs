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

	mkSrcLoc,
	noSrcLoc, isNoSrcLoc,	-- "I'm sorry, I haven't a clue"

	mkIfaceSrcLoc,		-- Unknown place in an interface
				-- (this one can die eventually ToDo)

	mkBuiltinSrcLoc,	-- Something wired into the compiler

	mkGeneratedSrcLoc,	-- Code generated within the compiler

	incSrcLine, replaceSrcLine,
	
	srcLocFile,		-- return the file name part.
	srcLocLine		-- return the line part.
    ) where

#include "HsVersions.h"

import Util		( thenCmp )
import Outputable
import FastString	( unpackFS )
import GlaExts		( Int(..), (+#) )
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
  = NoSrcLoc

  | SrcLoc	FAST_STRING	-- A precise location (file name)
		FAST_INT

  | UnhelpfulSrcLoc FAST_STRING	-- Just a general indication
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
noSrcLoc	    = NoSrcLoc
mkSrcLoc x IBOX(y)  = SrcLoc x y

mkIfaceSrcLoc	    = UnhelpfulSrcLoc SLIT("<an interface file>")
mkBuiltinSrcLoc	    = UnhelpfulSrcLoc SLIT("<built-into-the-compiler>")
mkGeneratedSrcLoc   = UnhelpfulSrcLoc SLIT("<compiler-generated-code>")

isNoSrcLoc NoSrcLoc = True
isNoSrcLoc other    = False

srcLocFile :: SrcLoc -> FAST_STRING
srcLocFile (SrcLoc fname _) = fname

srcLocLine :: SrcLoc -> FAST_INT
srcLocLine (SrcLoc _ l) = l

incSrcLine :: SrcLoc -> SrcLoc
incSrcLine (SrcLoc s l) = SrcLoc s (l +# 1#)
incSrcLine loc  	= loc

replaceSrcLine :: SrcLoc -> FAST_INT -> SrcLoc
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

cmpSrcLoc NoSrcLoc NoSrcLoc = EQ
cmpSrcLoc NoSrcLoc other    = LT

cmpSrcLoc (UnhelpfulSrcLoc s1) (UnhelpfulSrcLoc s2) = s1 `compare` s2
cmpSrcLoc (UnhelpfulSrcLoc s1) other		    = GT

cmpSrcLoc (SrcLoc s1 l1) NoSrcLoc	     = GT
cmpSrcLoc (SrcLoc s1 l1) (UnhelpfulSrcLoc _) = LT
cmpSrcLoc (SrcLoc s1 l1) (SrcLoc s2 l2)      = (s1 `compare` s2) `thenCmp` (l1 `cmpline` l2)
					     where
				  		l1 `cmpline` l2 | l1 <#  l2 = LT
								| l1 ==# l2 = EQ
								| otherwise = GT 
					  
instance Outputable SrcLoc where
    ppr (SrcLoc src_path src_line)
      = getPprStyle $ \ sty ->
        if userStyle sty then
	   hcat [ text src_file, char ':', int IBOX(src_line) ]
	else
	if debugStyle sty then
	   hcat [ ptext src_path, char ':', int IBOX(src_line) ]
	else
	   hcat [text "{-# LINE ", int IBOX(src_line), space,
		 char '\"', ptext src_path, text " #-}"]
      where
	src_file = remove_directory_prefix (unpackFS src_path)

	remove_directory_prefix path = case break (== '/') path of
					  (filename, [])           -> filename
					  (prefix,   slash : rest) -> ASSERT( slash == '/' )
								      remove_directory_prefix rest

    ppr (UnhelpfulSrcLoc s) = ptext s

    ppr NoSrcLoc = text "<NoSrcLoc>"
\end{code}
