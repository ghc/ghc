%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
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

	incSrcLine,
	
	srcLocFile		-- return the file name part.
    ) where

#include "HsVersions.h"

import Outputable
import FastString	( unpackFS )
import GlaExts		( Int(..), Int#, (+#) )
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

incSrcLine :: SrcLoc -> SrcLoc
incSrcLine (SrcLoc s l) = SrcLoc s (l +# 1#)
incSrcLine loc  	= loc
\end{code}

%************************************************************************
%*									*
\subsection[SrcLoc-instances]{Instance declarations for various names}
%*									*
%************************************************************************

\begin{code}
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
