%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
%************************************************************************
%*									*
\section[SrcLoc]{The @SrcLoc@ type}
%*									*
%************************************************************************

\begin{code}
#include "HsVersions.h"

module SrcLoc {- (
	SrcLoc,			-- Abstract

	mkSrcLoc,
	noSrcLoc, isNoSrcLoc,	-- "I'm sorry, I haven't a clue"

	mkIfaceSrcLoc,		-- Unknown place in an interface
				-- (this one can die eventually ToDo)

	mkBuiltinSrcLoc,	-- Something wired into the compiler

	mkGeneratedSrcLoc	-- Code generated within the compiler
    ) -} where

IMP_Ubiq()

import Outputable
import PprStyle		( PprStyle(..), userStyle )
import Pretty

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

  | SrcLoc	FAST_STRING	-- A precise location
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
\end{code}

%************************************************************************
%*									*
\subsection[SrcLoc-instances]{Instance declarations for various names}
%*									*
%************************************************************************

\begin{code}
instance Outputable SrcLoc where
    ppr sty (SrcLoc src_file src_line)
      | userStyle sty
      = hcat [ ptext src_file, char ':', text (show IBOX(src_line)) ]

      | otherwise
      = hcat [text "{-# LINE ", text (show IBOX(src_line)), space,
		   char '\"', ptext src_file, text " #-}"]
    ppr sty (UnhelpfulSrcLoc s) = ptext s

    ppr sty NoSrcLoc = text "<NoSrcLoc>"
\end{code}

{-
      = hcat [ptext SLIT("{-# LINE "), text (show IBOX(src_line)), space,
		   char '"', ptext src_file, ptext SLIT(" #-}")]
 --ptext SLIT("\" #-}")]
-}
