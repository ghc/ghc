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

module SrcLoc (
	SrcLoc,			-- abstract

	mkSrcLoc, mkSrcLoc2,	-- the usual
	mkUnknownSrcLoc,	-- "I'm sorry, I haven't a clue"
	mkBuiltinSrcLoc,	-- something wired into the compiler
	mkGeneratedSrcLoc,	-- code generated within the compiler
	unpackSrcLoc
    ) where

import Outputable
import Pretty
import Util
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
  = SrcLoc	FAST_STRING	-- source file name
		FAST_STRING	-- line number in source file
  | SrcLoc2	FAST_STRING	-- same, but w/ an Int line#
		FAST_INT
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
mkSrcLoc	    = SrcLoc
mkSrcLoc2 x IBOX(y) = SrcLoc2 x y
mkUnknownSrcLoc	    = SrcLoc SLIT("<unknown>") SLIT("<unknown>")
mkBuiltinSrcLoc	    = SrcLoc SLIT("<built-into-the-compiler>") SLIT("<none>")
mkGeneratedSrcLoc   = SrcLoc SLIT("<compiler-generated-code>") SLIT("<none>")

unpackSrcLoc (SrcLoc  src_file src_line) = (src_file, src_line)
unpackSrcLoc (SrcLoc2 src_file src_line) = (src_file, _PK_ (show IBOX(src_line)))
\end{code}

%************************************************************************
%*									*
\subsection[SrcLoc-instances]{Instance declarations for various names}
%*									*
%************************************************************************

\begin{code}
instance Outputable SrcLoc where
    ppr PprForUser (SrcLoc src_file src_line)
      = ppBesides [ ppChar '"', ppPStr src_file, ppPStr SLIT("\", line "), ppPStr src_line ]

    ppr sty (SrcLoc src_file src_line)
      = ppBesides [ppPStr SLIT("{-# LINE "), ppPStr src_line, ppSP,
		   ppChar '"', ppPStr src_file, ppPStr SLIT("\" #-}")]

    ppr sty (SrcLoc2 src_file src_line)
      = ppr sty (SrcLoc src_file (_PK_ (show IBOX(src_line))))
\end{code}
