%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Unpretty]{Unpretty-printing data type}

\begin{code}
#include "HsVersions.h"

module Unpretty (
	Unpretty(..),

	uppNil, uppStr, uppPStr, uppChar, uppInt, uppInteger,
	uppSP, uppLbrack, uppRbrack, uppLparen, uppRparen,
	uppSemi, uppComma, uppEquals,

	uppCat, uppBeside, uppBesides, uppAbove, uppAboves,
	uppNest, uppSep, uppInterleave, uppIntersperse,
	uppShow,
	uppAppendFile,

	-- abstract type, to complete the interface...
	CSeq
   ) where

CHK_Ubiq() -- debugging consistency check

import CharSeq
\end{code}

Same interface as @Pretty@, but doesn't do anything.

The pretty type is redefined here:
\begin{code}
type Unpretty = CSeq
\end{code}

%************************************************
%*						*
	\subsection{The interface}
%*						*
%************************************************

\begin{code}
uppNil		:: Unpretty
uppSP, uppLbrack, uppRbrack, uppLparen, uppRparen, uppSemi, uppComma, uppEquals :: Unpretty

uppStr		:: [Char] -> Unpretty
uppPStr		:: FAST_STRING -> Unpretty
uppChar		:: Char -> Unpretty
uppInt		:: Int -> Unpretty
uppInteger	:: Integer -> Unpretty

uppBeside	:: Unpretty -> Unpretty -> Unpretty
uppBesides	:: [Unpretty] -> Unpretty
ppBesideSP	:: Unpretty -> Unpretty -> Unpretty
uppCat		:: [Unpretty] -> Unpretty		-- i.e., ppBesidesSP

uppAbove	:: Unpretty -> Unpretty -> Unpretty
uppAboves	:: [Unpretty] -> Unpretty

uppInterleave	:: Unpretty -> [Unpretty] -> Unpretty
uppIntersperse	:: Unpretty -> [Unpretty] -> Unpretty	-- no spaces between
uppSep		:: [Unpretty] -> Unpretty
uppNest		:: Int -> Unpretty -> Unpretty

uppShow		:: Int -> Unpretty -> [Char]

uppAppendFile	:: _FILE -> Int -> Unpretty -> PrimIO ()
\end{code}

%************************************************
%*						*
	\subsection{The representation}
%*						*
%************************************************

\begin{code}
uppShow _ p	= cShow p

uppAppendFile f _ p = cAppendFile f p

uppNil		= cNil
uppStr s	= cStr s
uppPStr s	= cPStr s
uppChar c	= cCh c
uppInt n	= cInt n

uppInteger n	= cStr (show n)

uppSP		= cCh ' '
uppLbrack	= cCh '['
uppRbrack	= cCh ']'
uppLparen	= cCh '('
uppRparen	= cCh ')'
uppSemi		= cCh ';'
uppComma	= cCh ','
uppEquals	= cCh '='

uppInterleave sep ps = uppSep (pi ps)
  where
   pi []	= []
   pi [x]	= [x]
   pi (x:xs)	= (cAppend{-uppBeside-} x sep) : pi xs
\end{code}

\begin{code}
uppIntersperse sep ps = uppBesides (pi ps)
  where
   pi []	= []
   pi [x]	= [x]
   pi (x:xs)	= (cAppend{-uppBeside-} x sep) : pi xs
\end{code}

\begin{code}
uppBeside p1 p2  = p1 `cAppend` p2

uppBesides []	  = cNil{-uppNil-}
uppBesides [p]	  = p
uppBesides (p:ps) = p `cAppend`{-uppBeside-} uppBesides ps
\end{code}

\begin{code}
ppBesideSP p1 p2 = p1 `cAppend` (cCh ' ') `cAppend` p2
\end{code}

@uppCat@ is the name I (WDP) happen to have been using for @ppBesidesSP@.

\begin{code}
uppCat []     = cNil{-uppNil-}
uppCat [p]    = p
uppCat (p:ps) = ppBesideSP p (uppCat ps)

uppAbove p1 p2 = p1 `cAppend` (cCh '\n') `cAppend` p2

uppAboves []	 = cNil{-uppNil-}
uppAboves [p]	 = p
uppAboves (p:ps) = p `cAppend` (cCh '\n') `cAppend` (uppAboves ps)

uppNest n p = p

uppSep ps = uppBesides ps
\end{code}
