%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Unpretty]{Unpretty-printing data type}

\begin{code}
#include "HsVersions.h"

module Unpretty (
	Unpretty(..),
	PprStyle(..),	-- re-exported from Pretty
	uppNil, uppStr, uppPStr, uppChar, uppInt, uppInteger, --UNUSED: uppDouble,
	uppSP, uppLbrack, uppRbrack, uppLparen, uppRparen, -- UNUSED: upp'SP,
	uppSemi, uppComma, uppEquals,

	uppCat, uppBeside, uppBesides, uppAbove, uppAboves,
	uppNest, uppSep, uppInterleave, uppIntersperse, --UNUSED: uppHang,
	uppShow,
#ifdef __GLASGOW_HASKELL__
	uppAppendFile,
	IF_ATTACK_PRAGMAS(cAppendFile COMMA)
	IF_ATTACK_PRAGMAS(cInt COMMA)
#endif
#ifdef DPH
	unprettyToStr,
#endif {- Data Parallel Haskell -}

	-- abstract type, to complete the interface...
	CSeq, GlobalSwitch
   ) where

import CharSeq
import Outputable
import Pretty		( PprStyle(..), Pretty(..), GlobalSwitch )
import Util
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
--UNUSED: upp'SP :: Unpretty

uppStr		:: [Char] -> Unpretty
uppPStr		:: FAST_STRING -> Unpretty
uppChar		:: Char -> Unpretty
uppInt		:: Int -> Unpretty
uppInteger	:: Integer -> Unpretty
--UNUSED:uppDouble	:: Double -> Unpretty

uppBeside	:: Unpretty -> Unpretty -> Unpretty
uppBesides	:: [Unpretty] -> Unpretty
ppBesideSP	:: Unpretty -> Unpretty -> Unpretty
uppCat		:: [Unpretty] -> Unpretty		-- i.e., ppBesidesSP

uppAbove	:: Unpretty -> Unpretty -> Unpretty
uppAboves	:: [Unpretty] -> Unpretty

uppInterleave	:: Unpretty -> [Unpretty] -> Unpretty
uppIntersperse	:: Unpretty -> [Unpretty] -> Unpretty	-- no spaces between
uppSep		:: [Unpretty] -> Unpretty
--UNUSED:uppHang		:: Unpretty -> Int -> Unpretty -> Unpretty
uppNest		:: Int -> Unpretty -> Unpretty

uppShow		:: Int -> Unpretty -> [Char]

#ifdef __GLASGOW_HASKELL__
uppAppendFile	:: _FILE -> Int -> Unpretty -> PrimIO ()
#endif
\end{code}

%************************************************
%*						*
	\subsection{The representation}
%*						*
%************************************************

\begin{code}
uppShow _ p	= cShow p

#ifdef __GLASGOW_HASKELL__
uppAppendFile f _ p = cAppendFile f p
#endif

uppNil		= cNil
uppStr s	= cStr s
uppPStr s	= cPStr s
uppChar c	= cCh c
uppInt n	= cInt n

uppInteger n	= cStr (show n)
--UNUSED:uppDouble  n	= cStr (show n)

uppSP		= cCh ' '
--UNUSED:upp'SP		= cStr  ", "
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
\end{code}

\begin{code}
--UNUSED: uppHang p1 n p2 = ppBesideSP p1 p2

uppSep ps = uppBesides ps
\end{code}

\begin{code}
#ifdef DPH
unprettyToStr:: Unpretty -> String
unprettyToStr thing = uppShow 80 thing
#endif {- Data Parallel Haskell -}
\end{code}
