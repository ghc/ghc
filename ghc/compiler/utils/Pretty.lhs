%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Pretty]{Pretty-printing data type}

\begin{code}
#if defined(COMPILING_GHC)
# include "HsVersions.h"
#else
# define FAST_STRING String
# define _LENGTH_    length
#endif

module Pretty (
	Pretty(..),

#if defined(COMPILING_GHC)
	PprStyle(..),
	prettyToUn,
	codeStyle, -- UNUSED: stySwitch,
#endif
	ppNil, ppStr, ppPStr, ppChar, ppInt, ppInteger,
	ppFloat, ppDouble,
#if __GLASGOW_HASKELL__ >= 23
	-- may be able to *replace* ppDouble
	ppRational,
#endif
	ppSP, pp'SP, ppLbrack, ppRbrack, ppLparen, ppRparen,
	ppSemi, ppComma, ppEquals,

	ppCat, ppBeside, ppBesides, ppAbove, ppAboves,
	ppNest, ppSep, ppHang, ppInterleave, ppIntersperse,
	ppShow,
#if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ >= 22
	ppAppendFile,
#endif

	-- abstract type, to complete the interface...
	PrettyRep(..), CSeq, Delay
#if defined(COMPILING_GHC)
	, GlobalSwitch, Unpretty(..)
#endif
   ) where

import CharSeq
#if defined(COMPILING_GHC)
import Unpretty		( Unpretty(..) )
import CmdLineOpts	( GlobalSwitch )
#endif
\end{code}

Based on John Hughes's pretty-printing library.  For now, that code
and notes for it are in files \tr{pp-rjmh*} (ToDo: rm).

%************************************************
%*						*
	\subsection{The interface}
%*						*
%************************************************

\begin{code}
ppNil		:: Pretty
ppSP, pp'SP, ppLbrack, ppRbrack, ppLparen, ppRparen, ppSemi, ppComma, ppEquals :: Pretty

ppStr		:: [Char] -> Pretty
ppPStr		:: FAST_STRING -> Pretty
ppChar		:: Char	   -> Pretty
ppInt		:: Int	   -> Pretty
ppInteger	:: Integer -> Pretty
ppDouble	:: Double  -> Pretty
ppFloat		:: Float   -> Pretty
#if __GLASGOW_HASKELL__ >= 23
ppRational	:: Rational -> Pretty
#endif

ppBeside	:: Pretty -> Pretty -> Pretty
ppBesides	:: [Pretty] -> Pretty
ppBesideSP	:: Pretty -> Pretty -> Pretty
ppCat		:: [Pretty] -> Pretty		-- i.e., ppBesidesSP

ppAbove		:: Pretty -> Pretty -> Pretty
ppAboves	:: [Pretty] -> Pretty

ppInterleave	:: Pretty -> [Pretty] -> Pretty
ppIntersperse	:: Pretty -> [Pretty] -> Pretty	-- no spaces between, no ppSep
ppSep		:: [Pretty] -> Pretty
ppHang		:: Pretty -> Int -> Pretty -> Pretty
ppNest		:: Int -> Pretty -> Pretty

ppShow		:: Int -> Pretty -> [Char]

#if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ >= 22
# if __GLASGOW_HASKELL__ < 23
#  define _FILE _Addr
# endif
ppAppendFile	:: _FILE -> Int -> Pretty -> PrimIO ()
#endif
\end{code}

%************************************************
%*						*
	\subsection{The representation}
%*						*
%************************************************

\begin{code}
type Pretty = Int	-- The width to print in
	   -> Bool	-- True => vertical context
	   -> PrettyRep

data PrettyRep
  = MkPrettyRep	CSeq	-- The text
		(Delay Int) -- No of chars in last line
		Bool	-- True if empty object
		Bool	-- Fits on a single line in specified width

data Delay a = MkDelay a

forceDel (MkDelay _) r = r

forceBool True  r = r
forceBool False r = r

forceInfo ll emp sl r = forceDel ll (forceBool emp (forceBool sl r))

ppShow width p
  = case (p width False) of
      MkPrettyRep seq ll emp sl -> cShow seq

#if defined(COMPILING_GHC) && __GLASGOW_HASKELL__ >= 22
ppAppendFile f width p
  = case (p width False) of
      MkPrettyRep seq ll emp sl -> cAppendFile f seq
#endif

ppNil    width is_vert = MkPrettyRep cNil (MkDelay 0) True (width >= 0)
			   -- Doesn't fit if width < 0, otherwise, ppNil
			   -- will make ppBesides always return True.

ppStr  s width is_vert = MkPrettyRep (cStr s) (MkDelay ls) False (width >= ls)
			   where ls = length s
ppPStr s width is_vert = MkPrettyRep (cPStr s) (MkDelay ls) False (width >= ls)
			   where ls = _LENGTH_ s
ppChar c width is_vert = MkPrettyRep (cCh c) (MkDelay 1) False (width >= 1)

ppInt  n width is_vert = MkPrettyRep (cStr s) (MkDelay ls) False (width >= ls)
			   where s = show n; ls = length s

ppInteger n  = ppStr (show n)
ppDouble  n  = ppStr (show n)
ppFloat   n  = ppStr (show n)
#if __GLASGOW_HASKELL__ >= 23
--ppRational n = ppStr (_showRational 30 n)
ppRational n = ppStr (show (fromRationalX n)) -- _showRational 30 n)
#endif

ppSP	  = ppChar ' '
pp'SP	  = ppStr ", "
ppLbrack  = ppChar '['
ppRbrack  = ppChar ']'
ppLparen  = ppChar '('
ppRparen  = ppChar ')'
ppSemi    = ppChar ';'
ppComma   = ppChar ','
ppEquals  = ppChar '='

ppInterleave sep ps = ppSep (pi ps)
  where
   pi []	= []
   pi [x]	= [x]
   pi (x:xs)	= (ppBeside x sep) : pi xs
\end{code}

ToDo: this could be better: main pt is: no extra spaces in between.

\begin{code}
ppIntersperse sep ps = ppBesides (pi ps)
  where
   pi []	= []
   pi [x]	= [x]
   pi (x:xs)	= (ppBeside x sep) : pi xs
\end{code}

Laziness is important in @ppBeside@.  If the first thing is not a
single line it will return @False@ for the single-line boolean without
laying out the second.

\begin{code}
ppBeside p1 p2 width is_vert
  = case (p1 width False) of
      MkPrettyRep seq1 (MkDelay ll1) emp1 sl1 ->
	  MkPrettyRep (seq1 `cAppend` (cIndent ll1 seq2))
		      (MkDelay (ll1 + ll2))
		      (emp1 && emp2)
		      ((width >= 0) && (sl1 && sl2))
		      -- This sequence of (&&)'s ensures that ppBeside
		      -- returns a False for sl as soon as possible.
       where -- NB: for case alt
	 seq2 = forceInfo x_ll2 emp2 sl2 x_seq2
	 MkDelay ll2 = x_ll2
	 MkPrettyRep x_seq2 x_ll2 emp2 sl2 = p2 (width-ll1) False
	 -- ToDo: if emp{1,2} then we really
	 -- should be passing on "is_vert" to p{2,1}.

ppBesides [] = ppNil
ppBesides ps = foldr1 ppBeside ps
\end{code}

@ppBesideSP@ puts two things beside each other separated by a space.

\begin{code}
ppBesideSP p1 p2 width is_vert
  = case (p1 width False) of
      MkPrettyRep seq1 (MkDelay ll1) emp1 sl1 ->
	  MkPrettyRep (seq1 `cAppend` (sp `cAppend` (cIndent li seq2)))
		   (MkDelay (li + ll2))
		   (emp1 && emp2)
		   ((width >= wi) && (sl1 && sl2))
       where -- NB: for case alt
	 seq2 = forceInfo x_ll2 emp2 sl2 x_seq2
	 MkDelay ll2 = x_ll2
	 MkPrettyRep x_seq2 x_ll2 emp2 sl2 = p2 (width-li) False
	 li, wi :: Int
	 li = if emp1 then 0 else ll1+1
	 wi = if emp1 then 0 else 1
	 sp = if emp1 || emp2 then cNil else (cCh ' ')
\end{code}

@ppCat@ is the name I (WDP) happen to have been using for @ppBesidesSP@.

\begin{code}
ppCat []  = ppNil
ppCat ps  = foldr1 ppBesideSP ps
\end{code}

\begin{code}
ppAbove p1 p2 width is_vert
  = case (p1 width True) of
      MkPrettyRep seq1 (MkDelay ll1) emp1 sl1 ->
	  MkPrettyRep (seq1 `cAppend` (nl `cAppend` seq2))
		      (MkDelay ll2)
		      -- ToDo: make ll depend on empties?
		      (emp1 && emp2)
		      False
       where -- NB: for case alt
	 nl = if emp1 || emp2 then cNil else cNL
	 seq2 = forceInfo x_ll2 emp2 sl2 x_seq2
	 MkDelay ll2 = x_ll2 -- Don't "optimise" this away!
	 MkPrettyRep x_seq2 x_ll2 emp2 sl2 = p2 width True
	     -- ToDo: ditto about passing is_vert if empties

ppAboves [] = ppNil
ppAboves ps = foldr1 ppAbove ps
\end{code}

\begin{code}
ppNest n p width False = p width False
ppNest n p width True
  = case (p (width-n) True) of
      MkPrettyRep seq (MkDelay ll) emp sl ->
    	MkPrettyRep (cIndent n seq) (MkDelay (ll+n)) emp sl
\end{code}

The length-check below \tr{(ll1+ll2+1) <= width} should really check for
max widths not the width of the last line.

\begin{code}
ppHang p1 n p2 width is_vert	-- This is a little bit stricter than it could
				-- be made with a little more effort.
				-- Eg the output always starts with seq1
  = case (p1 width False) of
      MkPrettyRep seq1 (MkDelay ll1) emp1 sl1 ->
	  if emp1 then
	      p2 width is_vert
	  else 
	  if (ll1 <= n) || sl2 then	-- very ppBesideSP'ish
	      -- Hang it if p1 shorter than indent or if it doesn't fit
	      MkPrettyRep (seq1 `cAppend` ((cCh ' ') `cAppend` (cIndent (ll1+1) seq2)))
			(MkDelay (ll1 + 1 + ll2))
			False
			(sl1 && sl2)
	  else
	      -- Nest it (pretty ppAbove-ish)
	      MkPrettyRep (seq1 `cAppend` (cNL `cAppend` (cIndent n seq2')))
			(MkDelay ll2') -- ToDo: depend on empties
			False
			False
       where -- NB: for case alt
	 seq2 = forceInfo x_ll2 emp2 sl2 x_seq2
	 MkDelay ll2 = x_ll2
	 MkPrettyRep x_seq2 x_ll2 emp2 sl2 = p2 (width-(ll1+1)) False
	     -- ToDo: more "is_vert if empty" stuff

	 seq2' = forceInfo x_ll2' emp2' sl2' x_seq2'
	 MkDelay ll2' = x_ll2'		-- Don't "optimise" this away!
	 MkPrettyRep x_seq2' x_ll2' emp2' sl2' = p2 (width-n) False	-- ToDo: True?
\end{code}

\begin{code}
ppSep []  width is_vert = ppNil width is_vert
ppSep [p] width is_vert = p     width is_vert

-- CURRENT, but BAD.  Quadratic behaviour on the perfectly reasonable
--	ppSep [a, ppSep[b, ppSep [c, ... ]]]

ppSep ps  width is_vert
  = case (ppCat ps width is_vert) of
      MkPrettyRep seq x_ll emp sl ->
	if sl then			-- Fits on one line
	   MkPrettyRep seq x_ll emp sl
	else
	   ppAboves ps width is_vert	-- Takes several lines
\end{code}

%************************************************************************
%*									*
\subsection[Outputable-print]{Pretty-printing stuff}
%*									*
%************************************************************************

ToDo: this is here for no-original-name reasons (mv?).

There is no clearly definitive list of @PprStyles@; I suggest the
following:

\begin{code}
#if defined(COMPILING_GHC)
    -- to the end of file

data PprStyle
  = PprForUser	 		-- Pretty-print in a way that will
				-- make sense to the ordinary user;
				-- must be very close to Haskell
				-- syntax, etc.  ToDo: how diff is
				-- this from what pprInterface must
				-- do?
  | PprDebug			-- Standard debugging output
  | PprShowAll			-- Debugging output which leaves
				-- nothing to the imagination
  | PprInterface		-- Interface generation
	(GlobalSwitch -> Bool)	--  (we can look at cmd-line flags)
  | PprForC			-- must print out C-acceptable names
	(GlobalSwitch -> Bool)	--  (ditto)
  | PprUnfolding		-- for non-interface intermodule info
	(GlobalSwitch -> Bool)	-- the compiler writes/reads
  | PprForAsm			-- must print out assembler-acceptable names
	(GlobalSwitch -> Bool)	--  (ditto)
        Bool	        	-- prefix CLabel with underscore?
        (String -> String)    	-- format AsmTempLabel
\end{code}

The following test decides whether or not we are actually generating
code (either C or assembly).
\begin{code}
codeStyle :: PprStyle -> Bool
codeStyle (PprForC _) = True
codeStyle (PprForAsm _ _ _) = True
codeStyle _ = False

{- UNUSED:
stySwitch :: PprStyle -> GlobalSwitch -> Bool
stySwitch (PprInterface sw) = sw
stySwitch (PprForC sw) = sw
stySwitch (PprForAsm sw _ _) = sw
-}
\end{code}

Orthogonal to these printing styles are (possibly) some command-line
flags that affect printing (often carried with the style).  The most
likely ones are variations on how much type info is shown.

\begin{code}
prettyToUn :: Pretty -> Unpretty

prettyToUn p
  = case (p 999999{-totally bogus width-} False{-also invented-}) of
      MkPrettyRep seq ll emp sl -> seq

#endif {-COMPILING_GHC-}
\end{code}

-----------------------------------
\begin{code}
-- from Lennart
fromRationalX :: (RealFloat a) => Rational -> a

fromRationalX r =
	let 
	    h = ceiling (huge `asTypeOf` x)
	    b = toInteger (floatRadix x)
	    x = fromRat 0 r
	    fromRat e0 r' =
		let d = denominator r'
		    n = numerator r'
	        in  if d > h then
		       let e = integerLogBase b (d `div` h) + 1
		       in  fromRat (e0-e) (n % (d `div` (b^e)))
		    else if abs n > h then
		       let e = integerLogBase b (abs n `div` h) + 1
		       in  fromRat (e0+e) ((n `div` (b^e)) % d)
		    else
		       scaleFloat e0 (fromRational r')
	in  x

-- Compute the discrete log of i in base b.
-- Simplest way would be just divide i by b until it's smaller then b, but that would
-- be very slow!  We are just slightly more clever.
integerLogBase :: Integer -> Integer -> Int
integerLogBase b i =
     if i < b then
        0
     else
	-- Try squaring the base first to cut down the number of divisions.
        let l = 2 * integerLogBase (b*b) i

	    doDiv :: Integer -> Int -> Int
	    doDiv j k = if j < b then k else doDiv (j `div` b) (k+1)
	in
	doDiv (i `div` (b^l)) l


------------

-- Compute smallest and largest floating point values.
{-
tiny :: (RealFloat a) => a
tiny =
	let (l, _) = floatRange x
	    x = encodeFloat 1 (l-1)
	in  x
-}

huge :: (RealFloat a) => a
huge =
	let (_, u) = floatRange x
	    d = floatDigits x
	    x = encodeFloat (floatRadix x ^ d - 1) (u - d)
	in  x
\end{code}
