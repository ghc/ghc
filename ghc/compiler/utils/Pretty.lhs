%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
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
	SYN_IE(Pretty),

#if defined(COMPILING_GHC)
	prettyToUn,
#endif
	ppNil, ppStr, ppPStr, ppChar, ppInt, ppInteger,
	ppFloat, ppDouble,
#if __GLASGOW_HASKELL__
	-- may be able to *replace* ppDouble
	ppRational,
#endif
	ppSP, pp'SP, ppLbrack, ppRbrack, ppLparen, ppRparen,
	ppSemi, ppComma, ppEquals,
	ppBracket, ppParens, ppQuote,

	ppCat, ppBeside, ppBesides, ppAbove, ppAboves,
	ppNest, ppSep, ppHang, ppInterleave, ppIntersperse,
	ppShow, speakNth,

#if defined(COMPILING_GHC)
	ppPutStr,
#endif

	-- abstract type, to complete the interface...
	PrettyRep(..), Delay
   ) where

#if defined(COMPILING_GHC)

CHK_Ubiq() -- debugging consistency check
IMPORT_1_3(Ratio)
IMPORT_1_3(IO)

import Unpretty		( SYN_IE(Unpretty) )
#endif

import CharSeq
\end{code}

Based on John Hughes's pretty-printing library.  Loosely.  Very
loosely.

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
ppRational	:: Rational -> Pretty

ppBracket	:: Pretty -> Pretty -- put brackets around it
ppParens	:: Pretty -> Pretty -- put parens   around it

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

#if defined(COMPILING_GHC)
ppPutStr	:: Handle -> Int -> Pretty -> IO ()
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

#if defined(COMPILING_GHC)
ppPutStr f width p
  = case (p width False) of
      MkPrettyRep seq ll emp sl -> cPutStr f seq
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

ppRational n = ppStr (show (fromRationalX n)) -- _showRational 30 n)

ppSP	  = ppChar ' '
pp'SP	  = ppStr ", "
ppLbrack  = ppChar '['
ppRbrack  = ppChar ']'
ppLparen  = ppChar '('
ppRparen  = ppChar ')'
ppSemi    = ppChar ';'
ppComma   = ppChar ','
ppEquals  = ppChar '='

ppBracket p = ppBeside ppLbrack (ppBeside p ppRbrack)
ppParens  p = ppBeside ppLparen (ppBeside p ppRparen)
ppQuote   p = ppBeside (ppChar '`') (ppBeside p (ppChar '\''))

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


@speakNth@ converts an integer to a verbal index; eg 1 maps to
``first'' etc.

\begin{code}
speakNth :: Int -> Pretty

speakNth 1 = ppStr "first"
speakNth 2 = ppStr "second"
speakNth 3 = ppStr "third"
speakNth 4 = ppStr "fourth"
speakNth 5 = ppStr "fifth"
speakNth 6 = ppStr "sixth"
speakNth n = ppBesides [ ppInt n, ppStr st_nd_rd_th ]
  where
    st_nd_rd_th | n_rem_10 == 1 = "st"
		| n_rem_10 == 2 = "nd"
		| n_rem_10 == 3 = "rd"
		| otherwise     = "th"

    n_rem_10 = n `rem` 10
\end{code}


%************************************************************************
%*									*
\subsection[Outputable-print]{Pretty-printing stuff}
%*									*
%************************************************************************

\begin{code}
#if defined(COMPILING_GHC)
    -- to the end of file

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
