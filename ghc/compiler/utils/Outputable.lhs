%
% (c) The GRASP Project, Glasgow University, 1992-1998
%
\section[Outputable]{Classes for pretty-printing}

Defines classes for pretty-printing and forcing, both forms of
``output.''

\begin{code}
{-# OPTIONS -fno-prune-tydecls #-}
-- Hopefully temporary; 3.02 complained about not being able
-- to see the consructors for ForeignObj

module Outputable (
	Outputable(..),			-- Class

	PprStyle, CodeStyle(..), 
	getPprStyle, withPprStyle, pprDeeper,
	codeStyle, ifaceStyle, userStyle, debugStyle, asmStyle,
	ifPprDebug, ifNotPprForUser,

	SDoc, 		-- Abstract
	interppSP, interpp'SP, pprQuotedList, pprWithCommas,
	empty, nest,
	text, char, ptext,
	int, integer, float, double, rational,
	parens, brackets, braces, quotes, doubleQuotes, angleBrackets,
	semi, comma, colon, dcolon, space, equals, dot,
	lparen, rparen, lbrack, rbrack, lbrace, rbrace, underscore,
	(<>), (<+>), hcat, hsep, 
	($$), ($+$), vcat, 
	sep, cat, 
	fsep, fcat, 
	hang, punctuate,
	speakNth, speakNTimes,

	printSDoc, printErrs, printDump, 
	printForC, printForAsm, printForIface, printForUser,
	pprCode, pprCols,
	showSDoc, showSDocDebug, showSDocIface, showsPrecSDoc, 
	pprFSAsString,


	-- error handling
	pprPanic, pprPanic#, pprError, pprTrace, assertPprPanic, warnPprTrace,
	trace, panic, panic#, assertPanic
    ) where

#include "HsVersions.h"


import IO		( Handle, hPutChar, hPutStr, stderr, stdout )
import CmdLineOpts	( opt_PprStyle_Debug, opt_PprUserLength )
import FastString
import qualified Pretty
import Pretty		( Doc, Mode(..), TextDetails(..), fullRender )
import Panic
import ST		( runST )
import Foreign
\end{code}


%************************************************************************
%*									*
\subsection{The @PprStyle@ data type}
%*									*
%************************************************************************

\begin{code}
data PprStyle
  = PprUser Depth		-- Pretty-print in a way that will
				-- make sense to the ordinary user;
				-- must be very close to Haskell
				-- syntax, etc.

  | PprDebug			-- Standard debugging output

  | PprInterface		-- Interface generation

  | PprCode CodeStyle		-- Print code; either C or assembler


data CodeStyle = CStyle		-- The format of labels differs for C and assembler
	       | AsmStyle

data Depth = AllTheWay
           | PartWay Int	-- 0 => stop
\end{code}

Orthogonal to the above printing styles are (possibly) some
command-line flags that affect printing (often carried with the
style).  The most likely ones are variations on how much type info is
shown.

The following test decides whether or not we are actually generating
code (either C or assembly), or generating interface files.

%************************************************************************
%*									*
\subsection{The @SDoc@ data type}
%*									*
%************************************************************************

\begin{code}
type SDoc = PprStyle -> Doc

withPprStyle :: PprStyle -> SDoc -> SDoc
withPprStyle sty d sty' = d sty

pprDeeper :: SDoc -> SDoc
pprDeeper d (PprUser (PartWay 0)) = Pretty.text "..."
pprDeeper d (PprUser (PartWay n)) = d (PprUser (PartWay (n-1)))
pprDeeper d other_sty             = d other_sty

getPprStyle :: (PprStyle -> SDoc) -> SDoc
getPprStyle df sty = df sty sty
\end{code}

\begin{code}
codeStyle :: PprStyle -> Bool
codeStyle (PprCode _)	  = True
codeStyle _		  = False

asmStyle :: PprStyle -> Bool
asmStyle (PprCode AsmStyle)  = True
asmStyle other               = False

ifaceStyle :: PprStyle -> Bool
ifaceStyle PprInterface	  = True
ifaceStyle other	  = False

debugStyle :: PprStyle -> Bool
debugStyle PprDebug	  = True
debugStyle other	  = False

userStyle ::  PprStyle -> Bool
userStyle (PprUser _) = True
userStyle other       = False
\end{code}

\begin{code}
ifNotPprForUser :: SDoc -> SDoc	-- Returns empty document for User style
ifNotPprForUser d sty@(PprUser _) = Pretty.empty
ifNotPprForUser d sty             = d sty

ifPprDebug :: SDoc -> SDoc	  -- Empty for non-debug style
ifPprDebug d sty@PprDebug = d sty
ifPprDebug d sty	  = Pretty.empty
\end{code}

\begin{code}
printSDoc :: SDoc -> PprStyle -> IO ()
printSDoc d sty = printDoc PageMode stdout (d sty)

-- I'm not sure whether the direct-IO approach of printDoc
-- above is better or worse than the put-big-string approach here
printErrs :: SDoc -> IO ()
printErrs doc = printDoc PageMode stderr (final_doc user_style)
	      where
		final_doc = doc 	-- $$ text ""
		user_style = mkUserStyle (PartWay opt_PprUserLength)

printDump :: SDoc -> IO ()
printDump doc = printForUser stderr (doc $$ text "")
		-- We used to always print in debug style, but I want
		-- to try the effect of a more user-ish style (unless you
		-- say -dppr-debug

printForUser :: Handle -> SDoc -> IO ()
printForUser handle doc = printDoc PageMode handle (doc (mkUserStyle AllTheWay))

-- printForC, printForAsm do what they sound like
printForC :: Handle -> SDoc -> IO ()
printForC handle doc = printDoc LeftMode handle (doc (PprCode CStyle))

printForAsm :: Handle -> SDoc -> IO ()
printForAsm handle doc = printDoc LeftMode handle (doc (PprCode AsmStyle))

-- printForIface prints all on one line for interface files.
-- It's called repeatedly for successive lines
printForIface :: Handle -> SDoc -> IO ()
printForIface handle doc = printDoc OneLineMode handle (doc PprInterface)

pprCode :: CodeStyle -> SDoc -> SDoc
pprCode cs d = withPprStyle (PprCode cs) d

-- Can't make SDoc an instance of Show because SDoc is just a function type
-- However, Doc *is* an instance of Show
-- showSDoc just blasts it out as a string
showSDoc :: SDoc -> String
showSDoc d = show (d (mkUserStyle AllTheWay))

showSDocIface :: SDoc -> String
showSDocIface doc = showDocWith OneLineMode (doc PprInterface)

showSDocDebug :: SDoc -> String
showSDocDebug d = show (d PprDebug)

showsPrecSDoc :: Int -> SDoc -> ShowS
showsPrecSDoc p d = showsPrec p (d (mkUserStyle AllTheWay))

mkUserStyle depth |  opt_PprStyle_Debug = PprDebug
	          |  otherwise          = PprUser depth
\end{code}

\begin{code}
empty sty      = Pretty.empty
text s sty     = Pretty.text s
char c sty     = Pretty.char c
ptext s sty    = Pretty.ptext s
int n sty      = Pretty.int n
integer n sty  = Pretty.integer n
float n sty    = Pretty.float n
double n sty   = Pretty.double n
rational n sty = Pretty.rational n

parens d sty       = Pretty.parens (d sty)
braces d sty       = Pretty.braces (d sty)
brackets d sty     = Pretty.brackets (d sty)
doubleQuotes d sty = Pretty.doubleQuotes (d sty)
angleBrackets d    = char '<' <> d <> char '>'

-- quotes encloses something in single quotes...
-- but it omits them if the thing ends in a single quote
-- so that we don't get `foo''.  Instead we just have foo'.
quotes d sty = case show pp_d of
		 ('\'' : _) -> pp_d
		 other	    -> Pretty.quotes pp_d
	     where
	       pp_d = d sty

semi sty   = Pretty.semi
comma sty  = Pretty.comma
colon sty  = Pretty.colon
equals sty = Pretty.equals
space sty  = Pretty.space
lparen sty = Pretty.lparen
rparen sty = Pretty.rparen
lbrack sty = Pretty.lbrack
rbrack sty = Pretty.rbrack
lbrace sty = Pretty.lbrace
rbrace sty = Pretty.rbrace
dcolon sty = Pretty.ptext SLIT("::")
underscore = char '_'
dot	   = char '.'

nest n d sty    = Pretty.nest n (d sty)
(<>) d1 d2 sty  = (Pretty.<>)  (d1 sty) (d2 sty)
(<+>) d1 d2 sty = (Pretty.<+>) (d1 sty) (d2 sty)
($$) d1 d2 sty  = (Pretty.$$)  (d1 sty) (d2 sty)
($+$) d1 d2 sty = (Pretty.$+$) (d1 sty) (d2 sty)

hcat ds sty = Pretty.hcat [d sty | d <- ds]
hsep ds sty = Pretty.hsep [d sty | d <- ds]
vcat ds sty = Pretty.vcat [d sty | d <- ds]
sep ds sty  = Pretty.sep  [d sty | d <- ds]
cat ds sty  = Pretty.cat  [d sty | d <- ds]
fsep ds sty = Pretty.fsep [d sty | d <- ds]
fcat ds sty = Pretty.fcat [d sty | d <- ds]

hang d1 n d2 sty   = Pretty.hang (d1 sty) n (d2 sty)

punctuate :: SDoc -> [SDoc] -> [SDoc]
punctuate p []     = []
punctuate p (d:ds) = go d ds
		   where
		     go d [] = [d]
		     go d (e:es) = (d <> p) : go e es
\end{code}


%************************************************************************
%*									*
\subsection[Outputable-class]{The @Outputable@ class}
%*									*
%************************************************************************

\begin{code}
class Outputable a where
	ppr :: a -> SDoc
\end{code}

\begin{code}
instance Outputable Bool where
    ppr True  = ptext SLIT("True")
    ppr False = ptext SLIT("False")

instance Outputable Int where
   ppr n = int n

instance (Outputable a) => Outputable [a] where
    ppr xs = brackets (fsep (punctuate comma (map ppr xs)))

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr (x,y) = parens (sep [ppr x <> comma, ppr y])

instance Outputable a => Outputable (Maybe a) where
  ppr Nothing = text "Nothing"
  ppr (Just x) = text "Just" <+> ppr x

-- ToDo: may not be used
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    ppr (x,y,z) =
      parens (sep [ppr x <> comma,
		   ppr y <> comma,
		   ppr z ])

instance (Outputable a, Outputable b, Outputable c, Outputable d) =>
	 Outputable (a, b, c, d) where
    ppr (x,y,z,w) =
      parens (sep [ppr x <> comma,
		   ppr y <> comma,
		   ppr z <> comma,
		   ppr w])

instance Outputable FastString where
    ppr fs = text (unpackFS fs)		-- Prints an unadorned string,
					-- no double quotes or anything

pprFSAsString :: FastString -> SDoc			-- The Char instance of Show prints
pprFSAsString fs = text (showList (unpackFS fs) "")	-- strings with double quotes and escapes

instance Show FastString  where
    showsPrec p fs = showsPrecSDoc p (ppr fs)
\end{code}


%************************************************************************
%*									*
\subsection{Other helper functions}
%*									*
%************************************************************************

\begin{code}
pprCols = (100 :: Int) -- could make configurable

printDoc :: Mode -> Handle -> Doc -> IO ()
printDoc mode hdl doc
  = fullRender mode pprCols 1.5 put done doc
  where
    put (Chr c)  next = hPutChar hdl c >> next 
    put (Str s)  next = hPutStr  hdl s >> next 
    put (PStr s) next = hPutFS   hdl s >> next 

    done = hPutChar hdl '\n'

showDocWith :: Mode -> Doc -> String
showDocWith mode doc
  = fullRender PageMode 100 1.5 put "" doc
  where
    put (Chr c)   s  = c:s
    put (Str s1)  s2 = s1 ++ s2
    put (PStr s1) s2 = _UNPK_ s1 ++ s2
\end{code}


\begin{code}
pprWithCommas :: (a -> SDoc) -> [a] -> SDoc
pprWithCommas pp xs = hsep (punctuate comma (map pp xs))

interppSP  :: Outputable a => [a] -> SDoc
interppSP  xs = hsep (map ppr xs)

interpp'SP :: Outputable a => [a] -> SDoc
interpp'SP xs = hsep (punctuate comma (map ppr xs))

pprQuotedList :: Outputable a => [a] -> SDoc
-- [x,y,z]  ==>  `x', `y', `z'
pprQuotedList xs = hsep (punctuate comma (map (quotes . ppr) xs))
\end{code}


%************************************************************************
%*									*
\subsection{Printing numbers verbally}
%*									*
%************************************************************************

@speakNth@ converts an integer to a verbal index; eg 1 maps to
``first'' etc.

\begin{code}
speakNth :: Int -> SDoc

speakNth 1 = ptext SLIT("first")
speakNth 2 = ptext SLIT("second")
speakNth 3 = ptext SLIT("third")
speakNth 4 = ptext SLIT("fourth")
speakNth 5 = ptext SLIT("fifth")
speakNth 6 = ptext SLIT("sixth")
speakNth n = hcat [ int n, text st_nd_rd_th ]
  where
    st_nd_rd_th | n_rem_10 == 1 = "st"
		| n_rem_10 == 2 = "nd"
		| n_rem_10 == 3 = "rd"
		| otherwise     = "th"

    n_rem_10 = n `rem` 10
\end{code}

\begin{code}
speakNTimes :: Int {- >=1 -} -> SDoc
speakNTimes t | t == 1 	   = ptext SLIT("once")
              | t == 2 	   = ptext SLIT("twice")
              | otherwise  = int t <+> ptext SLIT("times")
\end{code}


%************************************************************************
%*									*
\subsection{Error handling}
%*									*
%************************************************************************

\begin{code}
pprPanic :: String -> SDoc -> a
pprError :: String -> SDoc -> a
pprTrace :: String -> SDoc -> a -> a
pprPanic  = pprAndThen panic
pprError  = pprAndThen error
pprTrace  = pprAndThen trace

pprPanic# heading pretty_msg = panic# (show (doc PprDebug))
			     where
			       doc = text heading <+> pretty_msg

pprAndThen :: (String -> a) -> String -> SDoc -> a
pprAndThen cont heading pretty_msg = cont (show (doc PprDebug))
    where
     doc = sep [text heading, nest 4 pretty_msg]

assertPprPanic :: String -> Int -> SDoc -> a
assertPprPanic file line msg
  = panic (show (doc PprDebug))
  where
    doc = sep [hsep[text "ASSERT failed! file", 
		 	   text file, 
			   text "line", int line], 
		    msg]

warnPprTrace :: Bool -> String -> Int -> SDoc -> a -> a
warnPprTrace False file line msg x = x
warnPprTrace True  file line msg x
  = trace (show (doc PprDebug)) x
  where
    doc = sep [hsep [text "WARNING: file", text file, text "line", int line],
	       msg]
\end{code}
