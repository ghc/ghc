%
% (c) The GRASP Project, Glasgow University, 1992-1998
%
\section[Outputable]{Classes for pretty-printing}

Defines classes for pretty-printing and forcing, both forms of
``output.''

\begin{code}

module Outputable (
	Outputable(..), OutputableBndr(..),	-- Class

	BindingSite(..),

	PprStyle, CodeStyle(..), PrintUnqualified, alwaysQualify,
	getPprStyle, withPprStyle, withPprStyleDoc, pprDeeper,
	codeStyle, userStyle, debugStyle, dumpStyle, asmStyle,
	ifPprDebug, unqualStyle, 
	mkErrStyle, defaultErrStyle,

	SDoc, 		-- Abstract
	docToSDoc,
	interppSP, interpp'SP, pprQuotedList, pprWithCommas,
	empty, nest,
	text, char, ftext, ptext,
	int, integer, float, double, rational,
	parens, brackets, braces, quotes, doubleQuotes, angleBrackets,
	semi, comma, colon, dcolon, space, equals, dot, arrow,
	lparen, rparen, lbrack, rbrack, lbrace, rbrace, underscore,
	(<>), (<+>), hcat, hsep, 
	($$), ($+$), vcat, 
	sep, cat, 
	fsep, fcat, 
	hang, punctuate,
	speakNth, speakNTimes,

	printSDoc, printErrs, printDump,
	printForC, printForAsm, printForUser,
	pprCode, mkCodeStyle,
	showSDoc, showSDocForUser, showSDocDebug,
	showSDocUnqual, showsPrecSDoc,
	pprHsChar, pprHsString,

	-- error handling
	pprPanic, assertPprPanic, pprPanic#, pprPgmError, 
	pprTrace, warnPprTrace,
	trace, pgmError, panic, panic#, assertPanic
    ) where

#include "HsVersions.h"


import {-# SOURCE #-} 	Module( Module )
import {-# SOURCE #-} 	OccName( OccName )

import StaticFlags	( opt_PprStyle_Debug, opt_PprUserLength )
import PackageConfig	( PackageId, packageIdString )
import FastString
import qualified Pretty
import Pretty		( Doc, Mode(..) )
import Panic

import DATA_WORD	( Word32 )

import IO		( Handle, stderr, stdout, hFlush )
import Char             ( ord )
\end{code}


%************************************************************************
%*									*
\subsection{The @PprStyle@ data type}
%*									*
%************************************************************************

\begin{code}
data PprStyle
  = PprUser PrintUnqualified Depth
		-- Pretty-print in a way that will make sense to the
		-- ordinary user; must be very close to Haskell
		-- syntax, etc.
		-- Assumes printing tidied code: non-system names are
		-- printed without uniques.

  | PprCode CodeStyle
		-- Print code; either C or assembler

  | PprDump	-- For -ddump-foo; less verbose than PprDebug.
		-- Does not assume tidied code: non-external names
		-- are printed with uniques.

  | PprDebug	-- Full debugging output

data CodeStyle = CStyle		-- The format of labels differs for C and assembler
	       | AsmStyle

data Depth = AllTheWay
           | PartWay Int	-- 0 => stop


type PrintUnqualified = Module -> OccName -> Bool
	-- This function tells when it's ok to print 
	-- a (Global) name unqualified

alwaysQualify,neverQualify :: PrintUnqualified
alwaysQualify m n = False
neverQualify  m n = True

defaultUserStyle = mkUserStyle alwaysQualify AllTheWay

defaultDumpStyle |  opt_PprStyle_Debug = PprDebug
		 |  otherwise          = PprDump

mkErrStyle :: PrintUnqualified -> PprStyle
-- Style for printing error messages
mkErrStyle print_unqual = mkUserStyle print_unqual (PartWay opt_PprUserLength)

defaultErrStyle :: PprStyle
-- Default style for error messages
-- It's a bit of a hack because it doesn't take into account what's in scope
-- Only used for desugarer warnings, and typechecker errors in interface sigs
defaultErrStyle 
  | opt_PprStyle_Debug = mkUserStyle alwaysQualify AllTheWay
  | otherwise	       = mkUserStyle alwaysQualify  (PartWay opt_PprUserLength)

mkUserStyle unqual depth |  opt_PprStyle_Debug = PprDebug
	          	 |  otherwise          = PprUser unqual depth
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

withPprStyleDoc :: PprStyle -> SDoc -> Doc
withPprStyleDoc sty d = d sty

pprDeeper :: SDoc -> SDoc
pprDeeper d (PprUser unqual (PartWay 0)) = Pretty.text "..."
pprDeeper d (PprUser unqual (PartWay n)) = d (PprUser unqual (PartWay (n-1)))
pprDeeper d other_sty        		 = d other_sty

getPprStyle :: (PprStyle -> SDoc) -> SDoc
getPprStyle df sty = df sty sty
\end{code}

\begin{code}
unqualStyle :: PprStyle -> PrintUnqualified
unqualStyle (PprUser    unqual _) m n = unqual m n
unqualStyle other		  m n = False

codeStyle :: PprStyle -> Bool
codeStyle (PprCode _)	  = True
codeStyle _		  = False

asmStyle :: PprStyle -> Bool
asmStyle (PprCode AsmStyle)  = True
asmStyle other               = False

dumpStyle :: PprStyle -> Bool
dumpStyle PprDump = True
dumpStyle other   = False

debugStyle :: PprStyle -> Bool
debugStyle PprDebug	  = True
debugStyle other	  = False

userStyle ::  PprStyle -> Bool
userStyle (PprUser _ _) = True
userStyle other         = False

ifPprDebug :: SDoc -> SDoc	  -- Empty for non-debug style
ifPprDebug d sty@PprDebug = d sty
ifPprDebug d sty	  = Pretty.empty
\end{code}

\begin{code}
-- Unused [7/02 sof]
printSDoc :: SDoc -> PprStyle -> IO ()
printSDoc d sty = do
  Pretty.printDoc PageMode stdout (d sty)
  hFlush stdout

-- I'm not sure whether the direct-IO approach of Pretty.printDoc
-- above is better or worse than the put-big-string approach here
printErrs :: Doc -> IO ()
printErrs doc = do Pretty.printDoc PageMode stderr doc
		   hFlush stderr

printDump :: SDoc -> IO ()
printDump doc = do
   Pretty.printDoc PageMode stdout (better_doc defaultDumpStyle)
   hFlush stdout
 where
   better_doc = doc $$ text ""
    -- We used to always print in debug style, but I want
    -- to try the effect of a more user-ish style (unless you
    -- say -dppr-debug)

printForUser :: Handle -> PrintUnqualified -> SDoc -> IO ()
printForUser handle unqual doc 
  = Pretty.printDoc PageMode handle (doc (mkUserStyle unqual AllTheWay))

-- printForC, printForAsm do what they sound like
printForC :: Handle -> SDoc -> IO ()
printForC handle doc = Pretty.printDoc LeftMode handle (doc (PprCode CStyle))

printForAsm :: Handle -> SDoc -> IO ()
printForAsm handle doc = Pretty.printDoc LeftMode handle (doc (PprCode AsmStyle))

pprCode :: CodeStyle -> SDoc -> SDoc
pprCode cs d = withPprStyle (PprCode cs) d

mkCodeStyle :: CodeStyle -> PprStyle
mkCodeStyle = PprCode

-- Can't make SDoc an instance of Show because SDoc is just a function type
-- However, Doc *is* an instance of Show
-- showSDoc just blasts it out as a string
showSDoc :: SDoc -> String
showSDoc d = show (d defaultUserStyle)

showSDocForUser :: PrintUnqualified -> SDoc -> String
showSDocForUser unqual doc = show (doc (mkUserStyle unqual AllTheWay))

showSDocUnqual :: SDoc -> String
-- Only used in the gruesome HsExpr.isOperator
showSDocUnqual d = show (d (mkUserStyle neverQualify AllTheWay))

showsPrecSDoc :: Int -> SDoc -> ShowS
showsPrecSDoc p d = showsPrec p (d defaultUserStyle)

showSDocDebug :: SDoc -> String
showSDocDebug d = show (d PprDebug)
\end{code}

\begin{code}
docToSDoc :: Doc -> SDoc
docToSDoc d = \_ -> d

empty sty      = Pretty.empty
text s sty     = Pretty.text s
char c sty     = Pretty.char c
ftext s sty    = Pretty.ftext s
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
arrow  sty = Pretty.ptext SLIT("->")
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

instance Outputable () where
   ppr _ = text "()"

instance (Outputable a) => Outputable [a] where
    ppr xs = brackets (fsep (punctuate comma (map ppr xs)))

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr (x,y) = parens (sep [ppr x <> comma, ppr y])

instance Outputable a => Outputable (Maybe a) where
  ppr Nothing = ptext SLIT("Nothing")
  ppr (Just x) = ptext SLIT("Just") <+> ppr x

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

instance Outputable PackageId where
   ppr pid = text (packageIdString pid)
\end{code}


%************************************************************************
%*									*
\subsection{The @OutputableBndr@ class}
%*									*
%************************************************************************

When we print a binder, we often want to print its type too.
The @OutputableBndr@ class encapsulates this idea.

@BindingSite@ is used to tell the thing that prints binder what
language construct is binding the identifier.  This can be used
to decide how much info to print.

\begin{code}
data BindingSite = LambdaBind | CaseBind | LetBind

class Outputable a => OutputableBndr a where
   pprBndr :: BindingSite -> a -> SDoc
   pprBndr b x = ppr x
\end{code}



%************************************************************************
%*									*
\subsection{Random printing helpers}
%*									*
%************************************************************************

\begin{code}
-- We have 31-bit Chars and will simply use Show instances
-- of Char and String.

pprHsChar :: Char -> SDoc
pprHsChar c | c > '\x10ffff' = char '\\' <> text (show (fromIntegral (ord c) :: Word32))
            | otherwise      = text (show c)

pprHsString :: FastString -> SDoc
pprHsString fs = text (show (unpackFS fs))

instance Show FastString  where
    showsPrec p fs = showsPrecSDoc p (ppr fs)
\end{code}


%************************************************************************
%*									*
\subsection{Other helper functions}
%*									*
%************************************************************************

\begin{code}
pprWithCommas :: (a -> SDoc) -> [a] -> SDoc
pprWithCommas pp xs = fsep (punctuate comma (map pp xs))

interppSP  :: Outputable a => [a] -> SDoc
interppSP  xs = sep (map ppr xs)

interpp'SP :: Outputable a => [a] -> SDoc
interpp'SP xs = sep (punctuate comma (map ppr xs))

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
speakNth n = hcat [ int n, text suffix ]
  where
    suffix | n <= 20       = "th"	-- 11,12,13 are non-std
	   | last_dig == 1 = "st"
	   | last_dig == 2 = "nd"
	   | last_dig == 3 = "rd"
	   | otherwise     = "th"

    last_dig = n `rem` 10
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
pprPanic, pprPgmError :: String -> SDoc -> a
pprTrace :: String -> SDoc -> a -> a
pprPanic    = pprAndThen panic		-- Throw an exn saying "bug in GHC"

pprPgmError = pprAndThen pgmError	-- Throw an exn saying "bug in pgm being compiled"
					--	(used for unusual pgm errors)
pprTrace    = pprAndThen trace

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
