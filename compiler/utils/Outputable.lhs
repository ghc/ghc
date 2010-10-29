%
% (c) The University of Glasgow 2006
% (c) The GRASP Project, Glasgow University, 1992-1998
%

\begin{code}
-- | This module defines classes and functions for pretty-printing. It also
-- exports a number of helpful debugging and other utilities such as 'trace' and 'panic'.
--
-- The interface to this module is very similar to the standard Hughes-PJ pretty printing
-- module, except that it exports a number of additional functions that are rarely used,
-- and works over the 'SDoc' type.
module Outputable (
	-- * Type classes
	Outputable(..), OutputableBndr(..),

        -- * Pretty printing combinators
	SDoc,
	docToSDoc,
	interppSP, interpp'SP, pprQuotedList, pprWithCommas, quotedListWithOr,
	empty, nest,
	char,
	text, ftext, ptext,
	int, integer, float, double, rational,
	parens, cparen, brackets, braces, quotes, doubleQuotes, angleBrackets,
	semi, comma, colon, dcolon, space, equals, dot, arrow, darrow,
	lparen, rparen, lbrack, rbrack, lbrace, rbrace, underscore,
	blankLine,
	(<>), (<+>), hcat, hsep, 
	($$), ($+$), vcat,
	sep, cat, 
	fsep, fcat, 
	hang, punctuate, ppWhen, ppUnless,
	speakNth, speakNTimes, speakN, speakNOf, plural,

        -- * Converting 'SDoc' into strings and outputing it
	printSDoc, printErrs, printOutput, hPrintDump, printDump,
	printForC, printForAsm, printForUser, printForUserPartWay,
	pprCode, mkCodeStyle,
	showSDoc, showSDocOneLine,
        showSDocForUser, showSDocDebug, showSDocDump, showSDocDumpOneLine,
        showPpr,
	showSDocUnqual, showsPrecSDoc,

	pprInfixVar, pprPrefixVar,
	pprHsChar, pprHsString, pprHsInfix, pprHsVar,
        pprFastFilePath,

        -- * Controlling the style in which output is printed
	BindingSite(..),

	PprStyle, CodeStyle(..), PrintUnqualified, alwaysQualify, neverQualify,
        QualifyName(..),
	getPprStyle, withPprStyle, withPprStyleDoc, 
	pprDeeper, pprDeeperList, pprSetDepth,
	codeStyle, userStyle, debugStyle, dumpStyle, asmStyle,
	ifPprDebug, qualName, qualModule,
	mkErrStyle, defaultErrStyle, defaultDumpStyle, defaultUserStyle,
        mkUserStyle, cmdlineParserStyle, Depth(..),
	
	-- * Error handling and debugging utilities
	pprPanic, pprSorry, assertPprPanic, pprPanicFastInt, pprPgmError, 
	pprTrace, warnPprTrace,
	trace, pgmError, panic, sorry, panicFastInt, assertPanic
    ) where

import {-# SOURCE #-} 	Module( Module, ModuleName, moduleName )
import {-# SOURCE #-} 	OccName( OccName )

import StaticFlags
import FastString 
import FastTypes
import qualified Pretty
import Pretty		( Doc, Mode(..) )
import Panic

import Data.Char
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Word
import System.IO	( Handle, stderr, stdout, hFlush )
import System.FilePath


#if __GLASGOW_HASKELL__ >= 700
import GHC.Show		( showMultiLineString )
#else
showMultiLineString :: String -> [String]
-- Crude version
showMultiLineString s = [s]
#endif
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


-- -----------------------------------------------------------------------------
-- Printing original names

-- When printing code that contains original names, we need to map the
-- original names back to something the user understands.  This is the
-- purpose of the pair of functions that gets passed around
-- when rendering 'SDoc'.

-- | given an /original/ name, this function tells you which module
-- name it should be qualified with when printing for the user, if
-- any.  For example, given @Control.Exception.catch@, which is in scope
-- as @Exception.catch@, this fuction will return @Just "Exception"@.
-- Note that the return value is a ModuleName, not a Module, because
-- in source code, names are qualified by ModuleNames.
type QueryQualifyName = Module -> OccName -> QualifyName

-- See Note [Printing original names] in HscTypes
data QualifyName                        -- given P:M.T
        = NameUnqual                    -- refer to it as "T"
        | NameQual ModuleName           -- refer to it as "X.T" for the supplied X
        | NameNotInScope1               
                -- it is not in scope at all, but M.T is not bound in the current
                -- scope, so we can refer to it as "M.T"
        | NameNotInScope2
                -- it is not in scope at all, and M.T is already bound in the
                -- current scope, so we must refer to it as "P:M.T"


-- | For a given module, we need to know whether to print it with
-- a package name to disambiguate it.
type QueryQualifyModule = Module -> Bool

type PrintUnqualified = (QueryQualifyName, QueryQualifyModule)

alwaysQualifyNames :: QueryQualifyName
alwaysQualifyNames m _ = NameQual (moduleName m)

neverQualifyNames :: QueryQualifyName
neverQualifyNames _ _ = NameUnqual

alwaysQualifyModules :: QueryQualifyModule
alwaysQualifyModules _ = True

neverQualifyModules :: QueryQualifyModule
neverQualifyModules _ = False

alwaysQualify, neverQualify :: PrintUnqualified
alwaysQualify = (alwaysQualifyNames, alwaysQualifyModules)
neverQualify  = (neverQualifyNames,  neverQualifyModules)

defaultUserStyle, defaultDumpStyle :: PprStyle

defaultUserStyle = mkUserStyle alwaysQualify AllTheWay

defaultDumpStyle |  opt_PprStyle_Debug = PprDebug
		 |  otherwise          = PprDump

-- | Style for printing error messages
mkErrStyle :: PrintUnqualified -> PprStyle
mkErrStyle qual = mkUserStyle qual (PartWay opt_PprUserLength)

defaultErrStyle :: PprStyle
-- Default style for error messages
-- It's a bit of a hack because it doesn't take into account what's in scope
-- Only used for desugarer warnings, and typechecker errors in interface sigs
defaultErrStyle 
  | opt_PprStyle_Debug   = mkUserStyle alwaysQualify AllTheWay
  | otherwise            = mkUserStyle alwaysQualify (PartWay opt_PprUserLength)

mkUserStyle :: PrintUnqualified -> Depth -> PprStyle
mkUserStyle unqual depth
   | opt_PprStyle_Debug = PprDebug
   | otherwise          = PprUser unqual depth

cmdlineParserStyle :: PprStyle
cmdlineParserStyle = PprUser alwaysQualify AllTheWay
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
withPprStyle sty d _sty' = d sty

withPprStyleDoc :: PprStyle -> SDoc -> Doc
withPprStyleDoc sty d = d sty

pprDeeper :: SDoc -> SDoc
pprDeeper _ (PprUser _ (PartWay 0)) = Pretty.text "..."
pprDeeper d (PprUser q (PartWay n)) = d (PprUser q (PartWay (n-1)))
pprDeeper d other_sty        	    = d other_sty

pprDeeperList :: ([SDoc] -> SDoc) -> [SDoc] -> SDoc
-- Truncate a list that list that is longer than the current depth
pprDeeperList f ds (PprUser q (PartWay n))
  | n==0      = Pretty.text "..."
  | otherwise = f (go 0 ds) (PprUser q (PartWay (n-1)))
  where
    go _ [] = []
    go i (d:ds) | i >= n    = [text "...."]
		| otherwise = d : go (i+1) ds

pprDeeperList f ds other_sty
  = f ds other_sty

pprSetDepth :: Depth -> SDoc -> SDoc
pprSetDepth depth  doc (PprUser q _) = doc (PprUser q depth)
pprSetDepth _depth doc other_sty     = doc other_sty

getPprStyle :: (PprStyle -> SDoc) -> SDoc
getPprStyle df sty = df sty sty
\end{code}

\begin{code}
qualName :: PprStyle -> QueryQualifyName
qualName (PprUser (qual_name,_) _) m  n = qual_name m n
qualName _other		           m _n = NameQual (moduleName m)

qualModule :: PprStyle -> QueryQualifyModule
qualModule (PprUser (_,qual_mod) _)  m = qual_mod m
qualModule _other                   _m = True

codeStyle :: PprStyle -> Bool
codeStyle (PprCode _)	  = True
codeStyle _		  = False

asmStyle :: PprStyle -> Bool
asmStyle (PprCode AsmStyle)  = True
asmStyle _other              = False

dumpStyle :: PprStyle -> Bool
dumpStyle PprDump = True
dumpStyle _other  = False

debugStyle :: PprStyle -> Bool
debugStyle PprDebug	  = True
debugStyle _other	  = False

userStyle ::  PprStyle -> Bool
userStyle (PprUser _ _) = True
userStyle _other        = False

ifPprDebug :: SDoc -> SDoc	  -- Empty for non-debug style
ifPprDebug d sty@PprDebug = d sty
ifPprDebug _ _  	  = Pretty.empty
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

printOutput :: Doc -> IO ()
printOutput doc = Pretty.printDoc PageMode stdout doc

printDump :: SDoc -> IO ()
printDump doc = hPrintDump stdout doc

hPrintDump :: Handle -> SDoc -> IO ()
hPrintDump h doc = do
   Pretty.printDoc PageMode h (better_doc defaultDumpStyle)
   hFlush h
 where
   better_doc = doc $$ blankLine

printForUser :: Handle -> PrintUnqualified -> SDoc -> IO ()
printForUser handle unqual doc 
  = Pretty.printDoc PageMode handle (doc (mkUserStyle unqual AllTheWay))

printForUserPartWay :: Handle -> Int -> PrintUnqualified -> SDoc -> IO ()
printForUserPartWay handle d unqual doc
  = Pretty.printDoc PageMode handle (doc (mkUserStyle unqual (PartWay d)))

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
showSDoc d = Pretty.showDocWith PageMode (d defaultUserStyle)

-- This shows an SDoc, but on one line only. It's cheaper than a full
-- showSDoc, designed for when we're getting results like "Foo.bar"
-- and "foo{uniq strictness}" so we don't want fancy layout anyway.
showSDocOneLine :: SDoc -> String
showSDocOneLine d = Pretty.showDocWith PageMode (d defaultUserStyle)

showSDocForUser :: PrintUnqualified -> SDoc -> String
showSDocForUser unqual doc = show (doc (mkUserStyle unqual AllTheWay))

showSDocUnqual :: SDoc -> String
-- Only used in the gruesome isOperator
showSDocUnqual d = show (d (mkUserStyle neverQualify AllTheWay))

showsPrecSDoc :: Int -> SDoc -> ShowS
showsPrecSDoc p d = showsPrec p (d defaultUserStyle)

showSDocDump :: SDoc -> String
showSDocDump d = Pretty.showDocWith PageMode (d PprDump)

showSDocDumpOneLine :: SDoc -> String
showSDocDumpOneLine d = Pretty.showDocWith OneLineMode (d PprDump)

showSDocDebug :: SDoc -> String
showSDocDebug d = show (d PprDebug)

showPpr :: Outputable a => a -> String
showPpr = showSDoc . ppr
\end{code}

\begin{code}
docToSDoc :: Doc -> SDoc
docToSDoc d = \_ -> d

empty    :: SDoc
char     :: Char       -> SDoc
text     :: String     -> SDoc
ftext    :: FastString -> SDoc
ptext    :: LitString  -> SDoc
int      :: Int        -> SDoc
integer  :: Integer    -> SDoc
float    :: Float      -> SDoc
double   :: Double     -> SDoc
rational :: Rational   -> SDoc

empty _sty      = Pretty.empty
char c _sty     = Pretty.char c
text s _sty     = Pretty.text s
ftext s _sty    = Pretty.ftext s
ptext s _sty    = Pretty.ptext s
int n _sty      = Pretty.int n
integer n _sty  = Pretty.integer n
float n _sty    = Pretty.float n
double n _sty   = Pretty.double n
rational n _sty = Pretty.rational n

parens, braces, brackets, quotes, doubleQuotes, angleBrackets :: SDoc -> SDoc

parens d sty       = Pretty.parens (d sty)
braces d sty       = Pretty.braces (d sty)
brackets d sty     = Pretty.brackets (d sty)
doubleQuotes d sty = Pretty.doubleQuotes (d sty)
angleBrackets d    = char '<' <> d <> char '>'

cparen :: Bool -> SDoc -> SDoc

cparen b d sty       = Pretty.cparen b (d sty)

-- quotes encloses something in single quotes...
-- but it omits them if the thing ends in a single quote
-- so that we don't get `foo''.  Instead we just have foo'.
quotes d sty = case show pp_d of
		 ('\'' : _) -> pp_d
		 _other	    -> Pretty.quotes pp_d
	     where
	       pp_d = d sty

semi, comma, colon, equals, space, dcolon, arrow, underscore, dot :: SDoc
darrow, lparen, rparen, lbrack, rbrack, lbrace, rbrace, blankLine :: SDoc

blankLine _sty = Pretty.ptext (sLit "")
dcolon _sty    = Pretty.ptext (sLit "::")
arrow  _sty    = Pretty.ptext (sLit "->")
darrow _sty    = Pretty.ptext (sLit "=>")
semi _sty      = Pretty.semi
comma _sty     = Pretty.comma
colon _sty     = Pretty.colon
equals _sty    = Pretty.equals
space _sty     = Pretty.space
underscore     = char '_'
dot	       = char '.'
lparen _sty    = Pretty.lparen
rparen _sty    = Pretty.rparen
lbrack _sty    = Pretty.lbrack
rbrack _sty    = Pretty.rbrack
lbrace _sty    = Pretty.lbrace
rbrace _sty    = Pretty.rbrace

nest :: Int -> SDoc -> SDoc
-- ^ Indent 'SDoc' some specified amount
(<>) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together horizontally without a gap
(<+>) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together horizontally with a gap between them
($$) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together vertically; if there is 
-- no vertical overlap it "dovetails" the two onto one line
($+$) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together vertically

nest n d sty    = Pretty.nest n (d sty)
(<>) d1 d2 sty  = (Pretty.<>)  (d1 sty) (d2 sty)
(<+>) d1 d2 sty = (Pretty.<+>) (d1 sty) (d2 sty)
($$) d1 d2 sty  = (Pretty.$$)  (d1 sty) (d2 sty)
($+$) d1 d2 sty = (Pretty.$+$) (d1 sty) (d2 sty)

hcat :: [SDoc] -> SDoc
-- ^ Concatenate 'SDoc' horizontally
hsep :: [SDoc] -> SDoc
-- ^ Concatenate 'SDoc' horizontally with a space between each one
vcat :: [SDoc] -> SDoc
-- ^ Concatenate 'SDoc' vertically with dovetailing
sep :: [SDoc] -> SDoc
-- ^ Separate: is either like 'hsep' or like 'vcat', depending on what fits
cat :: [SDoc] -> SDoc
-- ^ Catenate: is either like 'hcat' or like 'vcat', depending on what fits
fsep :: [SDoc] -> SDoc
-- ^ A paragraph-fill combinator. It's much like sep, only it
-- keeps fitting things on one line until it can't fit any more.
fcat :: [SDoc] -> SDoc
-- ^ This behaves like 'fsep', but it uses '<>' for horizontal conposition rather than '<+>'


hcat ds sty = Pretty.hcat [d sty | d <- ds]
hsep ds sty = Pretty.hsep [d sty | d <- ds]
vcat ds sty = Pretty.vcat [d sty | d <- ds]
sep ds sty  = Pretty.sep  [d sty | d <- ds]
cat ds sty  = Pretty.cat  [d sty | d <- ds]
fsep ds sty = Pretty.fsep [d sty | d <- ds]
fcat ds sty = Pretty.fcat [d sty | d <- ds]

hang :: SDoc  -- ^ The header
      -> Int  -- ^ Amount to indent the hung body
      -> SDoc -- ^ The hung body, indented and placed below the header
      -> SDoc
hang d1 n d2 sty   = Pretty.hang (d1 sty) n (d2 sty)

punctuate :: SDoc   -- ^ The punctuation
          -> [SDoc] -- ^ The list that will have punctuation added between every adjacent pair of elements
          -> [SDoc] -- ^ Punctuated list
punctuate _ []     = []
punctuate p (d:ds) = go d ds
		   where
		     go d [] = [d]
		     go d (e:es) = (d <> p) : go e es

ppWhen, ppUnless :: Bool -> SDoc -> SDoc
ppWhen True  doc = doc
ppWhen False _   = empty

ppUnless True  _   = empty
ppUnless False doc = doc
\end{code}


%************************************************************************
%*									*
\subsection[Outputable-class]{The @Outputable@ class}
%*									*
%************************************************************************

\begin{code}
-- | Class designating that some type has an 'SDoc' representation
class Outputable a where
	ppr :: a -> SDoc
\end{code}

\begin{code}
instance Outputable Bool where
    ppr True  = ptext (sLit "True")
    ppr False = ptext (sLit "False")

instance Outputable Int where
   ppr n = int n

instance Outputable Word16 where
   ppr n = integer $ fromIntegral n

instance Outputable Word32 where
   ppr n = integer $ fromIntegral n

instance Outputable Word where
   ppr n = integer $ fromIntegral n

instance Outputable () where
   ppr _ = text "()"

instance (Outputable a) => Outputable [a] where
    ppr xs = brackets (fsep (punctuate comma (map ppr xs)))

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr (x,y) = parens (sep [ppr x <> comma, ppr y])

instance Outputable a => Outputable (Maybe a) where
  ppr Nothing = ptext (sLit "Nothing")
  ppr (Just x) = ptext (sLit "Just") <+> ppr x

instance (Outputable a, Outputable b) => Outputable (Either a b) where
  ppr (Left x)  = ptext (sLit "Left")  <+> ppr x
  ppr (Right y) = ptext (sLit "Right") <+> ppr y

-- ToDo: may not be used
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    ppr (x,y,z) =
      parens (sep [ppr x <> comma,
		   ppr y <> comma,
		   ppr z ])

instance (Outputable a, Outputable b, Outputable c, Outputable d) =>
	 Outputable (a, b, c, d) where
    ppr (a,b,c,d) =
      parens (sep [ppr a <> comma,
		   ppr b <> comma,
		   ppr c <> comma,
		   ppr d])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e) =>
	 Outputable (a, b, c, d, e) where
    ppr (a,b,c,d,e) =
      parens (sep [ppr a <> comma,
		   ppr b <> comma,
		   ppr c <> comma,
		   ppr d <> comma,
		   ppr e])

instance Outputable FastString where
    ppr fs = ftext fs		-- Prints an unadorned string,
				-- no double quotes or anything

instance (Outputable key, Outputable elt) => Outputable (M.Map key elt) where
    ppr m = ppr (M.toList m)
instance (Outputable elt) => Outputable (IM.IntMap elt) where
    ppr m = ppr (IM.toList m)
\end{code}

%************************************************************************
%*									*
\subsection{The @OutputableBndr@ class}
%*									*
%************************************************************************

\begin{code}
-- | 'BindingSite' is used to tell the thing that prints binder what
-- language construct is binding the identifier.  This can be used
-- to decide how much info to print.
data BindingSite = LambdaBind | CaseBind | LetBind

-- | When we print a binder, we often want to print its type too.
-- The @OutputableBndr@ class encapsulates this idea.
class Outputable a => OutputableBndr a where
   pprBndr :: BindingSite -> a -> SDoc
   pprBndr _b x = ppr x
\end{code}

%************************************************************************
%*									*
\subsection{Random printing helpers}
%*									*
%************************************************************************

\begin{code}
-- We have 31-bit Chars and will simply use Show instances of Char and String.

-- | Special combinator for showing character literals.
pprHsChar :: Char -> SDoc
pprHsChar c | c > '\x10ffff' = char '\\' <> text (show (fromIntegral (ord c) :: Word32))
            | otherwise      = text (show c)

-- | Special combinator for showing string literals.
pprHsString :: FastString -> SDoc
pprHsString fs = vcat (map text (showMultiLineString (unpackFS fs)))

---------------------
-- Put a name in parens if it's an operator
pprPrefixVar :: Bool -> SDoc -> SDoc
pprPrefixVar is_operator pp_v
  | is_operator = parens pp_v
  | otherwise	= pp_v

-- Put a name in backquotes if it's not an operator
pprInfixVar :: Bool -> SDoc -> SDoc
pprInfixVar is_operator pp_v 
  | is_operator = pp_v
  | otherwise   = char '`' <> pp_v <> char '`'

---------------------
-- pprHsVar and pprHsInfix use the gruesome isOperator, which
-- in turn uses (showSDoc (ppr v)), rather than isSymOcc (getOccName v).
-- Reason: it means that pprHsVar doesn't need a NamedThing context,
--         which none of the HsSyn printing functions do
pprHsVar, pprHsInfix :: Outputable name => name -> SDoc
pprHsVar   v = pprPrefixVar (isOperator pp_v) pp_v  
	     where pp_v = ppr v
pprHsInfix v = pprInfixVar  (isOperator pp_v) pp_v
	     where pp_v = ppr v

isOperator :: SDoc -> Bool
isOperator ppr_v 
  = case showSDocUnqual ppr_v of
        ('(':_)   -> False              -- (), (,) etc
        ('[':_)   -> False              -- []
        ('$':c:_) -> not (isAlpha c)    -- Don't treat $d as an operator
        (':':c:_) -> not (isAlpha c)    -- Don't treat :T as an operator
        ('_':_)   -> False              -- Not an operator
        (c:_)     -> not (isAlpha c)    -- Starts with non-alpha
        _         -> False

pprFastFilePath :: FastString -> SDoc
pprFastFilePath path = text $ normalise $ unpackFS path
\end{code}

%************************************************************************
%*									*
\subsection{Other helper functions}
%*									*
%************************************************************************

\begin{code}
pprWithCommas :: (a -> SDoc) -- ^ The pretty printing function to use
              -> [a]         -- ^ The things to be pretty printed
              -> SDoc        -- ^ 'SDoc' where the things have been pretty printed,
                             -- comma-separated and finally packed into a paragraph.
pprWithCommas pp xs = fsep (punctuate comma (map pp xs))

-- | Returns the seperated concatenation of the pretty printed things.
interppSP  :: Outputable a => [a] -> SDoc
interppSP  xs = sep (map ppr xs)

-- | Returns the comma-seperated concatenation of the pretty printed things.
interpp'SP :: Outputable a => [a] -> SDoc
interpp'SP xs = sep (punctuate comma (map ppr xs))

-- | Returns the comma-seperated concatenation of the quoted pretty printed things.
--
-- > [x,y,z]  ==>  `x', `y', `z'
pprQuotedList :: Outputable a => [a] -> SDoc
pprQuotedList = quotedList . map ppr

quotedList :: [SDoc] -> SDoc
quotedList xs = hsep (punctuate comma (map quotes xs))

quotedListWithOr :: [SDoc] -> SDoc
-- [x,y,z]  ==>  `x', `y' or `z'
quotedListWithOr xs@(_:_:_) = quotedList (init xs) <+> ptext (sLit "or") <+> quotes (last xs)
quotedListWithOr xs = quotedList xs
\end{code}


%************************************************************************
%*									*
\subsection{Printing numbers verbally}
%*									*
%************************************************************************

\begin{code}
-- | Converts an integer to a verbal index:
--
-- > speakNth 1 = text "first"
-- > speakNth 5 = text "fifth"
-- > speakNth 21 = text "21st"
speakNth :: Int -> SDoc
speakNth 1 = ptext (sLit "first")
speakNth 2 = ptext (sLit "second")
speakNth 3 = ptext (sLit "third")
speakNth 4 = ptext (sLit "fourth")
speakNth 5 = ptext (sLit "fifth")
speakNth 6 = ptext (sLit "sixth")
speakNth n = hcat [ int n, text suffix ]
  where
    suffix | n <= 20       = "th"	-- 11,12,13 are non-std
	   | last_dig == 1 = "st"
	   | last_dig == 2 = "nd"
	   | last_dig == 3 = "rd"
	   | otherwise     = "th"

    last_dig = n `rem` 10

-- | Converts an integer to a verbal multiplicity:
-- 
-- > speakN 0 = text "none"
-- > speakN 5 = text "five"
-- > speakN 10 = text "10"
speakN :: Int -> SDoc
speakN 0 = ptext (sLit "none")	-- E.g.  "he has none"
speakN 1 = ptext (sLit "one")	-- E.g.  "he has one"
speakN 2 = ptext (sLit "two")
speakN 3 = ptext (sLit "three")
speakN 4 = ptext (sLit "four")
speakN 5 = ptext (sLit "five")
speakN 6 = ptext (sLit "six")
speakN n = int n

-- | Converts an integer and object description to a statement about the
-- multiplicity of those objects:
--
-- > speakNOf 0 (text "melon") = text "no melons"
-- > speakNOf 1 (text "melon") = text "one melon"
-- > speakNOf 3 (text "melon") = text "three melons"
speakNOf :: Int -> SDoc -> SDoc
speakNOf 0 d = ptext (sLit "no") <+> d <> char 's'
speakNOf 1 d = ptext (sLit "one") <+> d			-- E.g. "one argument"
speakNOf n d = speakN n <+> d <> char 's'		-- E.g. "three arguments"

-- | Converts a strictly positive integer into a number of times:
--
-- > speakNTimes 1 = text "once"
-- > speakNTimes 2 = text "twice"
-- > speakNTimes 4 = text "4 times"
speakNTimes :: Int {- >=1 -} -> SDoc
speakNTimes t | t == 1 	   = ptext (sLit "once")
              | t == 2 	   = ptext (sLit "twice")
              | otherwise  = speakN t <+> ptext (sLit "times")

-- | Determines the pluralisation suffix appropriate for the length of a list:
--
-- > plural [] = char 's'
-- > plural ["Hello"] = empty
-- > plural ["Hello", "World"] = char 's'
plural :: [a] -> SDoc
plural [_] = empty  -- a bit frightening, but there you are
plural _   = char 's'
\end{code}


%************************************************************************
%*									*
\subsection{Error handling}
%*									*
%************************************************************************

\begin{code}

pprPanic :: String -> SDoc -> a
-- ^ Throw an exception saying "bug in GHC"
pprPanic    = pprAndThen panic

pprSorry :: String -> SDoc -> a
-- ^ Throw an exceptio saying "this isn't finished yet"
pprSorry    = pprAndThen sorry


pprPgmError :: String -> SDoc -> a
-- ^ Throw an exception saying "bug in pgm being compiled" (used for unusual program errors)
pprPgmError = pprAndThen pgmError


pprTrace :: String -> SDoc -> a -> a
-- ^ If debug output is on, show some 'SDoc' on the screen
pprTrace str doc x
   | opt_NoDebugOutput = x
   | otherwise         = pprAndThen trace str doc x


pprPanicFastInt :: String -> SDoc -> FastInt
-- ^ Specialization of pprPanic that can be safely used with 'FastInt'
pprPanicFastInt heading pretty_msg = panicFastInt (show (doc PprDebug))
			     where
			       doc = text heading <+> pretty_msg


pprAndThen :: (String -> a) -> String -> SDoc -> a
pprAndThen cont heading pretty_msg = cont (show (doc PprDebug))
    where
     doc = sep [text heading, nest 4 pretty_msg]

assertPprPanic :: String -> Int -> SDoc -> a
-- ^ Panic with an assertation failure, recording the given file and line number.
-- Should typically be accessed with the ASSERT family of macros
assertPprPanic file line msg
  = panic (show (doc PprDebug))
  where
    doc = sep [hsep[text "ASSERT failed! file", 
		 	   text file, 
			   text "line", int line], 
		    msg]

warnPprTrace :: Bool -> String -> Int -> SDoc -> a -> a
-- ^ Just warn about an assertion failure, recording the given file and line number.
-- Should typically be accessed with the WARN macros
warnPprTrace _     _file _line _msg x | opt_NoDebugOutput = x
warnPprTrace False _file _line _msg x = x
warnPprTrace True   file  line  msg x
  = trace (show (doc defaultDumpStyle)) x
  where
    doc = sep [hsep [text "WARNING: file", text file, text "line", int line],
	       msg]
\end{code}
