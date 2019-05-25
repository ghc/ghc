{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-
(c) The University of Glasgow 2006-2012
(c) The GRASP Project, Glasgow University, 1992-1998
-}

-- | This module defines classes and functions for pretty-printing. It also
-- exports a number of helpful debugging and other utilities such as 'trace' and 'panic'.
--
-- The interface to this module is very similar to the standard Hughes-PJ pretty printing
-- module, except that it exports a number of additional functions that are rarely used,
-- and works over the 'SDoc' type.
module Outputable (
        -- * Type classes
        Outputable(..), OutputableBndr(..),
        NoConstraint, PairConstraint,
        HasPprConfig(..),

        -- * Pretty printing combinators
        PprConfig (..),
        SDoc', runSDoc, initSDocContext,
        docToSDoc,
        interppSP, interpp'SP,
        pprQuotedList, pprWithCommas, quotedListWithOr, quotedListWithNor,
        pprWithBars,
        empty, isEmpty, nest,
        char,
        text, ftext, ptext, ztext,
        int, intWithCommas, integer, word, float, double, rational, doublePrec,
        parens, cparen, brackets, braces, quotes, quote,
        doubleQuotes, angleBrackets,
        semi, comma, colon, dcolon, space, equals, dot, vbar,
        arrow, larrow, darrow, arrowt, larrowt, arrowtt, larrowtt,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace, underscore,
        blankLine, forAllLit, kindType, bullet,
        (<>), (<+>), hcat, hsep,
        ($$), ($+$), vcat,
        sep, cat,
        fsep, fcat,
        hang, hangNotEmpty, punctuate, ppWhen, ppUnless,
        speakNth, speakN, speakNOf, plural, isOrAre, doOrDoes,
        unicodeSyntax,

        coloured, keyword,

        -- * Converting 'SDoc' into strings and outputing it
        bufLeftRenderSDoc, pprCodeCStyle,
        pprCode, mkCodeStyle,
        showSDocOneLine',
        showSDocDebug, showSDocDumpOneLine',
        renderWithStyle,

        pprInfixVar, pprPrefixVar,
        pprHsChar, pprHsString, pprHsBytes,

        primFloatSuffix, primCharSuffix, primWordSuffix, primDoubleSuffix,
        primInt64Suffix, primWord64Suffix, primIntSuffix,

        pprPrimChar, pprPrimInt, pprPrimWord, pprPrimInt64, pprPrimWord64,

        pprFastFilePath, pprFilePathString,

        -- * Controlling the style in which output is printed
        BindingSite(..),

        PprStyle, CodeStyle(..), PrintUnqualified(..),
        QueryQualifyName, QueryQualifyModule, QueryQualifyPackage,
        reallyAlwaysQualify, reallyAlwaysQualifyNames,
        alwaysQualify, alwaysQualifyNames, alwaysQualifyModules,
        neverQualify, neverQualifyNames, neverQualifyModules,
        alwaysQualifyPackages, neverQualifyPackages,
        QualifyName(..), queryQual,
        sdocWithDynFlags, sdocWithPlatform,
        updSDocDynFlags,
        getPprStyle, withPprStyle, withPprStyleDoc, setStyleColoured,
        pprDeeper, pprDeeperList, pprSetDepth,
        codeStyle, userStyle, debugStyle, dumpStyle, asmStyle,
        qualName, qualModule, qualPackage,
        mkErrStyle', defaultDumpStyle', mkDumpStyle', defaultUserStyle',
        mkUserStyle', cmdlineParserStyle', Depth(..),

        ifPprDebug, whenPprDebug, getPprDebug,

        -- * Error handling and debugging utilities
        callStackDoc
    ) where

import GhcPrelude

import {-# SOURCE #-}   Module( UnitId, Module, ModuleName, moduleName )
import {-# SOURCE #-}   OccName( OccName )

import BufWrite (BufHandle)
import FastString
import qualified Pretty
import Util
import Platform
import qualified PprColour as Col
import Pretty           ( Doc, Mode(..) )
import GHC.Serialized
import GHC.LanguageExtensions (Extension)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import Data.Kind (Constraint)
import qualified Data.Map as M
import Data.Int
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Word
import System.FilePath
import Text.Printf
import Numeric (showFFloat)
import Data.Graph (SCC(..))
import Data.List (intersperse)

import GHC.Fingerprint
import GHC.Show         ( showMultiLineString )
import GHC.Stack        ( callStack, prettyCallStack )

{-
************************************************************************
*                                                                      *
\subsection{The @PprStyle@ data type}
*                                                                      *
************************************************************************
-}

data PprStyle
  = PprUser PrintUnqualified Depth Coloured
                -- Pretty-print in a way that will make sense to the
                -- ordinary user; must be very close to Haskell
                -- syntax, etc.
                -- Assumes printing tidied code: non-system names are
                -- printed without uniques.

  | PprDump PrintUnqualified
                -- For -ddump-foo; less verbose than PprDebug, but more than PprUser
                -- Does not assume tidied code: non-external names
                -- are printed with uniques.

  | PprDebug    -- Full debugging output

  | PprCode CodeStyle
                -- Print code; either C or assembler

data CodeStyle = CStyle         -- The format of labels differs for C and assembler
               | AsmStyle

data Depth = AllTheWay
           | PartWay Int        -- 0 => stop

data Coloured
  = Uncoloured
  | Coloured

-- -----------------------------------------------------------------------------
-- Printing original names

-- | When printing code that contains original names, we need to map the
-- original names back to something the user understands.  This is the
-- purpose of the triple of functions that gets passed around
-- when rendering 'SDoc'.
data PrintUnqualified = QueryQualify {
    queryQualifyName    :: QueryQualifyName,
    queryQualifyModule  :: QueryQualifyModule,
    queryQualifyPackage :: QueryQualifyPackage
}

-- | Given a `Name`'s `Module` and `OccName`, decide whether and how to qualify
-- it.
type QueryQualifyName = Module -> OccName -> QualifyName

-- | For a given module, we need to know whether to print it with
-- a package name to disambiguate it.
type QueryQualifyModule = Module -> Bool

-- | For a given package, we need to know whether to print it with
-- the component id to disambiguate it.
type QueryQualifyPackage = UnitId -> Bool

-- See Note [Printing original names] in HscTypes
data QualifyName   -- Given P:M.T
  = NameUnqual           -- It's in scope unqualified as "T"
                         -- OR nothing called "T" is in scope

  | NameQual ModuleName  -- It's in scope qualified as "X.T"

  | NameNotInScope1      -- It's not in scope at all, but M.T is not bound
                         -- in the current scope, so we can refer to it as "M.T"

  | NameNotInScope2      -- It's not in scope at all, and M.T is already bound in
                         -- the current scope, so we must refer to it as "P:M.T"

instance Outputable QualifyName where
  type OutputableNeedsOfConfig QualifyName = NoConstraint
  ppr NameUnqual      = text "NameUnqual"
  ppr (NameQual _mod) = text "NameQual"  -- can't print the mod without module loops :(
  ppr NameNotInScope1 = text "NameNotInScope1"
  ppr NameNotInScope2 = text "NameNotInScope2"

reallyAlwaysQualifyNames :: QueryQualifyName
reallyAlwaysQualifyNames _ _ = NameNotInScope2

-- | NB: This won't ever show package IDs
alwaysQualifyNames :: QueryQualifyName
alwaysQualifyNames m _ = NameQual (moduleName m)

neverQualifyNames :: QueryQualifyName
neverQualifyNames _ _ = NameUnqual

alwaysQualifyModules :: QueryQualifyModule
alwaysQualifyModules _ = True

neverQualifyModules :: QueryQualifyModule
neverQualifyModules _ = False

alwaysQualifyPackages :: QueryQualifyPackage
alwaysQualifyPackages _ = True

neverQualifyPackages :: QueryQualifyPackage
neverQualifyPackages _ = False

reallyAlwaysQualify, alwaysQualify, neverQualify :: PrintUnqualified
reallyAlwaysQualify
              = QueryQualify reallyAlwaysQualifyNames
                             alwaysQualifyModules
                             alwaysQualifyPackages
alwaysQualify = QueryQualify alwaysQualifyNames
                             alwaysQualifyModules
                             alwaysQualifyPackages
neverQualify  = QueryQualify neverQualifyNames
                             neverQualifyModules
                             neverQualifyPackages

defaultUserStyle' :: Bool -> PprStyle
defaultUserStyle' hasPprDebug = mkUserStyle' hasPprDebug neverQualify AllTheWay

defaultDumpStyle' :: Bool -> PprStyle
 -- Print without qualifiers to reduce verbosity, unless -dppr-debug
defaultDumpStyle' hasPprDebug = mkDumpStyle' hasPprDebug neverQualify

mkDumpStyle' :: Bool -> PrintUnqualified -> PprStyle
mkDumpStyle' hasPprDebug print_unqual = if hasPprDebug
  then PprDebug
  else PprDump print_unqual

-- | Style for printing error messages
mkErrStyle' :: Bool -> Int -> PrintUnqualified -> PprStyle
mkErrStyle' hasPprDebug pprUserLength qual =
   mkUserStyle' hasPprDebug qual (PartWay pprUserLength)

cmdlineParserStyle' :: Bool -> PprStyle
cmdlineParserStyle' hasPprDebug = mkUserStyle' hasPprDebug alwaysQualify AllTheWay

mkUserStyle' :: Bool -> PrintUnqualified -> Depth -> PprStyle
mkUserStyle' hasPprDebug unqual depth = if hasPprDebug
  then PprDebug
  else PprUser unqual depth Uncoloured

setStyleColoured :: Bool -> PprStyle -> PprStyle
setStyleColoured col style =
  case style of
    PprUser q d _ -> PprUser q d c
    _             -> style
  where
    c | col       = Coloured
      | otherwise = Uncoloured

instance Outputable PprStyle where
  type OutputableNeedsOfConfig PprStyle = NoConstraint
  ppr (PprUser {})  = text "user-style"
  ppr (PprCode {})  = text "code-style"
  ppr (PprDump {})  = text "dump-style"
  ppr (PprDebug {}) = text "debug-style"

{-
Orthogonal to the above printing styles are (possibly) some
command-line flags that affect printing (often carried with the
style).  The most likely ones are variations on how much type info is
shown.

The following test decides whether or not we are actually generating
code (either C or assembly), or generating interface files.

************************************************************************
*                                                                      *
\subsection{The @SDoc@ data type}
*                                                                      *
************************************************************************
-}

-- | Represents a pretty-printable document.
--
-- To display an 'SDoc', use 'printSDoc', 'printSDocLn', 'bufLeftRenderSDoc',
-- or 'renderWithStyle'.  Avoid calling 'runSDoc' directly as it breaks the
-- abstraction layer.
--
-- The 'r' paramter is for the configuration object pretty printing depends on.
-- 'DynFlags' should contain this configuration, but for the purposes of
-- modularity and breaking import cycles in particular, something smaller can be
-- specified instead.
newtype SDoc' r = SDoc { runSDoc :: SDocContext' r -> Doc }

data SDocContext' r = SDC
  { sdocStyle      :: !PprStyle
  , sdocLastColour :: !Col.PprColour
    -- ^ The most recently used colour.  This allows nesting colours.
  , sdocDynFlags   :: !r
    -- ^ TODO rename
  }

instance IsString (SDoc' r) where
  fromString = text

-- The lazy programmer's friend.
instance Outputable (SDoc' r) where
  -- Context most be the same
  type OutputableNeedsOfConfig (SDoc' r) = (~) r
  ppr = id

initSDocContext :: r -> PprStyle -> SDocContext' r
initSDocContext r sty = SDC
  { sdocStyle = sty
  , sdocLastColour = Col.colReset
  , sdocDynFlags = r
  }

withPprStyle :: PprStyle -> SDoc' r -> SDoc' r
withPprStyle sty d = SDoc $ \ctxt -> runSDoc d ctxt{sdocStyle=sty}

-- TODO rename to remove "DynFlags"
sdocWithDynFlags :: (r -> SDoc' r) -> SDoc' r
sdocWithDynFlags f = SDoc $ \ctx -> runSDoc (f (sdocDynFlags ctx)) ctx

-- | This is not a recommended way to render 'SDoc', since it breaks the
-- abstraction layer of 'SDoc'.  Prefer to use 'printSDoc', 'printSDocLn',
-- 'bufLeftRenderSDoc', or 'renderWithStyle' instead.
withPprStyleDoc :: r -> PprStyle -> SDoc' r -> Doc
withPprStyleDoc r sty d = runSDoc d (initSDocContext r sty)

pprDeeper :: SDoc' r -> SDoc' r
pprDeeper d = SDoc $ \ctx -> case ctx of
  SDC{sdocStyle=PprUser _ (PartWay 0) _} -> Pretty.text "..."
  SDC{sdocStyle=PprUser q (PartWay n) c} ->
    runSDoc d ctx{sdocStyle = PprUser q (PartWay (n-1)) c}
  _ -> runSDoc d ctx

-- | Truncate a list that is longer than the current depth.
pprDeeperList :: ([SDoc' r] -> SDoc' r) -> [SDoc' r] -> SDoc' r
pprDeeperList f ds
  | null ds   = f []
  | otherwise = SDoc work
 where
  work ctx@SDC{sdocStyle=PprUser q (PartWay n) c}
   | n==0      = Pretty.text "..."
   | otherwise =
      runSDoc (f (go 0 ds)) ctx{sdocStyle = PprUser q (PartWay (n-1)) c}
   where
     go _ [] = []
     go i (d:ds) | i >= n    = [text "...."]
                 | otherwise = d : go (i+1) ds
  work other_ctx = runSDoc (f ds) other_ctx

pprSetDepth :: Depth -> SDoc' r -> SDoc' r
pprSetDepth depth doc = SDoc $ \ctx ->
    case ctx of
        SDC{sdocStyle=PprUser q _ c} ->
            runSDoc doc ctx{sdocStyle = PprUser q depth c}
        _ ->
            runSDoc doc ctx

getPprStyle :: (PprStyle -> SDoc' r) -> SDoc' r
getPprStyle df = SDoc $ \ctx -> runSDoc (df (sdocStyle ctx)) ctx

sdocWithPlatform :: HasPlatform r => (Platform -> SDoc' r) -> SDoc' r
sdocWithPlatform f = sdocWithDynFlags (f . getPlatform)

-- TODO rename
updSDocDynFlags :: (r -> r) -> SDoc' r -> SDoc' r
updSDocDynFlags upd doc
  = SDoc $ \ctx -> runSDoc doc (ctx { sdocDynFlags = upd (sdocDynFlags ctx) })

qualName :: PprStyle -> QueryQualifyName
qualName (PprUser q _ _) mod occ = queryQualifyName q mod occ
qualName (PprDump q)     mod occ = queryQualifyName q mod occ
qualName _other          mod _   = NameQual (moduleName mod)

qualModule :: PprStyle -> QueryQualifyModule
qualModule (PprUser q _ _)  m = queryQualifyModule q m
qualModule (PprDump q)      m = queryQualifyModule q m
qualModule _other          _m = True

qualPackage :: PprStyle -> QueryQualifyPackage
qualPackage (PprUser q _ _)  m = queryQualifyPackage q m
qualPackage (PprDump q)      m = queryQualifyPackage q m
qualPackage _other          _m = True

queryQual :: PprStyle -> PrintUnqualified
queryQual s = QueryQualify (qualName s)
                           (qualModule s)
                           (qualPackage s)

codeStyle :: PprStyle -> Bool
codeStyle (PprCode _)     = True
codeStyle _               = False

asmStyle :: PprStyle -> Bool
asmStyle (PprCode AsmStyle)  = True
asmStyle _other              = False

dumpStyle :: PprStyle -> Bool
dumpStyle (PprDump {}) = True
dumpStyle _other       = False

debugStyle :: PprStyle -> Bool
debugStyle PprDebug = True
debugStyle _other   = False

userStyle ::  PprStyle -> Bool
userStyle (PprUser {}) = True
userStyle _other       = False

getPprDebug :: (Bool -> SDoc' r) -> SDoc' r
getPprDebug d = getPprStyle $ \ sty -> d (debugStyle sty)

ifPprDebug :: SDoc' r -> SDoc' r -> SDoc' r
-- ^ Says what to do with and without -dppr-debug
ifPprDebug yes no = getPprDebug $ \ dbg -> if dbg then yes else no

whenPprDebug :: SDoc' r -> SDoc' r        -- Empty for non-debug style
-- ^ Says what to do with -dppr-debug; without, return empty
whenPprDebug d = ifPprDebug d empty

pprCodeCStyle :: PprStyle
pprCodeCStyle = PprCode CStyle

-- | An efficient variant of 'printSDoc' specialized for 'LeftMode' that
-- outputs to a 'BufHandle'.
bufLeftRenderSDoc :: r -> BufHandle -> PprStyle -> SDoc' r -> IO ()
bufLeftRenderSDoc cfg bufHandle sty doc =
  Pretty.bufLeftRender bufHandle (runSDoc doc (initSDocContext cfg sty))

pprCode :: CodeStyle -> SDoc' r -> SDoc' r
pprCode cs d = withPprStyle (PprCode cs) d

mkCodeStyle :: CodeStyle -> PprStyle
mkCodeStyle = PprCode

renderWithStyle :: HasPprConfig r => r -> SDoc' r -> PprStyle -> String
renderWithStyle cfg sdoc sty
  = let s = Pretty.style{ Pretty.mode = PageMode,
                          Pretty.lineLength = pprConfig_numColumns $ getPprConfig cfg }
    in Pretty.renderStyle s $ runSDoc sdoc (initSDocContext cfg sty)

showSDocDebug :: HasPprConfig r => r -> SDoc' r -> String
showSDocDebug cfg d = renderWithStyle cfg d PprDebug

-- | This shows an SDoc, but on one line only. It's cheaper than a full
-- showSDoc, designed for when we're getting results like "Foo.bar"
-- and "foo{uniq strictness}" so we don't want fancy layout anyway.
showSDocOneLine' :: HasPprConfig r => PprStyle -> r -> SDoc' r -> String
showSDocOneLine' userStyle cfg d
 = let s = Pretty.style{ Pretty.mode = OneLineMode,
                         Pretty.lineLength = pprConfig_numColumns $ getPprConfig cfg } in
   Pretty.renderStyle s $
      runSDoc d (initSDocContext cfg userStyle)

showSDocDumpOneLine' :: PprStyle -> r -> SDoc' r -> String
showSDocDumpOneLine' dumpStyle cfg d
 = let s = Pretty.style{ Pretty.mode = OneLineMode,
                         Pretty.lineLength = irrelevantNCols } in
   Pretty.renderStyle s $
      runSDoc d (initSDocContext cfg dumpStyle)

irrelevantNCols :: Int
-- Used for OneLineMode and LeftMode when number of cols isn't used
irrelevantNCols = 1

isEmpty :: r -> SDoc' r -> Bool
isEmpty dflags sdoc = Pretty.isEmpty $ runSDoc sdoc dummySDocContext
   where dummySDocContext = initSDocContext dflags PprDebug

docToSDoc :: Doc -> SDoc' r
docToSDoc d = SDoc (\_ -> d)

empty    :: SDoc' r
char     :: Char       -> SDoc' r
text     :: String     -> SDoc' r
ftext    :: FastString -> SDoc' r
ptext    :: PtrString  -> SDoc' r
ztext    :: FastZString -> SDoc' r
int      :: Int        -> SDoc' r
integer  :: Integer    -> SDoc' r
float    :: Float      -> SDoc' r
double   :: Double     -> SDoc' r
rational :: Rational   -> SDoc' r

empty       = docToSDoc $ Pretty.empty
char c      = docToSDoc $ Pretty.char c

text s      = docToSDoc $ Pretty.text s
{-# INLINE text #-}   -- Inline so that the RULE Pretty.text will fire

ftext s     = docToSDoc $ Pretty.ftext s
ptext s     = docToSDoc $ Pretty.ptext s
ztext s     = docToSDoc $ Pretty.ztext s
int n       = docToSDoc $ Pretty.int n
integer n   = docToSDoc $ Pretty.integer n
float n     = docToSDoc $ Pretty.float n
double n    = docToSDoc $ Pretty.double n
rational n  = docToSDoc $ Pretty.rational n

word :: HasPprConfig r => Integer -> SDoc' r
word n      = sdocWithDynFlags $ \cfg ->
    -- See Note [Print Hexadecimal Literals] in Pretty.hs
    if pprConfig_shouldUseHexWordLiterals $ getPprConfig cfg
        then docToSDoc $ Pretty.hex n
        else docToSDoc $ Pretty.integer n

-- | @doublePrec p n@ shows a floating point number @n@ with @p@
-- digits of precision after the decimal point.
doublePrec :: Int -> Double -> SDoc' r
doublePrec p n = text (showFFloat (Just p) n "")

parens, braces, brackets, quote,
        doubleQuotes, angleBrackets :: SDoc' r -> SDoc' r
quotes :: HasPprConfig r => SDoc' r -> SDoc' r

parens d        = SDoc $ Pretty.parens . runSDoc d
braces d        = SDoc $ Pretty.braces . runSDoc d
brackets d      = SDoc $ Pretty.brackets . runSDoc d
quote d         = SDoc $ Pretty.quote . runSDoc d
doubleQuotes d  = SDoc $ Pretty.doubleQuotes . runSDoc d
angleBrackets d = char '<' <> d <> char '>'

cparen :: Bool -> SDoc' r -> SDoc' r
cparen b d = SDoc $ Pretty.maybeParens b . runSDoc d

-- 'quotes' encloses something in single quotes...
-- but it omits them if the thing begins or ends in a single quote
-- so that we don't get `foo''.  Instead we just have foo'.
quotes d = sdocWithDynFlags $ \cfg ->
      if pprConfig_useUnicode $ getPprConfig cfg
      then char '‘' <> d <> char '’'
      else SDoc $ \sty ->
           let pp_d = runSDoc d sty
               str  = show pp_d
           in case (str, lastMaybe str) of
             (_, Just '\'') -> pp_d
             ('\'' : _, _)       -> pp_d
             _other              -> Pretty.quotes pp_d

semi, comma, colon, equals, space, underscore, dot, vbar :: SDoc' r
dcolon :: HasPprConfig r =>  SDoc' r
arrow, larrow, darrow, arrowt, larrowt, arrowtt, larrowtt :: HasPprConfig r =>  SDoc' r
lparen, rparen, lbrack, rbrack, lbrace, rbrace, blankLine :: SDoc' r

blankLine  = docToSDoc $ Pretty.text ""
dcolon     = unicodeSyntax (char '∷') (docToSDoc $ Pretty.text "::")
arrow      = unicodeSyntax (char '→') (docToSDoc $ Pretty.text "->")
larrow     = unicodeSyntax (char '←') (docToSDoc $ Pretty.text "<-")
darrow     = unicodeSyntax (char '⇒') (docToSDoc $ Pretty.text "=>")
arrowt     = unicodeSyntax (char '⤚') (docToSDoc $ Pretty.text ">-")
larrowt    = unicodeSyntax (char '⤙') (docToSDoc $ Pretty.text "-<")
arrowtt    = unicodeSyntax (char '⤜') (docToSDoc $ Pretty.text ">>-")
larrowtt   = unicodeSyntax (char '⤛') (docToSDoc $ Pretty.text "-<<")
semi       = docToSDoc $ Pretty.semi
comma      = docToSDoc $ Pretty.comma
colon      = docToSDoc $ Pretty.colon
equals     = docToSDoc $ Pretty.equals
space      = docToSDoc $ Pretty.space
underscore = char '_'
dot        = char '.'
vbar       = char '|'
lparen     = docToSDoc $ Pretty.lparen
rparen     = docToSDoc $ Pretty.rparen
lbrack     = docToSDoc $ Pretty.lbrack
rbrack     = docToSDoc $ Pretty.rbrack
lbrace     = docToSDoc $ Pretty.lbrace
rbrace     = docToSDoc $ Pretty.rbrace

forAllLit :: HasPprConfig r => SDoc' r
forAllLit = unicodeSyntax (char '∀') (text "forall")

kindType :: HasPprConfig r => SDoc' r
kindType = sdocWithDynFlags $ \cfg ->
    if pprConfig_useStarIsType $ getPprConfig cfg
    then unicodeSyntax (char '★') (char '*')
    else text "Type"

bullet :: HasPprConfig r => SDoc' r
bullet = unicode (char '•') (char '*')

unicodeSyntax :: HasPprConfig r => SDoc' r -> SDoc' r -> SDoc' r
unicodeSyntax unicode plain = sdocWithDynFlags $ \cfg ->
    if pprConfig_useUnicode (getPprConfig cfg) && pprConfig_useUnicodeSyntax (getPprConfig cfg)
    then unicode
    else plain

unicode :: HasPprConfig r => SDoc' r -> SDoc' r -> SDoc' r
unicode unicode plain = sdocWithDynFlags $ \cfg ->
    if pprConfig_useUnicode $ getPprConfig cfg
    then unicode
    else plain

nest :: Int -> SDoc' r -> SDoc' r
-- ^ Indent 'SDoc' some specified amount
(<>) :: SDoc' r -> SDoc' r -> SDoc' r
-- ^ Join two 'SDoc' together horizontally without a gap
(<+>) :: SDoc' r -> SDoc' r -> SDoc' r
-- ^ Join two 'SDoc' together horizontally with a gap between them
($$) :: SDoc' r -> SDoc' r -> SDoc' r
-- ^ Join two 'SDoc' together vertically; if there is
-- no vertical overlap it "dovetails" the two onto one line
($+$) :: SDoc' r -> SDoc' r -> SDoc' r
-- ^ Join two 'SDoc' together vertically

nest n d    = SDoc $ Pretty.nest n . runSDoc d
(<>) d1 d2  = SDoc $ \sty -> (Pretty.<>)  (runSDoc d1 sty) (runSDoc d2 sty)
(<+>) d1 d2 = SDoc $ \sty -> (Pretty.<+>) (runSDoc d1 sty) (runSDoc d2 sty)
($$) d1 d2  = SDoc $ \sty -> (Pretty.$$)  (runSDoc d1 sty) (runSDoc d2 sty)
($+$) d1 d2 = SDoc $ \sty -> (Pretty.$+$) (runSDoc d1 sty) (runSDoc d2 sty)

hcat :: [SDoc' r] -> SDoc' r
-- ^ Concatenate 'SDoc' horizontally
hsep :: [SDoc' r] -> SDoc' r
-- ^ Concatenate 'SDoc' horizontally with a space between each one
vcat :: [SDoc' r] -> SDoc' r
-- ^ Concatenate 'SDoc' vertically with dovetailing
sep :: [SDoc' r] -> SDoc' r
-- ^ Separate: is either like 'hsep' or like 'vcat', depending on what fits
cat :: [SDoc' r] -> SDoc' r
-- ^ Catenate: is either like 'hcat' or like 'vcat', depending on what fits
fsep :: [SDoc' r] -> SDoc' r
-- ^ A paragraph-fill combinator. It's much like sep, only it
-- keeps fitting things on one line until it can't fit any more.
fcat :: [SDoc' r] -> SDoc' r
-- ^ This behaves like 'fsep', but it uses '<>' for horizontal conposition rather than '<+>'


hcat ds = SDoc $ \sty -> Pretty.hcat [runSDoc d sty | d <- ds]
hsep ds = SDoc $ \sty -> Pretty.hsep [runSDoc d sty | d <- ds]
vcat ds = SDoc $ \sty -> Pretty.vcat [runSDoc d sty | d <- ds]
sep ds  = SDoc $ \sty -> Pretty.sep  [runSDoc d sty | d <- ds]
cat ds  = SDoc $ \sty -> Pretty.cat  [runSDoc d sty | d <- ds]
fsep ds = SDoc $ \sty -> Pretty.fsep [runSDoc d sty | d <- ds]
fcat ds = SDoc $ \sty -> Pretty.fcat [runSDoc d sty | d <- ds]

hang :: SDoc' r  -- ^ The header
      -> Int  -- ^ Amount to indent the hung body
      -> SDoc' r -- ^ The hung body, indented and placed below the header
      -> SDoc' r
hang d1 n d2   = SDoc $ \sty -> Pretty.hang (runSDoc d1 sty) n (runSDoc d2 sty)

-- | This behaves like 'hang', but does not indent the second document
-- when the header is empty.
hangNotEmpty :: SDoc' r -> Int -> SDoc' r -> SDoc' r
hangNotEmpty d1 n d2 =
    SDoc $ \sty -> Pretty.hangNotEmpty (runSDoc d1 sty) n (runSDoc d2 sty)

punctuate :: SDoc' r   -- ^ The punctuation
          -> [SDoc' r] -- ^ The list that will have punctuation added between every adjacent pair of elements
          -> [SDoc' r] -- ^ Punctuated list
punctuate _ []     = []
punctuate p (d:ds) = go d ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d <> p) : go e es

ppWhen, ppUnless :: Bool -> SDoc' r -> SDoc' r
ppWhen True  doc = doc
ppWhen False _   = empty

ppUnless True  _   = empty
ppUnless False doc = doc

-- | Apply the given colour\/style for the argument.
--
-- Only takes effect if colours are enabled.
coloured :: HasPprConfig r => Col.PprColour -> SDoc' r -> SDoc' r
coloured col sdoc =
  sdocWithDynFlags $ \cfg ->
    if pprConfig_shouldUseColor $ getPprConfig cfg
    then SDoc $ \ctx@SDC{ sdocLastColour = lastCol } ->
         case ctx of
           SDC{ sdocStyle = PprUser _ _ Coloured } ->
             let ctx' = ctx{ sdocLastColour = lastCol `mappend` col } in
             Pretty.zeroWidthText (Col.renderColour col)
               Pretty.<> runSDoc sdoc ctx'
               Pretty.<> Pretty.zeroWidthText (Col.renderColourAfresh lastCol)
           _ -> runSDoc sdoc ctx
    else sdoc

keyword :: HasPprConfig r => SDoc' r -> SDoc' r
keyword = coloured Col.colBold

{-
************************************************************************
*                                                                      *
\subsection[Outputable-class]{The @Outputable@ class}
*                                                                      *
************************************************************************
-}

-- | Class designating that some type has an 'SDoc' representation
class Outputable a where
        type OutputableNeedsOfConfig a :: * -> Constraint
        type OutputableNeedsOfConfig a = NoConstraint

        ppr :: OutputableNeedsOfConfig a r => a -> SDoc' r
        pprPrec :: OutputableNeedsOfConfig a r => Rational -> a -> SDoc' r
                -- 0 binds least tightly
                -- We use Rational because there is always a
                -- Rational between any other two Rationals

        ppr = pprPrec 0
        pprPrec _ = ppr

instance Outputable Char where
    type OutputableNeedsOfConfig Char = NoConstraint
    ppr c = text [c]

instance Outputable Bool where
    type OutputableNeedsOfConfig Bool = NoConstraint
    ppr True  = text "True"
    ppr False = text "False"

instance Outputable Ordering where
    type OutputableNeedsOfConfig Ordering = NoConstraint
    ppr LT = text "LT"
    ppr EQ = text "EQ"
    ppr GT = text "GT"

instance Outputable Int32 where
   type OutputableNeedsOfConfig Int32 = NoConstraint
   ppr n = integer $ fromIntegral n

instance Outputable Int64 where
   type OutputableNeedsOfConfig Int64 = NoConstraint
   ppr n = integer $ fromIntegral n

instance Outputable Int where
    type OutputableNeedsOfConfig Int = NoConstraint
    ppr n = int n

instance Outputable Integer where
    type OutputableNeedsOfConfig Integer = NoConstraint
    ppr n = integer n

instance Outputable Word16 where
    type OutputableNeedsOfConfig Word16 = NoConstraint
    ppr n = integer $ fromIntegral n

instance Outputable Word32 where
    type OutputableNeedsOfConfig Word32 = NoConstraint
    ppr n = integer $ fromIntegral n

instance Outputable Word where
    type OutputableNeedsOfConfig Word = NoConstraint
    ppr n = integer $ fromIntegral n

instance Outputable () where
    type OutputableNeedsOfConfig () = NoConstraint
    ppr _ = text "()"

instance (Outputable a) => Outputable [a] where
    type OutputableNeedsOfConfig [a] = OutputableNeedsOfConfig a
    ppr xs = brackets (fsep (punctuate comma (map ppr xs)))

instance (Outputable a) => Outputable (Set a) where
    type OutputableNeedsOfConfig (Set a) = OutputableNeedsOfConfig a
    ppr s = braces (fsep (punctuate comma (map ppr (Set.toList s))))

instance (Outputable a, Outputable b) => Outputable (a, b) where
    type OutputableNeedsOfConfig (a, b) = PairConstraint
       (OutputableNeedsOfConfig a)
       (OutputableNeedsOfConfig b)
    ppr (x,y) = parens (sep [ppr x <> comma, ppr y])

instance Outputable a => Outputable (Maybe a) where
    type OutputableNeedsOfConfig (Maybe a) = OutputableNeedsOfConfig a
    ppr Nothing  = text "Nothing"
    ppr (Just x) = text "Just" <+> ppr x

instance (Outputable a, Outputable b) => Outputable (Either a b) where
    type OutputableNeedsOfConfig (Either a b) = PairConstraint
       (OutputableNeedsOfConfig a)
       (OutputableNeedsOfConfig b)
    ppr (Left x)  = text "Left"  <+> ppr x
    ppr (Right y) = text "Right" <+> ppr y

-- ToDo: may not be used
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    type OutputableNeedsOfConfig (a, b, c) = PairConstraint
      (OutputableNeedsOfConfig a)
      (PairConstraint (OutputableNeedsOfConfig b) (OutputableNeedsOfConfig c))
    ppr (x,y,z) =
      parens (sep [ppr x <> comma,
                   ppr y <> comma,
                   ppr z ])

instance (Outputable a, Outputable b, Outputable c, Outputable d) =>
         Outputable (a, b, c, d) where
    type OutputableNeedsOfConfig (a, b, c, d) = PairConstraint
      (PairConstraint (OutputableNeedsOfConfig a) (OutputableNeedsOfConfig b))
      (PairConstraint (OutputableNeedsOfConfig c) (OutputableNeedsOfConfig d))
    ppr (a,b,c,d) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e) =>
         Outputable (a, b, c, d, e) where
    type OutputableNeedsOfConfig (a, b, c, d, e) = PairConstraint
      (PairConstraint (OutputableNeedsOfConfig a) (OutputableNeedsOfConfig b))
      (PairConstraint (OutputableNeedsOfConfig c)
       (PairConstraint
        (OutputableNeedsOfConfig d)
        (OutputableNeedsOfConfig e)))
    ppr (a,b,c,d,e) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e, Outputable f) =>
         Outputable (a, b, c, d, e, f) where
    type OutputableNeedsOfConfig (a, b, c, d, e, f) = PairConstraint
      (PairConstraint (OutputableNeedsOfConfig a) (OutputableNeedsOfConfig b))
      (PairConstraint
       (PairConstraint
        (OutputableNeedsOfConfig c)
        (OutputableNeedsOfConfig d))
       (PairConstraint
        (OutputableNeedsOfConfig e)
        (OutputableNeedsOfConfig f)))
    ppr (a,b,c,d,e,f) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e, Outputable f, Outputable g) =>
         Outputable (a, b, c, d, e, f, g) where
    type OutputableNeedsOfConfig (a, b, c, d, e, f, g) = PairConstraint
      (PairConstraint
       (OutputableNeedsOfConfig a)
       (PairConstraint
        (OutputableNeedsOfConfig b)
        (OutputableNeedsOfConfig c)))
      (PairConstraint
       (PairConstraint
        (OutputableNeedsOfConfig d)
        (OutputableNeedsOfConfig e))
       (PairConstraint
        (OutputableNeedsOfConfig f)
        (OutputableNeedsOfConfig g)))
    ppr (a,b,c,d,e,f,g) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f <> comma,
                   ppr g])

instance Outputable FastString where
    type OutputableNeedsOfConfig FastString = NoConstraint
    ppr fs = ftext fs           -- Prints an unadorned string,
                                -- no double quotes or anything

instance (Outputable key, Outputable elt) => Outputable (M.Map key elt) where
    type OutputableNeedsOfConfig (M.Map key elt) = PairConstraint
      (OutputableNeedsOfConfig key)
      (OutputableNeedsOfConfig elt)
    ppr m = ppr (M.toList m)
instance (Outputable elt) => Outputable (IM.IntMap elt) where
    type OutputableNeedsOfConfig (IM.IntMap elt) = OutputableNeedsOfConfig elt
    ppr m = ppr (IM.toList m)

instance Outputable Fingerprint where
    type OutputableNeedsOfConfig Fingerprint = NoConstraint
    ppr (Fingerprint w1 w2) = text (printf "%016x%016x" w1 w2)

instance Outputable a => Outputable (SCC a) where
   type OutputableNeedsOfConfig (SCC a) = OutputableNeedsOfConfig a
   ppr (AcyclicSCC v) = text "NONREC" $$ (nest 3 (ppr v))
   ppr (CyclicSCC vs) = text "REC" $$ (nest 3 (vcat (map ppr vs)))

instance Outputable Serialized where
    type OutputableNeedsOfConfig Serialized = NoConstraint
    ppr (Serialized the_type bytes) = int (length bytes) <+> text "of type" <+> text (show the_type)

instance Outputable Extension where
    type OutputableNeedsOfConfig Extension = NoConstraint
    ppr = text . show

{-
************************************************************************
*                                                                      *
\subsection{The @OutputableBndr@ class}
*                                                                      *
************************************************************************
-}

-- | 'BindingSite' is used to tell the thing that prints binder what
-- language construct is binding the identifier.  This can be used
-- to decide how much info to print.
-- Also see Note [Binding-site specific printing] in PprCore
data BindingSite
    = LambdaBind  -- ^ The x in   (\x. e)
    | CaseBind    -- ^ The x in   case scrut of x { (y,z) -> ... }
    | CasePatBind -- ^ The y,z in case scrut of x { (y,z) -> ... }
    | LetBind     -- ^ The x in   (let x = rhs in e)

-- | When we print a binder, we often want to print its type too.
-- The @OutputableBndr@ class encapsulates this idea.
class Outputable a => OutputableBndr a where
   pprBndr :: OutputableNeedsOfConfig a r => BindingSite -> a -> SDoc' r
   pprBndr _b x = ppr x

   pprPrefixOcc, pprInfixOcc :: OutputableNeedsOfConfig a r => a -> SDoc' r
      -- Print an occurrence of the name, suitable either in the
      -- prefix position of an application, thus   (f a b) or  ((+) x)
      -- or infix position,                 thus   (a `f` b) or  (x + y)

   bndrIsJoin_maybe :: a -> Maybe Int
   bndrIsJoin_maybe _ = Nothing
      -- When pretty-printing we sometimes want to find
      -- whether the binder is a join point.  You might think
      -- we could have a function of type (a->Var), but Var
      -- isn't available yet, alas

class NoConstraint a
instance NoConstraint a

class (f a, g a) => PairConstraint f g a
instance (f a, g a) => PairConstraint f g a

{-
************************************************************************
*                                                                      *
\subsection{Random printing helpers}
*                                                                      *
************************************************************************
-}

-- We have 31-bit Chars and will simply use Show instances of Char and String.

-- | Special combinator for showing character literals.
pprHsChar :: Char -> SDoc' r
pprHsChar c | c > '\x10ffff' = char '\\' <> text (show (fromIntegral (ord c) :: Word32))
            | otherwise      = text (show c)

-- | Special combinator for showing string literals.
pprHsString :: FastString -> SDoc' r
pprHsString fs = vcat (map text (showMultiLineString (unpackFS fs)))

-- | Special combinator for showing bytestring literals.
pprHsBytes :: ByteString -> SDoc' r
pprHsBytes bs = let escaped = concatMap escape $ BS.unpack bs
                in vcat (map text (showMultiLineString escaped)) <> char '#'
    where escape :: Word8 -> String
          escape w = let c = chr (fromIntegral w)
                     in if isAscii c
                        then [c]
                        else '\\' : show w

-- Postfix modifiers for unboxed literals.
-- See Note [Printing of literals in Core] in `basicTypes/Literal.hs`.
primCharSuffix, primFloatSuffix, primIntSuffix :: SDoc' r
primDoubleSuffix, primWordSuffix, primInt64Suffix, primWord64Suffix :: SDoc' r
primCharSuffix   = char '#'
primFloatSuffix  = char '#'
primIntSuffix    = char '#'
primDoubleSuffix = text "##"
primWordSuffix   = text "##"
primInt64Suffix  = text "L#"
primWord64Suffix = text "L##"

-- | Special combinator for showing unboxed literals.
pprPrimChar :: Char -> SDoc' r
pprPrimChar c   = pprHsChar c <> primCharSuffix

pprPrimInt, pprPrimWord, pprPrimInt64, pprPrimWord64 :: HasPprConfig r => Integer -> SDoc' r
pprPrimInt i    = integer i   <> primIntSuffix
pprPrimWord w   = word    w   <> primWordSuffix
pprPrimInt64 i  = integer i   <> primInt64Suffix
pprPrimWord64 w = word    w   <> primWord64Suffix

---------------------
-- Put a name in parens if it's an operator
pprPrefixVar :: Bool -> SDoc' r -> SDoc' r
pprPrefixVar is_operator pp_v
  | is_operator = parens pp_v
  | otherwise   = pp_v

-- Put a name in backquotes if it's not an operator
pprInfixVar :: Bool -> SDoc' r -> SDoc' r
pprInfixVar is_operator pp_v
  | is_operator = pp_v
  | otherwise   = char '`' <> pp_v <> char '`'

---------------------
pprFastFilePath :: FastString -> SDoc' r
pprFastFilePath path = text $ normalise $ unpackFS path

-- | Normalise, escape and render a string representing a path
--
-- e.g. "c:\\whatever"
pprFilePathString :: FilePath -> SDoc' r
pprFilePathString path = doubleQuotes $ text (escape (normalise path))
   where
      escape []        = []
      escape ('\\':xs) = '\\':'\\':escape xs
      escape (x:xs)    = x:escape xs

{-
************************************************************************
*                                                                      *
\subsection{Other helper functions}
*                                                                      *
************************************************************************
-}

pprWithCommas :: (a -> SDoc' r) -- ^ The pretty printing function to use
              -> [a]         -- ^ The things to be pretty printed
              -> SDoc' r        -- ^ 'SDoc' where the things have been pretty printed,
                             -- comma-separated and finally packed into a paragraph.
pprWithCommas pp xs = fsep (punctuate comma (map pp xs))

pprWithBars :: (a -> SDoc' r) -- ^ The pretty printing function to use
            -> [a]         -- ^ The things to be pretty printed
            -> SDoc' r        -- ^ 'SDoc' where the things have been pretty printed,
                           -- bar-separated and finally packed into a paragraph.
pprWithBars pp xs = fsep (intersperse vbar (map pp xs))

-- | Returns the separated concatenation of the pretty printed things.
interppSP
  :: (Outputable a, OutputableNeedsOfConfig a r)
  => [a]
  -> SDoc' r
interppSP  xs = sep (map ppr xs)

-- | Returns the comma-separated concatenation of the pretty printed things.
interpp'SP
  :: (Outputable a, OutputableNeedsOfConfig a r)
  => [a]
  -> SDoc' r
interpp'SP xs = sep (punctuate comma (map ppr xs))

-- | Returns the comma-separated concatenation of the quoted pretty printed things.
--
-- > [x,y,z]  ==>  `x', `y', `z'
pprQuotedList
  :: (Outputable a, OutputableNeedsOfConfig a r, HasPprConfig r)
  => [a]
  -> SDoc' r
pprQuotedList = quotedList . map ppr

quotedList :: HasPprConfig r => [SDoc' r] -> SDoc' r
quotedList xs = fsep (punctuate comma (map quotes xs))

quotedListWithOr :: HasPprConfig r => [SDoc' r] -> SDoc' r
-- [x,y,z]  ==>  `x', `y' or `z'
quotedListWithOr xs@(_:_:_) = quotedList (init xs) <+> text "or" <+> quotes (last xs)
quotedListWithOr xs = quotedList xs

quotedListWithNor :: HasPprConfig r => [SDoc' r] -> SDoc' r
-- [x,y,z]  ==>  `x', `y' nor `z'
quotedListWithNor xs@(_:_:_) = quotedList (init xs) <+> text "nor" <+> quotes (last xs)
quotedListWithNor xs = quotedList xs

{-
************************************************************************
*                                                                      *
\subsection{Printing numbers verbally}
*                                                                      *
************************************************************************
-}

intWithCommas :: Integral a => a -> SDoc' r
-- Prints a big integer with commas, eg 345,821
intWithCommas n
  | n < 0     = char '-' <> intWithCommas (-n)
  | q == 0    = int (fromIntegral r)
  | otherwise = intWithCommas q <> comma <> zeroes <> int (fromIntegral r)
  where
    (q,r) = n `quotRem` 1000
    zeroes | r >= 100  = empty
           | r >= 10   = char '0'
           | otherwise = text "00"

-- | Converts an integer to a verbal index:
--
-- > speakNth 1 = text "first"
-- > speakNth 5 = text "fifth"
-- > speakNth 21 = text "21st"
speakNth :: Int -> SDoc' r
speakNth 1 = text "first"
speakNth 2 = text "second"
speakNth 3 = text "third"
speakNth 4 = text "fourth"
speakNth 5 = text "fifth"
speakNth 6 = text "sixth"
speakNth n = hcat [ int n, text suffix ]
  where
    suffix | n <= 20       = "th"       -- 11,12,13 are non-std
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
speakN :: Int -> SDoc' r
speakN 0 = text "none"  -- E.g.  "he has none"
speakN 1 = text "one"   -- E.g.  "he has one"
speakN 2 = text "two"
speakN 3 = text "three"
speakN 4 = text "four"
speakN 5 = text "five"
speakN 6 = text "six"
speakN n = int n

-- | Converts an integer and object description to a statement about the
-- multiplicity of those objects:
--
-- > speakNOf 0 (text "melon") = text "no melons"
-- > speakNOf 1 (text "melon") = text "one melon"
-- > speakNOf 3 (text "melon") = text "three melons"
speakNOf :: Int -> SDoc' r -> SDoc' r
speakNOf 0 d = text "no" <+> d <> char 's'
speakNOf 1 d = text "one" <+> d                 -- E.g. "one argument"
speakNOf n d = speakN n <+> d <> char 's'               -- E.g. "three arguments"

-- | Determines the pluralisation suffix appropriate for the length of a list:
--
-- > plural [] = char 's'
-- > plural ["Hello"] = empty
-- > plural ["Hello", "World"] = char 's'
plural :: [a] -> SDoc' r
plural [_] = empty  -- a bit frightening, but there you are
plural _   = char 's'

-- | Determines the form of to be appropriate for the length of a list:
--
-- > isOrAre [] = text "are"
-- > isOrAre ["Hello"] = text "is"
-- > isOrAre ["Hello", "World"] = text "are"
isOrAre :: [a] -> SDoc' r
isOrAre [_] = text "is"
isOrAre _   = text "are"

-- | Determines the form of to do appropriate for the length of a list:
--
-- > doOrDoes [] = text "do"
-- > doOrDoes ["Hello"] = text "does"
-- > doOrDoes ["Hello", "World"] = text "do"
doOrDoes :: [a] -> SDoc' r
doOrDoes [_] = text "does"
doOrDoes _   = text "do"

{-
************************************************************************
*                                                                      *
\subsection{The @PprConfig@ data type}
*                                                                      *
************************************************************************
-}

-- A bunch of configuration options about pretty printing. 'DynFlags' would
-- contain this, for example.
data PprConfig = PprConfig
 { pprConfig_shouldUseColor :: Bool
 , pprConfig_useUnicode :: Bool
 , pprConfig_useUnicodeSyntax :: Bool
 , pprConfig_useStarIsType :: Bool
 , -- | See Note [Print Hexadecimal Literals] in Pretty.hs
   pprConfig_shouldUseHexWordLiterals :: Bool
 , pprConfig_numColumns :: Int
 }

-- TODO Lens?
class HasPprConfig c where
  getPprConfig :: c -> PprConfig

{-
************************************************************************
*                                                                      *
\subsection{Error handling}
*                                                                      *
************************************************************************
-}

callStackDoc :: HasCallStack => SDoc' r
callStackDoc =
    hang (text "Call stack:")
       4 (vcat $ map text $ lines (prettyCallStack callStack))
