{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
module GHC.Utils.Outputable (
        -- * Type classes
        Outputable(..), OutputableBndr(..), OutputableP(..),

        -- * Pretty printing combinators
        SDoc, runSDoc, PDoc(..),
        docToSDoc,
        interppSP, interpp'SP, interpp'SP',
        pprQuotedList, pprWithCommas, quotedListWithOr, quotedListWithNor,
        pprWithBars,
        empty, isEmpty, nest,
        char,
        text, ftext, ptext, ztext,
        int, intWithCommas, integer, word, float, double, rational, doublePrec,
        parens, cparen, brackets, braces, quotes, quote,
        doubleQuotes, angleBrackets,
        semi, comma, colon, dcolon, space, equals, dot, vbar,
        arrow, lollipop, larrow, darrow, arrowt, larrowt, arrowtt, larrowtt,
        lambda,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace, underscore, mulArrow,
        blankLine, forAllLit, bullet,
        (<>), (<+>), hcat, hsep,
        ($$), ($+$), vcat,
        sep, cat,
        fsep, fcat,
        hang, hangNotEmpty, punctuate, ppWhen, ppUnless,
        ppWhenOption, ppUnlessOption,
        speakNth, speakN, speakNOf, plural, singular, isOrAre, doOrDoes, itsOrTheir, thisOrThese,
        unicodeSyntax,

        coloured, keyword,

        -- * Converting 'SDoc' into strings and outputting it
        printSDoc, printSDocLn,
        bufLeftRenderSDoc,
        pprCode,
        showSDocOneLine,
        showSDocUnsafe,
        showPprUnsafe,
        renderWithContext,
        pprDebugAndThen,

        pprInfixVar, pprPrefixVar,
        pprHsChar, pprHsString, pprHsBytes,

        primFloatSuffix, primCharSuffix, primDoubleSuffix,
        primInt8Suffix, primWord8Suffix,
        primInt16Suffix, primWord16Suffix,
        primInt32Suffix, primWord32Suffix,
        primInt64Suffix, primWord64Suffix,
        primIntSuffix, primWordSuffix,

        pprPrimChar, pprPrimInt, pprPrimWord,
        pprPrimInt8, pprPrimWord8,
        pprPrimInt16, pprPrimWord16,
        pprPrimInt32, pprPrimWord32,
        pprPrimInt64, pprPrimWord64,

        pprFastFilePath, pprFilePathString,

        -- * Controlling the style in which output is printed
        BindingSite(..),

        PprStyle(..), LabelStyle(..), PrintUnqualified(..),
        QueryQualifyName, QueryQualifyModule, QueryQualifyPackage,
        reallyAlwaysQualify, reallyAlwaysQualifyNames,
        alwaysQualify, alwaysQualifyNames, alwaysQualifyModules,
        neverQualify, neverQualifyNames, neverQualifyModules,
        alwaysQualifyPackages, neverQualifyPackages,
        QualifyName(..), queryQual,
        sdocOption,
        updSDocContext,
        SDocContext (..), sdocWithContext, defaultSDocContext,
        getPprStyle, withPprStyle, setStyleColoured,
        pprDeeper, pprDeeperList, pprSetDepth,
        codeStyle, userStyle, dumpStyle, asmStyle,
        qualName, qualModule, qualPackage,
        mkErrStyle, defaultErrStyle, defaultDumpStyle, mkDumpStyle, defaultUserStyle,
        mkUserStyle, cmdlineParserStyle, Depth(..),
        withUserStyle, withErrStyle,

        ifPprDebug, whenPprDebug, getPprDebug,

    ) where

import GHC.Prelude

import {-# SOURCE #-}   GHC.Unit.Types ( Unit, Module, moduleName )
import {-# SOURCE #-}   GHC.Unit.Module.Name( ModuleName )
import {-# SOURCE #-}   GHC.Types.Name.Occurrence( OccName )

import GHC.Utils.BufHandle (BufHandle)
import GHC.Data.FastString
import GHC.Data.STuple
import qualified GHC.Utils.Ppr as Pretty
import qualified GHC.Utils.Ppr.Colour as Col
import GHC.Utils.Ppr       ( Doc, Mode(..) )
import GHC.Serialized
import GHC.LanguageExtensions (Extension)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import qualified Data.Map as M
import Data.Int
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.String
import Data.Word
import System.IO        ( Handle )
import System.FilePath
import Text.Printf
import Numeric (showFFloat)
import Data.Graph (SCC(..))
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Time
import Data.Time.Format.ISO8601

import GHC.Fingerprint
import GHC.Show         ( showMultiLineString )
import GHC.Utils.Exception
import GHC.Exts (oneShot)

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
                -- For -ddump-foo; less verbose than in ppr-debug mode, but more than PprUser
                -- Does not assume tidied code: non-external names
                -- are printed with uniques.

  | PprCode !LabelStyle -- ^ Print code; either C or assembler

-- | Style of label pretty-printing.
--
-- When we produce C sources or headers, we have to take into account that C
-- compilers transform C labels when they convert them into symbols. For
-- example, they can add prefixes (e.g., "_" on Darwin) or suffixes (size for
-- stdcalls on Windows). So we provide two ways to pretty-print CLabels: C style
-- or Asm style.
--
data LabelStyle
   = CStyle   -- ^ C label style (used by C and LLVM backends)
   | AsmStyle -- ^ Asm label style (used by NCG backend)
   deriving (Eq,Ord,Show)

data Depth
   = AllTheWay
   | PartWay Int  -- ^ 0 => stop
   | DefaultDepth -- ^ Use 'sdocDefaultDepth' field as depth

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
type QueryQualifyPackage = Unit -> Bool

-- See Note [Printing original names] in GHC.Types.Name.Ppr
data QualifyName   -- Given P:M.T
  = NameUnqual           -- It's in scope unqualified as "T"
                         -- OR nothing called "T" is in scope

  | NameQual ModuleName  -- It's in scope qualified as "X.T"

  | NameNotInScope1      -- It's not in scope at all, but M.T is not bound
                         -- in the current scope, so we can refer to it as "M.T"

  | NameNotInScope2      -- It's not in scope at all, and M.T is already bound in
                         -- the current scope, so we must refer to it as "P:M.T"

instance Outputable QualifyName where
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

defaultUserStyle :: PprStyle
defaultUserStyle = mkUserStyle neverQualify AllTheWay

defaultDumpStyle :: PprStyle
 -- Print without qualifiers to reduce verbosity, unless -dppr-debug
defaultDumpStyle = PprDump neverQualify

mkDumpStyle :: PrintUnqualified -> PprStyle
mkDumpStyle print_unqual = PprDump print_unqual

-- | Default style for error messages, when we don't know PrintUnqualified
-- It's a bit of a hack because it doesn't take into account what's in scope
-- Only used for desugarer warnings, and typechecker errors in interface sigs
defaultErrStyle :: PprStyle
defaultErrStyle = mkErrStyle neverQualify

-- | Style for printing error messages
mkErrStyle :: PrintUnqualified -> PprStyle
mkErrStyle unqual = mkUserStyle unqual DefaultDepth

cmdlineParserStyle :: PprStyle
cmdlineParserStyle = mkUserStyle alwaysQualify AllTheWay

mkUserStyle :: PrintUnqualified -> Depth -> PprStyle
mkUserStyle unqual depth = PprUser unqual depth Uncoloured

withUserStyle :: PrintUnqualified -> Depth -> SDoc -> SDoc
withUserStyle unqual depth doc = withPprStyle (PprUser unqual depth Uncoloured) doc

withErrStyle :: PrintUnqualified -> SDoc -> SDoc
withErrStyle unqual doc =
   withPprStyle (mkErrStyle unqual) doc

setStyleColoured :: Bool -> PprStyle -> PprStyle
setStyleColoured col style =
  case style of
    PprUser q d _ -> PprUser q d c
    _             -> style
  where
    c | col       = Coloured
      | otherwise = Uncoloured

instance Outputable PprStyle where
  ppr (PprUser {})  = text "user-style"
  ppr (PprCode {})  = text "code-style"
  ppr (PprDump {})  = text "dump-style"

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
-- or 'renderWithContext'.  Avoid calling 'runSDoc' directly as it breaks the
-- abstraction layer.
newtype SDoc = SDoc' (SDocContext -> Doc)

-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
{-# COMPLETE SDoc #-}
pattern SDoc :: (SDocContext -> Doc) -> SDoc
pattern SDoc m <- SDoc' m
  where
    SDoc m = SDoc' (oneShot m)

runSDoc :: SDoc -> (SDocContext -> Doc)
runSDoc (SDoc m) = m

data SDocContext = SDC
  { sdocStyle                       :: !PprStyle
  , sdocColScheme                   :: !Col.Scheme
  , sdocLastColour                  :: !Col.PprColour
      -- ^ The most recently used colour.
      -- This allows nesting colours.
  , sdocShouldUseColor              :: !Bool
  , sdocDefaultDepth                :: !Int
  , sdocLineLength                  :: !Int
  , sdocCanUseUnicode               :: !Bool
      -- ^ True if Unicode encoding is supported
      -- and not disable by GHC_NO_UNICODE environment variable
  , sdocHexWordLiterals             :: !Bool
  , sdocPprDebug                    :: !Bool
  , sdocPrintUnicodeSyntax          :: !Bool
  , sdocPrintCaseAsLet              :: !Bool
  , sdocPrintTypecheckerElaboration :: !Bool
  , sdocPrintAxiomIncomps           :: !Bool
  , sdocPrintExplicitKinds          :: !Bool
  , sdocPrintExplicitCoercions      :: !Bool
  , sdocPrintExplicitRuntimeReps    :: !Bool
  , sdocPrintExplicitForalls        :: !Bool
  , sdocPrintPotentialInstances     :: !Bool
  , sdocPrintEqualityRelations      :: !Bool
  , sdocSuppressTicks               :: !Bool
  , sdocSuppressTypeSignatures      :: !Bool
  , sdocSuppressTypeApplications    :: !Bool
  , sdocSuppressIdInfo              :: !Bool
  , sdocSuppressCoercions           :: !Bool
  , sdocSuppressUnfoldings          :: !Bool
  , sdocSuppressVarKinds            :: !Bool
  , sdocSuppressUniques             :: !Bool
  , sdocSuppressModulePrefixes      :: !Bool
  , sdocSuppressStgExts             :: !Bool
  , sdocErrorSpans                  :: !Bool
  , sdocStarIsType                  :: !Bool
  , sdocLinearTypes                 :: !Bool
  , sdocImpredicativeTypes          :: !Bool
  , sdocPrintTypeAbbreviations      :: !Bool
  , sdocUnitIdForUser               :: !(FastString -> SDoc)
      -- ^ Used to map UnitIds to more friendly "package-version:component"
      -- strings while pretty-printing.
      --
      -- Use `GHC.Unit.State.pprWithUnitState` to set it. Users should never
      -- have to set it to pretty-print SDocs emitted by GHC, otherwise it's a
      -- bug. It's an internal field used to thread the UnitState so that the
      -- Outputable instance of UnitId can use it.
      --
      -- See Note [Pretty-printing UnitId] in "GHC.Unit" for more details.
      --
      -- Note that we use `FastString` instead of `UnitId` to avoid boring
      -- module inter-dependency issues.
  }

instance IsString SDoc where
  fromString = text

-- The lazy programmer's friend.
instance Outputable SDoc where
  ppr = id

-- | Default pretty-printing options
defaultSDocContext :: SDocContext
defaultSDocContext = SDC
  { sdocStyle                       = defaultDumpStyle
  , sdocColScheme                   = Col.defaultScheme
  , sdocLastColour                  = Col.colReset
  , sdocShouldUseColor              = False
  , sdocDefaultDepth                = 5
  , sdocLineLength                  = 100
  , sdocCanUseUnicode               = False
  , sdocHexWordLiterals             = False
  , sdocPprDebug                    = False
  , sdocPrintUnicodeSyntax          = False
  , sdocPrintCaseAsLet              = False
  , sdocPrintTypecheckerElaboration = False
  , sdocPrintAxiomIncomps           = False
  , sdocPrintExplicitKinds          = False
  , sdocPrintExplicitCoercions      = False
  , sdocPrintExplicitRuntimeReps    = False
  , sdocPrintExplicitForalls        = False
  , sdocPrintPotentialInstances     = False
  , sdocPrintEqualityRelations      = False
  , sdocSuppressTicks               = False
  , sdocSuppressTypeSignatures      = False
  , sdocSuppressTypeApplications    = False
  , sdocSuppressIdInfo              = False
  , sdocSuppressCoercions           = False
  , sdocSuppressUnfoldings          = False
  , sdocSuppressVarKinds            = False
  , sdocSuppressUniques             = False
  , sdocSuppressModulePrefixes      = False
  , sdocSuppressStgExts             = False
  , sdocErrorSpans                  = False
  , sdocStarIsType                  = False
  , sdocImpredicativeTypes          = False
  , sdocLinearTypes                 = False
  , sdocPrintTypeAbbreviations      = True
  , sdocUnitIdForUser               = ftext
  }

withPprStyle :: PprStyle -> SDoc -> SDoc
{-# INLINE CONLIKE withPprStyle #-}
withPprStyle sty d = SDoc $ \ctxt -> runSDoc d ctxt{sdocStyle=sty}

pprDeeper :: SDoc -> SDoc
pprDeeper d = SDoc $ \ctx -> case sdocStyle ctx of
  PprUser q depth c ->
   let deeper 0 = Pretty.text "..."
       deeper n = runSDoc d ctx{sdocStyle = PprUser q (PartWay (n-1)) c}
   in case depth of
         DefaultDepth -> deeper (sdocDefaultDepth ctx)
         PartWay n    -> deeper n
         AllTheWay    -> runSDoc d ctx
  _ -> runSDoc d ctx


-- | Truncate a list that is longer than the current depth.
pprDeeperList :: ([SDoc] -> SDoc) -> [SDoc] -> SDoc
pprDeeperList f ds
  | null ds   = f []
  | otherwise = SDoc work
 where
  work ctx@SDC{sdocStyle=PprUser q depth c}
   | DefaultDepth <- depth
   = work (ctx { sdocStyle = PprUser q (PartWay (sdocDefaultDepth ctx)) c })
   | PartWay 0 <- depth
   = Pretty.text "..."
   | PartWay n <- depth
   = let
        go _ [] = []
        go i (d:ds) | i >= n    = [text "...."]
                    | otherwise = d : go (i+1) ds
     in runSDoc (f (go 0 ds)) ctx{sdocStyle = PprUser q (PartWay (n-1)) c}
  work other_ctx = runSDoc (f ds) other_ctx

pprSetDepth :: Depth -> SDoc -> SDoc
pprSetDepth depth doc = SDoc $ \ctx ->
    case ctx of
        SDC{sdocStyle=PprUser q _ c} ->
            runSDoc doc ctx{sdocStyle = PprUser q depth c}
        _ ->
            runSDoc doc ctx

getPprStyle :: (PprStyle -> SDoc) -> SDoc
{-# INLINE CONLIKE getPprStyle #-}
getPprStyle df = SDoc $ \ctx -> runSDoc (df (sdocStyle ctx)) ctx

sdocWithContext :: (SDocContext -> SDoc) -> SDoc
{-# INLINE CONLIKE sdocWithContext #-}
sdocWithContext f = SDoc $ \ctx -> runSDoc (f ctx) ctx

sdocOption :: (SDocContext -> a) -> (a -> SDoc) -> SDoc
{-# INLINE CONLIKE sdocOption #-}
sdocOption f g = sdocWithContext (g . f)

updSDocContext :: (SDocContext -> SDocContext) -> SDoc -> SDoc
{-# INLINE CONLIKE updSDocContext #-}
updSDocContext upd doc
  = SDoc $ \ctx -> runSDoc doc (upd ctx)

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

userStyle ::  PprStyle -> Bool
userStyle (PprUser {}) = True
userStyle _other       = False

-- | Indicate if -dppr-debug mode is enabled
getPprDebug :: (Bool -> SDoc) -> SDoc
{-# INLINE CONLIKE getPprDebug #-}
getPprDebug d = sdocWithContext $ \ctx -> d (sdocPprDebug ctx)

-- | Says what to do with and without -dppr-debug
ifPprDebug :: SDoc -> SDoc -> SDoc
{-# INLINE CONLIKE ifPprDebug #-}
ifPprDebug yes no = getPprDebug $ \dbg -> if dbg then yes else no

-- | Says what to do with -dppr-debug; without, return empty
whenPprDebug :: SDoc -> SDoc        -- Empty for non-debug style
{-# INLINE CONLIKE whenPprDebug #-}
whenPprDebug d = ifPprDebug d empty

-- | The analog of 'Pretty.printDoc_' for 'SDoc', which tries to make sure the
--   terminal doesn't get screwed up by the ANSI color codes if an exception
--   is thrown during pretty-printing.
printSDoc :: SDocContext -> Mode -> Handle -> SDoc -> IO ()
printSDoc ctx mode handle doc =
  Pretty.printDoc_ mode cols handle (runSDoc doc ctx)
    `finally`
      Pretty.printDoc_ mode cols handle
        (runSDoc (coloured Col.colReset empty) ctx)
  where
    cols = sdocLineLength ctx

-- | Like 'printSDoc' but appends an extra newline.
printSDocLn :: SDocContext -> Mode -> Handle -> SDoc -> IO ()
printSDocLn ctx mode handle doc =
  printSDoc ctx mode handle (doc $$ text "")

-- | An efficient variant of 'printSDoc' specialized for 'LeftMode' that
-- outputs to a 'BufHandle'.
bufLeftRenderSDoc :: SDocContext -> BufHandle -> SDoc -> IO ()
bufLeftRenderSDoc ctx bufHandle doc =
  Pretty.bufLeftRender bufHandle (runSDoc doc ctx)

pprCode :: LabelStyle -> SDoc -> SDoc
{-# INLINE CONLIKE pprCode #-}
pprCode cs d = withPprStyle (PprCode cs) d

renderWithContext :: SDocContext -> SDoc -> String
renderWithContext ctx sdoc
  = let s = Pretty.style{ Pretty.mode       = PageMode False,
                          Pretty.lineLength = sdocLineLength ctx }
    in Pretty.renderStyle s $ runSDoc sdoc ctx

-- This shows an SDoc, but on one line only. It's cheaper than a full
-- showSDoc, designed for when we're getting results like "Foo.bar"
-- and "foo{uniq strictness}" so we don't want fancy layout anyway.
showSDocOneLine :: SDocContext -> SDoc -> String
showSDocOneLine ctx d
 = let s = Pretty.style{ Pretty.mode = OneLineMode,
                         Pretty.lineLength = sdocLineLength ctx } in
   Pretty.renderStyle s $
      runSDoc d ctx

showSDocUnsafe :: SDoc -> String
showSDocUnsafe sdoc = renderWithContext defaultSDocContext sdoc

showPprUnsafe :: Outputable a => a -> String
showPprUnsafe a = renderWithContext defaultSDocContext (ppr a)


pprDebugAndThen :: SDocContext -> (String -> a) -> SDoc -> SDoc -> a
pprDebugAndThen ctx cont heading pretty_msg
 = cont (renderWithContext ctx doc)
 where
     doc = withPprStyle defaultDumpStyle (sep [heading, nest 2 pretty_msg])


isEmpty :: SDocContext -> SDoc -> Bool
isEmpty ctx sdoc = Pretty.isEmpty $ runSDoc sdoc (ctx {sdocPprDebug = True})

docToSDoc :: Doc -> SDoc
docToSDoc d = SDoc (\_ -> d)

empty    :: SDoc
char     :: Char       -> SDoc
text     :: String     -> SDoc
ftext    :: FastString -> SDoc
ptext    :: PtrString  -> SDoc
ztext    :: FastZString -> SDoc
int      :: Int        -> SDoc
integer  :: Integer    -> SDoc
word     :: Integer    -> SDoc
float    :: Float      -> SDoc
double   :: Double     -> SDoc
rational :: Rational   -> SDoc

{-# INLINE CONLIKE empty #-}
empty       = docToSDoc $ Pretty.empty
{-# INLINE CONLIKE char #-}
char c      = docToSDoc $ Pretty.char c

{-# INLINE CONLIKE text #-}   -- Inline so that the RULE Pretty.text will fire
text s      = docToSDoc $ Pretty.text s

{-# INLINE CONLIKE ftext #-}
ftext s     = docToSDoc $ Pretty.ftext s
{-# INLINE CONLIKE ptext #-}
ptext s     = docToSDoc $ Pretty.ptext s
{-# INLINE CONLIKE ztext #-}
ztext s     = docToSDoc $ Pretty.ztext s
{-# INLINE CONLIKE int #-}
int n       = docToSDoc $ Pretty.int n
{-# INLINE CONLIKE integer #-}
integer n   = docToSDoc $ Pretty.integer n
{-# INLINE CONLIKE float #-}
float n     = docToSDoc $ Pretty.float n
{-# INLINE CONLIKE double #-}
double n    = docToSDoc $ Pretty.double n
{-# INLINE CONLIKE rational #-}
rational n  = docToSDoc $ Pretty.rational n
              -- See Note [Print Hexadecimal Literals] in GHC.Utils.Ppr
{-# INLINE CONLIKE word #-}
word n      = sdocOption sdocHexWordLiterals $ \case
               True  -> docToSDoc $ Pretty.hex n
               False -> docToSDoc $ Pretty.integer n

-- | @doublePrec p n@ shows a floating point number @n@ with @p@
-- digits of precision after the decimal point.
doublePrec :: Int -> Double -> SDoc
doublePrec p n = text (showFFloat (Just p) n "")

parens, braces, brackets, quotes, quote,
        doubleQuotes, angleBrackets :: SDoc -> SDoc

{-# INLINE CONLIKE parens #-}
parens d        = SDoc $ Pretty.parens . runSDoc d
{-# INLINE CONLIKE braces #-}
braces d        = SDoc $ Pretty.braces . runSDoc d
{-# INLINE CONLIKE brackets #-}
brackets d      = SDoc $ Pretty.brackets . runSDoc d
{-# INLINE CONLIKE quote #-}
quote d         = SDoc $ Pretty.quote . runSDoc d
{-# INLINE CONLIKE doubleQuotes #-}
doubleQuotes d  = SDoc $ Pretty.doubleQuotes . runSDoc d
{-# INLINE CONLIKE angleBrackets #-}
angleBrackets d = char '<' <> d <> char '>'

cparen :: Bool -> SDoc -> SDoc
{-# INLINE CONLIKE cparen #-}
cparen b d = SDoc $ Pretty.maybeParens b . runSDoc d

-- 'quotes' encloses something in single quotes...
-- but it omits them if the thing begins or ends in a single quote
-- so that we don't get `foo''.  Instead we just have foo'.
quotes d = sdocOption sdocCanUseUnicode $ \case
   True  -> char '‘' <> d <> char '’'
   False -> SDoc $ \sty ->
      let pp_d = runSDoc d sty
          str  = show pp_d
      in case str of
         []                   -> Pretty.quotes pp_d
         '\'' : _             -> pp_d
         _ | '\'' <- last str -> pp_d
           | otherwise        -> Pretty.quotes pp_d

semi, comma, colon, equals, space, dcolon, underscore, dot, vbar :: SDoc
arrow, lollipop, larrow, darrow, arrowt, larrowt, arrowtt, larrowtt, lambda :: SDoc
lparen, rparen, lbrack, rbrack, lbrace, rbrace, blankLine :: SDoc

blankLine  = docToSDoc Pretty.emptyText
dcolon     = unicodeSyntax (char '∷') (docToSDoc $ Pretty.text "::")
arrow      = unicodeSyntax (char '→') (docToSDoc $ Pretty.text "->")
lollipop   = unicodeSyntax (char '⊸') (docToSDoc $ Pretty.text "%1 ->")
larrow     = unicodeSyntax (char '←') (docToSDoc $ Pretty.text "<-")
darrow     = unicodeSyntax (char '⇒') (docToSDoc $ Pretty.text "=>")
arrowt     = unicodeSyntax (char '⤚') (docToSDoc $ Pretty.text ">-")
larrowt    = unicodeSyntax (char '⤙') (docToSDoc $ Pretty.text "-<")
arrowtt    = unicodeSyntax (char '⤜') (docToSDoc $ Pretty.text ">>-")
larrowtt   = unicodeSyntax (char '⤛') (docToSDoc $ Pretty.text "-<<")
lambda     = unicodeSyntax (char 'λ') (char '\\')
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

mulArrow :: SDoc -> SDoc
mulArrow d = text "%" <> d <+> arrow


forAllLit :: SDoc
forAllLit = unicodeSyntax (char '∀') (text "forall")

bullet :: SDoc
bullet = unicode (char '•') (char '*')

unicodeSyntax :: SDoc -> SDoc -> SDoc
unicodeSyntax unicode plain =
   sdocOption sdocCanUseUnicode $ \can_use_unicode ->
   sdocOption sdocPrintUnicodeSyntax $ \print_unicode_syntax ->
    if can_use_unicode && print_unicode_syntax
    then unicode
    else plain

unicode :: SDoc -> SDoc -> SDoc
unicode unicode plain = sdocOption sdocCanUseUnicode $ \case
   True  -> unicode
   False -> plain

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

{-# INLINE CONLIKE nest #-}
nest n d    = SDoc $ Pretty.nest n . runSDoc d
{-# INLINE CONLIKE (<>) #-}
(<>) d1 d2  = SDoc $ \ctx -> (Pretty.<>)  (runSDoc d1 ctx) (runSDoc d2 ctx)
{-# INLINE CONLIKE (<+>) #-}
(<+>) d1 d2 = SDoc $ \ctx -> (Pretty.<+>) (runSDoc d1 ctx) (runSDoc d2 ctx)
{-# INLINE CONLIKE ($$) #-}
($$) d1 d2  = SDoc $ \ctx -> (Pretty.$$)  (runSDoc d1 ctx) (runSDoc d2 ctx)
{-# INLINE CONLIKE ($+$) #-}
($+$) d1 d2 = SDoc $ \ctx -> (Pretty.$+$) (runSDoc d1 ctx) (runSDoc d2 ctx)

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


-- Inline all those wrappers to help ensure we create lists of Doc, not of SDoc
-- later applied to the same SDocContext. It helps the worker/wrapper
-- transformation extracting only the required fields from the SDocContext.
{-# INLINE CONLIKE hcat #-}
hcat ds = SDoc $ \ctx -> Pretty.hcat [runSDoc d ctx | d <- ds]
{-# INLINE CONLIKE hsep #-}
hsep ds = SDoc $ \ctx -> Pretty.hsep [runSDoc d ctx | d <- ds]
{-# INLINE CONLIKE vcat #-}
vcat ds = SDoc $ \ctx -> Pretty.vcat [runSDoc d ctx | d <- ds]
{-# INLINE CONLIKE sep #-}
sep ds  = SDoc $ \ctx -> Pretty.sep  [runSDoc d ctx | d <- ds]
{-# INLINE CONLIKE cat #-}
cat ds  = SDoc $ \ctx -> Pretty.cat  [runSDoc d ctx | d <- ds]
{-# INLINE CONLIKE fsep #-}
fsep ds = SDoc $ \ctx -> Pretty.fsep [runSDoc d ctx | d <- ds]
{-# INLINE CONLIKE fcat #-}
fcat ds = SDoc $ \ctx -> Pretty.fcat [runSDoc d ctx | d <- ds]

hang :: SDoc  -- ^ The header
      -> Int  -- ^ Amount to indent the hung body
      -> SDoc -- ^ The hung body, indented and placed below the header
      -> SDoc
{-# INLINE CONLIKE hang #-}
hang d1 n d2   = SDoc $ \sty -> Pretty.hang (runSDoc d1 sty) n (runSDoc d2 sty)

-- | This behaves like 'hang', but does not indent the second document
-- when the header is empty.
hangNotEmpty :: SDoc -> Int -> SDoc -> SDoc
{-# INLINE CONLIKE hangNotEmpty #-}
hangNotEmpty d1 n d2 =
    SDoc $ \ctx -> Pretty.hangNotEmpty (runSDoc d1 ctx) n (runSDoc d2 ctx)

punctuate :: SDoc   -- ^ The punctuation
          -> [SDoc] -- ^ The list that will have punctuation added between every adjacent pair of elements
          -> [SDoc] -- ^ Punctuated list
punctuate _ []     = []
punctuate p (d:ds) = go d ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d <> p) : go e es

ppWhen, ppUnless :: Bool -> SDoc -> SDoc
{-# INLINE CONLIKE ppWhen #-}
ppWhen True  doc = doc
ppWhen False _   = empty

{-# INLINE CONLIKE ppUnless #-}
ppUnless True  _   = empty
ppUnless False doc = doc

{-# INLINE CONLIKE ppWhenOption #-}
ppWhenOption :: (SDocContext -> Bool) -> SDoc -> SDoc
ppWhenOption f doc = sdocOption f $ \case
   True  -> doc
   False -> empty

{-# INLINE CONLIKE ppUnlessOption #-}
ppUnlessOption :: (SDocContext -> Bool) -> SDoc -> SDoc
ppUnlessOption f doc = sdocOption f $ \case
   True  -> empty
   False -> doc

-- | Apply the given colour\/style for the argument.
--
-- Only takes effect if colours are enabled.
coloured :: Col.PprColour -> SDoc -> SDoc
coloured col sdoc = sdocOption sdocShouldUseColor $ \case
   True -> SDoc $ \case
      ctx@SDC{ sdocLastColour = lastCol, sdocStyle = PprUser _ _ Coloured } ->
         let ctx' = ctx{ sdocLastColour = lastCol `mappend` col } in
         Pretty.zeroWidthText (Col.renderColour col)
           Pretty.<> runSDoc sdoc ctx'
           Pretty.<> Pretty.zeroWidthText (Col.renderColourAfresh lastCol)
      ctx -> runSDoc sdoc ctx
   False -> sdoc

keyword :: SDoc -> SDoc
keyword = coloured Col.colBold

-----------------------------------------------------------------------
-- The @Outputable@ class
-----------------------------------------------------------------------

-- | Class designating that some type has an 'SDoc' representation
class Outputable a where
    ppr :: a -> SDoc

instance Outputable Char where
    ppr c = text [c]

instance Outputable Bool where
    ppr True  = text "True"
    ppr False = text "False"

instance Outputable Ordering where
    ppr LT = text "LT"
    ppr EQ = text "EQ"
    ppr GT = text "GT"

instance Outputable Int32 where
   ppr n = integer $ fromIntegral n

instance Outputable Int64 where
   ppr n = integer $ fromIntegral n

instance Outputable Int where
    ppr n = int n

instance Outputable Integer where
    ppr n = integer n

instance Outputable Word16 where
    ppr n = integer $ fromIntegral n

instance Outputable Word32 where
    ppr n = integer $ fromIntegral n

instance Outputable Word64 where
    ppr n = integer $ fromIntegral n

instance Outputable Word where
    ppr n = integer $ fromIntegral n

instance Outputable Float where
    ppr f = float f

instance Outputable Double where
    ppr f = double f

instance Outputable () where
    ppr _ = text "()"

instance Outputable UTCTime where
    ppr = text . formatShow iso8601Format

instance (Outputable a) => Outputable [a] where
    ppr xs = brackets (fsep (punctuate comma (map ppr xs)))

instance (Outputable a) => Outputable (NonEmpty a) where
    ppr = ppr . NEL.toList

instance (Outputable a) => Outputable (Set a) where
    ppr s = braces (fsep (punctuate comma (map ppr (Set.toList s))))

instance Outputable IntSet.IntSet where
    ppr s = braces (fsep (punctuate comma (map ppr (IntSet.toList s))))

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr (x,y) = parens (sep [ppr x <> comma, ppr y])

instance (Outputable a, Outputable b) => Outputable (SPair a b) where
    ppr = ppr . toPair

instance Outputable a => Outputable (Maybe a) where
    ppr Nothing  = text "Nothing"
    ppr (Just x) = text "Just" <+> ppr x

instance (Outputable a, Outputable b) => Outputable (Either a b) where
    ppr (Left x)  = text "Left"  <+> ppr x
    ppr (Right y) = text "Right" <+> ppr y

-- ToDo: may not be used
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    ppr (x,y,z) =
      parens (sep [ppr x <> comma,
                   ppr y <> comma,
                   ppr z ])

instance (Outputable a, Outputable b, Outputable c) => Outputable (STriple a b c) where
    ppr = ppr . toTriple

instance (Outputable a, Outputable b, Outputable c, Outputable d) =>
         Outputable (a, b, c, d) where
    ppr (a,b,c,d) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d])

instance (Outputable a, Outputable b, Outputable c, Outputable d) => Outputable (SQuad a b c d) where
    ppr = ppr . toQuad

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e) =>
         Outputable (a, b, c, d, e) where
    ppr (a,b,c,d,e) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e, Outputable f) =>
         Outputable (a, b, c, d, e, f) where
    ppr (a,b,c,d,e,f) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e, Outputable f, Outputable g) =>
         Outputable (a, b, c, d, e, f, g) where
    ppr (a,b,c,d,e,f,g) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f <> comma,
                   ppr g])

instance Outputable FastString where
    ppr fs = ftext fs           -- Prints an unadorned string,
                                -- no double quotes or anything

deriving newtype instance Outputable NonDetFastString
deriving newtype instance Outputable LexicalFastString

instance (Outputable key, Outputable elt) => Outputable (M.Map key elt) where
    ppr m = ppr (M.toList m)

instance (Outputable elt) => Outputable (IM.IntMap elt) where
    ppr m = ppr (IM.toList m)

instance Outputable Fingerprint where
    ppr (Fingerprint w1 w2) = text (printf "%016x%016x" w1 w2)

instance Outputable a => Outputable (SCC a) where
   ppr (AcyclicSCC v) = text "NONREC" $$ (nest 3 (ppr v))
   ppr (CyclicSCC vs) = text "REC" $$ (nest 3 (vcat (map ppr vs)))

instance Outputable Serialized where
    ppr (Serialized the_type bytes) = int (length bytes) <+> text "of type" <+> text (show the_type)

instance Outputable Extension where
    ppr = text . show

-----------------------------------------------------------------------
-- The @OutputableP@ class
-----------------------------------------------------------------------

-- Note [The OutputableP class]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- SDoc has become the common type to
--    * display messages in the terminal
--    * dump outputs (Cmm, Asm, C, etc.)
--    * return messages to ghc-api clients
--
-- SDoc is a kind of state Monad: SDoc ~ State SDocContext Doc
-- I.e. to render a SDoc, a SDocContext must be provided.
--
-- SDocContext contains legit rendering options (e.g., line length, color and
-- unicode settings). Sadly SDocContext ended up also being used to thread
-- values that were considered bothersome to thread otherwise:
--    * current HomeModule: to decide if module names must be printed qualified
--    * current UnitState: to print unit-ids as "packagename-version:component"
--    * target platform: to render labels, instructions, etc.
--    * selected backend: to display CLabel as C labels or Asm labels
--
-- In fact the whole compiler session state that is DynFlags was passed in
-- SDocContext and these values were retrieved from it.
--
-- The Outputable class makes SDoc creation easy for many values by providing
-- the ppr method:
--
--    class Outputable a where
--       ppr :: a -> SDoc
--
-- Almost every type is Outputable in the compiler and it seems great because it
-- is similar to the Show class. But it's a fallacious simplicity because `SDoc`
-- needs a `SDocContext` to be transformed into a renderable `Doc`: who is going
-- to provide the SDocContext with the correct values in it?
--
--    E.g. if a SDoc is returned in an exception, how could we know the home
--    module at the time it was thrown?
--
-- A workaround is to pass dummy values (no home module, empty UnitState) at SDoc
-- rendering time and to hope that the code that produced the SDoc has updated
-- the SDocContext with meaningful values (e.g. using withPprStyle or
-- pprWithUnitState). If the context isn't correctly updated, a dummy value is
-- used and the printed result isn't what we expected. Note that the compiler
-- doesn't help us finding spots where we need to update the SDocContext.
--
-- In some cases we can't pass a dummy value because we can't create one. For
-- example, how can we create a dummy Platform value? In the old days, GHC only
-- supported a single Platform set when it was built, so we could use it without
-- any risk of mistake. But now GHC starts supporting several Platform in the
-- same session so it becomes an issue. We could be tempted to use the
-- workaround described above by using "undefined" as a dummy Platform value.
-- However in this case, if we forget to update it we will get a runtime
-- error/crash. We could use "Maybe Platform" and die with a better error
-- message at places where we really really need to know if we are on Windows or
-- not, or if we use 32- or 64-bit. Still the compiler would not help us in
-- finding spots where to update the context with a valid Platform.
--
-- So finally here comes the OutputableP class:
--
--    class OutputableP env a where
--       pdoc :: env -> a -> SDoc
--
-- OutputableP forces us to thread an environment necessary to print a value.
-- For now we only use it to thread a Platform environment, so we have several
-- "Outputable Platform XYZ" instances. In the future we could imagine using a
-- Has class to retrieve a value from a generic environment to make the code
-- more composable. E.g.:
--
--    instance Has Platform env => OutputableP env XYZ where
--       pdoc env a = ... (getter env :: Platform)
--
-- A drawback of this approach over Outputable is that we have to thread an
-- environment explicitly to use "pdoc" and it's more cumbersome. But it's the
-- price to pay to have some help from the compiler to ensure that we... thread
-- an environment down to the places where we need it, i.e. where SDoc are
-- created (not rendered). On the other hand, it makes life easier for SDoc
-- renderers as they only have to deal with pretty-printing related options in
-- SDocContext.
--
-- TODO:
--
-- 1) we could use OutputableP to thread a UnitState and replace the Outputable
-- instance of UnitId with:
--
--       instance OutputableP UnitState UnitId where ...
--
--    This would allow the removal of the `sdocUnitIdForUser` field.
--
--    Be warned: I've tried to do it, but there are A LOT of other Outputable
--    instances depending on UnitId's one. In particular:
--       UnitId <- Unit <- Module <- Name <- Var <- Core.{Type,Expr} <- ...
--
-- 2) Use it to pass the HomeModule (but I fear it will be as difficult as for
-- UnitId).
--
--

-- | Outputable class with an additional environment value
--
-- See Note [The OutputableP class]
class OutputableP env a where
   pdoc :: env -> a -> SDoc

-- | Wrapper for types having a Outputable instance when an OutputableP instance
-- is required.
newtype PDoc a = PDoc a

instance Outputable a => OutputableP env (PDoc a) where
   pdoc _ (PDoc a) = ppr a

instance OutputableP env a => OutputableP env [a] where
   pdoc env xs = ppr (fmap (pdoc env) xs)

instance OutputableP env a => OutputableP env (Maybe a) where
   pdoc env xs = ppr (fmap (pdoc env) xs)

instance (OutputableP env a, OutputableP env b) => OutputableP env (a, b) where
    pdoc env (a,b) = ppr (pdoc env a, pdoc env b)

instance (OutputableP env a, OutputableP env b, OutputableP env c) => OutputableP env (a, b, c) where
    pdoc env (a,b,c) = ppr (pdoc env a, pdoc env b, pdoc env c)


instance (OutputableP env key, OutputableP env elt) => OutputableP env (M.Map key elt) where
    pdoc env m = ppr $ fmap (\(x,y) -> (pdoc env x, pdoc env y)) $ M.toList m

instance OutputableP env a => OutputableP env (SCC a) where
   pdoc env scc = ppr (fmap (pdoc env) scc)

instance OutputableP env SDoc where
   pdoc _ x = x

instance (OutputableP env a) => OutputableP env (Set a) where
    pdoc env s = braces (fsep (punctuate comma (map (pdoc env) (Set.toList s))))


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
-- Also see Note [Binding-site specific printing] in "GHC.Core.Ppr"
data BindingSite
    = LambdaBind  -- ^ The x in   (\x. e)
    | CaseBind    -- ^ The x in   case scrut of x { (y,z) -> ... }
    | CasePatBind -- ^ The y,z in case scrut of x { (y,z) -> ... }
    | LetBind     -- ^ The x in   (let x = rhs in e)

-- | When we print a binder, we often want to print its type too.
-- The @OutputableBndr@ class encapsulates this idea.
class Outputable a => OutputableBndr a where
   pprBndr :: BindingSite -> a -> SDoc
   pprBndr _b x = ppr x

   pprPrefixOcc, pprInfixOcc :: a -> SDoc
      -- Print an occurrence of the name, suitable either in the
      -- prefix position of an application, thus   (f a b) or  ((+) x)
      -- or infix position,                 thus   (a `f` b) or  (x + y)

   bndrIsJoin_maybe :: a -> Maybe Int
   bndrIsJoin_maybe _ = Nothing
      -- When pretty-printing we sometimes want to find
      -- whether the binder is a join point.  You might think
      -- we could have a function of type (a->Var), but Var
      -- isn't available yet, alas

{-
************************************************************************
*                                                                      *
\subsection{Random printing helpers}
*                                                                      *
************************************************************************
-}

-- We have 31-bit Chars and will simply use Show instances of Char and String.

-- | Special combinator for showing character literals.
pprHsChar :: Char -> SDoc
pprHsChar c | c > '\x10ffff' = char '\\' <> text (show (fromIntegral (ord c) :: Word32))
            | otherwise      = text (show c)

-- | Special combinator for showing string literals.
pprHsString :: FastString -> SDoc
pprHsString fs = vcat (map text (showMultiLineString (unpackFS fs)))

-- | Special combinator for showing bytestring literals.
pprHsBytes :: ByteString -> SDoc
pprHsBytes bs = let escaped = concatMap escape $ BS.unpack bs
                in vcat (map text (showMultiLineString escaped)) <> char '#'
    where escape :: Word8 -> String
          escape w = let c = chr (fromIntegral w)
                     in if isAscii c
                        then [c]
                        else '\\' : show w

-- Postfix modifiers for unboxed literals.
-- See Note [Printing of literals in Core] in "GHC.Types.Literal".
primCharSuffix, primFloatSuffix, primDoubleSuffix,
  primIntSuffix, primWordSuffix,
  primInt8Suffix, primWord8Suffix,
  primInt16Suffix, primWord16Suffix,
  primInt32Suffix, primWord32Suffix,
  primInt64Suffix, primWord64Suffix
  :: SDoc
primCharSuffix   = char '#'
primFloatSuffix  = char '#'
primIntSuffix    = char '#'
primDoubleSuffix = text "##"
primWordSuffix   = text "##"
primInt8Suffix   = text "#8"
primWord8Suffix  = text "##8"
primInt16Suffix  = text "#16"
primWord16Suffix = text "##16"
primInt32Suffix  = text "#32"
primWord32Suffix = text "##32"
primInt64Suffix  = text "#64"
primWord64Suffix = text "##64"

-- | Special combinator for showing unboxed literals.
pprPrimChar :: Char -> SDoc
pprPrimInt, pprPrimWord,
  pprPrimInt8, pprPrimWord8,
  pprPrimInt16, pprPrimWord16,
  pprPrimInt32, pprPrimWord32,
  pprPrimInt64, pprPrimWord64
  :: Integer -> SDoc
pprPrimChar c   = pprHsChar c <> primCharSuffix
pprPrimInt i    = integer i   <> primIntSuffix
pprPrimWord w   = word    w   <> primWordSuffix
pprPrimInt8 i   = integer i   <> primInt8Suffix
pprPrimInt16 i  = integer i   <> primInt16Suffix
pprPrimInt32 i  = integer i   <> primInt32Suffix
pprPrimInt64 i  = integer i   <> primInt64Suffix
pprPrimWord8 w  = word    w   <> primWord8Suffix
pprPrimWord16 w = word    w   <> primWord16Suffix
pprPrimWord32 w = word    w   <> primWord32Suffix
pprPrimWord64 w = word    w   <> primWord64Suffix

---------------------
-- Put a name in parens if it's an operator
pprPrefixVar :: Bool -> SDoc -> SDoc
pprPrefixVar is_operator pp_v
  | is_operator = parens pp_v
  | otherwise   = pp_v

-- Put a name in backquotes if it's not an operator
pprInfixVar :: Bool -> SDoc -> SDoc
pprInfixVar is_operator pp_v
  | is_operator = pp_v
  | otherwise   = char '`' <> pp_v <> char '`'

---------------------
pprFastFilePath :: FastString -> SDoc
pprFastFilePath path = text $ normalise $ unpackFS path

-- | Normalise, escape and render a string representing a path
--
-- e.g. "c:\\whatever"
pprFilePathString :: FilePath -> SDoc
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

pprWithCommas :: (a -> SDoc) -- ^ The pretty printing function to use
              -> [a]         -- ^ The things to be pretty printed
              -> SDoc        -- ^ 'SDoc' where the things have been pretty printed,
                             -- comma-separated and finally packed into a paragraph.
pprWithCommas pp xs = fsep (punctuate comma (map pp xs))

pprWithBars :: (a -> SDoc) -- ^ The pretty printing function to use
            -> [a]         -- ^ The things to be pretty printed
            -> SDoc        -- ^ 'SDoc' where the things have been pretty printed,
                           -- bar-separated and finally packed into a paragraph.
pprWithBars pp xs = fsep (intersperse vbar (map pp xs))

-- | Returns the separated concatenation of the pretty printed things.
interppSP  :: Outputable a => [a] -> SDoc
interppSP  xs = sep (map ppr xs)

-- | Returns the comma-separated concatenation of the pretty printed things.
interpp'SP :: Outputable a => [a] -> SDoc
interpp'SP xs = interpp'SP' ppr xs

interpp'SP' :: (a -> SDoc) -> [a] -> SDoc
interpp'SP' f xs = sep (punctuate comma (map f xs))

-- | Returns the comma-separated concatenation of the quoted pretty printed things.
--
-- > [x,y,z]  ==>  `x', `y', `z'
pprQuotedList :: Outputable a => [a] -> SDoc
pprQuotedList = quotedList . map ppr

quotedList :: [SDoc] -> SDoc
quotedList xs = fsep (punctuate comma (map quotes xs))

quotedListWithOr :: [SDoc] -> SDoc
-- [x,y,z]  ==>  `x', `y' or `z'
quotedListWithOr xs@(_:_:_) = quotedList (init xs) <+> text "or" <+> quotes (last xs)
quotedListWithOr xs = quotedList xs

quotedListWithNor :: [SDoc] -> SDoc
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

intWithCommas :: Integral a => a -> SDoc
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
speakNth :: Int -> SDoc
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
speakN :: Int -> SDoc
speakN 0 = text "none"  -- E.g.  "they have none"
speakN 1 = text "one"   -- E.g.  "they have one"
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
speakNOf :: Int -> SDoc -> SDoc
speakNOf 0 d = text "no" <+> d <> char 's'
speakNOf 1 d = text "one" <+> d                 -- E.g. "one argument"
speakNOf n d = speakN n <+> d <> char 's'               -- E.g. "three arguments"

-- | Determines the pluralisation suffix appropriate for the length of a list:
--
-- > plural [] = char 's'
-- > plural ["Hello"] = empty
-- > plural ["Hello", "World"] = char 's'
plural :: [a] -> SDoc
plural [_] = empty  -- a bit frightening, but there you are
plural _   = char 's'

-- | Determines the singular verb suffix appropriate for the length of a list:
--
-- > singular [] = empty
-- > singular["Hello"] = char 's'
-- > singular ["Hello", "World"] = empty
singular :: [a] -> SDoc
singular [_] = char 's'
singular _   = empty

-- | Determines the form of to be appropriate for the length of a list:
--
-- > isOrAre [] = text "are"
-- > isOrAre ["Hello"] = text "is"
-- > isOrAre ["Hello", "World"] = text "are"
isOrAre :: [a] -> SDoc
isOrAre [_] = text "is"
isOrAre _   = text "are"

-- | Determines the form of to do appropriate for the length of a list:
--
-- > doOrDoes [] = text "do"
-- > doOrDoes ["Hello"] = text "does"
-- > doOrDoes ["Hello", "World"] = text "do"
doOrDoes :: [a] -> SDoc
doOrDoes [_] = text "does"
doOrDoes _   = text "do"

-- | Determines the form of possessive appropriate for the length of a list:
--
-- > itsOrTheir [x]   = text "its"
-- > itsOrTheir [x,y] = text "their"
-- > itsOrTheir []    = text "their"  -- probably avoid this
itsOrTheir :: [a] -> SDoc
itsOrTheir [_] = text "its"
itsOrTheir _   = text "their"


-- | Determines the form of subject appropriate for the length of a list:
--
-- > thisOrThese [x]   = text "This"
-- > thisOrThese [x,y] = text "These"
-- > thisOrThese []    = text "These"  -- probably avoid this
thisOrThese :: [a] -> SDoc
thisOrThese [_] = text "This"
thisOrThese _   = text "These"
