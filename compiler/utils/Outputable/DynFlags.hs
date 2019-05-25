module Outputable.DynFlags (
        -- * Type classes
        Outputable(..), OutputableBndr(..),

        -- * Pretty printing combinators
        SDoc, runSDoc, initSDocContext,
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
        printSDoc, printSDocLn, printForUser, printForUserPartWay,
        printForC, bufLeftRenderSDoc,
        pprCode, mkCodeStyle,
        showSDoc, showSDocUnsafe, showSDocOneLine,
        showSDocForUser, showSDocDebug, showSDocDump, showSDocDumpOneLine,
        showSDocUnqual, showPpr,
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
        mkErrStyle, defaultErrStyle, defaultDumpStyle, mkDumpStyle, defaultUserStyle,
        mkUserStyle, cmdlineParserStyle, Depth(..),

        ifPprDebug, whenPprDebug, getPprDebug,

        -- * Error handling and debugging utilities
        pprPanic, pprSorry, assertPprPanic, pprPgmError,
        pprTrace, pprTraceDebug, pprTraceIt, warnPprTrace, pprSTrace,
        pprTraceException, pprTraceM,
        trace, pgmError, panic, sorry, assertPanic,
        pprDebugAndThen, callStackDoc,
    ) where

import GhcPrelude

import DynFlags( DynFlags, hasPprDebug, hasNoDebugOutput,
                 targetPlatform, pprUserLength, pprCols,
                 useUnicode, useUnicodeSyntax, useStarIsType,
                 shouldUseColor, unsafeGlobalDynFlags,
                 shouldUseHexWordLiterals )
import Module( UnitId, Module, ModuleName, moduleName )
import OccName( OccName )

import BufWrite (BufHandle)
import FastString
import qualified Pretty
import Util
import Platform
import qualified PprColour as Col
import Pretty           ( Doc, Mode(..) )
import Panic
import Outputtable
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
import Data.String
import Data.Word
import System.IO        ( Handle )
import System.FilePath
import Text.Printf
import Numeric (showFFloat)
import Data.Graph (SCC(..))
import Data.List (intersperse)

import GHC.Fingerprint
import GHC.Show         ( showMultiLineString )
import GHC.Stack        ( callStack, prettyCallStack )
import Control.Monad.IO.Class
import Exception

defaultUserStyle :: DynFlags -> PprStyle
defaultUserStyle = defaultUserStyle' . hasPprDebug

defaultDumpStyle :: DynFlags -> PprStyle
 -- Print without qualifiers to reduce verbosity, unless -dppr-debug
defaultDumpStyle = defaultDumpStyle' . hasPprDebug

mkDumpStyle :: DynFlags -> PrintUnqualified -> PprStyle
mkDumpStyle dflags print_unqual = mkDumpStyle (hasPprDebug dflags) print_unqual

defaultErrStyle :: DynFlags -> PprStyle
-- | Default style for error messages, when we don't know PrintUnqualified
-- It's a bit of a hack because it doesn't take into account what's in scope
-- Only used for desugarer warnings, and typechecker errors in interface sigs
-- NB that -dppr-debug will still get into PprDebug style
defaultErrStyle dflags = mkErrStyle dflags neverQualify

-- | Style for printing error messages
mkErrStyle :: DynFlags -> PrintUnqualified -> PprStyle
mkErrStyle dflags qual = mkErrStyle' (hasPprDebug dflags) qual

cmdlineParserStyle :: DynFlags -> PprStyle
cmdlineParserStyle dflags = cmdlineParserStyle' $ hasPprDebug dflags

type SDoc = SDoc' DynFlags

type SDocContext = SDocContext' DynFlags

showPpr :: Outputable a => DynFlags -> a -> String
showPpr dflags thing = showSDoc dflags (ppr thing)

showSDocUnqual :: DynFlags -> SDoc' r -> String
-- Only used by Haddock
showSDocUnqual dflags sdoc = showSDoc dflags sdoc

showSDocForUser :: DynFlags -> PrintUnqualified -> SDoc' r -> String
-- Allows caller to specify the PrintUnqualified to use
showSDocForUser dflags unqual doc
 = renderWithStyle dflags doc (mkUserStyle dflags unqual AllTheWay)

-- | An efficient variant of 'printSDoc' specialized for 'LeftMode' that
-- outputs to a 'BufHandle'.
bufLeftRenderSDoc :: DynFlags -> BufHandle -> PprStyle -> SDoc' r -> IO ()
bufLeftRenderSDoc dflags bufHandle sty doc =
  Pretty.bufLeftRender bufHandle (runSDoc doc (initSDocContext dflags sty))

-- | The analog of 'Pretty.printDoc_' for 'SDoc', which tries to make sure the
--   terminal doesn't get screwed up by the ANSI color codes if an exception
--   is thrown during pretty-printing.
printSDoc :: Mode -> DynFlags -> Handle -> PprStyle -> SDoc' r -> IO ()
printSDoc mode dflags handle sty doc =
  Pretty.printDoc_ mode cols handle (runSDoc doc ctx)
    `finally`
      Pretty.printDoc_ mode cols handle
        (runSDoc (coloured Col.colReset empty) ctx)
  where
    cols = pprCols dflags
    ctx = initSDocContext dflags sty

-- | Like 'printSDoc' but appends an extra newline.
printSDocLn :: Mode -> DynFlags -> Handle -> PprStyle -> SDoc' r -> IO ()
printSDocLn mode dflags handle sty doc =
  printSDoc mode dflags handle sty (doc $$ text "")

printForUser :: DynFlags -> Handle -> PrintUnqualified -> SDoc' r -> IO ()
printForUser dflags handle unqual doc
  = printSDocLn PageMode dflags handle
               (mkUserStyle dflags unqual AllTheWay) doc

printForUserPartWay :: DynFlags -> Handle -> Int -> PrintUnqualified -> SDoc' r
                    -> IO ()
printForUserPartWay dflags handle d unqual doc
  = printSDocLn PageMode dflags handle
                (mkUserStyle dflags unqual (PartWay d)) doc

-- | Like 'printSDocLn' but specialized with 'LeftMode' and
-- @'PprCode' 'CStyle'@.  This is typically used to output C-- code.
printForC :: DynFlags -> Handle -> SDoc' r -> IO ()
printForC dflags handle doc =
  printSDocLn LeftMode dflags handle (PprCode CStyle) doc

-- Can't make SDoc an instance of Show because SDoc is just a function type
-- However, Doc *is* an instance of Show
-- showSDoc just blasts it out as a string
showSDoc :: DynFlags -> SDoc' r -> String
showSDoc dflags sdoc = renderWithStyle dflags sdoc (defaultUserStyle dflags)

-- showSDocUnsafe is unsafe, because `unsafeGlobalDynFlags` might not be
-- initialised yet.
showSDocUnsafe :: SDoc' r -> String
showSDocUnsafe sdoc = showSDoc unsafeGlobalDynFlags sdoc

showSDocDump :: DynFlags -> SDoc' r -> String
showSDocDump dflags d = renderWithStyle dflags d (defaultDumpStyle dflags)

-- See haddock on worker
showSDocOneLine :: DynFlags -> SDoc' r -> String
showSDocOneLine dflags d
  showSDocOneLine' (defaultUserStyle dflags) dflags d

showSDocDumpOneLine :: DynFlags -> SDoc' r -> String
showSDocDumpOneLine dflags d =
  showSDocDumpOneLine' (defaultDumpStyle dflags) dflags d

isEmpty :: DynFlags -> SDoc' r -> Bool
isEmpty dflags sdoc = Pretty.isEmpty $ runSDoc sdoc dummySDocContext
   where dummySDocContext = initSDocContext dflags PprDebug

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

pprPanic :: HasCallStack => String -> SDoc' r -> a
-- ^ Throw an exception saying "bug in GHC"
pprPanic s doc = panicDoc s (doc $$ callStackDoc)

pprSorry :: String -> SDoc' r -> a
-- ^ Throw an exception saying "this isn't finished yet"
pprSorry    = sorryDoc

pprPgmError :: String -> SDoc' r -> a
-- ^ Throw an exception saying "bug in pgm being compiled" (used for unusual program errors)
pprPgmError = pgmErrorDoc

pprTraceDebug :: String -> SDoc' r -> a -> a
pprTraceDebug str doc x
   | debugIsOn && hasPprDebug unsafeGlobalDynFlags = pprTrace str doc x
   | otherwise                                     = x

-- | @pprTraceException desc x action@ runs action, printing a message
-- if it throws an exception.
pprTraceException :: ExceptionMonad m => String -> SDoc' r -> m a -> m a
pprTraceException heading doc =
    handleGhcException $ \exc -> liftIO $ do
        putStrLn $ showSDocDump unsafeGlobalDynFlags (sep [text heading, nest 2 doc])
        throwGhcExceptionIO exc

pprTrace :: String -> SDoc' r -> a -> a
-- ^ If debug output is on, show some 'SDoc' on the screen
pprTrace str doc x
   | hasNoDebugOutput unsafeGlobalDynFlags = x
   | otherwise                             =
      pprDebugAndThen unsafeGlobalDynFlags trace (text str) doc x

pprTraceM :: Applicative f => String -> SDoc' r -> f ()
pprTraceM str doc = pprTrace str doc (pure ())

-- | @pprTraceIt desc x@ is equivalent to @pprTrace desc (ppr x) x@
pprTraceIt :: Outputable a => String -> a -> a
pprTraceIt desc x = pprTrace desc (ppr x) x

-- | If debug output is on, show some 'SDoc' on the screen along
-- with a call stack when available.
pprSTrace :: HasCallStack => SDoc' r -> a -> a
pprSTrace doc = pprTrace "" (doc $$ callStackDoc)

warnPprTrace :: HasCallStack => Bool -> String -> Int -> SDoc' r -> a -> a
-- ^ Just warn about an assertion failure, recording the given file and line number.
-- Should typically be accessed with the WARN macros
warnPprTrace _     _     _     _    x | not debugIsOn     = x
warnPprTrace _     _file _line _msg x
   | hasNoDebugOutput unsafeGlobalDynFlags = x
warnPprTrace False _file _line _msg x = x
warnPprTrace True   file  line  msg x
  = pprDebugAndThen unsafeGlobalDynFlags trace heading
                    (msg $$ callStackDoc )
                    x
  where
    heading = hsep [text "WARNING: file", text file <> comma, text "line", int line]

-- | Panic with an assertation failure, recording the given file and
-- line number. Should typically be accessed with the ASSERT family of macros
assertPprPanic :: HasCallStack => String -> Int -> SDoc' r -> a
assertPprPanic _file _line msg
  = pprPanic "ASSERT failed!" msg

pprDebugAndThen :: DynFlags -> (String -> a) -> SDoc' r -> SDoc' r -> a
pprDebugAndThen dflags cont heading pretty_msg
 = cont (showSDocDump dflags doc)
 where
     doc = sep [heading, nest 2 pretty_msg]
