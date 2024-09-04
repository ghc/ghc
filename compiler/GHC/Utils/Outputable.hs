{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

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
        BindingSite(..),  JoinPointHood(..), isJoinPoint,

        IsOutput(..), IsLine(..), IsDoc(..),
        HLine, HDoc,

        -- * Pretty printing combinators
        SDoc, runSDoc, PDoc(..),
        docToSDoc,
        interppSP, interpp'SP, interpp'SP',
        pprQuotedList, pprWithCommas, pprWithSemis,
        unquotedListWith,
        quotedListWithOr, quotedListWithNor, quotedListWithAnd,
        pprWithBars,
        spaceIfSingleQuote,
        isEmpty, nest,
        ptext,
        int, intWithCommas, integer, word64, word, float, double, rational, doublePrec,
        parens, cparen, brackets, braces, quotes, quote, quoteIfPunsEnabled,
        doubleQuotes, angleBrackets,
        semi, comma, colon, dcolon, space, equals, dot, vbar,
        arrow, lollipop, larrow, darrow, arrowt, larrowt, arrowtt, larrowtt,
        lambda,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace, underscore,
        blankLine, forAllLit, bullet,
        ($+$),
        cat, fcat,
        hang, hangNotEmpty, punctuate, punctuateFinal,
        ppWhen, ppUnless, ppWhenOption, ppUnlessOption,
        speakNth, speakN, speakNOf, plural, singular,
        isOrAre, doOrDoes, itsOrTheir, thisOrThese, hasOrHave,
        itOrThey,
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

        pprModuleName,

        -- * Controlling the style in which output is printed
        PprStyle(..), NamePprCtx(..),
        QueryQualifyName, QueryQualifyModule, QueryQualifyPackage, QueryPromotionTick,
        PromotedItem(..), IsEmptyOrSingleton(..), isListEmptyOrSingleton,
        PromotionTickContext(..),
        reallyAlwaysQualify, reallyAlwaysQualifyNames,
        alwaysQualify, alwaysQualifyNames, alwaysQualifyModules,
        neverQualify, neverQualifyNames, neverQualifyModules,
        alwaysQualifyPackages, neverQualifyPackages,
        alwaysPrintPromTick,
        QualifyName(..), queryQual,
        sdocOption,
        updSDocContext,
        SDocContext (..), sdocWithContext,
        defaultSDocContext, traceSDocContext,
        getPprStyle, withPprStyle, setStyleColoured,
        pprDeeper, pprDeeperList, pprSetDepth,
        codeStyle, userStyle, dumpStyle,
        qualName, qualModule, qualPackage, promTick,
        mkErrStyle, defaultErrStyle, defaultDumpStyle, mkDumpStyle, defaultUserStyle,
        mkUserStyle, cmdlineParserStyle, Depth(..),
        withUserStyle, withErrStyle,

        ifPprDebug, whenPprDebug, getPprDebug,

        bPutHDoc
    ) where

import Language.Haskell.Syntax.Module.Name ( ModuleName(..) )

import GHC.Prelude.Basic

import {-# SOURCE #-}   GHC.Unit.Types ( Unit, Module, moduleName )
import {-# SOURCE #-}   GHC.Types.Name.Occurrence( OccName )

import GHC.Utils.BufHandle (BufHandle, bPutChar, bPutStr, bPutFS, bPutFZS)
import GHC.Data.FastString
import qualified GHC.Utils.Ppr as Pretty
import qualified GHC.Utils.Ppr.Colour as Col
import GHC.Utils.Ppr       ( Doc, Mode(..) )
import GHC.Utils.Panic.Plain (assert)
import GHC.Serialized
import GHC.LanguageExtensions (Extension)
import GHC.Utils.GlobalVars( unsafeHasPprDebug )
import GHC.Utils.Misc (lastMaybe, snocView)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import qualified Data.Map as M
import Data.Int
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified GHC.Data.Word64Set as Word64Set
import Data.String
import Data.Word
import System.IO        ( Handle )
import System.FilePath
import Text.Printf
import Numeric (showFFloat)
import Data.Graph (SCC(..))
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Arg(..))
import qualified Data.List.NonEmpty as NEL
import Data.Time ( UTCTime )
import Data.Time.Format.ISO8601
import Data.Void
import Control.DeepSeq (NFData(rnf))

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
  = PprUser NamePprCtx Depth Coloured
                -- Pretty-print in a way that will make sense to the
                -- ordinary user; must be very close to Haskell
                -- syntax, etc.
                -- Assumes printing tidied code: non-system names are
                -- printed without uniques.

  | PprDump NamePprCtx
                -- For -ddump-foo; less verbose than in ppr-debug mode, but more than PprUser
                -- Does not assume tidied code: non-external names
                -- are printed with uniques.

  | PprCode -- ^ Print code; either C or assembler

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
data NamePprCtx = QueryQualify {
    queryQualifyName    :: QueryQualifyName,
    queryQualifyModule  :: QueryQualifyModule,
    queryQualifyPackage :: QueryQualifyPackage,
    queryPromotionTick  :: QueryPromotionTick
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

-- | Given a promoted data constructor,
-- decide whether to print a tick to disambiguate the namespace.
type QueryPromotionTick = PromotedItem -> Bool

-- | Flags that affect whether a promotion tick is printed.
data PromotionTickContext =
  PromTickCtx {
    ptcListTuplePuns :: !Bool,
    ptcPrintRedundantPromTicks :: !Bool
  }

data PromotedItem =
    PromotedItemListSyntax IsEmptyOrSingleton -- '[x]
  | PromotedItemTupleSyntax                   -- '(x, y)
  | PromotedItemDataCon OccName               -- 'MkT

newtype IsEmptyOrSingleton = IsEmptyOrSingleton Bool

isListEmptyOrSingleton :: [a] -> IsEmptyOrSingleton
isListEmptyOrSingleton xs =
  IsEmptyOrSingleton $ case xs of
    []  -> True
    [_] -> True
    _   -> False

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

alwaysPrintPromTick :: QueryPromotionTick
alwaysPrintPromTick _ = True

reallyAlwaysQualify, alwaysQualify, neverQualify :: NamePprCtx
reallyAlwaysQualify
              = QueryQualify reallyAlwaysQualifyNames
                             alwaysQualifyModules
                             alwaysQualifyPackages
                             alwaysPrintPromTick
alwaysQualify = QueryQualify alwaysQualifyNames
                             alwaysQualifyModules
                             alwaysQualifyPackages
                             alwaysPrintPromTick
neverQualify  = QueryQualify neverQualifyNames
                             neverQualifyModules
                             neverQualifyPackages
                             alwaysPrintPromTick

defaultUserStyle :: PprStyle
defaultUserStyle = mkUserStyle neverQualify AllTheWay

defaultDumpStyle :: PprStyle
 -- Print without qualifiers to reduce verbosity, unless -dppr-debug
defaultDumpStyle = PprDump neverQualify

mkDumpStyle :: NamePprCtx -> PprStyle
mkDumpStyle name_ppr_ctx = PprDump name_ppr_ctx

-- | Default style for error messages, when we don't know NamePprCtx
-- It's a bit of a hack because it doesn't take into account what's in scope
-- Only used for desugarer warnings, and typechecker errors in interface sigs
defaultErrStyle :: PprStyle
defaultErrStyle = mkErrStyle neverQualify

-- | Style for printing error messages
mkErrStyle :: NamePprCtx -> PprStyle
mkErrStyle name_ppr_ctx = mkUserStyle name_ppr_ctx DefaultDepth

cmdlineParserStyle :: PprStyle
cmdlineParserStyle = mkUserStyle alwaysQualify AllTheWay

mkUserStyle :: NamePprCtx -> Depth -> PprStyle
mkUserStyle name_ppr_ctx depth = PprUser name_ppr_ctx depth Uncoloured

withUserStyle :: NamePprCtx -> Depth -> SDoc -> SDoc
withUserStyle name_ppr_ctx depth doc = withPprStyle (PprUser name_ppr_ctx depth Uncoloured) doc

withErrStyle :: NamePprCtx -> SDoc -> SDoc
withErrStyle name_ppr_ctx doc =
   withPprStyle (mkErrStyle name_ppr_ctx) doc

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
      -- and not disabled by GHC_NO_UNICODE environment variable
  , sdocPrintErrIndexLinks          :: !Bool
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
  , sdocSuppressCoercionTypes       :: !Bool
  , sdocSuppressUnfoldings          :: !Bool
  , sdocSuppressVarKinds            :: !Bool
  , sdocSuppressUniques             :: !Bool
  , sdocSuppressModulePrefixes      :: !Bool
  , sdocSuppressStgExts             :: !Bool
  , sdocSuppressStgReps             :: !Bool
  , sdocErrorSpans                  :: !Bool
  , sdocStarIsType                  :: !Bool
  , sdocLinearTypes                 :: !Bool
  , sdocListTuplePuns               :: !Bool
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
  , sdocPrintErrIndexLinks          = False
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
  , sdocSuppressCoercionTypes       = False
  , sdocSuppressUnfoldings          = False
  , sdocSuppressVarKinds            = False
  , sdocSuppressUniques             = False
  , sdocSuppressModulePrefixes      = False
  , sdocSuppressStgExts             = False
  , sdocSuppressStgReps             = True
  , sdocErrorSpans                  = False
  , sdocStarIsType                  = False
  , sdocLinearTypes                 = False
  , sdocListTuplePuns               = True
  , sdocPrintTypeAbbreviations      = True
  , sdocUnitIdForUser               = ftext
  }

traceSDocContext :: SDocContext
-- Used for pprTrace, when we want to see lots of info
traceSDocContext = defaultSDocContext
  { sdocPprDebug                    = unsafeHasPprDebug
  , sdocPrintTypecheckerElaboration = True
  , sdocPrintExplicitKinds          = True
  , sdocPrintExplicitCoercions      = True
  , sdocPrintExplicitRuntimeReps    = True
  , sdocPrintExplicitForalls        = True
  , sdocPrintEqualityRelations      = True
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

promTick :: PprStyle -> QueryPromotionTick
promTick (PprUser q _ _) occ = queryPromotionTick q occ
promTick (PprDump q)     occ = queryPromotionTick q occ
promTick _               _   = True

queryQual :: PprStyle -> NamePprCtx
queryQual s = QueryQualify (qualName s)
                           (qualModule s)
                           (qualPackage s)
                           (promTick s)

codeStyle :: PprStyle -> Bool
codeStyle PprCode     = True
codeStyle _           = False

dumpStyle :: PprStyle -> Bool
dumpStyle (PprDump {}) = True
dumpStyle _other       = False

userStyle ::  PprStyle -> Bool
userStyle (PprUser {}) = True
userStyle _other       = False

-- | Indicate if -dppr-debug mode is enabled
getPprDebug :: IsOutput doc => (Bool -> doc) -> doc
{-# INLINE CONLIKE getPprDebug #-}
getPprDebug d = docWithContext $ \ctx -> d (sdocPprDebug ctx)

-- | Says what to do with and without -dppr-debug
ifPprDebug :: IsOutput doc => doc -> doc -> doc
{-# INLINE CONLIKE ifPprDebug #-}
ifPprDebug yes no = getPprDebug $ \dbg -> if dbg then yes else no

-- | Says what to do with -dppr-debug; without, return empty
whenPprDebug :: IsOutput doc => doc -> doc        -- Empty for non-debug style
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

pprCode :: SDoc -> SDoc
{-# INLINE CONLIKE pprCode #-}
pprCode d = withPprStyle PprCode d

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

ptext    ::               PtrString  -> SDoc
int      :: IsLine doc => Int        -> doc
integer  :: IsLine doc => Integer    -> doc
word     ::               Integer    -> SDoc
word64   :: IsLine doc => Word64     -> doc
float    :: IsLine doc => Float      -> doc
double   :: IsLine doc => Double     -> doc
rational ::               Rational   -> SDoc

{-# INLINE CONLIKE ptext #-}
ptext s     = docToSDoc $ Pretty.ptext s
{-# INLINE CONLIKE int #-}
int n       = text $ show n
{-# INLINE CONLIKE integer #-}
integer n   = text $ show n
{-# INLINE CONLIKE float #-}
float n     = text $ show n
{-# INLINE CONLIKE double #-}
double n    = text $ show n
{-# INLINE CONLIKE rational #-}
rational n  = text $ show n
              -- See Note [Print Hexadecimal Literals] in GHC.Utils.Ppr
{-# INLINE CONLIKE word64 #-}
word64 n    = text $ show n
{-# INLINE CONLIKE word #-}
word n      = sdocOption sdocHexWordLiterals $ \case
               True  -> docToSDoc $ Pretty.hex n
               False -> docToSDoc $ Pretty.integer n

-- | @doublePrec p n@ shows a floating point number @n@ with @p@
-- digits of precision after the decimal point.
doublePrec :: Int -> Double -> SDoc
doublePrec p n = text (showFFloat (Just p) n "")

quotes, quote :: SDoc -> SDoc
parens, brackets, braces, doubleQuotes, angleBrackets :: IsLine doc => doc -> doc

{-# INLINE CONLIKE parens #-}
parens d        = char '(' <> d <> char ')'
{-# INLINE CONLIKE braces #-}
braces d        = char '{' <> d <> char '}'
{-# INLINE CONLIKE brackets #-}
brackets d      = char '[' <> d <> char ']'
{-# INLINE CONLIKE quote #-}
quote d         = SDoc $ Pretty.quote . runSDoc d
{-# INLINE CONLIKE doubleQuotes #-}
doubleQuotes d  = char '"' <> d <> char '"'
{-# INLINE CONLIKE angleBrackets #-}
angleBrackets d = char '<' <> d <> char '>'

cparen :: Bool -> SDoc -> SDoc
{-# INLINE CONLIKE cparen #-}
cparen b d = SDoc $ Pretty.maybeParens b . runSDoc d

quoteIfPunsEnabled :: SDoc -> SDoc
quoteIfPunsEnabled doc =
  sdocOption sdocListTuplePuns $ \case
    True -> quote doc
    False -> doc

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
         _ | Just '\'' <- lastMaybe str -> pp_d
           | otherwise        -> Pretty.quotes pp_d

blankLine, dcolon, arrow, lollipop, larrow, darrow, arrowt, larrowt, arrowtt,
  larrowtt, lambda :: SDoc

blankLine  = docToSDoc Pretty.emptyText
dcolon     = unicodeSyntax (char '∷') (text "::")
arrow      = unicodeSyntax (char '→') (text "->")
lollipop   = unicodeSyntax (char '⊸') (text "%1 ->")
larrow     = unicodeSyntax (char '←') (text "<-")
darrow     = unicodeSyntax (char '⇒') (text "=>")
arrowt     = unicodeSyntax (char '⤚') (text ">-")
larrowt    = unicodeSyntax (char '⤙') (text "-<")
arrowtt    = unicodeSyntax (char '⤜') (text ">>-")
larrowtt   = unicodeSyntax (char '⤛') (text "-<<")
lambda     = unicodeSyntax (char 'λ') (char '\\')

semi, comma, colon, equals, space, underscore, dot, vbar :: IsLine doc => doc
lparen, rparen, lbrack, rbrack, lbrace, rbrace :: IsLine doc => doc
semi       = char ';'
comma      = char ','
colon      = char ':'
equals     = char '='
space      = char ' '
underscore = char '_'
dot        = char '.'
vbar       = char '|'
lparen     = char '('
rparen     = char ')'
lbrack     = char '['
rbrack     = char ']'
lbrace     = char '{'
rbrace     = char '}'

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
($+$) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together vertically

{-# INLINE CONLIKE nest #-}
nest n d    = SDoc $ Pretty.nest n . runSDoc d
{-# INLINE CONLIKE ($+$) #-}
($+$) d1 d2 = SDoc $ \ctx -> (Pretty.$+$) (runSDoc d1 ctx) (runSDoc d2 ctx)

cat :: [SDoc] -> SDoc
-- ^ A paragraph-fill combinator. It's much like sep, only it
-- keeps fitting things on one line until it can't fit any more.
fcat :: [SDoc] -> SDoc
-- ^ This behaves like 'fsep', but it uses '<>' for horizontal composition rather than '<+>'


-- Inline all those wrappers to help ensure we create lists of Doc, not of SDoc
-- later applied to the same SDocContext. It helps the worker/wrapper
-- transformation extracting only the required fields from the SDocContext.
{-# INLINE CONLIKE cat #-}
cat ds  = SDoc $ \ctx -> Pretty.cat  [runSDoc d ctx | d <- ds]
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

punctuate :: IsLine doc
          => doc   -- ^ The punctuation
          -> [doc] -- ^ The list that will have punctuation added between every adjacent pair of elements
          -> [doc] -- ^ Punctuated list
punctuate _ []     = []
punctuate p (d:ds) = go d ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d <> p) : go e es

-- | Punctuate a list, e.g. with commas and dots.
--
-- > sep $ punctuateFinal comma dot [text "ab", text "cd", text "ef"]
-- > ab, cd, ef.
punctuateFinal :: IsLine doc
               => doc   -- ^ The interstitial punctuation
               -> doc   -- ^ The final punctuation
               -> [doc] -- ^ The list that will have punctuation added between every adjacent pair of elements
               -> [doc] -- ^ Punctuated list
punctuateFinal _ _ []     = []
punctuateFinal p q (d:ds) = go d ds
  where
    go d [] = [d <> q]
    go d (e:es) = (d <> p) : go e es

ppWhen, ppUnless :: IsOutput doc => Bool -> doc -> doc
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

-- There's no Outputable for Char; it's too easy to use Outputable
-- on String and have ppr "hello" rendered as "h,e,l,l,o".

instance Outputable Void where
    ppr _ = text "<<Void>>"

instance Outputable Bool where
    ppr True  = text "True"
    ppr False = text "False"

instance Outputable Ordering where
    ppr LT = text "LT"
    ppr EQ = text "EQ"
    ppr GT = text "GT"

instance Outputable Int8 where
   ppr n = integer $ fromIntegral n

instance Outputable Int16 where
   ppr n = integer $ fromIntegral n

instance Outputable Int32 where
   ppr n = integer $ fromIntegral n

instance Outputable Int64 where
   ppr n = integer $ fromIntegral n

instance Outputable Int where
    ppr n = int n

instance Outputable Integer where
    ppr n = integer n

instance Outputable Word8 where
    ppr n = integer $ fromIntegral n

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
    ppr xs = brackets (pprWithCommas ppr xs)

instance (Outputable a) => Outputable (NonEmpty a) where
    ppr = ppr . NEL.toList

instance (Outputable a, Outputable b) => Outputable (Arg a b) where
    ppr (Arg a b) = text "Arg" <+> ppr a <+> ppr b

instance (Outputable a) => Outputable (Set a) where
    ppr s = braces (pprWithCommas ppr (Set.toList s))

instance Outputable Word64Set.Word64Set where
    ppr s = braces (pprWithCommas ppr (Word64Set.toList s))

instance Outputable IntSet.IntSet where
    ppr s = braces (pprWithCommas ppr (IntSet.toList s))

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr (x,y) = parens (sep [ppr x <> comma, ppr y])

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

instance Outputable ModuleName where
  ppr = pprModuleName

pprModuleName :: IsLine doc => ModuleName -> doc
pprModuleName (ModuleName nm) =
    docWithStyle (ztext (zEncodeFS nm)) (\_ -> ftext nm)
{-# SPECIALIZE pprModuleName :: ModuleName -> SDoc #-}
{-# SPECIALIZE pprModuleName :: ModuleName -> HLine #-} -- see Note [SPECIALIZE to HDoc]

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

instance OutputableP env Void where
    pdoc _ = \ case

{-
************************************************************************
*                                                                      *
\subsection{The @OutputableBndr@ class}
*                                                                      *
************************************************************************
-}

-- | When we print a binder, we often want to print its type too.
-- The @OutputableBndr@ class encapsulates this idea.
class Outputable a => OutputableBndr a where
   pprBndr :: BindingSite -> a -> SDoc
   pprBndr _b x = ppr x

   pprPrefixOcc, pprInfixOcc :: a -> SDoc
      -- Print an occurrence of the name, suitable either in the
      -- prefix position of an application, thus   (f a b) or  ((+) x)
      -- or infix position,                 thus   (a `f` b) or  (x + y)

   bndrIsJoin_maybe :: a -> JoinPointHood
   bndrIsJoin_maybe _ = NotJoinPoint
      -- When pretty-printing we sometimes want to find
      -- whether the binder is a join point.  You might think
      -- we could have a function of type (a->Var), but Var
      -- isn't available yet, alas

-- | 'BindingSite' is used to tell the thing that prints binder what
-- language construct is binding the identifier.  This can be used
-- to decide how much info to print.
-- Also see Note [Binding-site specific printing] in "GHC.Core.Ppr"
data BindingSite
    = LambdaBind  -- ^ The x in   (\x. e)
    | CaseBind    -- ^ The x in   case scrut of x { (y,z) -> ... }
    | CasePatBind -- ^ The y,z in case scrut of x { (y,z) -> ... }
    | LetBind     -- ^ The x in   (let x = rhs in e)
    deriving Eq

data JoinPointHood
  = JoinPoint {-# UNPACK #-} !Int   -- The JoinArity (but an Int here because
  | NotJoinPoint                    -- synonym JoinArity is defined in Types.Basic)
  deriving( Eq )

isJoinPoint :: JoinPointHood -> Bool
isJoinPoint (JoinPoint {}) = True
isJoinPoint NotJoinPoint   = False

instance Outputable JoinPointHood where
  ppr NotJoinPoint      = text "NotJoinPoint"
  ppr (JoinPoint arity) = text "JoinPoint" <> parens (ppr arity)

instance NFData JoinPointHood where
  rnf x = x `seq` ()

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
primInt8Suffix   = text "#Int8"
primWord8Suffix  = text "#Word8"
primInt16Suffix  = text "#Int16"
primWord16Suffix = text "#Word16"
primInt32Suffix  = text "#Int32"
primWord32Suffix = text "#Word32"
primInt64Suffix  = text "#Int64"
primWord64Suffix = text "#Word64"

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
pprFilePathString :: IsLine doc => FilePath -> doc
pprFilePathString path = doubleQuotes $ text (escape (normalise path))
   where
      escape []        = []
      escape ('\\':xs) = '\\':'\\':escape xs
      escape (x:xs)    = x:escape xs
{-# SPECIALIZE pprFilePathString :: FilePath -> SDoc #-}
{-# SPECIALIZE pprFilePathString :: FilePath -> HLine #-} -- see Note [SPECIALIZE to HDoc]

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

pprWithSemis :: (a -> SDoc) -- ^ The pretty printing function to use
              -> [a]         -- ^ The things to be pretty printed
              -> SDoc        -- ^ 'SDoc' where the things have been pretty printed,
                             -- semicolon-separated and finally packed into a paragraph.
pprWithSemis pp xs = fsep (punctuate semi (map pp xs))

pprWithBars :: (a -> SDoc) -- ^ The pretty printing function to use
            -> [a]         -- ^ The things to be pretty printed
            -> SDoc        -- ^ 'SDoc' where the things have been pretty printed,
                           -- bar-separated and finally packed into a paragraph.
pprWithBars pp xs = fsep (intersperse vbar (map pp xs))

-- Prefix the document with a space if it starts with a single quote.
-- See Note [Printing promoted type constructors] in GHC.Iface.Type
spaceIfSingleQuote :: SDoc -> SDoc
spaceIfSingleQuote (SDoc m) =
  SDoc $ \ctx ->
    let (mHead, d) = Pretty.docHead (m ctx)
    in if mHead == Just '\''
       then Pretty.space Pretty.<> d
       else d

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

quotedListWithAnd :: [SDoc] -> SDoc
-- [x,y,z]  ==>  `x', `y' and `z'
quotedListWithAnd xs@(_:_:_) = quotedList (init xs) <+> text "and" <+> quotes (last xs)
quotedListWithAnd xs = quotedList xs


unquotedListWith :: SDoc -> [SDoc] -> SDoc
-- "whatever" [x,y,z] ==> x, y whatever z
unquotedListWith d xs
  | Just (fs@(_:_), l) <- snocView xs = unquotedList fs <+> d <+> l
  | otherwise                         = unquotedList xs
  where
    unquotedList = fsep . punctuate comma

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

-- | 'it' or 'they', depeneding on the length of the list.
--
-- > itOrThey [x]   = text "it"
-- > itOrThey [x,y] = text "they"
-- > itOrThey []    = text "they"  -- probably avoid this
itOrThey :: [a] -> SDoc
itOrThey [_] = text "it"
itOrThey _   = text "they"


-- | Determines the form of subject appropriate for the length of a list:
--
-- > thisOrThese [x]   = text "This"
-- > thisOrThese [x,y] = text "These"
-- > thisOrThese []    = text "These"  -- probably avoid this
thisOrThese :: [a] -> SDoc
thisOrThese [_] = text "This"
thisOrThese _   = text "These"

-- | @"has"@ or @"have"@ depending on the length of a list.
hasOrHave :: [a] -> SDoc
hasOrHave [_] = text "has"
hasOrHave _   = text "have"

{- Note [SDoc versus HDoc]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The SDoc type is used pervasively throughout the compiler to represent pretty-
printable output. Almost all text written by GHC, from the Haskell types and
expressions included in error messages to debug dumps, is assembled using SDoc.
SDoc is nice because it handles multiline layout in a semi-automatic fashion,
enabling printed expressions to wrap to fit a given line width while correctly
indenting the following lines to preserve alignment.

SDoc’s niceties necessarily have some performance cost, but this is normally
okay, as printing output is rarely a performance bottleneck. However, one
notable exception to this is code generation: GHC must sometimes write
megabytes’ worth of generated assembly when compiling a single module, in which
case the overhead of SDoc has a significant cost (see #21853 for some numbers).
Moreover, generated assembly does not have the complex layout requirements of
pretty-printed Haskell code, so using SDoc does not buy us much, anyway.

Nevertheless, we do still want to be able to share some logic between writing
assembly and pretty-printing. For example, the logic for printing basic block
labels (GHC.Cmm.CLabel.pprCLabel) is nontrivial, so we want to have a single
implementation that can be used both when generating code and when generating
Cmm dumps. This is where HDoc comes in: HDoc provides a subset of the SDoc
interface, but it is implemented in a far more efficient way, writing directly
to a `Handle` (via a `BufHandle`) without building any intermediate structures.
We can then use typeclasses to parameterize functions like `pprCLabel` over the
printing implementation.

One might imagine this would result in one IsDoc typeclass, and two instances,
one for SDoc and one for HDoc. However, in fact, we need two *variants* of HDoc,
as described in Note [HLine versus HDoc], and this gives rise to a small
typeclass hierarchy consisting of IsOutput, IsLine, and IsDoc;
see Note [The outputable class hierarchy] for details.

Note [HLine versus HDoc]
~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [SDoc versus HDoc], HDoc does not support any of the layout
niceties of SDoc for efficiency. However, this presents a small problem if we
want to be compatible with the SDoc API, as expressions like

    text "foo" <+> (text "bar" $$ text "baz")

are expected to produce

    foo bar
        baz

which requires tracking line widths to know how far to indent the second line.
We can’t throw out vertical composition altogether, as we need to be able to
construct multiline HDocs, but we *can* restrict vertical composition to
concatenating whole lines at a time, as this is all that is necessary to
generate assembly in the code generator.

To implement this restriction, we provide two distinct types: HLine and HDoc.
As their names suggests, an HLine represents a single line of output, while an
HDoc represents a multiline document. Atoms formed from `char` and `text` begin
their lives as HLines, which can be horizontally (but not vertically) composed:

    char :: Char -> HLine
    text :: String -> HLine
    (<+>) :: HLine -> HLine -> HLine

Once a line has been fully assembled, it can be “locked up” into a single-line
HDoc via `line`, and HDocs can be vertically (but not horizontally) composed:

    line :: HLine -> HDoc
    ($$) :: HLine -> HLine -> HLine

Note that, at runtime, HLine and HDoc use exactly the same representation. This
distinction only exists in the type system to rule out the cases we don’t want
to have to handle.

Note [The outputable class hierarchy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [SDoc versus HDoc], we want to be able to parameterize over
the choice of printing implementation when implementing common bits of printing
logic. However, as described in Note [HLine versus HDoc], we also want to
distinguish code that does single-line printing from code that does multi-line
printing. Therefore, code that is parameterized over the choice of printer must
respect this single- versus multi-line distinction. This naturally leads to two
typeclasses:

    class IsLine doc where
      char :: Char -> doc
      text :: String -> doc
      (<>) :: doc -> doc -> doc
      ...

    class IsLine (Line doc) => IsDoc doc where
      type Line doc = r | r -> doc
      line :: Line doc -> doc
      ($$) :: doc -> doc -> doc
      ...

These classes support the following instances:

    instance IsLine SDoc
    instance IsLine SDoc where
      type Line SDoc = SDoc

    instance IsLine HLine
    instance IsDoc HDoc where
      type Line HDoc = HLine

However, we run into a new problem: we provide many useful combinators on docs
that don’t care at all about the single-/multi-line distinction. For example,
ppWhen and ppUnless provide conditional logic, and docWithContext provides
access to the ambient SDocContext. Given the above classes, we would need two
variants of each of these combinators:

    ppWhenL :: IsLine doc => Bool -> doc -> doc
    ppWhenL c d = if c then d else emptyL

    ppWhenD :: IsDoc  doc => Bool -> doc -> doc
    ppWhenD c d = if c then d else emptyD

This is a needlessly annoying distinction, so we introduce a common superclass,
IsOutput, that allows these combinators to be generic over both variants:

    class IsOutput doc where
      empty :: doc
      docWithContext :: (SDocContext -> doc) -> doc
      docWithStyle :: doc -> (PprStyle -> SDoc) -> doc

    class IsOutput doc => IsLine doc
    class (IsOutput doc, IsLine (Line doc)) => IsDoc doc

In practice, IsOutput isn’t used explicitly very often, but it makes code that
uses the combinators derived from it significantly less noisy.

Note [SPECIALIZE to HDoc]
~~~~~~~~~~~~~~~~~~~~~~~~~
The IsLine and IsDoc classes are useful to share printing logic between code
that uses SDoc and code that uses HDoc, but we must take some care when doing
so. Much HDoc’s efficiency comes from GHC’s ability to optimize code that uses
it to eliminate unnecessary indirection, but the HDoc primitives must be inlined
before these opportunities can be exposed. Therefore, we want to explicitly
request that GHC generate HDoc (or HLine) specializations of any polymorphic
printing functions used by the code generator.

In code generators (CmmToAsm.{AArch64,PPC,X86}.Ppr) we add a specialize
pragma just to the entry point pprNatCmmDecl, to avoid cluttering
the entire module. Because specialization is transitive, this makes sure
that other functions in that module are specialized too.

Note [dualLine and dualDoc]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The IsLine and IsDoc classes provide the dualLine and dualDoc methods,
respectively, which have the following types:

    dualLine :: IsLine doc => SDoc -> HLine -> doc
    dualDoc  :: IsDoc  doc => SDoc -> HDoc  -> doc

These are effectively a form of type-`case`, selecting between each of their two
arguments depending on the type they are instantiated at. They serve as a
“nuclear option” for code that is, for some reason or another, unreasonably
difficult to make completely equivalent under both printer implementations.

These operations should generally be avoided, as they can result in surprising
changes in behavior when the printer implementation is changed.
Right now, they are used only when outputting debugging comments in
codegen, as it is difficult to adapt that code to use HLine and not necessary.

Use these operations wisely.

Note [docWithStyle]
~~~~~~~~~~~~~~~~~~~
Sometimes when printing, we consult the printing style. This can be done
with 'docWithStyle c f'. This is similar to 'docWithContext (f . sdocStyle)',
but:
* For code style, 'docWithStyle c f' will return 'c'.
* For other styles, 'docWithStyle c f', will call 'f style', but expect
  an SDoc rather than doc. This removes the need to write code polymorphic
  in SDoc and HDoc, since the latter is used only for code style.
-}

-- | Represents a single line of output that can be efficiently printed directly
-- to a 'System.IO.Handle' (actually a 'BufHandle').
-- See Note [SDoc versus HDoc] and Note [HLine versus HDoc] for more details.
newtype HLine = HLine' { runHLine :: SDocContext -> BufHandle -> IO () }

-- | Represents a (possibly empty) sequence of lines that can be efficiently
-- printed directly to a 'System.IO.Handle' (actually a 'BufHandle').
-- See Note [SDoc versus HDoc] and Note [HLine versus HDoc] for more details.
newtype HDoc = HDoc' { runHDoc :: SDocContext -> BufHandle -> IO () }

-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
pattern HLine :: (SDocContext -> BufHandle -> IO ()) -> HLine
pattern HLine f <- HLine' f
  where HLine f = HLine' (oneShot (\ctx -> oneShot (\h -> f ctx h)))
{-# COMPLETE HLine #-}

-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
pattern HDoc :: (SDocContext -> BufHandle -> IO ()) -> HDoc
pattern HDoc f <- HDoc' f
  where HDoc f = HDoc' (oneShot (\ctx -> oneShot (\h -> f ctx h)))
{-# COMPLETE HDoc #-}

bPutHDoc :: BufHandle -> SDocContext -> HDoc -> IO ()
bPutHDoc h ctx (HDoc f) = assert (codeStyle (sdocStyle ctx)) (f ctx h)

-- | A superclass for 'IsLine' and 'IsDoc' that provides an identity, 'empty',
-- as well as access to the shared 'SDocContext'.
--
-- See Note [The outputable class hierarchy] for more details.
class IsOutput doc where
  empty :: doc
  docWithContext :: (SDocContext -> doc) -> doc
  docWithStyle :: doc -> (PprStyle -> SDoc) -> doc  -- see Note [docWithStyle]

-- | A class of types that represent a single logical line of text, with support
-- for horizontal composition.
--
-- See Note [HLine versus HDoc] and Note [The outputable class hierarchy] for
-- more details.
class IsOutput doc => IsLine doc where
  char :: Char -> doc
  text :: String -> doc
  ftext :: FastString -> doc
  ztext :: FastZString -> doc

  -- | Join two @doc@s together horizontally without a gap.
  (<>) :: doc -> doc -> doc
  -- | Join two @doc@s together horizontally with a gap between them.
  (<+>) :: doc -> doc -> doc
  -- | Separate: is either like 'hsep' or like 'vcat', depending on what fits.
  sep :: [doc] -> doc
  -- | A paragraph-fill combinator. It's much like 'sep', only it keeps fitting
  -- things on one line until it can't fit any more.
  fsep :: [doc] -> doc

  -- | Concatenate @doc@s horizontally without gaps.
  hcat :: [doc] -> doc
  hcat docs = foldr (<>) empty docs
  {-# INLINE CONLIKE hcat #-}

  -- | Concatenate @doc@s horizontally with a space between each one.
  hsep :: [doc] -> doc
  hsep docs = foldr (<+>) empty docs
  {-# INLINE CONLIKE hsep #-}

  -- | Prints as either the given 'SDoc' or the given 'HLine', depending on
  -- which type the result is instantiated to. This should generally be avoided;
  -- see Note [dualLine and dualDoc] for details.
  dualLine :: SDoc -> HLine -> doc


-- | A class of types that represent a multiline document, with support for
-- vertical composition.
--
-- See Note [HLine versus HDoc] and Note [The outputable class hierarchy] for
-- more details.
class (IsOutput doc, IsLine (Line doc)) => IsDoc doc where
  type Line doc = r | r -> doc
  line :: Line doc -> doc

  -- | Join two @doc@s together vertically. If there is no vertical overlap it
  -- "dovetails" the two onto one line.
  ($$) :: doc -> doc -> doc

  lines_ :: [Line doc] -> doc
  lines_ = vcat . map line
  {-# INLINE CONLIKE lines_ #-}

  -- | Concatenate @doc@s vertically with dovetailing.
  vcat :: [doc] -> doc
  vcat ls = foldr ($$) empty ls
  {-# INLINE CONLIKE vcat #-}

  -- | Prints as either the given 'SDoc' or the given 'HDoc', depending on
  -- which type the result is instantiated to. This should generally be avoided;
  -- see Note [dualLine and dualDoc] for details.
  dualDoc :: SDoc -> HDoc -> doc

instance IsOutput SDoc where
  empty       = docToSDoc $ Pretty.empty
  {-# INLINE CONLIKE empty #-}
  docWithContext = sdocWithContext
  {-# INLINE docWithContext #-}
  docWithStyle c f = sdocWithContext (\ctx -> let sty = sdocStyle ctx
                                              in if codeStyle sty then c
                                                                  else f sty)
                     -- see Note [docWithStyle]
  {-# INLINE CONLIKE docWithStyle #-}

instance IsLine SDoc where
  char c = docToSDoc $ Pretty.char c
  {-# INLINE CONLIKE char #-}
  text s = docToSDoc $ Pretty.text s
  {-# INLINE CONLIKE text #-}   -- Inline so that the RULE Pretty.text will fire
  ftext s = docToSDoc $ Pretty.ftext s
  {-# INLINE CONLIKE ftext #-}
  ztext s = docToSDoc $ Pretty.ztext s
  {-# INLINE CONLIKE ztext #-}
  (<>) d1 d2 = SDoc $ \ctx -> (Pretty.<>)  (runSDoc d1 ctx) (runSDoc d2 ctx)
  {-# INLINE CONLIKE (<>) #-}
  (<+>) d1 d2 = SDoc $ \ctx -> (Pretty.<+>) (runSDoc d1 ctx) (runSDoc d2 ctx)
  {-# INLINE CONLIKE (<+>) #-}
  hcat ds = SDoc $ \ctx -> Pretty.hcat [runSDoc d ctx | d <- ds]
  {-# INLINE CONLIKE hcat #-}
  hsep ds = SDoc $ \ctx -> Pretty.hsep [runSDoc d ctx | d <- ds]
  {-# INLINE CONLIKE hsep #-}
  sep ds  = SDoc $ \ctx -> Pretty.sep  [runSDoc d ctx | d <- ds]
  {-# INLINE CONLIKE sep #-}
  fsep ds = SDoc $ \ctx -> Pretty.fsep [runSDoc d ctx | d <- ds]
  {-# INLINE CONLIKE fsep #-}
  dualLine s _ = s
  {-# INLINE CONLIKE dualLine #-}

instance IsDoc SDoc where
  type Line SDoc = SDoc
  line = id
  {-# INLINE line #-}
  lines_ = vcat
  {-# INLINE lines_ #-}

  ($$) d1 d2  = SDoc $ \ctx -> (Pretty.$$)  (runSDoc d1 ctx) (runSDoc d2 ctx)
  {-# INLINE CONLIKE ($$) #-}
  vcat ds = SDoc $ \ctx -> Pretty.vcat [runSDoc d ctx | d <- ds]
  {-# INLINE CONLIKE vcat #-}
  dualDoc s _ = s
  {-# INLINE CONLIKE dualDoc #-}

instance IsOutput HLine where
  empty = HLine (\_ _ -> pure ())
  {-# INLINE empty #-}
  docWithContext f = HLine $ \ctx h -> runHLine (f ctx) ctx h
  {-# INLINE CONLIKE docWithContext #-}
  docWithStyle c _ = c  -- see Note [docWithStyle]
  {-# INLINE CONLIKE docWithStyle #-}

instance IsOutput HDoc where
  empty = HDoc (\_ _ -> pure ())
  {-# INLINE empty #-}
  docWithContext f = HDoc $ \ctx h -> runHDoc (f ctx) ctx h
  {-# INLINE CONLIKE docWithContext #-}
  docWithStyle c _ = c  -- see Note [docWithStyle]
  {-# INLINE CONLIKE docWithStyle #-}

instance IsLine HLine where
  char c = HLine (\_ h -> bPutChar h c)
  {-# INLINE CONLIKE char #-}
  text str = HLine (\_ h -> bPutStr h str)
  {-# INLINE CONLIKE text #-}
  ftext fstr = HLine (\_ h -> bPutFS h fstr)
  {-# INLINE CONLIKE ftext #-}
  ztext fstr = HLine (\_ h -> bPutFZS h fstr)
  {-# INLINE CONLIKE ztext #-}

  HLine f <> HLine g = HLine (\ctx h -> f ctx h *> g ctx h)
  {-# INLINE CONLIKE (<>) #-}
  f <+> g = f <> char ' ' <> g
  {-# INLINE CONLIKE (<+>) #-}
  sep = hsep
  {-# INLINE sep #-}
  fsep = hsep
  {-# INLINE fsep #-}

  dualLine _ h = h
  {-# INLINE CONLIKE dualLine #-}

instance IsDoc HDoc where
  type Line HDoc = HLine
  line (HLine f) = HDoc (\ctx h -> f ctx h *> bPutChar h '\n')
  {-# INLINE CONLIKE line #-}
  HDoc f $$ HDoc g = HDoc (\ctx h -> f ctx h *> g ctx h)
  {-# INLINE CONLIKE ($$) #-}
  dualDoc _ h = h
  {-# INLINE CONLIKE dualDoc #-}
