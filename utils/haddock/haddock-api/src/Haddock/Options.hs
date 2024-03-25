-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Options
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Definition of the command line interface of Haddock.
module Haddock.Options
  ( parseHaddockOpts
  , Flag (..)
  , Visibility (..)
  , getUsage
  , optTitle
  , outputDir
  , optContentsUrl
  , optIndexUrl
  , optCssFile
  , optSourceCssFile
  , sourceUrls
  , wikiUrls
  , baseUrl
  , optParCount
  , optDumpInterfaceFile
  , optShowInterfaceFile
  , optLaTeXStyle
  , optMathjax
  , qualification
  , sinceQualification
  , verbosity
  , ghcFlags
  , reexportFlags
  , readIfaceArgs
  , optPackageName
  , optPackageVersion
  , modulePackageInfo
  , ignoredSymbols
  ) where

import Control.Applicative
import qualified Data.Char as Char
import Data.List (dropWhileEnd)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Version
import GHC (Module, moduleUnit)
import GHC.Data.FastString
import GHC.Unit.State
import Haddock.Types
import Haddock.Utils
import System.Console.GetOpt
import qualified Text.ParserCombinators.ReadP as RP

data Flag
  = Flag_BuiltInThemes
  | Flag_CSS String
  | --  | Flag_DocBook
    Flag_ReadInterface String
  | Flag_DumpInterface String
  | Flag_ShowInterface String
  | Flag_Heading String
  | Flag_Html
  | Flag_Hoogle
  | Flag_Lib String
  | Flag_OutputDir FilePath
  | Flag_Prologue FilePath
  | Flag_SourceBaseURL String
  | Flag_SourceModuleURL String
  | Flag_SourceEntityURL String
  | Flag_SourceLEntityURL String
  | Flag_WikiBaseURL String
  | Flag_BaseURL String
  | Flag_WikiModuleURL String
  | Flag_WikiEntityURL String
  | Flag_LaTeX
  | Flag_LaTeXStyle String
  | Flag_QuickJumpIndex
  | Flag_HyperlinkedSource
  | Flag_SourceCss String
  | Flag_Mathjax String
  | Flag_Help
  | Flag_Verbosity String
  | Flag_Version
  | Flag_CompatibleInterfaceVersions
  | Flag_InterfaceVersion
  | Flag_BypassInterfaceVersonCheck
  | Flag_UseContents String
  | Flag_GenContents
  | Flag_UseIndex String
  | Flag_GenIndex
  | Flag_IgnoreAllExports
  | Flag_HideModule String
  | Flag_ShowModule String
  | Flag_ShowAllModules
  | Flag_ShowExtensions String
  | Flag_OptGhc String
  | Flag_GhcLibDir String
  | Flag_GhcVersion
  | Flag_PrintGhcPath
  | Flag_PrintGhcLibDir
  | Flag_NoWarnings
  | Flag_UseUnicode
  | Flag_NoTmpCompDir
  | Flag_Qualification String
  | Flag_PrettyHtml
  | Flag_NoPrintMissingDocs
  | Flag_PackageName String
  | Flag_PackageVersion String
  | Flag_Reexport String
  | Flag_SinceQualification String
  | Flag_IgnoreLinkSymbol String
  | Flag_ParCount (Maybe Int)
  | Flag_TraceArgs
  deriving (Eq, Show)

options :: Bool -> [OptDescr Flag]
options backwardsCompat =
  [ Option
      ['B']
      []
      (ReqArg Flag_GhcLibDir "DIR")
      "path to a GHC lib dir, to override the default path"
  , Option
      ['o']
      ["odir"]
      (ReqArg Flag_OutputDir "DIR")
      "directory in which to put the output files"
  , Option
      ['l']
      ["lib"]
      (ReqArg Flag_Lib "DIR")
      "location of Haddock's auxiliary files"
  , Option
      ['i']
      ["read-interface"]
      (ReqArg Flag_ReadInterface "FILE")
      "read an interface from FILE"
  , Option
      ['D']
      ["dump-interface"]
      (ReqArg Flag_DumpInterface "FILE")
      "write the resulting interface to FILE"
  , Option
      []
      ["show-interface"]
      (ReqArg Flag_ShowInterface "FILE")
      "print the interface in a human readable form"
  , --    Option ['S']  ["docbook"]  (NoArg Flag_DocBook)
    --  "output in DocBook XML",
    Option
      ['h']
      ["html"]
      (NoArg Flag_Html)
      "output in HTML (XHTML 1.0)"
  , Option [] ["latex"] (NoArg Flag_LaTeX) "use experimental LaTeX rendering"
  , Option [] ["latex-style"] (ReqArg Flag_LaTeXStyle "FILE") "provide your own LaTeX style in FILE"
  , Option [] ["mathjax"] (ReqArg Flag_Mathjax "URL") "URL FOR mathjax"
  , Option ['U'] ["use-unicode"] (NoArg Flag_UseUnicode) "use Unicode in HTML output"
  , Option
      []
      ["hoogle"]
      (NoArg Flag_Hoogle)
      "output for Hoogle; you may want --package-name and --package-version too"
  , Option
      []
      ["quickjump"]
      (NoArg Flag_QuickJumpIndex)
      "generate an index for interactive documentation navigation"
  , Option
      []
      ["hyperlinked-source"]
      (NoArg Flag_HyperlinkedSource)
      "generate highlighted and hyperlinked source code (for use with --html)"
  , Option
      []
      ["source-css"]
      (ReqArg Flag_SourceCss "FILE")
      "use custom CSS file instead of default one in hyperlinked source"
  , Option
      []
      ["source-base"]
      (ReqArg Flag_SourceBaseURL "URL")
      "URL for a source code link on the contents\nand index pages"
  , Option
      ['s']
      (if backwardsCompat then ["source", "source-module"] else ["source-module"])
      (ReqArg Flag_SourceModuleURL "URL")
      "URL for a source code link for each module\n(using the %{FILE} or %{MODULE} vars)"
  , Option
      []
      ["source-entity"]
      (ReqArg Flag_SourceEntityURL "URL")
      "URL for a source code link for each entity\n(using the %{FILE}, %{MODULE}, %{NAME},\n%{KIND} or %{LINE} vars)"
  , Option
      []
      ["source-entity-line"]
      (ReqArg Flag_SourceLEntityURL "URL")
      "URL for a source code link for each entity.\nUsed if name links are unavailable, eg. for TH splices."
  , Option
      []
      ["comments-base"]
      (ReqArg Flag_WikiBaseURL "URL")
      "URL for a comments link on the contents\nand index pages"
  , Option
      []
      ["base-url"]
      (ReqArg Flag_BaseURL "URL")
      "Base URL for static assets (eg. css, javascript, json files etc.).\nWhen given statis assets will not be copied."
  , Option
      []
      ["comments-module"]
      (ReqArg Flag_WikiModuleURL "URL")
      "URL for a comments link for each module\n(using the %{MODULE} var)"
  , Option
      []
      ["comments-entity"]
      (ReqArg Flag_WikiEntityURL "URL")
      "URL for a comments link for each entity\n(using the %{FILE}, %{MODULE}, %{NAME},\n%{KIND} or %{LINE} vars)"
  , Option
      ['c']
      ["css", "theme"]
      (ReqArg Flag_CSS "PATH")
      "the CSS file or theme directory to use for HTML output"
  , Option
      []
      ["built-in-themes"]
      (NoArg Flag_BuiltInThemes)
      "include all the built-in haddock themes"
  , Option
      ['p']
      ["prologue"]
      (ReqArg Flag_Prologue "FILE")
      "file containing prologue text"
  , Option
      ['t']
      ["title"]
      (ReqArg Flag_Heading "TITLE")
      "page heading"
  , Option
      ['q']
      ["qual"]
      (ReqArg Flag_Qualification "QUAL")
      "qualification of names, one of \n'none' (default), 'full', 'local'\n'relative' or 'aliased'"
  , Option
      ['?']
      ["help"]
      (NoArg Flag_Help)
      "display this help and exit"
  , Option
      ['V']
      ["version"]
      (NoArg Flag_Version)
      "output version information and exit"
  , Option
      []
      ["compatible-interface-versions"]
      (NoArg Flag_CompatibleInterfaceVersions)
      "output compatible interface file versions and exit"
  , Option
      []
      ["interface-version"]
      (NoArg Flag_InterfaceVersion)
      "output interface file version and exit"
  , Option
      []
      ["bypass-interface-version-check"]
      (NoArg Flag_BypassInterfaceVersonCheck)
      "bypass the interface file version check (dangerous)"
  , Option
      ['v']
      ["verbosity"]
      (ReqArg Flag_Verbosity "VERBOSITY")
      "set verbosity level"
  , Option
      []
      ["use-contents"]
      (ReqArg Flag_UseContents "URL")
      "use a separately-generated HTML contents page"
  , Option
      []
      ["gen-contents"]
      (NoArg Flag_GenContents)
      "generate an HTML contents from specified\ninterfaces"
  , Option
      []
      ["use-index"]
      (ReqArg Flag_UseIndex "URL")
      "use a separately-generated HTML index"
  , Option
      []
      ["gen-index"]
      (NoArg Flag_GenIndex)
      "generate an HTML index from specified\ninterfaces"
  , Option
      []
      ["ignore-all-exports"]
      (NoArg Flag_IgnoreAllExports)
      "behave as if all modules have the\nignore-exports attribute"
  , Option
      []
      ["hide"]
      (ReqArg Flag_HideModule "MODULE")
      "behave as if MODULE has the hide attribute"
  , Option
      []
      ["show"]
      (ReqArg Flag_ShowModule "MODULE")
      "behave as if MODULE does not have the hide attribute"
  , Option
      []
      ["show-all"]
      (NoArg Flag_ShowAllModules)
      "behave as if not modules have the hide attribute"
  , Option
      []
      ["show-extensions"]
      (ReqArg Flag_ShowExtensions "MODULE")
      "behave as if MODULE has the show-extensions attribute"
  , Option
      []
      ["optghc"]
      (ReqArg Flag_OptGhc "OPTION")
      "option to be forwarded to GHC"
  , Option
      []
      ["ghc-version"]
      (NoArg Flag_GhcVersion)
      "output GHC version in numeric format"
  , Option
      []
      ["print-ghc-path"]
      (NoArg Flag_PrintGhcPath)
      "output path to GHC binary"
  , Option
      []
      ["print-ghc-libdir"]
      (NoArg Flag_PrintGhcLibDir)
      "output GHC lib dir"
  , Option ['w'] ["no-warnings"] (NoArg Flag_NoWarnings) "turn off all warnings"
  , Option
      []
      ["no-tmp-comp-dir"]
      (NoArg Flag_NoTmpCompDir)
      "do not re-direct compilation output to a temporary directory"
  , Option
      []
      ["pretty-html"]
      (NoArg Flag_PrettyHtml)
      "generate html with newlines and indenting (for use with --html)"
  , Option
      []
      ["no-print-missing-docs"]
      (NoArg Flag_NoPrintMissingDocs)
      "don't print information about any undocumented entities"
  , Option
      []
      ["reexport"]
      (ReqArg Flag_Reexport "MOD")
      "reexport the module MOD, adding it to the index"
  , Option
      []
      ["package-name"]
      (ReqArg Flag_PackageName "NAME")
      "name of the package being documented"
  , Option
      []
      ["package-version"]
      (ReqArg Flag_PackageVersion "VERSION")
      "version of the package being documented in usual x.y.z.w format"
  , Option
      []
      ["since-qual"]
      (ReqArg Flag_SinceQualification "QUAL")
      "package qualification of @since, one of\n'always' (default) or 'only-external'"
  , Option
      []
      ["ignore-link-symbol"]
      (ReqArg Flag_IgnoreLinkSymbol "SYMBOL")
      "name of a symbol which does not trigger a warning in case of link issue"
  , Option
      ['j']
      []
      (OptArg (\count -> Flag_ParCount (fmap read count)) "n")
      "load modules in parallel"
  , Option
      []
      ["trace-args"]
      (NoArg Flag_TraceArgs)
      "print the arguments provided for this invocation to stdout"
  ]

getUsage :: IO String
getUsage = do
  prog <- getProgramName
  return $ usageInfo (usageHeader prog) (options False)
  where
    usageHeader :: String -> String
    usageHeader prog = "Usage: " ++ prog ++ " [OPTION...] file...\n"

parseHaddockOpts :: [String] -> IO ([Flag], [String])
parseHaddockOpts params =
  case getOpt Permute (options True) params of
    (flags, args, []) -> return (flags, args)
    (_, _, errors) -> do
      usage <- getUsage
      throwE (concat errors ++ usage)

optPackageVersion :: [Flag] -> Maybe Data.Version.Version
optPackageVersion flags =
  let ver = optLast [v | Flag_PackageVersion v <- flags]
   in ver >>= fmap fst . optLast . RP.readP_to_S parseVersion

optPackageName :: [Flag] -> Maybe PackageName
optPackageName flags =
  optLast [PackageName $ mkFastString n | Flag_PackageName n <- flags]

optTitle :: [Flag] -> Maybe String
optTitle flags =
  case [str | Flag_Heading str <- flags] of
    [] -> Nothing
    (t : _) -> Just t

outputDir :: [Flag] -> FilePath
outputDir flags =
  case [path | Flag_OutputDir path <- flags] of
    [] -> "."
    paths -> last paths

optContentsUrl :: [Flag] -> Maybe String
optContentsUrl flags = optLast [url | Flag_UseContents url <- flags]

optIndexUrl :: [Flag] -> Maybe String
optIndexUrl flags = optLast [url | Flag_UseIndex url <- flags]

optCssFile :: [Flag] -> Maybe FilePath
optCssFile flags = optLast [str | Flag_CSS str <- flags]

optSourceCssFile :: [Flag] -> Maybe FilePath
optSourceCssFile flags = optLast [str | Flag_SourceCss str <- flags]

sourceUrls :: [Flag] -> (Maybe String, Maybe String, Maybe String, Maybe String)
sourceUrls flags =
  ( optLast [str | Flag_SourceBaseURL str <- flags]
  , optLast [str | Flag_SourceModuleURL str <- flags]
  , optLast [str | Flag_SourceEntityURL str <- flags]
  , optLast [str | Flag_SourceLEntityURL str <- flags]
  )

wikiUrls :: [Flag] -> (Maybe String, Maybe String, Maybe String)
wikiUrls flags =
  ( optLast [str | Flag_WikiBaseURL str <- flags]
  , optLast [str | Flag_WikiModuleURL str <- flags]
  , optLast [str | Flag_WikiEntityURL str <- flags]
  )

baseUrl :: [Flag] -> Maybe String
baseUrl flags = optLast [str | Flag_BaseURL str <- flags]

optDumpInterfaceFile :: [Flag] -> Maybe FilePath
optDumpInterfaceFile flags = optLast [str | Flag_DumpInterface str <- flags]

optShowInterfaceFile :: [Flag] -> Maybe FilePath
optShowInterfaceFile flags = optLast [str | Flag_ShowInterface str <- flags]

optLaTeXStyle :: [Flag] -> Maybe String
optLaTeXStyle flags = optLast [str | Flag_LaTeXStyle str <- flags]

optMathjax :: [Flag] -> Maybe String
optMathjax flags = optLast [str | Flag_Mathjax str <- flags]

optParCount :: [Flag] -> Maybe (Maybe Int)
optParCount flags = optLast [n | Flag_ParCount n <- flags]

qualification :: [Flag] -> Either String QualOption
qualification flags =
  case map (map Char.toLower) [str | Flag_Qualification str <- flags] of
    [] -> Right OptNoQual
    ["none"] -> Right OptNoQual
    ["full"] -> Right OptFullQual
    ["local"] -> Right OptLocalQual
    ["relative"] -> Right OptRelativeQual
    [arg] -> Left $ "unknown qualification type " ++ show arg
    _ : _ -> Left "qualification option given multiple times"

sinceQualification :: [Flag] -> Either String SinceQual
sinceQualification flags =
  case map (map Char.toLower) [str | Flag_SinceQualification str <- flags] of
    [] -> Right Always
    ["always"] -> Right Always
    ["external"] -> Right External
    [arg] -> Left $ "unknown since-qualification type " ++ show arg
    _ : _ -> Left "since-qualification option given multiple times"

verbosity :: [Flag] -> Verbosity
verbosity flags =
  case [str | Flag_Verbosity str <- flags] of
    [] -> Normal
    x : _ -> case parseVerbosity x of
      Left e -> throwE e
      Right v -> v

-- | Get the ignored symbols from the given flags. These are the symbols for
-- which no link warnings will be generated if their link destinations cannot be
-- determined.
--
-- Symbols may be provided as qualified or unqualified names (e.g.
-- 'Data.Map.dropWhileEnd' or 'dropWhileEnd', resp). If qualified, no link
-- warnings will be produced for occurances of that name when it is imported
-- from that module. If unqualified, no link warnings will be produced for any
-- occurances of that name from any module.
ignoredSymbols :: [Flag] -> Map (Maybe String) (Set String)
ignoredSymbols flags =
  foldr addToMap Map.empty [splitSymbol symbol | Flag_IgnoreLinkSymbol symbol <- flags]
  where
    -- Split a symbol into its module name and unqualified name, producing
    -- 'Nothing' for the module name if the given symbol is already unqualified
    splitSymbol :: String -> (Maybe String, String)
    splitSymbol s =
      -- Drop the longest suffix not containing a '.' character
      case dropWhileEnd (/= '.') s of
        -- If the longest suffix is empty, there was no '.'.
        -- Assume it is an unqualified name (no module string).
        "" -> (Nothing, s)
        -- If the longest suffix is not empty, there was a '.'.
        -- Assume it is a qualified name. `s'` will be the module string followed
        -- by the last '.', e.g. "Data.List.", so take `init s'` as the module
        -- string. Drop the length of `s'` from the original string `s` to
        -- obtain to the unqualified name.
        s' -> (Just $ init s', drop (length s') s)

    -- Add a (module name, name) pair to the map from modules to their ignored
    -- symbols
    addToMap :: (Maybe String, String) -> Map (Maybe String) (Set String) -> Map (Maybe String) (Set String)
    addToMap (m, name) symbs = Map.insertWith (Set.union) m (Set.singleton name) symbs

ghcFlags :: [Flag] -> [String]
ghcFlags flags = [option | Flag_OptGhc option <- flags]

reexportFlags :: [Flag] -> [String]
reexportFlags flags = [option | Flag_Reexport option <- flags]

data Visibility = Visible | Hidden
  deriving (Eq, Show)

readIfaceArgs :: [Flag] -> [(DocPaths, Visibility, FilePath)]
readIfaceArgs flags = [parseIfaceOption s | Flag_ReadInterface s <- flags]
  where
    parseIfaceOption :: String -> (DocPaths, Visibility, FilePath)
    parseIfaceOption str =
      case break (== ',') str of
        (fpath, ',' : rest) ->
          case break (== ',') rest of
            (src, ',' : rest') ->
              let src' = case src of
                    "" -> Nothing
                    _  -> Just (src ++ "/%M.html")
               in case break (== ',') rest' of
                    (visibility, ',' : file)
                      | visibility == "hidden" ->
                          ((fpath, src'), Hidden, file)
                      | otherwise ->
                          ((fpath, src'), Visible, file)
                    (file, _) ->
                      ((fpath, src'), Visible, file)
            (file, _) -> ((fpath, Nothing), Visible, file)
        (file, _) -> (("", Nothing), Visible, file)

-- | Like 'listToMaybe' but returns the last element instead of the first.
optLast :: [a] -> Maybe a
optLast [] = Nothing
optLast xs = Just (last xs)

-- | This function has a potential to return 'Nothing' because package name and
-- versions can no longer reliably be extracted in all cases: if the package is
-- not installed yet then this info is no longer available.
--
-- The @--package-name@ and @--package-version@ Haddock flags allow the user to
-- specify this information manually and it is returned here if present.
modulePackageInfo
  :: UnitState
  -> [Flag]
  -- ^ Haddock flags are checked as they may contain
  -- the package name or version provided by the user
  -- which we prioritise
  -> Maybe Module
  -> (Maybe PackageName, Maybe Data.Version.Version)
modulePackageInfo _unit_state _flags Nothing = (Nothing, Nothing)
modulePackageInfo unit_state flags (Just modu) =
  ( optPackageName flags <|> fmap unitPackageName pkgDb
  , optPackageVersion flags <|> fmap unitPackageVersion pkgDb
  )
  where
    pkgDb = lookupUnit unit_state (moduleUnit modu)
