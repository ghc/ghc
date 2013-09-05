-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Options
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Definition of the command line interface of Haddock.
-----------------------------------------------------------------------------
module Haddock.Options (
  parseHaddockOpts,
  Flag(..),
  getUsage,
  optTitle,
  outputDir,
  optContentsUrl,
  optIndexUrl,
  optCssFile,
  sourceUrls,
  wikiUrls,
  optDumpInterfaceFile,
  optLaTeXStyle,
  qualification,
  verbosity,
  ghcFlags,
  readIfaceArgs
) where


import Data.Maybe
import Distribution.Verbosity
import Haddock.Utils
import Haddock.Types
import System.Console.GetOpt
import qualified Data.Char as Char


data Flag
  = Flag_BuiltInThemes
  | Flag_CSS String
--  | Flag_DocBook
  | Flag_ReadInterface String
  | Flag_DumpInterface String
  | Flag_Heading String
  | Flag_Html
  | Flag_Hoogle
  | Flag_Lib String
  | Flag_OutputDir FilePath
  | Flag_Prologue FilePath
  | Flag_SourceBaseURL   String
  | Flag_SourceModuleURL String
  | Flag_SourceEntityURL String
  | Flag_WikiBaseURL   String
  | Flag_WikiModuleURL String
  | Flag_WikiEntityURL String
  | Flag_LaTeX
  | Flag_LaTeXStyle String
  | Flag_Help
  | Flag_Verbosity String
  | Flag_Version
  | Flag_CompatibleInterfaceVersions
  | Flag_InterfaceVersion
  | Flag_UseContents String
  | Flag_GenContents
  | Flag_UseIndex String
  | Flag_GenIndex
  | Flag_IgnoreAllExports
  | Flag_HideModule String
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
  deriving (Eq)


options :: Bool -> [OptDescr Flag]
options backwardsCompat =
  [
    Option ['B']  []     (ReqArg Flag_GhcLibDir "DIR")
      "path to a GHC lib dir, to override the default path",
    Option ['o']  ["odir"]     (ReqArg Flag_OutputDir "DIR")
      "directory in which to put the output files",
    Option ['l']  ["lib"]         (ReqArg Flag_Lib "DIR")
      "location of Haddock's auxiliary files",
    Option ['i'] ["read-interface"] (ReqArg Flag_ReadInterface "FILE")
      "read an interface from FILE",
    Option ['D']  ["dump-interface"] (ReqArg Flag_DumpInterface "FILE")
      "write the resulting interface to FILE",
--    Option ['S']  ["docbook"]  (NoArg Flag_DocBook)
--  "output in DocBook XML",
    Option ['h']  ["html"]     (NoArg Flag_Html)
      "output in HTML (XHTML 1.0)",
    Option []  ["latex"]  (NoArg Flag_LaTeX) "use experimental LaTeX rendering",
    Option []  ["latex-style"]  (ReqArg Flag_LaTeXStyle "FILE") "provide your own LaTeX style in FILE",
    Option ['U'] ["use-unicode"] (NoArg Flag_UseUnicode) "use Unicode in HTML output",
    Option []  ["hoogle"]     (NoArg Flag_Hoogle)
      "output for Hoogle",
    Option []  ["source-base"]   (ReqArg Flag_SourceBaseURL "URL")
      "URL for a source code link on the contents\nand index pages",
    Option ['s'] (if backwardsCompat then ["source", "source-module"] else ["source-module"])
      (ReqArg Flag_SourceModuleURL "URL")
      "URL for a source code link for each module\n(using the %{FILE} or %{MODULE} vars)",
    Option []  ["source-entity"]  (ReqArg Flag_SourceEntityURL "URL")
      "URL for a source code link for each entity\n(using the %{FILE}, %{MODULE}, %{NAME},\n%{KIND} or %{LINE} vars)",
    Option []  ["comments-base"]   (ReqArg Flag_WikiBaseURL "URL")
      "URL for a comments link on the contents\nand index pages",
    Option []  ["comments-module"]  (ReqArg Flag_WikiModuleURL "URL")
      "URL for a comments link for each module\n(using the %{MODULE} var)",
    Option []  ["comments-entity"]  (ReqArg Flag_WikiEntityURL "URL")
      "URL for a comments link for each entity\n(using the %{FILE}, %{MODULE}, %{NAME},\n%{KIND} or %{LINE} vars)",
    Option ['c']  ["css", "theme"] (ReqArg Flag_CSS "PATH")
      "the CSS file or theme directory to use for HTML output",
    Option []  ["built-in-themes"] (NoArg Flag_BuiltInThemes)
      "include all the built-in haddock themes",
    Option ['p']  ["prologue"] (ReqArg Flag_Prologue "FILE")
      "file containing prologue text",
    Option ['t']  ["title"]    (ReqArg Flag_Heading "TITLE")
      "page heading",
    Option ['q']  ["qual"] (ReqArg Flag_Qualification "QUAL")
      "qualification of names, one of \n'none' (default), 'full', 'local'\n'relative' or 'aliased'",
    Option ['?']  ["help"]  (NoArg Flag_Help)
      "display this help and exit",
    Option ['V']  ["version"]  (NoArg Flag_Version)
      "output version information and exit",
    Option []  ["compatible-interface-versions"]  (NoArg Flag_CompatibleInterfaceVersions)
      "output compatible interface file versions and exit",
    Option []  ["interface-version"]  (NoArg Flag_InterfaceVersion)
      "output interface file version and exit",
    Option ['v']  ["verbosity"]  (ReqArg Flag_Verbosity "VERBOSITY")
      "set verbosity level",
    Option [] ["use-contents"] (ReqArg Flag_UseContents "URL")
      "use a separately-generated HTML contents page",
    Option [] ["gen-contents"] (NoArg Flag_GenContents)
      "generate an HTML contents from specified\ninterfaces",
    Option [] ["use-index"] (ReqArg Flag_UseIndex "URL")
      "use a separately-generated HTML index",
    Option [] ["gen-index"] (NoArg Flag_GenIndex)
      "generate an HTML index from specified\ninterfaces",
    Option [] ["ignore-all-exports"] (NoArg Flag_IgnoreAllExports)
      "behave as if all modules have the\nignore-exports atribute",
    Option [] ["hide"] (ReqArg Flag_HideModule "MODULE")
      "behave as if MODULE has the hide attribute",
    Option [] ["optghc"] (ReqArg Flag_OptGhc "OPTION")
      "option to be forwarded to GHC",
    Option []  ["ghc-version"]  (NoArg Flag_GhcVersion)
      "output GHC version in numeric format",
    Option []  ["print-ghc-path"]  (NoArg Flag_PrintGhcPath)
      "output path to GHC binary",
    Option []  ["print-ghc-libdir"]  (NoArg Flag_PrintGhcLibDir)
      "output GHC lib dir",
    Option ['w'] ["no-warnings"] (NoArg Flag_NoWarnings) "turn off all warnings",
    Option [] ["no-tmp-comp-dir"] (NoArg Flag_NoTmpCompDir)
      "do not re-direct compilation output to a temporary directory",
    Option [] ["pretty-html"] (NoArg Flag_PrettyHtml)
      "generate html with newlines and indenting (for use with --html)"
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
  case getOpt Permute (options True) params  of
    (flags, args, []) -> return (flags, args)
    (_, _, errors)    -> do
      usage <- getUsage
      throwE (concat errors ++ usage)


optTitle :: [Flag] -> Maybe String
optTitle flags =
  case [str | Flag_Heading str <- flags] of
    [] -> Nothing
    (t:_) -> Just t


outputDir :: [Flag] -> FilePath
outputDir flags =
  case [ path | Flag_OutputDir path <- flags ] of
    []    -> "."
    paths -> last paths


optContentsUrl :: [Flag] -> Maybe String
optContentsUrl flags = optLast [ url | Flag_UseContents url <- flags ]


optIndexUrl :: [Flag] -> Maybe String
optIndexUrl flags = optLast [ url | Flag_UseIndex url <- flags ]


optCssFile :: [Flag] -> Maybe FilePath
optCssFile flags = optLast [ str | Flag_CSS str <- flags ]


sourceUrls :: [Flag] -> (Maybe String, Maybe String, Maybe String)
sourceUrls flags =
  (listToMaybe [str | Flag_SourceBaseURL   str <- flags]
  ,listToMaybe [str | Flag_SourceModuleURL str <- flags]
  ,listToMaybe [str | Flag_SourceEntityURL str <- flags])


wikiUrls :: [Flag] -> (Maybe String, Maybe String, Maybe String)
wikiUrls flags =
  (listToMaybe [str | Flag_WikiBaseURL   str <- flags]
  ,listToMaybe [str | Flag_WikiModuleURL str <- flags]
  ,listToMaybe [str | Flag_WikiEntityURL str <- flags])


optDumpInterfaceFile :: [Flag] -> Maybe FilePath
optDumpInterfaceFile flags = optLast [ str | Flag_DumpInterface str <- flags ]


optLaTeXStyle :: [Flag] -> Maybe String
optLaTeXStyle flags = optLast [ str | Flag_LaTeXStyle str <- flags ]


qualification :: [Flag] -> Either String QualOption
qualification flags =
  case map (map Char.toLower) [ str | Flag_Qualification str <- flags ] of
      []             -> Right OptNoQual
      ["none"]       -> Right OptNoQual
      ["full"]       -> Right OptFullQual
      ["local"]      -> Right OptLocalQual
      ["relative"]   -> Right OptRelativeQual
      ["aliased"]    -> Right OptAliasedQual
      [arg]          -> Left $ "unknown qualification type " ++ show arg
      _:_            -> Left "qualification option given multiple times"


verbosity :: [Flag] -> Verbosity
verbosity flags =
  case [ str | Flag_Verbosity str <- flags ] of
    []  -> normal
    x:_ -> case parseVerbosity x of
      Left e -> throwE e
      Right v -> v


ghcFlags :: [Flag] -> [String]
ghcFlags flags = [ option | Flag_OptGhc option <- flags ]


readIfaceArgs :: [Flag] -> [(DocPaths, FilePath)]
readIfaceArgs flags = [ parseIfaceOption s | Flag_ReadInterface s <- flags ]
  where
    parseIfaceOption :: String -> (DocPaths, FilePath)
    parseIfaceOption str =
      case break (==',') str of
        (fpath, ',':rest) ->
          case break (==',') rest of
            (src, ',':file) -> ((fpath, Just src), file)
            (file, _) -> ((fpath, Nothing), file)
        (file, _) -> (("", Nothing), file)


-- | Like 'listToMaybe' but returns the last element instead of the first.
optLast :: [a] -> Maybe a
optLast [] = Nothing
optLast xs = Just (last xs)
