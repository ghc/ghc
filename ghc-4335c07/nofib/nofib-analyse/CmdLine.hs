-----------------------------------------------------------------------------
-- CmdLine.hs

-- (c) Simon Marlow 2005
-----------------------------------------------------------------------------

module CmdLine
    (
    flags, other_args, cmdline_errors,
    devs, nodevs, tooquick_threshold, reportTitle,
    CLIFlags(..), usage,
    )
    where

import System.Console.GetOpt
import System.Environment       ( getArgs )
import System.IO.Unsafe         ( unsafePerformIO )

-----------------------------------------------------------------------------
-- Command line arguments

args :: [String]
args = unsafePerformIO getArgs

flags :: [CLIFlags]
other_args :: [String]
cmdline_errors :: [String]
(flags, other_args, cmdline_errors) = getOpt Permute argInfo args

default_tooquick_threshold, tooquick_threshold :: Float
default_tooquick_threshold = 0.2 {- secs -}
tooquick_threshold
 = case [ i | OptIgnoreSmallTimes i <- flags ] of
        [] -> default_tooquick_threshold
        (i:_) -> i

devs, nodevs :: Bool
devs   = OptDeviations   `elem` flags
nodevs = OptNoDeviations `elem` flags

default_title, reportTitle :: String
default_title = "NoFib Results"
reportTitle = case [ t | OptTitle t <- flags ] of
        []    -> default_title
        (t:_) -> t

data CLIFlags
  = OptASCIIOutput
  | OptLaTeXOutput (Maybe String)
  | OptIgnoreSmallTimes Float
  | OptDeviations
  | OptNoDeviations
  | OptTitle String
  | OptColumns String
  | OptRows String
  | OptCSV String
  | OptNormalise String
  | OptStdDev
  | OptShowBaseline
  | OptHelp
  deriving Eq

usageHeader :: String
usageHeader = "usage: nofib-analyse [OPTION...] <logfile1> <logfile2> ..."

usage :: String
usage = usageInfo usageHeader argInfo

argInfo :: [ OptDescr CLIFlags ]
argInfo =
  [ Option ['?'] ["help"]    (NoArg OptHelp)
        "Display this message"
  , Option ['a'] ["ascii"]   (NoArg OptASCIIOutput)
        "Produce ASCII output (default)"
  , Option ['i'] ["ignore"]  (ReqArg (OptIgnoreSmallTimes . read) "secs")
        "Ignore runtimes smaller than <secs>"
  , Option ['d'] ["deviations"] (NoArg OptDeviations)
        "Display deviations (default)"
  , Option ['l'] ["latex"]    (OptArg OptLaTeXOutput "TABLE")
        "Produce LaTeX output"
  , Option [] ["columns"] (ReqArg OptColumns "COLUMNS")
        "Specify columns for summary table (comma separates)"
  , Option [] ["rows"] (ReqArg OptRows "ROWS")
        "Specify rows for summary table (comma separates)"
  , Option [] ["csv"] (ReqArg OptCSV "TABLE")
        "Output a single table in CSV format"
  , Option [] ["normalise"] (ReqArg OptNormalise "percent|ratio|none")
        "normalise to the baseline"
  , Option [] ["stddev"] (NoArg OptStdDev)
        "include standard deviations in CSV output"
  , Option ['n'] ["nodeviations"] (NoArg OptNoDeviations)
        "Hide deviations"
  , Option ['t'] ["title"] (ReqArg OptTitle "title")
        "Specify report title"
  , Option ['b'] ["include-baseline"] (NoArg OptShowBaseline)
        "Include the baseline column (LaTeX)"
  ]

