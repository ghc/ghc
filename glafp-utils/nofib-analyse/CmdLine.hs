-----------------------------------------------------------------------------
-- CmdLine.hs

-- (c) Simon Marlow 1999
-----------------------------------------------------------------------------

module CmdLine where

import GetOpt
import System
import IOExts

-----------------------------------------------------------------------------
-- Command line arguments

args = unsafePerformIO getArgs
(flags, other_args, cmdline_errors) = getOpt Permute argInfo args 

default_tooquick_threshold = 0.2 {- secs -} :: Float
tooquick_threshold
 = case [ i | OptIgnoreSmallTimes i <- flags ] of
	[] -> default_tooquick_threshold
	(i:_) -> i

devs   = OptDeviations   `elem` flags
nodevs = OptNoDeviations `elem` flags

data CLIFlags
  = OptASCIIOutput
  | OptHTMLOutput
  | OptIgnoreSmallTimes Float
  | OptDeviations
  | OptNoDeviations
  | OptHelp
  deriving Eq

argInfo :: [ OptDescr CLIFlags ]
argInfo = 
  [ Option ['?'] ["help"]    (NoArg OptHelp)        
	"Display this message"
  , Option ['a'] ["ascii"]   (NoArg OptASCIIOutput) 
	"Produce ASCII output (default)"
  , Option ['h'] ["html"]    (NoArg OptHTMLOutput)  
	"Produce HTML output"
  , Option ['i'] ["ignore"]  (ReqArg (OptIgnoreSmallTimes . read) "secs")
	"Ignore runtimes smaller than <secs>"
  , Option ['d'] ["deviations"] (NoArg OptDeviations)
	"Display deviations (default)"
  , Option ['n'] ["nodeviations"] (NoArg OptNoDeviations)
	"Hide deviations"
  ]

