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

data CLIFlags
  = OptASCIIOutput
  | OptHTMLOutput
  | OptIgnoreSmallTimes Float
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
  ]

