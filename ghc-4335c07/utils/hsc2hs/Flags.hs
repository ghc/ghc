
module Flags where

import System.Console.GetOpt

data Mode
    = Help
    | Version
    | UseConfig (ConfigM Maybe)

newtype Id a = Id { fromId :: a }
type Config = ConfigM Id

data ConfigM m = Config {
                     cmTemplate :: m FilePath,
                     cmCompiler :: m FilePath,
                     cmLinker   :: m FilePath,
                     cKeepFiles :: Bool,
                     cNoCompile :: Bool,
                     cCrossCompile :: Bool,
                     cCrossSafe :: Bool,
                     cColumn :: Bool,
                     cVerbose :: Bool,
                     cFlags :: [Flag]
                 }

cTemplate :: ConfigM Id -> FilePath
cTemplate c = fromId $ cmTemplate c

cCompiler :: ConfigM Id -> FilePath
cCompiler c = fromId $ cmCompiler c

cLinker :: ConfigM Id -> FilePath
cLinker c = fromId $ cmLinker c

emptyMode :: Mode
emptyMode = UseConfig $ Config {
                            cmTemplate    = Nothing,
                            cmCompiler    = Nothing,
                            cmLinker      = Nothing,
                            cKeepFiles    = False,
                            cNoCompile    = False,
                            cCrossCompile = False,
                            cCrossSafe    = False,
                            cColumn       = False,
                            cVerbose      = False,
                            cFlags        = []
                        }

data Flag
    = CompFlag  String
    | LinkFlag  String
    | Include   String
    | Define    String (Maybe String)
    | Output    String
    deriving Show

options :: [OptDescr (Mode -> Mode)]
options = [
    Option ['o'] ["output"]     (ReqArg (addFlag . Output)     "FILE")
        "name of main output file",
    Option ['t'] ["template"]   (ReqArg (withConfig . setTemplate)   "FILE")
        "template file",
    Option ['c'] ["cc"]         (ReqArg (withConfig . setCompiler)   "PROG")
        "C compiler to use",
    Option ['l'] ["ld"]         (ReqArg (withConfig . setLinker)     "PROG")
        "linker to use",
    Option ['C'] ["cflag"]      (ReqArg (addFlag . CompFlag)   "FLAG")
        "flag to pass to the C compiler",
    Option ['I'] []             (ReqArg (addFlag . CompFlag . ("-I"++)) "DIR")
        "passed to the C compiler",
    Option ['L'] ["lflag"]      (ReqArg (addFlag . LinkFlag)   "FLAG")
        "flag to pass to the linker",
    Option ['i'] ["include"]    (ReqArg (addFlag . include)    "FILE")
        "as if placed in the source",
    Option ['D'] ["define"]     (ReqArg (addFlag . define) "NAME[=VALUE]")
        "as if placed in the source",
    Option []    ["no-compile"] (NoArg  (withConfig $ setNoCompile True))
        "stop after writing *_hsc_make.c",
    Option ['x'] ["cross-compile"] (NoArg (withConfig $ setCrossCompile True))
        "activate cross-compilation mode",
    Option [] ["cross-safe"] (NoArg (withConfig $ setCrossSafe True))
        "restrict .hsc directives to those supported by --cross-compile",
    Option ['k'] ["keep-files"] (NoArg (withConfig $ setKeepFiles True))
        "do not remove temporary files",
    Option [] ["column"]     (NoArg (withConfig $ setColumn True))
        "annotate output with COLUMN pragmas (requires GHC 8.2)",
    Option ['v'] ["verbose"]    (NoArg  (withConfig $ setVerbose True))
        "dump commands to stderr",
    Option ['?'] ["help"]       (NoArg  (setMode Help))
        "display this help and exit",
    Option ['V'] ["version"]    (NoArg  (setMode Version))
        "output version information and exit" ]

addFlag :: Flag -> Mode -> Mode
addFlag f (UseConfig c) = UseConfig $ c { cFlags = f : cFlags c }
addFlag _ mode = mode

setMode :: Mode -> Mode -> Mode
setMode Help           _    = Help
setMode _              Help = Help
setMode Version        _    = Version
setMode (UseConfig {}) _    = error "setMode: UseConfig: Can't happen"

withConfig :: (ConfigM Maybe -> ConfigM Maybe) -> Mode -> Mode
withConfig f (UseConfig c) = UseConfig $ f c
withConfig _ m = m

setTemplate :: FilePath -> ConfigM Maybe -> ConfigM Maybe
setTemplate fp c = c { cmTemplate = Just fp }

setCompiler :: FilePath -> ConfigM Maybe -> ConfigM Maybe
setCompiler fp c = c { cmCompiler = Just fp }

setLinker :: FilePath -> ConfigM Maybe -> ConfigM Maybe
setLinker fp c = c { cmLinker = Just fp }

setKeepFiles :: Bool -> ConfigM Maybe -> ConfigM Maybe
setKeepFiles b c = c { cKeepFiles = b }

setNoCompile :: Bool -> ConfigM Maybe -> ConfigM Maybe
setNoCompile b c = c { cNoCompile = b }

setCrossCompile :: Bool -> ConfigM Maybe -> ConfigM Maybe
setCrossCompile b c = c { cCrossCompile = b }

setCrossSafe :: Bool -> ConfigM Maybe -> ConfigM Maybe
setCrossSafe b c = c { cCrossSafe = b }

setColumn :: Bool -> ConfigM Maybe -> ConfigM Maybe
setColumn b c = c { cColumn = b }

setVerbose :: Bool -> ConfigM Maybe -> ConfigM Maybe
setVerbose v c = c { cVerbose = v }

include :: String -> Flag
include s@('\"':_) = Include s
include s@('<' :_) = Include s
include s          = Include ("\""++s++"\"")

define :: String -> Flag
define s = case break (== '=') s of
    (name, [])      -> Define name Nothing
    (name, _:value) -> Define name (Just value)

