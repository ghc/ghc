import System.Environment
import System.Process
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
  [ghc] <- getArgs

  info <- readProcess ghc ["+RTS", "--info"] ""
  let fields = read info :: [(String,String)]
  getGhcFieldOrFail fields "HostOS" "Host OS"
  getGhcFieldOrFail fields "WORDSIZE" "Word size"
  getGhcFieldOrFail fields "TARGETPLATFORM" "Host platform"
  getGhcFieldOrFail fields "TargetOS_CPP" "Host OS"
  getGhcFieldOrFail fields "TargetARCH_CPP" "Host architecture"
  getGhcFieldOrFail fields "RTSWay" "RTS way"

  -- support for old GHCs (pre 9.13): infer target platform by querying the rts...
  let query_rts = isJust (lookup "Target platform" fields)
  when query_rts $ do
    getGhcFieldOrFail fields "WORDSIZE" "Word size"
    getGhcFieldOrFail fields "TARGETPLATFORM" "Target platform"
    getGhcFieldOrFail fields "TargetOS_CPP" "Target OS"
    getGhcFieldOrFail fields "TargetARCH_CPP" "Target architecture"

  info <- readProcess ghc ["--info"] ""
  let fields = read info :: [(String,String)]

  unless query_rts $ do
    getGhcFieldOrFail fields "WORDSIZE" "target word size in bits"
    getGhcFieldOrFail fields "TARGETPLATFORM" "target platform string"
    getGhcFieldOrFail fields "TargetOS_CPP" "target os string"
    getGhcFieldOrFail fields "TargetARCH_CPP" "target arch string"

  getGhcFieldOrFail fields "GhcStage" "Stage"
  getGhcFieldOrFail fields "GhcDebugAssertions" "Debug on"
  getGhcFieldOrFail fields "GhcWithNativeCodeGen" "Have native code generator"
  getGhcFieldOrFail fields "GhcWithInterpreter" "Have interpreter"
  getGhcFieldOrFail fields "CrossCompiling" "cross compiling"
  getGhcFieldOrFail fields "GhcWithRtsLinker" "target has RTS linker"
  getGhcFieldOrFail fields "GhcUnregisterised" "Unregisterised"
  getGhcFieldOrFail fields "GhcWithSMP" "Support SMP"
  getGhcFieldOrFail fields "GhcRTSWays" "RTS ways"
  getGhcFieldOrFail fields "GhcLibdir" "LibDir"
  getGhcFieldOrFail fields "GhcGlobalPackageDb" "Global Package DB"
  getGhcFieldOrDefault fields "TargetRTSLinkerOnlySupportsSharedLibs" "target RTS linker only supports shared libraries" "NO"
  getGhcFieldOrDefault fields "GhcDynamic" "GHC Dynamic" "NO"
  getGhcFieldOrDefault fields "GhcProfiled" "GHC Profiled" "NO"
  getGhcFieldOrDefault fields "LeadingUnderscore" "Leading underscore" "NO"
  getGhcFieldOrDefault fields "GhcTablesNextToCode" "Tables next to code" "NO"
  getGhcFieldProgWithDefault fields "AR" "ar command" "ar"
  getGhcFieldProgWithDefault fields "LLC" "LLVM llc command" "llc"
  getGhcFieldProgWithDefault fields "TEST_CC" "C compiler command" "gcc"
  getGhcFieldProgWithDefault fields "TEST_CC_OPTS" "C compiler flags" ""
  getGhcFieldProgWithDefault fields "TEST_CXX" "C++ compiler command" "g++"

getGhcFieldOrFail :: [(String,String)] -> String -> String -> IO ()
getGhcFieldOrFail fields mkvar key
   = getGhcField fields mkvar key id (fail ("No field: " ++ key))

getGhcFieldOrDefault :: [(String,String)] -> String -> String -> String -> IO ()
getGhcFieldOrDefault fields mkvar key deflt
  = getGhcField fields mkvar key id on_fail
  where
    on_fail = putStrLn (mkvar ++ '=' : deflt)

getGhcFieldProgWithDefault
   :: [(String,String)]
   -> String -> String -> String
   -> IO ()
getGhcFieldProgWithDefault fields mkvar key deflt
  = getGhcField fields mkvar key fix on_fail
  where
    fix val = fixSlashes (fixTopdir topdir val)
    topdir = fromMaybe "" (lookup "LibDir" fields)
    on_fail = putStrLn (mkvar ++ '=' : deflt)

getGhcField
   :: [(String,String)] -> String -> String
   -> (String -> String)
   -> IO ()
   -> IO ()
getGhcField fields mkvar key fix on_fail =
   case lookup key fields of
      Nothing  -> on_fail
      Just val -> putStrLn (mkvar ++ '=' : fix val)

fixTopdir :: String -> String -> String
fixTopdir t "" = ""
fixTopdir t ('$':'t':'o':'p':'d':'i':'r':s) = t ++ s
fixTopdir t (c:s) = c : fixTopdir t s

fixSlashes :: FilePath -> FilePath
fixSlashes = map f
    where f '\\' = '/'
          f c    = c

parseVersion :: String -> [Int]
parseVersion v = case break (== '.') v of
  (n, rest) -> read n : case rest of
    [] -> []
    ('.':v') -> parseVersion v'
    _ -> error "bug in parseVersion"
