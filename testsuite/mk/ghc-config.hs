import System.Environment
import System.Process
import Data.Maybe

main = do
  [ghc] <- getArgs

  info <- readProcess ghc ["+RTS", "--info"] ""
  let fields = read info :: [(String,String)]
  getGhcFieldOrFail fields "HostOS" "Host OS"
  getGhcFieldOrFail fields "WORDSIZE" "Word size"
  getGhcFieldOrFail fields "TARGETPLATFORM" "Target platform"
  getGhcFieldOrFail fields "TargetOS_CPP" "Target OS"
  getGhcFieldOrFail fields "TargetARCH_CPP" "Target architecture"

  info <- readProcess ghc ["--info"] ""
  let fields = read info :: [(String,String)]

  getGhcFieldOrFail fields "GhcStage" "Stage"
  getGhcFieldOrFail fields "GhcDebugged" "Debug on"
  getGhcFieldOrFail fields "GhcWithNativeCodeGen" "Have native code generator"
  getGhcFieldOrFail fields "GhcWithInterpreter" "Have interpreter"
  getGhcFieldOrFail fields "GhcUnregisterised" "Unregisterised"
  getGhcFieldOrFail fields "GhcWithSMP" "Support SMP"
  getGhcFieldOrFail fields "GhcRTSWays" "RTS ways"
  getGhcFieldOrDefault fields "GhcDynamicByDefault" "Dynamic by default" "NO"
  getGhcFieldOrDefault fields "GhcDynamic" "GHC Dynamic" "NO"
  getGhcFieldProgWithDefault fields "AR" "ar command" "ar"

  let pkgdb_flag = case lookup "Project version" fields of
        Just v
          | parseVersion v >= [7,5] -> "package-db"
        _ -> "package-conf"
  putStrLn $ "GhcPackageDbFlag" ++ '=':pkgdb_flag


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
