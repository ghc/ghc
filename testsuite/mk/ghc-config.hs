import System.Environment
import System.Process
import Data.Maybe

main = do
  [ghc] <- getArgs

  info <- readProcess ghc ["+RTS", "--info"] ""
  let fields = read info :: [(String,String)]
  getGhcField fields "HostOS" "Host OS"
  getGhcField fields "WORDSIZE" "Word size"
  getGhcField fields "TARGETPLATFORM" "Target platform"
  getGhcField fields "TargetOS_CPP" "Target OS"
  getGhcField fields "TargetARCH_CPP" "Target architecture"

  info <- readProcess ghc ["--info"] ""
  let fields = read info :: [(String,String)]

  getGhcField fields "GhcStage" "Stage"
  getGhcField fields "GhcWithNativeCodeGen" "Have native code generator"
  getGhcField fields "GhcWithInterpreter" "Have interpreter"
  getGhcField fields "GhcUnregisterised" "Unregisterised"
  getGhcField fields "GhcWithSMP" "Support SMP"
  getGhcField fields "GhcRTSWays" "RTS ways"
  getGhcFieldProgWithDefault fields "AR" "ar command" "ar"

  let pkgdb_flag = case lookup "Project version" fields of
        Just v
          | parseVersion v >= [7,5] -> "package-db"
        _ -> "package-conf"
  putStrLn $ "GhcPackageDbFlag" ++ '=':pkgdb_flag


getGhcField :: [(String,String)] -> String -> String -> IO ()
getGhcField fields mkvar key =
   case lookup key fields of
      Nothing  -> fail ("No field: " ++ key)
      Just val -> putStrLn (mkvar ++ '=':val)

getGhcFieldProgWithDefault :: [(String,String)]
                           -> String -> String -> String -> IO ()
getGhcFieldProgWithDefault fields mkvar key deflt = do
   case lookup key fields of
      Nothing  -> putStrLn (mkvar ++ '=' : deflt)
      Just val -> putStrLn (mkvar ++ '=' : fixSlashes (fixTopdir topdir val))
 where
  topdir = fromMaybe "" (lookup "LibDir" fields)

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
