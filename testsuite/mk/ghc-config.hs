import System.Environment
import System.Process

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
  getGhcField fields "AR" "ar command"

getGhcField :: [(String,String)] -> String -> String -> IO ()
getGhcField fields mkvar key = do
   case lookup key fields of
      Nothing  -> fail ("No field: " ++ key)
      Just val -> putStrLn (mkvar ++ '=':val)
