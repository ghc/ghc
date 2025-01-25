{-# OPTIONS_GHC -Wall -Wno-missing-fields #-}

import GHC hiding (parseModule)
import GHC.Data.StringBuffer
import GHC.Driver.Config.Parser
import GHC.Parser
import GHC.Parser.Lexer
import GHC.Platform
import GHC.Plugins
import GHC.Settings
import GHC.Settings.Config
import System.Mem.Weak

fakeSettings :: Settings
fakeSettings =
  Settings
    { sGhcNameVersion =
        GhcNameVersion
          { ghcNameVersion_programName =
              "ghc",
            ghcNameVersion_projectVersion =
              cProjectVersion
          },
      sFileSettings =
        FileSettings {},
      sToolSettings = ToolSettings {},
      sTargetPlatform =
        genericPlatform,
      sPlatformMisc = PlatformMisc {},
      sUnitSettings = UnitSettings { unitSettings_baseUnitId = stringToUnitId "base" }
    }

fakeDynFlags :: DynFlags
fakeDynFlags = defaultDynFlags fakeSettings

parse :: DynFlags -> String -> IO (Located (HsModule GhcPs))
parse dflags src = do
  let buf = stringToStringBuffer src
  let loc = mkRealSrcLoc (mkFastString "Main.hs") 1 1
  case unP parseModule (initParserState (initParserOpts dflags) buf loc) of
    PFailed _ -> fail "parseModule failed"
    POk _ rdr_module -> pure rdr_module

main :: IO ()
main = do
  _ <- mkWeak runGhc runGhc Nothing
  m <- parse fakeDynFlags "main = putStrLn \"hello world\""
  putStrLn $ showSDoc fakeDynFlags $ ppr m
