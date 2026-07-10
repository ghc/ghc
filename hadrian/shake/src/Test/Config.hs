
module Test.Config(main) where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Config
import Test.Type
import Data.List.Extra
import qualified Data.HashMap.Strict as Map
import Data.Maybe


main = testBuild test $ do
    want ["hsflags.var","cflags.var","none.var","keys"]
    usingConfigFile "config"
    "*.var" %> \out -> do
        cfg <- getConfig $ upper $ takeBaseName out
        liftIO $ appendFile (out -<.> "times") "X"
        writeFile' out $ fromMaybe "" cfg
    "keys" %> \out -> do
        liftIO $ appendFile "keys.times" "X"
        liftIO . writeFile out . unwords =<< getConfigKeys


test build = do
    build ["clean"]
    writeFile "config" $ unlines
        ["HEADERS_DIR = /path/to/dir"
        ,"CFLAGS = -O2 -I${HEADERS_DIR} -g"
        ,"HSFLAGS = -O2"]
    build []
    assertContents "cflags.var" "-O2 -I/path/to/dir -g"
    assertContents "hsflags.var" "-O2"
    assertContents "none.var" ""
    assertContents "keys" "CFLAGS HEADERS_DIR HSFLAGS"

    appendFile "config" $ unlines
        ["CFLAGS = $CFLAGS -w"]
    build []
    assertContents "cflags.var" "-O2 -I/path/to/dir -g -w"
    assertContents "hsflags.var" "-O2"
    assertContents "cflags.times" "XX"
    assertContents "hsflags.times" "X"
    assertContents "keys.times" "X"

    -- Test readConfigFileWithEnv
    writeFile "config" $ unlines
      ["HEADERS_DIR = ${SOURCE_DIR}/path/to/dir"
      ,"CFLAGS = -O2 -I${HEADERS_DIR} -g"]
    vars <- readConfigFileWithEnv [("SOURCE_DIR", "/path/to/src")] "config"
    assertBool (Map.lookup "HEADERS_DIR" vars == Just "/path/to/src/path/to/dir")
        $ "readConfigFileWithEnv:"
            ++ " Expected: " ++ show (Just "/path/to/src/path/to/dir")
            ++ " Got: " ++ show (Map.lookup "HEADERS_DIR" vars)
    assertBool (Map.lookup "CFLAGS" vars == Just "-O2 -I/path/to/src/path/to/dir -g")
        $ "readConfigFileWithEnv:"
            ++ " Expected: " ++ show (Just "-O2 -I/path/to/src/path/to/dir -g")
            ++ " Got: " ++ show (Map.lookup "CFLAGS" vars)
