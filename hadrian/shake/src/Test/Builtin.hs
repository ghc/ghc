{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Test.Builtin(main) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule
import System.Directory as IO
import qualified System.IO.Extra as IO
import qualified Data.ByteString.Char8 as BS
import Test.Type


-- WARNING: This code is also reproduced in "Development.Shake.Rule" as documentation.
-- If it needs editing, you probably need to edit it there too.

newtype File = File FilePath
    deriving (Show,Eq,Hashable,Binary,NFData,Typeable)
type instance RuleResult File = ()

data FileRule = FileRule File (Action ())
    deriving Typeable

addBuiltinFileRule :: Rules ()
addBuiltinFileRule = addBuiltinRule noLint noIdentity run
    where
        fileContents (File x) = do b <- IO.doesFileExist x; if b then IO.readFile' x else pure ""

        run :: BuiltinRun File ()
        run key old mode = do
            now <- liftIO $ fileContents key
            if mode == RunDependenciesSame && fmap BS.unpack old == Just now then
                pure $ RunResult ChangedNothing (BS.pack now) ()
            else do
                (_, act) <- getUserRuleOne key (const Nothing) $ \(FileRule k act) -> if k == key then Just act else Nothing
                act
                now <- liftIO $ fileContents key
                pure $ RunResult ChangedRecomputeDiff (BS.pack now) ()

fileRule :: FilePath -> Action () -> Rules ()
fileRule file act = addUserRule $ FileRule (File file) act

fileNeed :: FilePath -> Action ()
fileNeed = apply1 . File


main = testBuild test $ do
    addBuiltinFileRule

    fileRule "a.txt" $ pure ()
    fileRule "b.txt" $ do
        fileNeed "a.txt"
        liftIO $ appendFile "log.txt" "X"
        liftIO $ writeFile "b.txt" . reverse =<< readFile "a.txt"

    action $ fileNeed "b.txt"


test build = do
    writeFile "log.txt" ""
    writeFile "a.txt" "test"
    build []
    assertContents "b.txt" "tset"
    assertContents "log.txt" "X"

    build []
    assertContents "log.txt" "X" -- it doesn't rebuild

    writeFile "a.txt" "more"
    build []
    assertContents "b.txt" "erom"
